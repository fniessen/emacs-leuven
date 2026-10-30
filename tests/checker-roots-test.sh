#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
directory=$(mktemp -d)
trap 'rm -rf -- "$directory"' EXIT

mkdir -p "$directory/repo/org" "$directory/repo/subdir" "$directory/bin" \
    "$directory/outside"
git init --quiet "$directory/repo"
printf '* Heading\n' >"$directory/repo/org/headings.org"
printf 'https://example.invalid/checker-root-test\n' >"$directory/repo/link"
cat >"$directory/bin/curl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${!#}" >>"$CURL_CALLS"
if [[ ${CURL_MODE:-} == fail ||
      ( ${CURL_MODE:-} == mixed && ${!#} == *[02468] ) ]]; then
    printf '404'
    exit 0
elif [[ ${CURL_MODE:-} == network-error ]]; then
    printf 'Simulated network failure\n' >&2
    exit 7
fi
printf '200'
EOF
chmod +x "$directory/bin/curl"
export PATH="$directory/bin:$PATH"
export CURL_CALLS="$directory/curl-calls"

check_case() {
    local name=$1 cwd=$2 expected_status=$3 expected_output=$4
    shift 4
    local output status=0
    output=$(cd -- "$cwd" && "$@" 2>&1) || status=$?
    if [[ "$status" != "$expected_status" || "$output" != "$expected_output" ]]; then
        printf 'FAIL: %s\nExpected status: %s\nActual status: %s\nExpected output:\n%s\nActual output:\n%s\n' \
            "$name" "$expected_status" "$status" "$expected_output" "$output" >&2
        exit 1
    fi
    printf 'PASS: %s\n' "$name"
}

for checker in check-links check-org-structure; do
    ln -s "$ROOT/bin/$checker" "$directory/bin/$checker"
    if [[ "$checker" == check-links ]]; then
        expected=$'Checking 1 unique HTTP(S) links...\nProgress: 0/1 links checked (0%), 0/10 errors.\nProgress: 1/1 links checked (100%), 0/10 errors.\nAll 1 unique HTTP(S) links returned a successful status.'
    else
        expected='Org heading levels are valid in 1 file(s).'
    fi
    check_case "$checker uses the caller's repository" "$directory/repo" \
        0 "$expected" "$ROOT/bin/$checker"
    check_case "$checker uses the repository root from a subdirectory via PATH" \
        "$directory/repo/subdir" 0 "$expected" "$checker"

    status=0
    output=$(cd -- "$directory/outside" && "$checker" 2>&1) || status=$?
    if [[ "$status" != 2 || "$output" != *'Run this checker from inside a Git working tree'* ]]; then
        printf 'FAIL: %s must reject a directory outside Git (status %s):\n%s\n' \
            "$checker" "$status" "$output" >&2
        exit 1
    fi
    printf 'PASS: %s rejects a directory outside Git\n' "$checker"
done

check_case 'explicit Org directory still works outside Git' "$directory/outside" \
    0 'Org heading levels are valid in 1 file(s).' \
    check-org-structure "$directory/repo/org"

if [[ $(cat "$CURL_CALLS") != $'https://example.invalid/checker-root-test\nhttps://example.invalid/checker-root-test' ]]; then
    printf 'FAIL: link checker scanned outside the test repository.\n' >&2
    exit 1
fi
printf 'PASS: only links from the caller repository were requested\n'

check_links_case() {
    local name=$1 count=$2 mode=$3 expected_status=$4 expected_calls=$5
    local status=0
    : >"$directory/repo/link"
    : >"$CURL_CALLS"
    for ((i = 1; i <= count; i++)); do
        printf 'https://example.invalid/%03d\n' "$i"
    done >"$directory/repo/link"
    output=$(cd -- "$directory/repo" &&
        CURL_MODE="$mode" "$ROOT/bin/check-links" 2>&1) || status=$?
    if [[ "$status" != "$expected_status" ||
          $(wc -l <"$CURL_CALLS") != "$expected_calls" ]]; then
        printf 'FAIL: %s (status %s, expected %s; calls %s, expected %s)\n%s\n' \
            "$name" "$status" "$expected_status" \
            "$(wc -l <"$CURL_CALLS")" "$expected_calls" "$output" >&2
        exit 1
    fi
    printf 'PASS: %s\n' "$name"
}

check_links_case 'limit to 100 requests' 105 success 0 100
[[ "$output" == *'Checking 100 of 105 unique HTTP(S) links (limit: 100)'* ]]
[[ "$output" == *'All 100 unique HTTP(S) links returned a successful status.'* ]]
[[ "$output" == *'Partial check: 5 of 105 unique HTTP(S) links were not checked.'* ]]
[[ "$output" == *'Progress: 0/100 links checked (0%), 0/10 errors.'* ]]
[[ "$output" == *'Progress: 8/100 links checked (8%), 0/10 errors.'* ]]
[[ "$output" == *'Progress: 100/100 links checked (100%), 0/10 errors.'* ]]
[[ $(LC_ALL=C sort "$CURL_CALLS" | tail -n 1) == 'https://example.invalid/100' ]]

check_links_case 'stop at exactly 10 HTTP failures' 105 fail 1 10
[[ "$output" == *'10 link(s) failed out of 10 checked.'* ]]
[[ "$output" == *'Stopped after 10 errors.'* ]]
[[ "$output" == *'Partial check: 95 of 105 unique HTTP(S) links were not checked.'* ]]
[[ "$output" == *'referenced in link:'* ]]
[[ "$output" == *'Progress: 10/100 links checked (10%), 10/10 errors.'* ]]

check_links_case 'network failures count toward the error limit' 105 network-error 1 10
[[ "$output" == *'curl exited with status 7'* ]]

check_links_case 'mixed results stop at the tenth failure without overshoot' 105 mixed 1 20
[[ "$output" == *'10 link(s) failed out of 20 checked.'* ]]
[[ "$output" == *'Partial check: 85 of 105 unique HTTP(S) links were not checked.'* ]]
[[ "$output" == *'Progress: 20/100 links checked (20%), 10/10 errors.'* ]]

check_links_case 'fewer than 10 failures still fail the check' 3 fail 1 3
[[ "$output" == *'3 link(s) failed out of 3 checked.'* ]]
[[ "$output" != *'Partial check:'* ]]
[[ "$output" == *'Progress: 3/3 links checked (100%), 3/10 errors.'* ]]
[[ "$output" != *'Stopped after'* ]]

check_links_case 'empty repository makes no requests' 0 success 0 0
[[ "$output" == *'No static HTTP(S) links found.'* ]]
[[ "$output" != *'Partial check:'* ]]
[[ "$output" != *'Progress:'* ]]
