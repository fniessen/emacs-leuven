#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
directory=$(mktemp -d)
trap 'rm -f -- "$directory/headings.org"; rmdir -- "$directory"' EXIT

check_case() {
    local name=$1 expected_status=$2 expected_output=$3
    local output status=0
    cat >"$directory/headings.org"
    output=$(cd -- "$directory" && bash "$ROOT/bin/check-org-structure" "${4:-$directory}") || status=$?
    if [[ "$status" != "$expected_status" || "$output" != "$expected_output" ]]; then
        printf 'FAIL: %s\nExpected status: %s\nActual status: %s\nExpected output:\n%s\nActual output:\n%s\n' \
            "$name" "$expected_status" "$status" "$expected_output" "$output" >&2
        exit 1
    fi
    printf 'PASS: %s\n' "$name"
}

valid='Org heading levels are valid in 1 file(s).'
path=headings.org

check_case 'current directory is not included in error paths' 1 \
    "$path:1: level 2 follows level 0; expected at most level 1
Found 1 Org heading level error(s) in 1 file(s)." . <<'EOF'
** Invalid first heading
EOF

check_case 'inline task with END' 0 "$valid" <<'EOF'
* Heading
*************** TODO Inline task
Task description.
*************** END
** Child
EOF

check_case 'standalone and deeper inline tasks' 0 "$valid" <<'EOF'
*************** TODO Task before the first heading
* Heading
**************** TODO Deeper inline task
**************** END
** Child
*************** TODO Standalone task
*** Grandchild
EOF

check_case 'inline tasks do not hide outline errors' 1 \
    "$path:4: level 3 follows level 1; expected at most level 2
Found 1 Org heading level error(s) in 1 file(s)." <<'EOF'
* Heading
*************** TODO Inline task
*************** END
*** Skipped level
EOF

check_case 'inline task before an invalid first heading' 1 \
    "$path:2: level 2 follows level 0; expected at most level 1
Found 1 Org heading level error(s) in 1 file(s)." <<'EOF'
*************** TODO Inline task
** Invalid first heading
EOF

check_case 'level 14 is still an outline heading' 1 \
    "$path:2: level 14 follows level 1; expected at most level 2
Found 1 Org heading level error(s) in 1 file(s)." <<'EOF'
* Heading
************** Skipped level
EOF

check_case 'blocks remain ignored' 0 "$valid" <<'EOF'
* Heading
#+begin_src org
**** Not an outline heading
*************** TODO Not an inline task
#+end_src
** Child
EOF
