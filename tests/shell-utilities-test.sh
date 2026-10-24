#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
directory=$(mktemp -d)
trap 'rm -f -- "$directory/literal:20"; rmdir -- "$directory"' EXIT

emacsclient() {
    printf '<%s>\n' "$@"
    return "${client_status:-0}"
}

export SSH_CONNECTION=
shopt -s expand_aliases
alias e='emacsclient -n'
# shellcheck source=/dev/null
source "$ROOT/bin/rc-common-settings-emacs-leuven"
# shellcheck source=/dev/null
source "$ROOT/bin/rc-common-settings-emacs-leuven"

check_case() {
    local name=$1 expected=$2
    shift 2
    local output
    output=$(e "$@")
    if [[ "$output" != "$expected" ]]; then
        printf 'FAIL: %s\nExpected:\n%s\nActual:\n%s\n' \
            "$name" "$expected" "$output" >&2
        exit 1
    fi
    printf 'PASS: %s\n' "$name"
}

check_case 'file:line' $'<-n>\n<+20>\n<calendar.org>' calendar.org:20
check_case 'spaces and multiple files' \
    $'<-n>\n<+42>\n<mes notes.org>\n<+35>\n<tasks.org>' \
    'mes notes.org:42' tasks.org:35
check_case 'native arguments' $'<-n>\n<+20>\n<calendar.org>\n<.>' +20 calendar.org .
check_case 'no arguments' '<-n>'
check_case 'non-numeric suffix' $'<-n>\n<calendar.org:abc>' calendar.org:abc

touch "$directory/literal:20"
check_case 'existing colon filename' "<-n>
<$directory/literal:20>" "$directory/literal:20"

client_status=7
status=0
e calendar.org:20 >/dev/null || status=$?
if ((status != 7)); then
    printf 'FAIL: emacsclient exit status: expected 7, got %d\n' "$status" >&2
    exit 1
fi
printf 'PASS: emacsclient exit status\n'
