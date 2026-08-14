#!/usr/bin/env bash

#? bak, Time-stamp: <2026-08-14 Fri 13:52>

##? Usage: bak FILE
##?
##? Create a quick back-up copy of a file.
##?
##? Arguments:
##?   FILE                          file to copy

# {{{ Boilerplate

# Exit on undefined variables and pipeline errors.
set -uo pipefail

# BSD sysexits(3) exit codes.
readonly EX_OK=0
readonly EX_USAGE=64

# }}}

# {{{ Variables

n=10

# }}}

# {{{ Functions

say_hello() {
    local name="$1"
    echo "Hello, ${name}!"
}

write_log() {
    local message="$1"
    printf '%s\n' "$message" >> "$LOG_FILE"
}

# }}}

echo "Enter Your Name"
read name
echo "Welcome $name to the system"

if [ $n -lt 10 ];
then
    echo "It is a one digit number"
else
    echo "It is a two digit number"
fi
