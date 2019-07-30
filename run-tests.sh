#!/bin/bash
#
# Integration test runner for ox-s9y
#
# Copyright (C) 2018, 2019  Christian Garbs <mitch@cgarbs.de>
# Licensed under GNU GPL v3 or later.
#
# This file is part of ox-s9y.
#
# ox-s9y is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ox-s9y is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ox-s9y.  If not, see <http://www.gnu.org/licenses/>.

set -e

TEMPFILE="$(mktemp)"

remove_last_output()
{
    { [ "$OUTPUT" ] && [ -e "$OUTPUT" ] && rm "$OUTPUT" ; } || true
}

remove_last_output_and_tempfile()
{
    remove_last_output
    rm "$TEMPFILE"
}

travis_start_fold()
{
    { [ "$TRAVIS" = 'true' ] && travis_fold start "$1" ; } || true
}

travis_end_fold()
{
    { [ "$TRAVIS" = 'true' ] && travis_fold end "$1" ; } || true
}

travis_start_timer()
{
    { [ "$TRAVIS" = 'true' ] && travis_time_start ; } || true
}

travis_end_timer()
{
    { [ "$TRAVIS" = 'true' ] && travis_time_finish ; } || true
}

# don't leave stray files, even when we die
trap remove_last_output_and_tempfile EXIT

# set up colors
if tput sgr0 >/dev/null 2>&1; then
    RED=$(tput setaf 1)
    GREEN=$(tput setaf 2)
    YELLOW=$(tput setaf 3)
    WHITE=$(tput setaf 7)
    BOLD=$(tput bold)
    RESET=$(tput sgr0)
else
    RED=
    GREEN=
    YELLOW=
    WHITE=
    BOLD=
    RESET=
fi

# transform modules to load arguments for emacs
LIBS=(-l .travis-install-org.el -f use-orgmode)
while [[ $# -gt 0 ]]; do
    LIBS+=(-l "$1")
    shift
done

# run all tests
TOTAL=0
FAILED=0
for INPUT in testing/test-*.input; do
    TEST="${INPUT%.input}"
    EXPECTED="${TEST}.output"
    OUTPUT="${TEST}.html"

    travis_start_timer

    echo -n "running $TEST "
    emacs -Q --batch "${LIBS[@]}" "$INPUT" -f org-version -f org-s9y-export-to-html > "$TEMPFILE" 2>&1
    if diff -b -Narup "$EXPECTED" "$OUTPUT" >> "$TEMPFILE"; then
	echo "${GREEN}OK${RESET}"
    else
	echo "${RED}${BOLD}FAILED${RESET}${YELLOW}"
	travis_start_fold "$INPUT"
	cat "$TEMPFILE"
	travis_end_fold "$INPUT"
	echo "${RESET}"
	FAILED=$(( FAILED + 1))
    fi
    remove_last_output
    TOTAL=$(( TOTAL + 1 ))

    travis_end_timer
done

# report findings
if [[ $FAILED -eq 0 ]]; then
    echo "${BOLD}${GREEN}All $TOTAL tests OK.${RESET}"
    exit 0
else
    echo "${BOLD}${RED}$FAILED of $TOTAL tests failed.${RESET}"
    exit 1
fi
