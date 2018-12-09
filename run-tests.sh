#!/bin/bash
#
# Integration test runner for ox-s9y
#
# Copyright (C) 2018  Christian Garbs <mitch@cgarbs.de>
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

clean_last_output()
{
    [ "$OUTPUT" ] && [ -e "$OUTPUT" ] && rm "$OUTPUT" || true
}

# don't leave stray files, even when we die
trap clean_last_output EXIT

# transform modules to load arguments for emacs
LIBS=
while [[ $# -gt 0 ]]; do
    LIBS="$LIBS -l $1"
    shift
done

# run all tests
TOTAL=0
FAILED=0
for INPUT in testing/test-*.input; do
    TEST="${INPUT%.input}"
    EXPECTED="${TEST}.output"
    OUTPUT="${TEST}.html"

    echo running $TEST...
    emacs -Q --batch $LIBS "$INPUT" -f org-s9y-export-to-html
    if ! diff -b --color=always -Narup "$EXPECTED" "$OUTPUT"; then
	FAILED=$(( FAILED + 1))
    fi
    clean_last_output
    TOTAL=$(( TOTAL + 1 ))
done

# report findings
if [[ $FAILED -eq 0 ]]; then
    echo All $TOTAL tests OK.
    exit 0
else
    echo $FAILED of $TOTAL tests failed.
    exit 1
fi
