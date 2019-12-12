# Makefile for ox-bb
#
# Copyright (C) 2017-2019  Christian Garbs <mitch@cgarbs.de>
# Licensed under GNU GPL v3 or later.
#
# This file is part of ox-bb.
#
# ox-bb is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ox-bb is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ox-bb.  If not, see <http://www.gnu.org/licenses/>.

SOURCES=$(wildcard *.el)
TARGETS=$(addsuffix c,$(SOURCES))
UNITTESTS=$(basename $(wildcard testing/unit-test-*.el))
TESTS=$(basename $(wildcard testing/unit-test-*.el))

EMACS=emacs -Q --batch -l .travis-install-org.el -f use-orgmode

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

testing/unit-test-%: %.elc testing/unit-test-%.el
	$(EMACS) -l ert -l $< -l $@.el -f ert-run-tests-batch-and-exit

all:	compile test

compile: $(TARGETS)

test: show-org-version unit-test integration-test

show-org-version:
	$(EMACS) -f org-version

unit-test: $(UNITTESTS)

integration-test: $(TARGETS)
	./run-tests.sh $<

clean:
	rm -f *.elc
	rm -f *~ testing/*~
