;;; unit-test-ox-s9y.el --- unit tests for Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Christian Garbs <mitch@cgarbs.de>
;; Licensed under GNU GPL v3 or later.

;; This file is part of ox-s9y.

;; ox-s9y is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ox-s9y is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ox-s9y.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; unit tests for ox-s9y

;;; Code:

(require 'ox-s9y)

;;;;;
;;;;; tests for internal methods
;;;;;

;;; org-s9y--put-in-tag

(ert-deftest org-s9y/put-in-tag/no-attribute ()
  (should (equal (org-s9y--put-in-tag "p" "foo")
		 "<p>foo</p>")))

(ert-deftest org-s9y/put-in-tag/single-attribute ()
  (should (equal (org-s9y--put-in-tag "a" "foo" '(("href" "file.htm")))
		 "<a href=\"file.htm\">foo</a>")))

(ert-deftest org-s9y/put-in-tag/multiple-attributes ()
  (should (equal (org-s9y--put-in-tag "div" "foo" '(("class" "bar") ("style" "margin: 0;")))
		 "<div class=\"bar\" style=\"margin: 0;\">foo</div>")))

;;; org-s9y--put-a-href

(ert-deftest org-s9y/put-a-href/plain ()
  (should (equal (org-s9y--put-a-href "some text" "https://example.com/")
		 "<a href=\"https://example.com/\">some text</a>")))

(ert-deftest org-s9y/put-a-href/encode-url-only-once ()
  (should (equal (org-s9y--put-a-href "baz" "http://foo/%20bar")
		 "<a href=\"http://foo/%20bar\">baz</a>")))

;;; org-s9y--remove-trailing-newline

(ert-deftest org-s9y/remove-trailing-newline/remove ()
  (should (equal( org-s9y--remove-trailing-newline "some text\n")
		"some text")))

(ert-deftest org-s9y/remove-trailing-newline/only-remove-last-newline ()
  (should (equal( org-s9y--remove-trailing-newline "some text\n\n")
		"some text\n")))

(ert-deftest org-s9y/remove-trailing-newline/keep-newlines-within ()
  (should (equal( org-s9y--remove-trailing-newline "line 1\nline 2\n")
		"line 1\nline 2")))

(ert-deftest org-s9y/remove-trailing-newline/dont-fail-with-no-newline ()
  (should (equal( org-s9y--remove-trailing-newline "some text")
		"some text")))



;;; Register file

(provide 'test-ox-s9y)

;;; unit-test-ox-s9y.el ends here
