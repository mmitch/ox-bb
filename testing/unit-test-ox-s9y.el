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

(ert-deftest org-s9y/put-a-href/anchor ()
  (should (equal (org-s9y--put-a-href "anchor text" "#anchor")
		 "<a href=\"#anchor\">anchor text</a>")))

(ert-deftest org-s9y/put-a-href/encode-url-only-once ()
  (should (equal (org-s9y--put-a-href "baz" "http://foo/%20bar")
		 "<a href=\"http://foo/%20bar\">baz</a>")))

(ert-deftest org-s9y/put-a-href/with-class ()
  (should (equal (org-s9y--put-a-href "some text" "https://example.com/" "myclass")
		 "<a href=\"https://example.com/\" class=\"myclass\">some text</a>")))

(ert-deftest org-s9y/put-a-href/with-id ()
  (should (equal (org-s9y--put-a-href "some text" "https://example.com/" "myclass" "myid")
		 "<a href=\"https://example.com/\" class=\"myclass\" id=\"myid\">some text</a>")))

;;; org-s9y--remove-leading-newline

(ert-deftest org-s9y/remove-leading-newline/remove ()
  (should (equal( org-s9y--remove-leading-newline "\nsome text")
		"some text")))

(ert-deftest org-s9y/remove-leading-newline/keep-text-before-first-newline ()
  (should (equal( org-s9y--remove-leading-newline "no empty line\nsome more text\n")
		"no empty line\nsome more text\n")))

(ert-deftest org-s9y/remove-leading-newline/only-remove-first-newline ()
  (should (equal( org-s9y--remove-leading-newline "\n\nsome text")
		"\nsome text")))

(ert-deftest org-s9y/remove-leading-newline/keep-newlines-within ()
  (should (equal( org-s9y--remove-leading-newline "\nline 1\nline 2")
		"line 1\nline 2")))

(ert-deftest org-s9y/remove-leading-newline/dont-fail-with-no-newline ()
  (should (equal( org-s9y--remove-leading-newline "some text")
		"some text")))

;;; org-s9y--remove-trailing-newline

(ert-deftest org-s9y/remove-trailing-newline/remove ()
  (should (equal( org-s9y--remove-trailing-newline "some text\n")
		"some text")))

(ert-deftest org-s9y/remove-trailing-newline/keep-text-after-last-newline ()
  (should (equal( org-s9y--remove-trailing-newline "some text\nno empty line")
		"some text\nno empty line")))

(ert-deftest org-s9y/remove-trailing-newline/only-remove-last-newline ()
  (should (equal( org-s9y--remove-trailing-newline "some text\n\n")
		"some text\n")))

(ert-deftest org-s9y/remove-trailing-newline/keep-newlines-within ()
  (should (equal( org-s9y--remove-trailing-newline "line 1\nline 2\n")
		"line 1\nline 2")))

(ert-deftest org-s9y/remove-trailing-newline/dont-fail-with-no-newline ()
  (should (equal( org-s9y--remove-trailing-newline "some text")
		"some text")))

;;; org- org-s9y--map-to-geshi-language

(ert-deftest org-s9y/map-to-geshi-language/unchanged ()
  (should (equal( org-s9y--map-to-geshi-language "java")
		"java")))

(ert-deftest org-s9y/map-to-geshi-language/changed ()
  (should (equal( org-s9y--map-to-geshi-language "elisp")
		"lisp")))

(ert-deftest org-s9y/map-to-geshi-language/nil ()
  (should (equal( org-s9y--map-to-geshi-language nil)
		"plaintext")))

(ert-deftest org-s9y/map-to-geshi-language/empty ()
  (should (equal( org-s9y--map-to-geshi-language "")
		"plaintext")))


;;; Register file

(provide 'test-ox-s9y)

;;; unit-test-ox-s9y.el ends here
