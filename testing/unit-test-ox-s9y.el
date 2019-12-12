;;; unit-test-ox-bb.el --- unit tests for BBCode Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Christian Garbs <mitch@cgarbs.de>
;; Licensed under GNU GPL v3 or later.

;; This file is part of ox-bb.

;; ox-bb is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ox-bb is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ox-bb.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; unit tests for ox-bb

;;; Code:

(require 'ox-bb)

;;;;;
;;;;; tests for internal methods
;;;;;

;;; org-bb--put-in-tag

(ert-deftest org-bb/put-in-tag/no-attribute ()
  (should (equal (org-bb--put-in-tag "p" "foo")
		 "<p>foo</p>")))

(ert-deftest org-bb/put-in-tag/single-attribute ()
  (should (equal (org-bb--put-in-tag "a" "foo" '(("href" "file.htm")))
		 "<a href=\"file.htm\">foo</a>")))

(ert-deftest org-bb/put-in-tag/multiple-attributes ()
  (should (equal (org-bb--put-in-tag "div" "foo" '(("class" "bar") ("style" "margin: 0;")))
		 "<div class=\"bar\" style=\"margin: 0;\">foo</div>")))

;;; org-bb--put-a-href

(ert-deftest org-bb/put-a-href/plain ()
  (should (equal (org-bb--put-a-href "some text" "https://example.com/")
		 "<a href=\"https://example.com/\">some text</a>")))

(ert-deftest org-bb/put-a-href/anchor ()
  (should (equal (org-bb--put-a-href "anchor text" "#anchor")
		 "<a href=\"#anchor\">anchor text</a>")))

(ert-deftest org-bb/put-a-href/encode-url-only-once ()
  (should (equal (org-bb--put-a-href "baz" "http://foo/%20bar")
		 "<a href=\"http://foo/%20bar\">baz</a>")))

(ert-deftest org-bb/put-a-href/with-class ()
  (should (equal (org-bb--put-a-href "some text" "https://example.com/" "myclass")
		 "<a href=\"https://example.com/\" class=\"myclass\">some text</a>")))

(ert-deftest org-bb/put-a-href/with-id ()
  (should (equal (org-bb--put-a-href "some text" "https://example.com/" "myclass" "myid")
		 "<a href=\"https://example.com/\" class=\"myclass\" id=\"myid\">some text</a>")))

;;; org-bb--remove-leading-newline

(ert-deftest org-bb/remove-leading-newline/remove ()
  (should (equal( org-bb--remove-leading-newline "\nsome text")
		"some text")))

(ert-deftest org-bb/remove-leading-newline/keep-text-before-first-newline ()
  (should (equal( org-bb--remove-leading-newline "no empty line\nsome more text\n")
		"no empty line\nsome more text\n")))

(ert-deftest org-bb/remove-leading-newline/only-remove-first-newline ()
  (should (equal( org-bb--remove-leading-newline "\n\nsome text")
		"\nsome text")))

(ert-deftest org-bb/remove-leading-newline/keep-newlines-within ()
  (should (equal( org-bb--remove-leading-newline "\nline 1\nline 2")
		"line 1\nline 2")))

(ert-deftest org-bb/remove-leading-newline/dont-fail-with-no-newline ()
  (should (equal( org-bb--remove-leading-newline "some text")
		"some text")))

;;; org-bb--remove-trailing-newline

(ert-deftest org-bb/remove-trailing-newline/remove ()
  (should (equal( org-bb--remove-trailing-newline "some text\n")
		"some text")))

(ert-deftest org-bb/remove-trailing-newline/keep-text-after-last-newline ()
  (should (equal( org-bb--remove-trailing-newline "some text\nno empty line")
		"some text\nno empty line")))

(ert-deftest org-bb/remove-trailing-newline/only-remove-last-newline ()
  (should (equal( org-bb--remove-trailing-newline "some text\n\n")
		"some text\n")))

(ert-deftest org-bb/remove-trailing-newline/keep-newlines-within ()
  (should (equal( org-bb--remove-trailing-newline "line 1\nline 2\n")
		"line 1\nline 2")))

(ert-deftest org-bb/remove-trailing-newline/dont-fail-with-no-newline ()
  (should (equal( org-bb--remove-trailing-newline "some text")
		"some text")))

;;; org- org-bb--map-to-geshi-language

(ert-deftest org-bb/map-to-geshi-language/unchanged ()
  (should (equal( org-bb--map-to-geshi-language "java")
		"java")))

(ert-deftest org-bb/map-to-geshi-language/changed ()
  (should (equal( org-bb--map-to-geshi-language "elisp")
		"lisp")))

(ert-deftest org-bb/map-to-geshi-language/nil ()
  (should (equal( org-bb--map-to-geshi-language nil)
		"plaintext")))

(ert-deftest org-bb/map-to-geshi-language/empty ()
  (should (equal( org-bb--map-to-geshi-language "")
		"plaintext")))


;;; Register file

(provide 'test-ox-bb)

;;; unit-test-ox-bb.el ends here
