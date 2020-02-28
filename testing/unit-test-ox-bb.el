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

;;; org-bb--as-block

(ert-deftest org-bb/as-block/plain ()
  (should (equal( org-bb--as-block "some text\nline two")
		"\nsome text\nline two\n")))

;;; org-bb--force-leading-newline

(ert-deftest org-bb/force-leading-newline/add-missing-newline ()
  (should (equal( org-bb--force-leading-newline "some text")
		"\nsome text")))

(ert-deftest org-bb/force-leading-newline/keep-existing-newline ()
  (should (equal( org-bb--force-leading-newline "\nonly one newline")
		"\nonly one newline")))

(ert-deftest org-bb/force-leading-newline/remove-additional-newlines ()
  (should (equal( org-bb--force-leading-newline "\n\nsome text")
		"\nsome text")))

(ert-deftest org-bb/force-leading-newline/keep-newlines-within ()
  (should (equal( org-bb--force-leading-newline "\nline 1\nline 2\n")
		"\nline 1\nline 2\n")))

;;; org-bb--format-headline

(ert-deftest org-bb/format-headline/level-0 ()
  (should (equal( org-bb--format-headline "some text" 0)
		"[b][u]some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-1 ()
  (should (equal( org-bb--format-headline "some text" 1)
		"[b][u]# some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-2 ()
  (should (equal( org-bb--format-headline "some text" 2)
		"[b][u]== some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-3 ()
  (should (equal( org-bb--format-headline "some text" 3)
		"[b][u]+++ some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-4 ()
  (should (equal( org-bb--format-headline "some text" 4)
		"[b][u]:::: some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-5 ()
  (should (equal( org-bb--format-headline "some text" 5)
		"[b][u]----- some text[/u][/b]\n\n")))

;;; org-bb--put-in-tag

(ert-deftest org-bb/put-in-tag/no-attribute ()
  (should (equal (org-bb--put-in-tag "p" "foo")
		 "[p]foo[/p]")))

(ert-deftest org-bb/put-in-tag/single-attribute ()
  (should (equal (org-bb--put-in-tag "style" "foo" '(("size" "30px")))
		 "[style size=\"30px\"]foo[/style]")))

(ert-deftest org-bb/put-in-tag/multiple-attributes ()
  (should (equal (org-bb--put-in-tag "style" "foo" '(("color" "#00FF00") ("size" "30px")))
		 "[style color=\"#00FF00\" size=\"30px\"]foo[/style]")))

;;; org-bb--put-in-value-tag

(ert-deftest org-bb/put-in-value-tag/plain ()
  (should (equal (org-bb--put-in-value-tag "url" "foo" "file.htm")
		 "[url=file.htm]foo[/url]")))

;;; org-bb--put-url

(ert-deftest org-bb/put-url/plain ()
  (should (equal (org-bb--put-url "some text" "https://example.com/")
		 "[url=https://example.com/]some text[/url]")))

(ert-deftest org-bb/put-url/empty ()
  (should (equal (org-bb--put-url nil "https://example.com/")
		 "[url=https://example.com/]https://example.com/[/url]")))

(ert-deftest org-bb/put-url/anchor ()
  (should (equal (org-bb--put-url "anchor text" "#anchor")
		 "[url=#anchor]anchor text[/url]")))

(ert-deftest org-bb/put-url/encode-url-only-once ()
  (should (equal (org-bb--put-url "baz" "http://foo/%20bar")
		 "[url=http://foo/%20bar]baz[/url]")))

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
