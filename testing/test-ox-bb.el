;;; test-ox-bb.el --- Tests BBCode Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019, 2021  Christian Garbs <mitch@cgarbs.de>
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

;; Tests ox-bb.el.

;;; Code:

(require 'ox-bb)

;;;;;
;;;;; helper functions
;;;;;

(defun test-ox-bb-verbatim-regression ()
  "Return t if verbatim blocks generate an extra newline.
This is a possible regression in Org introduced with 7d9e4da447
which was released with Org 9.1.14.  See
https://lists.gnu.org/archive/html/emacs-orgmode/2021-01/msg00338.html
for details."
  (not (version< (org-release) "9.1.14")))

(defun test-ox-bb-remove-final-newline (text)
  "Remove the final newline from TEXT."
  (replace-regexp-in-string "\n\\'" "" text))

(defun test-ox-bb-export (input)
  "Transform INPUT to BBCode and return the result."
  (with-temp-buffer
    (org-mode)
    (insert input)
    (ox-bb-export-as-bbcode)
    (with-current-buffer "*Org BBCode Export*"
      (test-ox-bb-remove-final-newline (buffer-substring-no-properties (point-min) (point-max))))))

;;;;;
;;;;; tests of internal methods
;;;;;

;;; ox-bb--as-block

(ert-deftest org-bb/as-block/plain ()
  (should (equal( ox-bb--as-block "some text\nline two")
		"\nsome text\nline two\n")))

;;; ox-bb--force-leading-newline

(ert-deftest org-bb/force-leading-newline/add-missing-newline ()
  (should (equal( ox-bb--force-leading-newline "some text")
		"\nsome text")))

(ert-deftest org-bb/force-leading-newline/keep-existing-newline ()
  (should (equal( ox-bb--force-leading-newline "\nonly one newline")
		"\nonly one newline")))

(ert-deftest org-bb/force-leading-newline/remove-additional-newlines ()
  (should (equal( ox-bb--force-leading-newline "\n\nsome text")
		"\nsome text")))

(ert-deftest org-bb/force-leading-newline/keep-newlines-within ()
  (should (equal( ox-bb--force-leading-newline "\nline 1\nline 2\n")
		"\nline 1\nline 2\n")))

;;; ox-bb--format-headline

(ert-deftest org-bb/format-headline/level-0 ()
  (should (equal( ox-bb--format-headline "some text" 0)
		"[b][u]some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-1 ()
  (should (equal( ox-bb--format-headline "some text" 1)
		"[b][u]# some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-2 ()
  (should (equal( ox-bb--format-headline "some text" 2)
		"[b][u]== some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-3 ()
  (should (equal( ox-bb--format-headline "some text" 3)
		"[b][u]+++ some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-4 ()
  (should (equal( ox-bb--format-headline "some text" 4)
		"[b][u]:::: some text[/u][/b]\n\n")))

(ert-deftest org-bb/format-headline/level-5 ()
  (should (equal( ox-bb--format-headline "some text" 5)
		"[b][u]----- some text[/u][/b]\n\n")))

;;; ox-bb--put-in-tag

(ert-deftest org-bb/put-in-tag/no-attribute ()
  (should (equal (ox-bb--put-in-tag "p" "foo")
		 "[p]foo[/p]")))

(ert-deftest org-bb/put-in-tag/single-attribute ()
  (should (equal (ox-bb--put-in-tag "style" "foo" '(("size" "30px")))
		 "[style size=\"30px\"]foo[/style]")))

(ert-deftest org-bb/put-in-tag/multiple-attributes ()
  (should (equal (ox-bb--put-in-tag "style" "foo" '(("color" "#00FF00") ("size" "30px")))
		 "[style color=\"#00FF00\" size=\"30px\"]foo[/style]")))

;;; ox-bb--put-in-value-tag

(ert-deftest org-bb/put-in-value-tag/plain ()
  (should (equal (ox-bb--put-in-value-tag "url" "foo" "file.htm")
		 "[url=file.htm]foo[/url]")))

;;; ox-bb--put-url

(ert-deftest org-bb/put-url/plain ()
  (should (equal (ox-bb--put-url "some text" "https://example.com/")
		 "[url=https://example.com/]some text[/url]")))

(ert-deftest org-bb/put-url/empty ()
  (should (equal (ox-bb--put-url nil "https://example.com/")
		 "[url=https://example.com/]https://example.com/[/url]")))

(ert-deftest org-bb/put-url/anchor ()
  (should (equal (ox-bb--put-url "anchor text" "#anchor")
		 "[url=#anchor]anchor text[/url]")))

(ert-deftest org-bb/put-url/encode-url-only-once ()
  (should (equal (ox-bb--put-url "baz" "http://foo/%20bar")
		 "[url=http://foo/%20bar]baz[/url]")))

;;; ox-bb--remove-leading-newline

(ert-deftest org-bb/remove-leading-newline/remove ()
  (should (equal( ox-bb--remove-leading-newline "\nsome text")
		"some text")))

(ert-deftest org-bb/remove-leading-newline/keep-text-before-first-newline ()
  (should (equal( ox-bb--remove-leading-newline "no empty line\nsome more text\n")
		"no empty line\nsome more text\n")))

(ert-deftest org-bb/remove-leading-newline/only-remove-first-newline ()
  (should (equal( ox-bb--remove-leading-newline "\n\nsome text")
		"\nsome text")))

(ert-deftest org-bb/remove-leading-newline/keep-newlines-within ()
  (should (equal( ox-bb--remove-leading-newline "\nline 1\nline 2")
		"line 1\nline 2")))

(ert-deftest org-bb/remove-leading-newline/dont-fail-with-no-newline ()
  (should (equal( ox-bb--remove-leading-newline "some text")
		"some text")))

;;; ox-bb--remove-trailing-newline

(ert-deftest org-bb/remove-trailing-newline/remove ()
  (should (equal( ox-bb--remove-trailing-newline "some text\n")
		"some text")))

(ert-deftest org-bb/remove-trailing-newline/keep-text-after-last-newline ()
  (should (equal( ox-bb--remove-trailing-newline "some text\nno empty line")
		"some text\nno empty line")))

(ert-deftest org-bb/remove-trailing-newline/only-remove-last-newline ()
  (should (equal( ox-bb--remove-trailing-newline "some text\n\n")
		"some text\n")))

(ert-deftest org-bb/remove-trailing-newline/keep-newlines-within ()
  (should (equal( ox-bb--remove-trailing-newline "line 1\nline 2\n")
		"line 1\nline 2")))

(ert-deftest org-bb/remove-trailing-newline/dont-fail-with-no-newline ()
  (should (equal( ox-bb--remove-trailing-newline "some text")
		"some text")))

;;; org- ox-bb--map-to-geshi-language

(ert-deftest org-bb/map-to-geshi-language/unchanged ()
  (should (equal( ox-bb--map-to-geshi-language "java")
		"java")))

(ert-deftest org-bb/map-to-geshi-language/changed ()
  (should (equal( ox-bb--map-to-geshi-language "elisp")
		"lisp")))

(ert-deftest org-bb/map-to-geshi-language/nil ()
  (should (equal( ox-bb--map-to-geshi-language nil)
		"plaintext")))

(ert-deftest org-bb/map-to-geshi-language/empty ()
  (should (equal( ox-bb--map-to-geshi-language "")
		"plaintext")))


;;;;;
;;;;; whole-file export tests
;;;;;

(ert-deftest org-bb/export-bold ()
  (should (equal (test-ox-bb-export "foo *BAR* baz")
		 "foo [b]BAR[/b] baz")))

(ert-deftest org-bb/export-code ()
  (should (equal (test-ox-bb-export "foo ~BAR~ baz")
		 "foo [font=monospace]BAR[/font] baz")))

(ert-deftest org-bb/export-entity ()
  (should (equal (test-ox-bb-export "This is *bold* and this is in \\ast{}asterisks\\ast{}.")
		 "This is [b]bold[/b] and this is in &lowast;asterisks&lowast;.")))

(ert-deftest org-bb/export-fixed-width ()
  (should (equal (test-ox-bb-export "paragraph 1

: verbatim line
:   indented verbatim line

paragraph 2")
		 (if (test-ox-bb-verbatim-regression)
		     "paragraph 1

[code]
verbatim line
  indented verbatim line[/code]


paragraph 2"
		   "paragraph 1

[code]
verbatim line
  indented verbatim line
[/code]

paragraph 2"))))

(ert-deftest org-bb/export-footnote-multiple ()
  (should (equal (test-ox-bb-export "foo[fn:1] bar[fn:2]
* Footnotes

[fn:1] foo
[fn:2] bar")
		 "foo^1  bar^2

[b][u]Footnotes[/u][/b]

^1: foo
^2: bar")))

(ert-deftest org-bb/export-footnote-plain ()
  (should (equal (test-ox-bb-export "bar[fn:1]
* Footnotes

[fn:1] foo")
		 "bar^1

[b][u]Footnotes[/u][/b]

^1: foo")))

(ert-deftest org-bb/export-geshi-block-without-language ()
  (should (equal (test-ox-bb-export "#+BEGIN_SRC
package foo;
/* dummy dummy */
#+END_SRC")
		 "[geshi lang=plaintext]package foo;
/* dummy dummy */[/geshi]")))

(ert-deftest org-bb/export-geshi-block ()
  (should (equal (test-ox-bb-export "#+BEGIN_SRC java
package foo;
/* dummy dummy */
#+END_SRC")
		 "[geshi lang=java]package foo;
/* dummy dummy */[/geshi]")))

(ert-deftest org-bb/export-headline-lv1 ()
  (should (equal (test-ox-bb-export "* TOPIC")
		 "[b][u]# TOPIC[/u][/b]")))

(ert-deftest org-bb/export-headline-lv2 ()
  (should (equal (test-ox-bb-export "* dummy
** TOPIC")
		 "[b][u]# dummy[/u][/b]

[b][u]== TOPIC[/u][/b]")))

(ert-deftest org-bb/export-headline-lv3 ()
  (should (equal (test-ox-bb-export "* dummy
** dummy
*** TOPIC")
		 "[b][u]# dummy[/u][/b]

[b][u]== dummy[/u][/b]

[b][u]+++ TOPIC[/u][/b]")))

(ert-deftest org-bb/export-headline-lv4 ()
  (should (equal (test-ox-bb-export "* dummy
** dummy
*** dummy
**** TOPIC")
		 "[b][u]# dummy[/u][/b]

[b][u]== dummy[/u][/b]

[b][u]+++ dummy[/u][/b]

[b][u]:::: TOPIC[/u][/b]")))

(ert-deftest org-bb/export-headline-lv5 ()
  (should (equal (test-ox-bb-export "* dummy
** dummy
*** dummy
**** dummy
***** TOPIC")
		 "[b][u]# dummy[/u][/b]

[b][u]== dummy[/u][/b]

[b][u]+++ dummy[/u][/b]

[b][u]:::: dummy[/u][/b]

[b][u]----- TOPIC[/u][/b]")))

(ert-deftest org-bb/export-italic ()
  (should (equal (test-ox-bb-export "foo /BAR/ baz")
		 "foo [i]BAR[/i] baz")))

(ert-deftest org-bb/export-line-break ()
  (should (equal (test-ox-bb-export "foo\\\\
bar")
		 "foo[br]_[/br]
bar")))

(ert-deftest org-bb/export-link-about ()
  (should (equal (test-ox-bb-export "[[about:config][bar]]")
		 "[url=about:config]bar[/url]")))

(ert-deftest org-bb/export-link-empty ()
  (should (equal (test-ox-bb-export "http://example.com/")
		 "[url=http://example.com/]http://example.com/[/url]")))

(ert-deftest org-bb/export-link-encode-url-only-once ()
  (should (equal (test-ox-bb-export "[[http://foo/%20bar][baz]]")
		 "[url=http://foo/%20bar]baz[/url]")))

(ert-deftest org-bb/export-link-encode-url ()
  (should (equal (test-ox-bb-export "[[http://foo/ bar][baz]]")
		 "[url=http://foo/%20bar]baz[/url]")))

(ert-deftest org-bb/export-link-http ()
  (should (equal (test-ox-bb-export "[[http://foo/][bar]]")
		 "[url=http://foo/]bar[/url]")))

(ert-deftest org-bb/export-link-https ()
  (should (equal (test-ox-bb-export "[[https://foo/][bar]]")
		 "[url=https://foo/]bar[/url]")))

(ert-deftest org-bb/export-multiline-paragraph ()
  (should (equal (test-ox-bb-export "foo
bar")
		 "foo
bar")))

(ert-deftest org-bb/export-multiple-paragraphs ()
  (should (equal (test-ox-bb-export "foo

bar")
		 "foo

bar")))

(ert-deftest org-bb/export-table-plain ()
  (should (equal (test-ox-bb-export "| A1 | B1 |
|----+----|
| A2 | B2 |
|----+----|")
		 "[table][tr][td]A1[/td][td]B1[/td][/tr]
[tr][td]A2[/td][td]B2[/td][/tr]
[/table]")))

(ert-deftest org-bb/export-table-markup ()
  (should (equal (test-ox-bb-export "| *A1* | /B1/ |
| A2   | [[http://localhost/][B2]] x |")
		 "[table][tr][td][b]A1[/b][/td][td][i]B1[/i][/td][/tr]
[tr][td]A2[/td][td][url=http://localhost/]B2[/url] x[/td][/tr]
[/table]")))

(ert-deftest org-bb/export-plain-list-descriptive ()
  (should (equal (test-ox-bb-export "- foo :: pokey
- bar :: hokey")
		 "[list]
[*][i]foo:[/i] pokey
[*][i]bar:[/i] hokey
[/list]")))

(ert-deftest org-bb/export-plain-list-ordered ()
  (should (equal (test-ox-bb-export "1. foo
2. bar")
		 "[list=1]
[*]foo
[*]bar
[/list]")))

(ert-deftest org-bb/export-plain-list-unordered ()
  (should (equal (test-ox-bb-export "- foo
- bar")
		 "[list]
[*]foo
[*]bar
[/list]")))

(ert-deftest org-bb/export-quote-block ()
  (should (equal (test-ox-bb-export "#+BEGIN_QUOTE
Somebody
said
this.
#+END_QUOTE")
		 "[quote]
Somebody
said
this.
[/quote]")))

(ert-deftest org-bb/export-single-paragraph ()
  (should (equal (test-ox-bb-export "foo")
		 "foo")))

(ert-deftest org-bb/export-strike-through ()
  (should (equal (test-ox-bb-export "foo +BAR+ baz")
		 "foo [s]BAR[/s] baz")))

(ert-deftest org-bb/export-underline ()
  (should (equal (test-ox-bb-export "foo _BAR_ baz")
		 "foo [u]BAR[/u] baz")))

(ert-deftest org-bb/export-verbatim ()
  (should (equal (test-ox-bb-export "foo =BAR= baz")
		 "foo [font=monospace]BAR[/font] baz")))

;;; register file

(provide 'test-ox-bb)

;;; test-ox-bb.el ends here
