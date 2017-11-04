;;; test-ox-s9y.el --- unit tests for Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Christian Garbs <mitch@cgarbs.de>
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

(require 'ox-s9y)

(defmacro org-s9y-export-temp-text (text)
    "Create a temporary buffer with Org mode as the active mode
holding TEXT and export it with 'ox-s9y, returning the export
result as a string."
    (with-temp-buffer
      (org-mode)
      (insert text)
      (org-export-as 's9y nil nil nil nil)))
      
(ert-deftest org-s9y/put-in-tag/plain ()
  (should (equal (org-s9y--put-in-tag "p" "foo") "<p>foo</p>")))

(ert-deftest org-s9y/link/http ()
  (should (equal (org-s9y-export-temp-text "[[http://foo/][bar]]")
		 "<p><a href=\"http://foo/\">bar</a></p>\n")))

(ert-deftest org-s9y/link/https ()
  (should (equal (org-s9y-export-temp-text "[[https://foo/][bar]]")
		 "<p><a href=\"https://foo/\">bar</a></p>\n")))

(ert-deftest org-s9y/link/encode-url ()
  (should (equal (org-s9y-export-temp-text "[[http://foo/ bar][baz]]")
		 "<p><a href=\"http://foo/%20bar\">baz</a></p>\n")))

(ert-deftest org-s9y/link/encode-url-only-once ()
  (should (equal (org-s9y-export-temp-text "[[http://foo/%20bar][baz]]")
		 "<p><a href=\"http://foo/%20bar\">baz</a></p>\n")))
