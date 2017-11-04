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

(ert-deftest org-s9y/put-in-tag/plain ()
  (should (equal (org-s9y--put-in-tag "p" "foo") "<p>foo</p>")))
