;;; ox-s9y.el --- Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Christian Garbs <mitch@cgarbs.de>
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

;; export Org documents to Serendipity blog

; internal reminder: for Org format information see
; http://orgmode.org/worg/dev/org-element-api.html

;;; Code:

(require 'ox)

;;; Backend definition

(org-export-define-backend 's9y
  '((bold . org-s9y-bold)
    (center-block . org-s9y-undefined)
    (clock . org-s9y-undefined)
    (code . org-s9y-code)
    (drawer . org-s9y-undefined)
    (dynamic-block . org-s9y-undefined)
    (entity . org-s9y-entity)
    (example-block . org-s9y-undefined)
    (export-block . org-s9y-undefined)
    (export-snippet . org-s9y-undefined)
    (fixed-width . org-s9y-undefined)
    (footnote-definition . org-s9y-footnote-definition)
    (footnote-reference . org-s9y-footnote-reference)
    (headline . org-s9y-headline)
    (horizontal-rule . org-s9y-undefined)
    (inline-src-block . org-s9y-undefined)
    (inlinetask . org-s9y-undefined)
    (inner-template . org-s9y-inner-template)
    (italic . org-s9y-italic)
    (item . org-s9y-item)
    (keyword . org-s9y-undefined)
    (latex-environment . org-s9y-undefined)
    (latex-fragment . org-s9y-undefined)
    (line-break . org-s9y-line-break)
    (link . org-s9y-link)
    (node-property . org-s9y-undefined)
    (paragraph . org-s9y-paragraph)
    (plain-list . org-s9y-plain-list)
    (plain-text . org-s9y-plain-text)
    (planning . org-s9y-undefined)
    (property-drawer . org-s9y-undefined)
    (quote-block . org-s9y-quote-block)
    (radio-target . org-s9y-undefined)
    (section . org-s9y-section)
    (special-block . org-s9y-undefined)
    (src-block . org-s9y-geshi-block)
    (statistics-cookie . org-s9y-undefined)
    (strike-through . org-s9y-undefined)
    (subscript . org-s9y-undefined)
    (superscript . org-s9y-undefined)
    (table . org-s9y-undefined)
    (table-cell . org-s9y-undefined)
    (table-row . org-s9y-undefined)
    (target . org-s9y-undefined)
    (template . org-s9y-template)
    (timestamp . org-s9y-undefined)
    (underline . org-s9y-underline)
    (verbatim . org-s9y-verbatim)
    (verse-block . org-s9y-undefined))
  :menu-entry
  '(?S "Export to Serendipity"
       ((?H "As HTML buffer" org-s9y-export-as-html)
	(?h "As HTML file" org-s9y-export-to-html))))

;;; Customization

(defgroup org-export-s9y nil
  "Options for exporting Org mode files to Serendipity."
  :tag "Org Export Serendipity"
  :group 'org-export)

(defcustom org-s9y-todo-link-title "Artikel folgt"
  "String to use as <abbr> title for todo: LINKS."
  :group 'org-export-s9y
  :type 'string)

;;; Helper methods

(defun org-s9y--put-in-tag (tag contents &optional attributes)
  "Puts the HTML tag TAG around the CONTENTS string.
Optional ATTRIBUTES for the tag can be given as an alist of
key/value pairs (both strings)."
  (let ((attribute-string (if attributes
			      (mapconcat (function (lambda (attribute)
						     (let ((key (car attribute))
							   (value (cadr attribute)))
						       (format " %s=\"%s\"" key value))))
					 attributes
					 "")
			    "")))
    (format "<%s%s>%s</%s>" tag attribute-string contents tag)))

(defun org-s9y--fix-url (url)
  "Fix URL returned from `url-encode-url'.
Older versions of Emacs (eg. 24.3 used in the Travis CI minimal
image) prepend \"/\" to urls consisting only of an \"#anchor\"
part.  We don't want this, because we need relative anchors.  Fix
this the hard way."
  (if (string-prefix-p "/#" url)
      (substring url 1)
    url))

(defun org-s9y--put-a-href (contents href &optional class id)
  "Puts the CONTENTS inside a simple <a> tag pointing to HREF.
Automagically escapes the target URL.  An optional CLASS and ID can be
set on the <a> tag."
  (let* ((target (org-s9y--fix-url (url-encode-url (org-link-unescape href))))
	 (attributes (list (list "href" target))))
    (when class
      (setq attributes (append attributes (list (list "class" class))))
      (when id
	(setq attributes (append attributes (list (list "id" id))))))
    (org-s9y--put-in-tag "a" contents attributes)))

(defun org-s9y--remove-trailing-newline (text)
  "Remove the trailing newline from TEXT."
  (replace-regexp-in-string "\n\\'" "" text))

(defun org-s9y--map-to-geshi-language (language)
  "Map LANGUAGE from Org to Geshi."
  (cond ((string= language "elisp") "lisp")
	((string= language "shell") "bash")
	((string= language "") "plaintext")
	(language)
	(t "plaintext")))

;;; Backend callbacks

(defun org-s9y-bold (_bold contents _info)
  "Transcode a BOLD element from Org to Serendipity.
CONTENTS is the bold text, as a string.  INFO is
  a plist used as a communication channel."
  (org-s9y--put-in-tag "strong" contents))

(defun org-s9y-code (code _contents _info)
  "Transcode a CODE element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (org-s9y--put-in-tag "code" (org-element-property :value code)))

(defun org-s9y-entity (entity _contents _info)
  "Transcode an ENTITY element from Org to Serendipity.
CONTENTS is the definition itself.  INFO is a plist used as a
communication channel."
  (org-element-property :html entity))

(defun org-s9y-geshi-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to Serendipity GeSHi plugin.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "[geshi lang=%s]%s[/geshi]"
	  (org-s9y--map-to-geshi-language (org-element-property :language code-block))
	  (org-s9y--remove-trailing-newline
	   (org-export-format-code-default code-block info))))

(defun org-s9y-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (eq (org-element-property :type footnote-reference) 'inline)
      (error "Inline footnotes not supported yet")
    (concat
     ;; Insert separator between two footnotes in a row.
     (let ((prev (org-export-get-previous-element footnote-reference info)))
       (when (eq (org-element-type prev) 'footnote-reference)
	 "<sup>, </sup>"))
     (let* ((n (org-export-get-footnote-number footnote-reference info))
	    (anchor-to (format "#fn-to-%d" n))
	    (anchor-from (when (org-export-footnote-first-reference-p footnote-reference info)
			   (format "fn-from-%d" n)))
	    (reftext (format "[%d]" n)))
       (org-s9y--put-in-tag
	"sup"
	(org-s9y--put-a-href reftext anchor-to "footnote" anchor-from))))))

(defun org-s9y-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let* ((n (car fn))
	 (def (cdr fn))
	 (n-format (format "[%d]" n))
	 (anchor-to (format "fn-to-%d" n))
	 (anchor-from (format "#fn-from-%d" n))
	 (definition (concat (org-s9y--put-a-href n-format anchor-from)
			     ": "
			     def)))
    (org-s9y--put-in-tag "div"
			 definition
			 (list (list "class" "footnote")
			       (list "id" anchor-to)))))

(defun org-s9y-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    info (plist-get info :parse-tree)))
	 (fn-alist
	  (cl-loop for (n _label raw) in fn-alist collect
		   (cons n (org-trim (org-export-data raw info)))))
	 (text (mapconcat 'org-s9y-format-footnote-definition fn-alist "\n")))
    (if fn-alist
	(org-s9y--put-in-tag "div" text '((id footnotes)))
      "")))

(defun org-s9y-headline (headline contents info)
  "Transcode HEADLINE element from Org to Serendipity.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info))
	(level (org-export-get-relative-level headline info)))
    (if (org-element-property :footnote-section-p headline)
	""
      (concat
       (if (<= level 2)
	   (format "<!--  %s  -->" title)
	 (org-s9y--put-in-tag (format "h%d" level) title))
       "\n"
       contents))))

(defun org-s9y-inner-template (contents info)
  "Return body of document string after Serendipity conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   contents
   (org-s9y-footnote-section info)))

(defun org-s9y-italic (_italic contents _info)
  "Transcode a ITALIC element from Org to Serendipity.
CONTENTS is the italic text, as a string.  INFO is
  a plist used as a communication channel."
  (org-s9y--put-in-tag "em" contents))

(defun org-s9y-item (item contents info)
  "Transcode a ITEM element from Org to Serendipity.
CONTENTS is the contents of the item, as a string.  INFO is
  a plist used as a communication channel."
  (let* ((plain-list (org-export-get-parent item))
	 (value (let ((counter (org-element-property :counter item)))
		  (if counter
		      (list (list "value" counter))
		    ())))
	 (term (let ((tag (org-element-property :tag item)))
		 (and tag (org-export-data tag info))))
	 (type (org-element-property :type plain-list)))
    (concat
     (pcase type
       (`descriptive
	(concat
	 (org-s9y--put-in-tag "dt" (org-trim term))
	 "\n"
	 (org-s9y--put-in-tag "dd" (org-trim contents))
	 ))
       (_
	(org-s9y--put-in-tag "li" (org-trim contents) value)))
     "\n")))

(defun org-s9y-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Serendipity.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  "<br>\n")

(defun org-s9y-link (link contents _info)
  "Transcode a LINK element from Org to Serendipity.
CONTENTS is the contents of the link, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link))
	(raw  (org-element-property :raw-link link)))
    (cond
     ((string= type "fuzzy")
      (cond
       ((string-prefix-p "todo:" raw)
	(org-s9y--put-in-tag "abbr" contents (list (list "title" org-s9y-todo-link-title))))
       ((string-prefix-p "about:" raw)
	(org-s9y--put-a-href contents raw))
       (t (error "Unknown fuzzy LINK type encountered: `%s'" raw))))
     ((member type '("http" "https"))
      (org-s9y--put-a-href contents (concat type ":" path)))
     (t (error "LINK type `%s' not yet supported" type)))))

(defun org-s9y-paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Serendipity.
CONTENTS is the contents of the paragraph, as a string.  INFO is
  a plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent)))
    (if (eq parent-type 'section)
	(org-s9y--put-in-tag "p" (org-trim contents))
      (org-trim contents))))

(defun org-s9y-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Serendipity.
CONTENTS is the contents of the plain-list, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type plain-list)))
    (concat
     (pcase type
       (`descriptive (org-s9y--put-in-tag "dl" (org-trim contents)))
       (`unordered (org-s9y--put-in-tag "ul" (org-trim contents)))
       (`ordered (org-s9y--put-in-tag "ol" (org-trim contents)))
       (other (error "PLAIN-LIST type `%s' not yet supported" other)))
     "\n")))

(defun org-s9y-plain-text (text _info)
  "Transcode a TEXT string from Org to Serendipity.
INFO is a plist used as a communication channel."
  text)

(defun org-s9y-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Serendipity.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (org-s9y--put-in-tag "blockquote" contents))

(defun org-s9y-section (_section contents _info)
  "Transcode a SECTION element from Org to Serendipity.
CONTENTS is the contents of the section, as a string.  INFO is a
  plist used as a communication channel."
  (org-trim contents))

(defun org-s9y-template (contents _info)
  "Return complete document string after Serendipity conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun org-s9y-undefined (element &optional _contents _info)
  "Throw an error when an unsupported ELEMENT is encountered."
  (error "ELEMENT type `%s' not implemented yet" (car element)))

(defun org-s9y-underline (_underline contents _info)
  "Transcode a UNDERLINE element from Org to Serendipity.
CONTENTS is the underlined text, as a string.  INFO is
  a plist used as a communication channel."
  (org-s9y--put-in-tag "u" contents))

(defun org-s9y-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (org-s9y--put-in-tag "code" (org-element-property :value verbatim)))

;;; Export methods

;;;###autoload
(defun org-s9y-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Serendipity HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org S9Y Export*\"."
  (interactive)
  (org-export-to-buffer 's9y "*Org S9Y Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun org-s9y-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Serendipity HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				   org-html-extension
				   "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 's9y file
      async subtreep visible-only body-only ext-plist)))

;;; Register file

(provide 'ox-s9y)

;;; ox-s9y.el ends here
