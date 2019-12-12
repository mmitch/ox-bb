;;; ox-bb.el --- BBCode Back-End for Org Export Engine -*- lexical-binding: t; -*-

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

;; export Org documents to BBCode

; internal reminder: for Org format information see
; http://orgmode.org/worg/dev/org-element-api.html

;;; Code:

(require 'ox)

;;; Backend definition

(org-export-define-backend 'bb
  '((bold . org-bb-bold)
    (center-block . org-bb-undefined)
    (clock . org-bb-undefined)
    (code . org-bb-code)
    (drawer . org-bb-undefined)
    (dynamic-block . org-bb-undefined)
    (entity . org-bb-entity)
    (example-block . org-bb-undefined)
    (export-block . org-bb-undefined)
    (export-snippet . org-bb-undefined)
    (fixed-width . org-bb-fixed-width)
    (footnote-definition . org-bb-footnote-definition)
    (footnote-reference . org-bb-footnote-reference)
    (headline . org-bb-headline)
    (horizontal-rule . org-bb-undefined)
    (inline-src-block . org-bb-undefined)
    (inlinetask . org-bb-undefined)
    (inner-template . org-bb-inner-template)
    (italic . org-bb-italic)
    (item . org-bb-item)
    (keyword . org-bb-undefined)
    (latex-environment . org-bb-undefined)
    (latex-fragment . org-bb-undefined)
    (line-break . org-bb-line-break)
    (link . org-bb-link)
    (node-property . org-bb-undefined)
    (paragraph . org-bb-paragraph)
    (plain-list . org-bb-plain-list)
    (plain-text . org-bb-plain-text)
    (planning . org-bb-undefined)
    (property-drawer . org-bb-undefined)
    (quote-block . org-bb-quote-block)
    (radio-target . org-bb-undefined)
    (section . org-bb-section)
    (special-block . org-bb-undefined)
    (src-block . org-bb-geshi-block)
    (statistics-cookie . org-bb-undefined)
    (strike-through . org-bb-strike-through)
    (subscript . org-bb-undefined)
    (superscript . org-bb-undefined)
    (table . org-bb-undefined)
    (table-cell . org-bb-undefined)
    (table-row . org-bb-undefined)
    (target . org-bb-undefined)
    (template . org-bb-template)
    (timestamp . org-bb-undefined)
    (underline . org-bb-underline)
    (verbatim . org-bb-verbatim)
    (verse-block . org-bb-undefined))
  :menu-entry
  '(?S "Export to BBCode"
       ((?H "As BBCode buffer" org-bb-export-as-bbcode)
	(?h "As BBCode file" org-bb-export-to-bbcode))))

;;; Customization

(defgroup org-export-bb nil
  "Options for exporting Org mode files to BBCode."
  :tag "Org Export BBCode"
  :group 'org-export)

;;; Helper methods

(defun org-bb--put-in-tag (tag contents &optional attributes)
  "Puts the BBcode tag TAG around the CONTENTS string.
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
    (format "[%s%s]%s[/%s]" tag attribute-string contents tag)))

(defun org-bb--put-in-value-tag (tag contents value)
  "Puts the BBcode tag TAG given the VALUE around the CONTENTS string."
  (format "[%s=%s]%s[/%s]" tag value contents tag))

(defun org-bb--fix-url (url)
  "Fix URL returned from `url-encode-url'.
Older versions of Emacs (eg. 24.3 used in the Travis CI minimal
image) prepend \"/\" to urls consisting only of an \"#anchor\"
part.  We don't want this, because we need relative anchors.  Fix
this the hard way."
  (if (string-prefix-p "/#" url)
      (substring url 1)
    url))

(defun org-bb--put-url (contents href)
  "Puts the CONTENTS inside a [url] tag pointing to HREF.
Automagically escapes the target URL."
  (let* ((target (org-bb--fix-url (url-encode-url (org-link-unescape href)))))
    (org-bb--put-in-value-tag "url" contents target)))

(defun org-bb--remove-leading-newline (text)
  "Remove a leading empty line from TEXT."
  (replace-regexp-in-string "\\`\n" "" text))

(defun org-bb--remove-trailing-newline (text)
  "Remove the trailing newline from TEXT."
  (replace-regexp-in-string "\n\\'" "" text))

(defun org-bb--map-to-geshi-language (language)
  "Map LANGUAGE from Org to Geshi."
  (cond ((string= language "elisp") "lisp")
	((string= language "shell") "bash")
	((string= language "sh")    "bash")
	((string= language "") "plaintext")
	(language)
	(t "plaintext")))

;;; Backend callbacks

(defun org-bb-bold (_bold contents _info)
  "Transcode a BOLD element from Org to Serendipity.
CONTENTS is the bold text, as a string.  INFO is
  a plist used as a communication channel."
  (org-bb--put-in-tag "b" contents))

(defun org-bb-code (code _contents _info)
  "Transcode a CODE element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (org-bb--put-in-value-tag "font" (org-element-property :value code) "monospace"))

(defun org-bb-entity (entity _contents _info)
  "Transcode an ENTITY element from Org to Serendipity.
CONTENTS is the definition itself.  INFO is a plist used as a
communication channel."
  (org-element-property :html entity))

(defun org-bb-geshi-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to Serendipity GeSHi plugin.
CONTENTS is nil.  INFO is a plist holding
contextual information."
  (format "[geshi lang=%s]%s[/geshi]"
	  (org-bb--map-to-geshi-language (org-element-property :language code-block))
	  (org-bb--remove-trailing-newline
	   (org-export-format-code-default code-block info))))

(defun org-bb-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "[geshi lang=plaintext]%s[/geshi]"
	  (org-bb--remove-leading-newline
	   (org-bb--remove-trailing-newline
	    (org-element-property :value fixed-width)))))

(defun org-bb-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (eq (org-element-property :type footnote-reference) 'inline)
      (error "Inline footnotes not supported yet")
    (let ((n (org-export-get-footnote-number footnote-reference info)))
      (format "^%d " n))))

(defun org-bb-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn))
	(def (cdr fn)))
    (format "^%d: %s" n def)))

(defun org-bb-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    info (plist-get info :parse-tree)))
	 (fn-alist
	  (cl-loop for (n _label raw) in fn-alist collect
		   (cons n (org-trim (org-export-data raw info)))))
	 (text (mapconcat 'org-bb-format-footnote-definition fn-alist "\n")))
    (if fn-alist
	(concat "\n[u]Footnotes[/u]\n\n" text)
      "")))

(defun org-bb-headline (headline contents info)
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
	 (org-bb--put-in-tag (format "h%d" level) title))
       "\n"
       contents))))

(defun org-bb-inner-template (contents info)
  "Return body of document string after Serendipity conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   contents
   (org-bb-footnote-section info)))

(defun org-bb-italic (_italic contents _info)
  "Transcode a ITALIC element from Org to Serendipity.
CONTENTS is the italic text, as a string.  INFO is
  a plist used as a communication channel."
  (org-bb--put-in-tag "em" contents))

(defun org-bb-item (item contents info)
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
	 (org-bb--put-in-tag "dt" (org-trim term))
	 "\n"
	 (org-bb--put-in-tag "dd" (org-trim contents))
	 ))
       (_
	(org-bb--put-in-tag "li" (org-trim contents) value)))
     "\n")))

(defun org-bb-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Serendipity.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  "<br>\n")

(defun org-bb-link (link contents _info)
  "Transcode a LINK element from Org to Serendipity.
CONTENTS is the contents of the link, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link))
	(raw  (org-element-property :raw-link link)))
    (cond
     ((string= type "fuzzy")
      (cond
       ((string-prefix-p "about:" raw)
	(org-bb--put-url contents raw))
       (t (error "Unknown fuzzy LINK type encountered: `%s'" raw))))
     ((member type '("http" "https"))
      (org-bb--put-url contents (concat type ":" path)))
     (t (error "LINK type `%s' not yet supported" type)))))

(defun org-bb-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Serendipity.
CONTENTS is the contents of the paragraph, as a string.  INFO is
  a plist used as a communication channel."
  (org-trim contents))

(defun org-bb-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Serendipity.
CONTENTS is the contents of the plain-list, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type plain-list)))
    (concat
     (pcase type
       (`descriptive (org-bb--put-in-tag "dl" (org-trim contents)))
       (`unordered (org-bb--put-in-tag "ul" (org-trim contents)))
       (`ordered (org-bb--put-in-tag "ol" (org-trim contents)))
       (other (error "PLAIN-LIST type `%s' not yet supported" other)))
     "\n")))

(defun org-bb-plain-text (text _info)
  "Transcode a TEXT string from Org to Serendipity.
INFO is a plist used as a communication channel."
  text)

(defun org-bb-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Serendipity.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (org-bb--put-in-tag "blockquote" contents))

(defun org-bb-section (_section contents _info)
  "Transcode a SECTION element from Org to Serendipity.
CONTENTS is the contents of the section, as a string.  INFO is a
  plist used as a communication channel."
  (org-trim contents))

(defun org-bb-strike-through (_strike-through contents _info)
  "Transcode a STRIKE-THROUGH element from Org to Serendipity.
CONTENTS is the text with strike-through markup, as a string.
  INFO is a plist used as a communication channel."
  (org-bb--put-in-tag "s" contents))

(defun org-bb-template (contents _info)
  "Return complete document string after Serendipity conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun org-bb-undefined (element &optional _contents _info)
  "Throw an error when an unsupported ELEMENT is encountered."
  (error "ELEMENT type `%s' not implemented yet" (car element)))

(defun org-bb-underline (_underline contents _info)
  "Transcode a UNDERLINE element from Org to Serendipity.
CONTENTS is the underlined text, as a string.  INFO is
  a plist used as a communication channel."
  (org-bb--put-in-tag "u" contents))

(defun org-bb-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM element from Org to Serendipity.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (org-bb--put-in-tag "code" (org-element-property :value verbatim)))

;;; Export methods

;;;###autoload
(defun org-bb-export-as-bbcode
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a BBCode buffer.

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

Export is done in a buffer named \"*Org BBCode Export*\"."
  (interactive)
  (org-export-to-buffer 'bb "*Org BBCode Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (bbcode-mode))))

;;;###autoload
(defun org-bb-export-to-bbcode
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a BBCode file.

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
  (let* ((extension ".bbcode")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'bb file
      async subtreep visible-only body-only ext-plist)))

;;; Register file

(provide 'ox-bb)

;;; ox-bb.el ends here
