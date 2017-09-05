;;; ox-s9y.el --- Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

(require 'ox)

(org-export-define-derived-backend 's9y 'html
  :menu-entry
  '(?S "Export to Serendipity"
       ((?H "As HTML buffer" org-s9y-export-as-html)
	(?h "As HTML file" org-s9y-export-to-html)))
  :translate-alist '((src-block . org-s9y-geshi-block)))

(defun org-s9y-geshi-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to Serendipity GeSHi plugin.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "[geshi lang=\"%s\"]
%s[/geshi]"
	  (org-element-property :language code-block)
	  (org-export-format-code-default code-block info)))

;;;###autoload
(defun org-s9y-export-as-html
  (&optional async subtreep visible-only ext-plist)
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org S9Y Export*\"."
  (interactive)
  (let ((body-only t))
    (org-export-to-buffer 's9y "*Org S9Y Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (html-mode)))))

;;;###autoload
(defun org-s9y-export-to-html
  (&optional async subtreep visible-only ext-plist)
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				    org-html-extension
				    "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system)
	 (body-only t))
    (org-export-to-file 's9y file
      async subtreep visible-only body-only ext-plist)))
