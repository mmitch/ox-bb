;;; ox-s9y.el --- Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

(require 'ox)

(org-export-define-derived-backend 's9y 'html
  :menu-entry
  '(?S "Export to Serendipity"
       ((?H "As HTML buffer" org-s9y-export-as-html)
	(?h "As HTML file" org-s9y-export-to-html)
	)))

(defun org-s9y-export-as-html
  (&optional async subtreep visible-only ext-plist)
  "Export current buffer to an Serendipity HTML buffer.

See `org-html-export-as-html` for details.  BODY-ONLY will always
be set to non-nil, restricting the output to the content of the
\"<body>\" tag."
  (let ((body-only t))
    (org-html-export-as-html async subtreep visible-only body-only ext-plist)))

(defun org-s9y-export-to-html
  (&optional async subtreep visible-only ext-plist)
  "Export current buffer to an Serendipity HTML file.

See `org-html-export-to-html` for details.  BODY-ONLY will always
be set to non-nil, restricting the output to the content of the
\"<body>\" tag."
  (let ((body-only t))
    (org-html-export-to-html async subtreep visible-only body-only ext-plist)))
