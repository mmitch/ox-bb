;;; ox-s9y.el --- Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

(require 'ox)

(org-export-define-derived-backend 's9y 'html
  :menu-entry
  '(?S "Export to Serendipity"
       ((?H "As HTML buffer" org-html-export-as-html)
	(?h "As HTML file" org-html-export-to-html)
	)))
