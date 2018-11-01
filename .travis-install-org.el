(defun install-orgmode ()
  "Install orgmode package from orgmode ELPA."
  (progn
   (require 'package)
   (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'org)
   (package-initialize)))

(defun use-orgmode ()
  "Use orgmode package by initializing package."
  (progn
   (require 'package)
   (package-initialize)))
