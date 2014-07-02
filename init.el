(package-initialize nil)
;; Add any load-path customizations here if needed.
;; For example: (add-to-list 'load-path "~/.emacs.d/forks/org-mode/lisp")
(package-initialize t)

;; org must be updated before starting any org-mode related functionality
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(defvar org-mode-packages '(org))

(when (not package-archive-contents)
   (package-refresh-contents))

(dolist (p org-mode-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'org)

(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/dev/first_tech/mobile/first-tech.org" "~/dev/vantage/banking/ob.org" "~/org/work.org" "~/org/personal.org" "~/.emacs.d/config.org"))))

