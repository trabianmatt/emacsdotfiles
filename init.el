(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(js2-basic-offset 2 t)
 '(magit-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/dev/fcfcu/fcfcu.org" "~/dev/sffcu/banking/sffcu.org" "~/dev/first_tech/mobile/first-tech.org" "~/dev/vantage/banking/vantage.org" "~/org/work.org" "~/org/personal.org" "~/.emacs.d/config.org"))))

(require 'cask "/usr/local/Cellar/cask/0.6.0/cask.el")
(cask-initialize)
(require 'pallet)

(defvar org-mode-packages '(org))

(require 'org)

(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
