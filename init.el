(package-initialize nil)
;; Add any load-path customizations here if needed.
;; For example: (add-to-list 'load-path "~/.emacs.d/forks/org-mode/lisp")
(package-initialize t)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/mattdean.org"))

;; (require 'package)
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

;; (defvar my-packages '(better-defaults
;; 		      clojure-mode
;; 		      clojure-test-mode
;;                       paredit
;;                       rainbow-delimiters
;; 		      cider
;;                       clj-refactor))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; (add-hook 'cider-mode-hook 'paredit-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; (add-hook 'clojure-mode-hook 'paredit-mode)

;; (setq inhibit-startup-message t
;;   inhibit-startup-echo-area-message t)

;; (setq ido-use-filename-at-point nil)

;; (load-theme 'solarized-dark t)

;; (setq next-line-add-newlines nil)

;; (setq ns-right-option-modifier 'super)

;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; (defun ido-goto-symbol (&optional symbol-list)
;;       "Refresh imenu and jump to a place in the buffer using Ido."
;;       (interactive)
;;       (unless (featurep 'imenu)
;;         (require 'imenu nil t))
;;       (cond
;;        ((not symbol-list)
;;         (let ((ido-mode ido-mode)
;;               (ido-enable-flex-matching
;;                (if (boundp 'ido-enable-flex-matching)
;;                    ido-enable-flex-matching t))
;;               name-and-pos symbol-names position)
;;           (unless ido-mode
;;             (ido-mode 1)
;;             (setq ido-enable-flex-matching t))
;;           (while (progn
;;                    (imenu--cleanup)
;;                    (setq imenu--index-alist nil)
;;                    (ido-goto-symbol (imenu--make-index-alist))
;;                    (setq selected-symbol
;;                          (ido-completing-read "Symbol? " symbol-names))
;;                    (string= (car imenu--rescan-item) selected-symbol)))
;;           (unless (and (boundp 'mark-active) mark-active)
;;             (push-mark nil t nil))
;;           (setq position (cdr (assoc selected-symbol name-and-pos)))
;;           (cond
;;            ((overlayp position)
;;             (goto-char (overlay-start position)))
;;            (t
;;             (goto-char position)))))
;;        ((listp symbol-list)
;;         (dolist (symbol symbol-list)
;;           (let (name position)
;;             (cond
;;              ((and (listp symbol) (imenu--subalist-p symbol))
;;               (ido-goto-symbol symbol))
;;              ((listp symbol)
;;               (setq name (car symbol))
;;               (setq position (cdr symbol)))
;;              ((stringp symbol)
;;               (setq name symbol)
;;               (setq position
;;                     (get-text-property 1 'org-imenu-marker symbol))))
;;             (unless (or (null position) (null name)
;;                         (string= (car imenu--rescan-item) name))
;;               (add-to-list 'symbol-names name)
;;               (add-to-list 'name-and-pos (cons name position))))))))

;; (global-set-key (kbd "M-i") 'ido-goto-symbol)

;; ; (yas/global-mode t)

;; (require 'clj-refactor)

;; (setq cljr-sort-comparator 'cljr--semantic-comparator)

;; (add-hook 'clojure-mode-hook (lambda ()
;;                                (clj-refactor-mode 1)
;;                                ;; insert keybinding here
;;                                (cljr-add-keybindings-with-prefix "C-c C-m")))

;; (setq coffee-tab-width 2)
;; (setq coffee-args-compile '("-c" "--bare"))

;; (projectile-global-mode)

;; (setq projectile-enable-caching nil)

;; (require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-cc" 'org-capture)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cb" 'org-iswitchb)
;; (setq org-log-done t)

;; (setq org-hide-leading-stars t)

;; ;; automatically clean up bad whitespace
;; (setq whitespace-action '(auto-cleanup))

;; ;; only show bad whitespace
;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

;; (add-hook 'coffee-mode-hook 'whitespace-mode)

;; ;; This is particularly important when watching files via grunt --
;; ;; don't want autosave file to trigger the watchers.
;; (setq backup-directory-alist
;;           `((".*" . ,temporary-file-directory)))

;; (setq auto-save-file-name-transforms
;;           `((".*" ,temporary-file-directory t)))

;; (setq create-lockfiles nil)
