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
(org-babel-load-file (expand-file-name "~/.emacs.d/mattdean.org"))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (setq ido-use-filename-at-point nil)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(blink-cursor-mode nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (anki-card)))
 '(custom-safe-themes (quote ("a0615ea67652404396ac6094b31985077b9190b0a8ed62946a6ff2cb75cea023" "269d4b2e233f4b1476f7bf234cc3e1b812f20d7291d0832a9c4a8419102c12bf" "5b8427150bbc2207dae30edc6b20134245a315607a0f2fa3b2a9bc902cf34548" "5893a583d8c983d5af3cdd37068815c2c7748b14389b40bd4523bcd693a2fee3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(magit-diff-use-overlays nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900") (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600") (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65") (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba") (340 . "#268fc6") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
