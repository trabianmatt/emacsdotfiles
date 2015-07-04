
(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Improve initial scratch
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; No alarms
(setq ring-bell-function 'ignore)

;; When on a tab, make the cursor the tab length.
(setq-default x-stretch-cursor t)

;; Keep emacs custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Save point position between sessions.
(use-package saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Full path in frame title.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Check for updated file every second
(setq auto-revert-interval 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(show-paren-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
;; Displayed as (L,C) in mode line
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 70
(setq fill-column 80)

(setq-default tab-width 2)

;; Smooth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) .1))) ;; one line at a time

;; (setq scroll-margin 1
;;       scroll-conservatively 1000
;;       scroll-up-agggressively 0.01
;;       scroll-down-aggressively 0.01)

;; Scroll one line when hitting bottom of window
;;(setq scroll-conservatively 10000)

;; Don't scroll back up to the point
;;(setq auto-window-vscroll nil)
;;(setq scroll-step 1)

;; Change cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Easily navigate SillyCased words
(global-subword-mode 1)
(diminish 'subword-mode)

;; Word Wrap (t is no wrap, nil is wrap)
(setq-default truncate-lines nil)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; Don't use shift to mark things
(setq shift-select-mode nil)

;; from 'better-defaults.el'
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))  

(setq create-lockfiles nil)

(setq whitespace-action '(auto-cleanup))

(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(bind-key "M-j" '(lambda () (interactive) (join-line -1)))

(bind-key "<s-backspace>" '(lambda () (interactive) (kill-line 0)))

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

(bind-key "<RET>" 'newline-and-indent)

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(setq my-dark-theme 'solarized-dark)
(setq my-light-theme 'solarized-light)

;; (setq my-dark-theme 'base16-solarized-dark)
;; (setq my-light-theme 'base16-solarized-light)

;; last t is for NO-ENABLE
(load-theme my-dark-theme t t)
(load-theme my-light-theme t t)

;; Use the default theme at the shell.
(defun my/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
      (enable-theme my-dark-theme)
    (disable-theme my-dark-theme)))

;; (my/pick-color-theme (selected-frame))

;; (add-hook 'after-make-frame-functions 'my/pick-color-theme)

(enable-theme my-dark-theme)

(defun my/toggle-theme-dark-light ()
  "Toggles the current theme between 'light' and 'dark' variants."
  (interactive)
  (if (custom-theme-enabled-p my-dark-theme)
      (progn
        (disable-theme my-dark-theme)
        (enable-theme my-light-theme))
    (progn
        (disable-theme my-light-theme)
        (enable-theme my-dark-theme))))

(set-face-attribute 'default nil
                    :family "Menlo" :height 130)

;;; Fall back to DejaVu Sans for unicode.
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12.4
                               :weight 'normal)))

;; window resizing ("M-s" is alt+command)
(bind-key "M-s-<left>" 'shrink-window-horizontally)
(bind-key "M-s-<right>" 'enlarge-window-horizontally)
(bind-key "M-s-<down>" 'shrink-window)
(bind-key "M-s-<up>" 'enlarge-window)

(defun my/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

(defun my/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(bind-key "C-x 2" 'my/vsplit-last-buffer)
(bind-key "C-x 3" 'my/hsplit-last-buffer)

;; The following seemed like a good idea at first but prevented using
;; M-# as a prefix.

;; (bind-key "M-0" 'delete-window)
;; (bind-key "M-1" 'delete-other-windows)
;; (bind-key "M-2" 'split-window-vertically)
;; (bind-key "M-3" 'split-window-horizontally)
;; (bind-key "M-4" 'kill-buffer-and-window)

(bind-key "M-=" 'balance-windows)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun my/kill-buffer-other-window ()
  (interactive)
  (kill-buffer
   (window-buffer
    (next-window))))

(setq ns-pop-up-frames nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq split-height-threshold 1
      split-width-threshold  1)

(defvar focus-mode-hook nil)

(defun set-buffer-margins ()
  (let* ((margins (if (not (window-margins))
                      (window-margins) '(0 0)))
         (full-width (+ (first margins)
                        (second margins)
                        (window-width)))
         (target-width (if (>= 80 full-width)
                           80 80)) ;full-width 80))
         (move-left 3)
         (margin (/ (- full-width target-width) 2)))
    (if (and focus-mode (not (car (window-margins))))
        (set-window-margins (car (get-buffer-window-list (current-buffer)))
                            (- margin move-left) (+ margin move-left)))))

(defun on-focus-mode-disable ()
  (set-window-margins (car (get-buffer-window-list (current-buffer))) 0 0)
  (remove-hook 'focus-mode-off-hook 'on-focus-mode-disable)
  (remove-hook 'window-configuration-change-hook 'set-buffer-margins))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c j" . ace-jump-mode)))

(use-package ace-window
  :ensure t
  :bind (("C-x w" . ace-window)))

(use-package ag
  :ensure t)

(use-package coffee-mode
  :ensure t
  :init
  (setq coffee-tab-width 2
        coffee-args-compile '("-c" "--bare"))
  :config
  (add-hook 'coffee-mode-hook 'whitespace-mode))

(use-package company
  :diminish company-mode
  :init
  (setq
   company-idle-delay 0.3
   company-tooltip-limit 20
   company-minimum-prefix-length 2
   company-auto-complete nil)
  :config
  (delete (rassoc 'tramp-completion-file-name-handler
                  file-name-handler-alist)
          file-name-handler-alist)

  (global-company-mode 1))

(use-package emmet-mode
  :diminish emmet-mode
  :ensure t
  :config
  
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)

  (defun component-tag? (str)
    (let ((case-fold-search nil))
      (string-match-p "^[[:upper:]]" str)))

  (defun emmet-make-jsx-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
    "Create JSX markup string"
    (emmet-aif
     (gethash tag-name emmet-tag-snippets-table)

     (let ((fn (if (stringp it)
                   (emmet-html-snippets-instantiate-lambda it)
                 it)))
       (prog1
           (funcall fn content)
         (puthash tag-name fn emmet-tag-snippets-table)))

     (let* ((id           (emmet-concat-or-empty " id=\"" tag-id "\""))
            (classes      (emmet-mapconcat-or-empty " className=\"" tag-classes " " "\""))
            (props        (let* ((tag-props-default
                                  (and settings (gethash "defaultAttr" settings)))
                                 (merged-tag-props
                                  (emmet-merge-tag-props
                                   tag-props-default
                                   tag-props)))
                            (emmet-mapconcat-or-empty
                             " " merged-tag-props " " nil
                             (lambda (prop)
                               (let ((key (car prop)))
                                 (concat (if (symbolp key) (symbol-name key) key)
                                         "=\"" (cadr prop) "\""))))))
            (content-multiline? (and content (string-match "\n" content)))
            (block-tag?         (or (and settings (gethash "block" settings)) (component-tag? tag-name)))
            (self-closing?      (and (not (or tag-txt content))
                                     (or (not tag-has-body?)
                                         (and settings (gethash "selfClosing" settings)))))
            (block-indentation? (or content-multiline? (and block-tag? content)))
            (lf                 (if block-indentation? "\n")))
       (concat "<" tag-name id classes props
               (if self-closing? "/>"
                 (concat ">"
                         (if tag-txt
                             (if block-indentation? 
                                 (emmet-indent tag-txt)
                               tag-txt))
                         (if content
                             (if block-indentation?
                                 (emmet-indent content)
                               content))
                         lf
                         "</" tag-name ">"))))))


  (defun emmet-default-filter ()
    "Default filter(s) to be used if none is specified."
    (let* ((file-ext (car (emmet-regex ".*\\(\\..*\\)" (or (buffer-file-name) "") 1)))
           (defaults '(".html" ("html")
                       ".htm"  ("html")
                       ".haml" ("haml")
                       ".jsx"  ("jsx")
                       ".clj"  ("hic")))
           (default-else      '("html"))
           (selected-default (member file-ext defaults)))
      (if selected-default
          (cadr selected-default)
        default-else)))

  (setq emmet-filters (append emmet-filters
                              '("jsx" (emmet-primary-filter emmet-make-jsx-tag)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package gist
  :ensure t)

(use-package git-messenger
  :ensure t
  :defer t
  :bind (("C-x v m" . git-messenger:popup-message)))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (setq guide-key/guide-key-sequence '("C-x r"
                                       "C-x 4"
                                       "C-c"
                                       "C-x c"
                                       "s-o"
                                       (org-mode "C-c C-x"))
        guide-key/highlight-command-regexp "rectangle"
        guide-key/idle-delay 1
        guide-key/popup-window-position :bottom
        guide-key/recursive-key-sequence-flag t)
  :config
  (guide-key-mode 1))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)) 
  :ensure t
  :config
  
  ;; Move up and down like isearch
  (bind-key "C-r" 'helm-previous-line helm-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-swoop-map)
  (bind-key "C-r" 'helm-previous-line helm-multi-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-multi-swoop-map))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (setq 
        helm-idle-delay 0.0
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t)
  (helm-mode)
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-c f" . helm-recentf)
         ("C-x C-f" . helm-find-files)
         ("C-c <SPC>" . helm-all-mark-rings)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-:" . helm-eval-expression-with-eldoc)
         ("C-h d" . helm-info-at-point)
         ("C-c g" . helm-google-suggest)
         ("C-h C-f" . helm-apropos))
  :init

  (bind-key "C-d" 'helm-ff-persistent-delete helm-find-files-map)
  (bind-key "C-d" 'helm-buffer-run-kill-persistent helm-buffer-map)

  ;; Tab is easier to reach than C-z and is used more often
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map) ; Work in terminal
  (bind-key "C-z" 'helm-select-action helm-map)

  (setq
   helm-google-suggest-use-curl-p t
   helm-apropos-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-completion-in-region-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-org-headings-fontify t
   helm-autoresize-max-height 80
   helm-autoresize-min-height 20
   helm-buffers-to-resize-on-pa '("*helm apropos*" "*helm ack-grep*"
                                  "*helm grep*" "*helm occur*"
                                  "*helm multi occur*" "*helm lsgit*"
                                  "*helm git-grep*" "*helm hg files*")
   helm-search-suggest-action-wikipedia-url
   "https://fr.wikipedia.org/wiki/Special:Search?search=%s"
   helm-wikipedia-suggest-url
   "http://fr.wikipedia.org/w/api.php?action=opensearch&search="
   helm-wikipedia-summary-url
   "http://fr.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page="))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-ls-git
  :ensure t)

(use-package hippie-expand
  :bind
  ("M-/" . hippie-expand))

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2
        js2-basic-offset 2
        js2-highlight-level 3)
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;; Restore windows after exiting magit
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/compare/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-current-remote)
                         "url"))
             (magit-get-current-branch))))

  (bind-key "V" 'endless/visit-pull-request-url magit-mode-map)

  ;; Use our magit-quit-session to restore window config
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-open-command "/usr/local/bin/mark"))

(use-package neotree
  :ensure t
  :init
  (setq projectile-switch-project-action
        'neotree-projectile-action)
  :bind (("C-c d" . neotree-toggle)))

(define-minor-mode npm-project-mode
  "A mode for navigating npm projects"
  :lighter " npm"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m") 'find-npm-module)
            map))

;; node.js
(defun resolve-npm-path (name)
  (let* ((command (format "node -e 'try { require.extensions[\".jsx\"] = function() {}; require(\"util\").print(require.resolve(\"%s\")); } catch(e) { e }'"
                          name name))
         (result (shell-command-to-string command)))
    result))

;; Find the module name on the current line. Currently looks for a
;; string between quotes (single or double.
(defun find-import-on-line ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "['\"]\\(.*\\)['\"]")
        (match-string 1))))

;; Build path to a new file. If no extension, use the extension of
;; the current buffer.
(defun new-npm-file-path (module-name)
  (if (file-name-extension module-name)
      module-name
    (let ((current-ext (file-name-extension (buffer-file-name))))
      (concat module-name "." current-ext))))

;; Find the imported module on the current line and open it. If it
;; doesn't exist, create a new buffer at that path.
(defun find-npm-module ()
  (interactive)
  (let ((module-name (find-import-on-line)))
    (if module-name
        (let* ((path (resolve-npm-path module-name))
               (file-name (if (and (not (= (length path) 0))
                                   (file-exists-p path))
                              path
                            (new-npm-file-path module-name))))
          (find-file file-name)))))

(add-hook 'web-mode-hook 'npm-project-mode)
(add-hook 'js-mode-hook 'npm-project-mode)
(add-hook 'css-mode-hook 'npm-project-mode)

(defun org/init-tasks()
  
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-use-fast-todo-selection t)

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))))

(defun org/init-capture ()
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")

  (setq org-capture-templates
        (quote (("t" "todo" entry (file org-default-notes-file)
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("r" "respond" entry (file org-default-notes-file)
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file org-default-notes-file)
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file org-default-notes-file)
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file org-default-notes-file)
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file org-default-notes-file)
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file org-default-notes-file)
                 "* NEXT %?\n%U\n%a\nSCHEDULED: <%<%Y-%m-%d %a .+1d/3d>>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

(defun org/init-refile ()
  ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  (setq org-refile-use-outline-path 'file)

  (setq org-outline-path-complete-in-steps t)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (setq org-completion-use-ido t)

  (setq org-indirect-buffer-display 'current-window))

(defun org/init-agenda-views ()

  (setq org-agenda-dim-blocked-tasks nil)

  (setq org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                (" " "Agenda"
                 ((agenda "" nil)
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil))))
                 nil))))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/list-sublevels-for-projects ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/toggle-next-task-display ()
    (interactive)
    (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
    (when  (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun bh/skip-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil))
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              nil)
             ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
             (t subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun bh/skip-project-trees-and-habits ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and bh/hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((bh/is-project-p)
          next-headline)
         ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (bh/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-project-tasks-maybe ()
    "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (limit-to-project (marker-buffer org-agenda-restrict-begin)))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (not limit-to-project)
               (bh/is-project-subtree-p))
          subtree-end)
         ((and limit-to-project
               (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits ()
    "Skip trees that are projects and tasks that are habits"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-subprojects ()
    "Skip trees that are not projects"
    (let ((next-headline (save-excursion (outline-next-heading))))
      (if (bh/is-subproject-p)
          nil
        next-headline))))

(defun org/init-archive ()
  
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")

  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline)))))

(defun org/init-babel ()

  ;; (setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8002/plantuml.8002.jar")

  (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (ditaa . t)
           (R . t)
           (python . t)
           (ruby . t)
           (gnuplot . t)
           (clojure . t)
           (sh . t)
           (ledger . t)
           (org . t)
           (plantuml . t)
           (latex . t))))

  (setq org-babel-results-keyword "results")

  (defun bh/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))

  (setq org-confirm-babel-evaluate nil)

  (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental))))

(defun org/init-clock ()

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;;
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)

  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)

  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)

  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)

  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  ;; (setq org-clock-frame-title-format (append '((t org-mode-line-string)) '(" ") frame-title-format)
  ;;       org-clock-clocked-in-display 'both)
  (setq org-clock-clocked-in-display 'both)

  ;; Capturing tasks quickly may lead to empty clocks. Remove empty
  ;; LOGBOOK drawers on clock out

  (defun bh/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

  (setq org-clock-out-remove-zero-time-clocks t)

  (setq bh/keep-clock-running nil)

  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (bh/is-project-p))
        "TODO"))))


  (defun bh/punch-in (arg)
    "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
    (interactive "p")
    (setq bh/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (bh/clock-in-organization-task-as-default)))
      ;;
      ;; We are not in the agenda
      ;;
      (save-restriction
        (widen)
                                        ; Find the tags on the current task
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))))

  (defun bh/punch-out ()
    (interactive)
    (setq bh/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

  (defun bh/clock-in-default-task ()
    (save-excursion
      (org-with-point-at org-clock-default-task
        (org-clock-in))))

  (defun bh/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in"
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when bh/keep-clock-running
              (bh/clock-in-default-task)))))))

  (defvar bh/organization-task-id "82E2E854-A3C7-46FD-AB3E-3F766E357F0B")

  (defun bh/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun bh/clock-out-maybe ()
    (when (and bh/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (bh/clock-in-parent-task)))

  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

  (defun bh/clock-in-last-task (arg)
    "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
    (interactive "p")
    (let ((clock-in-to-task
           (cond
            ((eq arg 4) org-clock-default-task)
            ((and (org-clock-is-active)
                  (equal org-clock-default-task (cadr org-clock-history)))
             (caddr org-clock-history))
            ((org-clock-is-active) (cadr org-clock-history))
            ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
            (t (car org-clock-history)))))
      (widen)
      (org-with-point-at clock-in-to-task
        (org-clock-in nil))))

  (setq org-time-stamp-rounding-minutes (quote (1 1)))

  (setq org-agenda-clock-consistency-checks
        (quote (:max-duration "4:00"
                              :min-duration 0
                              :max-gap 0
                              :gap-ok-around ("4:00"))))

  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80 :emphasize t)))

  ;; Agenda log mode items to display (closed and state changes by
  ;; default). Useful for reporting to others.
  (setq org-agenda-log-mode-items (quote (closed state)))
  
  ; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

  ; global Effort estimate values
  ; global STYLE property values for completion
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))))

(defun org/init-structure ()

  (setq org-enforce-todo-dependencies t)

  (setq org-hide-leading-stars nil)

  (setq org-startup-indented t)

  (setq org-pretty-entities nil)

  ;; The following setting hides blank lines between headings which
  ;; keeps folded view nice and compact.
  (setq org-cycle-separator-lines 0)

  ;; The following setting prevents creating blank lines before headings
  ;; but allows list items to adapt to existing blank lines around the
  ;; items:
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))


  (setq org-insert-heading-respect-content t)

  (setq org-reverse-note-order nil)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t))

(defun org/init-speed-commands ()

  (setq org-use-speed-commands t)

  (setq org-speed-commands-user (quote (("0" . ignore)
                                        ("1" . ignore)
                                        ("2" . ignore)
                                        ("3" . ignore)
                                        ("4" . ignore)
                                        ("5" . ignore)
                                        ("6" . ignore)
                                        ("7" . ignore)
                                        ("8" . ignore)
                                        ("9" . ignore)

                                        ("a" . ignore)
                                        ("d" . ignore)
                                        ("h" . bh/hide-other)
                                        ("i" progn
                                         (forward-char 1)
                                         (call-interactively 'org-insert-heading-respect-content))
                                        ("k" . org-kill-note-or-show-branches)
                                        ("l" . ignore)
                                        ("m" . ignore)
                                        ("q" . bh/show-org-agenda)
                                        ("r" . ignore)
                                        ("s" . org-save-all-org-buffers)
                                        ("w" . org-refile)
                                        ("x" . ignore)
                                        ("y" . ignore)
                                        ("z" . org-add-note)

                                        ("A" . ignore)
                                        ("B" . ignore)
                                        ("E" . ignore)
                                        ("F" . bh/restrict-to-file-or-follow)
                                        ("G" . ignore)
                                        ("H" . ignore)
                                        ("J" . org-clock-goto)
                                        ("K" . ignore)
                                        ("L" . ignore)
                                        ("M" . ignore)
                                        ("N" . bh/narrow-to-org-subtree)
                                        ("P" . bh/narrow-to-org-project)
                                        ("Q" . ignore)
                                        ("R" . ignore)
                                        ("S" . ignore)
                                        ("T" . bh/org-todo)
                                        ("U" . bh/narrow-up-one-org-level)
                                        ("V" . ignore)
                                        ("W" . bh/widen)
                                        ("X" . ignore)
                                        ("Y" . ignore)
                                        ("Z" . ignore))))

  (defun bh/show-org-agenda ()
    (interactive)
    (if org-agenda-sticky
        (switch-to-buffer "*Org Agenda( )*")
      (switch-to-buffer "*Org Agenda*"))
    (delete-other-windows)))

(defun org/init-tags ()

  ;; The tags between :startgroup and :endgroup are mutually exclusive
  (setq org-tag-alist (quote ((:startgroup)
                              ("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?H)
                              (:endgroup)
                              ("WAITING" . ?w)
                              ("HOLD" . ?h)
                              ("PERSONAL" . ?P)
                              ("WORK" . ?W)
                              ("ORG" . ?O)
                              ("NOTE" . ?n)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

  (defun kk/run-with-no-helm (orig-func &rest args)
    "Run a function without helm completion."
    (if (boundp 'helm-mode)
        (let ((orig-helm-mode helm-mode))
          (unwind-protect
              (progn
                (helm-mode 0)
                (apply orig-func args)
                )
            (helm-mode (if orig-helm-mode 1 0))))
      (apply orig-func args)))
  
  (advice-add 'org-icompleting-read :around 'kk/run-with-no-helm)
  (advice-add 'org-completing-read :around 'kk/run-with-no-helm)
  (advice-add 'org-completing-read-no-i :around 'kk/run-with-no-helm))

(defun org/init-gtd ()
  
  (setq org-agenda-span 'day)

  (require 'org-checklist)

  ;; Disable default stuck project view in favor of our own
  (setq org-stuck-projects (quote ("" nil nil ""))))

(defun org/init-focus ()

  (defun bh/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
        (save-restriction
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (bh/narrow-to-org-subtree)
      (org-show-todo-tree nil)))

  (defun bh/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-agenda-remove-restriction-lock)
          (when org-agenda-sticky
            (org-agenda-redo)))
      (widen)))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
            'append)

  (defun bh/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock)))

  (defun bh/narrow-to-subtree ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (bh/narrow-to-org-subtree))
          (when org-agenda-sticky
            (org-agenda-redo)))
      (bh/narrow-to-org-subtree)))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
            'append)

  (defun bh/narrow-up-one-org-level ()
    (widen)
    (save-excursion
      (outline-up-heading 1 'invisible-ok)
      (bh/narrow-to-org-subtree)))

  (defun bh/get-pom-from-agenda-restriction-or-point ()
    (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
        (org-get-at-bol 'org-hd-marker)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

  (defun bh/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-up-one-org-level))
          (org-agenda-redo))
      (bh/narrow-up-one-org-level)))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
            'append)

  (defun bh/narrow-to-org-project ()
    (widen)
    (save-excursion
      (bh/find-project-task)
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-to-org-project)
            (save-excursion
              (bh/find-project-task)
              (org-agenda-set-restriction-lock)))
          (org-agenda-redo)
          (beginning-of-buffer))
      (bh/narrow-to-org-project)
      (save-restriction
        (org-agenda-set-restriction-lock))))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
            'append)

  (defvar bh/project-list nil)

  (defun bh/view-next-project ()
    (interactive)
    (let (num-project-left current-project)
      (unless (marker-position org-agenda-restrict-begin)
        (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
        (while bh/project-list
          (set-marker (pop bh/project-list) nil))
        (re-search-forward "Tasks to Refile")
        (forward-visible-line 1))

                                        ; Build a new project marker list
      (unless bh/project-list
        (while (< (point) (point-max))
          (while (and (< (point) (point-max))
                      (or (not (org-get-at-bol 'org-hd-marker))
                          (org-with-point-at (org-get-at-bol 'org-hd-marker)
                            (or (not (bh/is-project-p))
                                (bh/is-project-subtree-p)))))
            (forward-visible-line 1))
          (when (< (point) (point-max))
            (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
          (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
      (setq current-project (pop bh/project-list))
      (when current-project
        (org-with-point-at current-project
          (setq bh/hide-scheduled-and-waiting-next-tasks nil)
          (bh/narrow-to-project))
                                        ; Remove the marker
        (setq current-project nil)
        (org-agenda-redo)
        (beginning-of-buffer)
        (setq num-projects-left (length bh/project-list))
        (if (> num-projects-left 0)
            (message "%s projects left to view" num-projects-left)
          (beginning-of-buffer)
          (setq bh/hide-scheduled-and-waiting-next-tasks t)
          (error "All projects viewed.")))))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
            'append)

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
            'append)

  (defun bh/set-agenda-restriction-lock (arg)
    "Set restriction lock to current task subtree or file if prefix is specified"
    (interactive "p")
    (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
           (tags (org-with-point-at pom (org-get-tags-at))))
      (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
        (save-restriction
          (cond
           ((and (equal major-mode 'org-agenda-mode) pom)
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))
            (org-agenda-redo))
           ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
            (org-agenda-set-restriction-lock 'file))
           (pom
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))))))))

  ;; Prevent too many headlines from being folded together when working
  ;; on collapsed trees.
  (setq org-show-entry-below (quote ((default))))

  ;; Limit restriction lock highlighting to the headline only
  (setq org-agenda-restriction-lock-highlight-subtree nil))

(defun org/init-customize-agenda-views ()

  ;; Always hilight the current agenda line
  (add-hook 'org-agenda-mode-hook
            '(lambda () (hl-line-mode 1))
            'append)

  ;; Keep tasks with dates on the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)

  ;; Keep tasks with deadlines on the global todo lists
  (setq org-agenda-todo-ignore-deadlines nil)

  ;; Keep tasks with scheduled dates on the global todo lists
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; Keep tasks with timestamps on the global todo lists
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)

  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)

  (setq org-agenda-include-diary nil)
  (setq org-agenda-diary-file "~/org/diary.org")

  (setq org-agenda-insert-diary-extract-time t)

  ;; Include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))

  ;; Start the weekly agenda on Monday
  (setq org-agenda-start-on-weekday 1)

  ;; Enable display of the time grid so we can see the marker for the current time
  (setq org-agenda-time-grid (quote ((daily today remove-match)
                                     #("----------------" 0 16 (org-heading t))
                                     (0900 1100 1300 1500 1700))))

  ;; Display tags farther right
  (setq org-agenda-tags-column -102)

  ;;
  ;; Agenda sorting functions
  ;;
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

  (defun bh/agenda-sort (a b)
    "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
    (let (result num-a num-b)
      (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
       ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
       ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
       ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                                        ; scheduled items for today next
       ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
       ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
       ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
       (t (setq result nil)))
      result))

  (defmacro bh/agenda-sort-test (fn a b)
    "Test for agenda sort"
    `(cond
                                        ; if both match leave them unsorted
      ((and (apply ,fn (list ,a))
            (apply ,fn (list ,b)))
       (setq result nil))
                                        ; if a matches put a first
      ((apply ,fn (list ,a))
       (setq result -1))
                                        ; otherwise if b matches put b first
      ((apply ,fn (list ,b))
       (setq result 1))
                                        ; if none match leave them unsorted
      (t nil)))

  (defmacro bh/agenda-sort-test-num (fn compfn a b)
    `(cond
      ((apply ,fn (list ,a))
       (setq num-a (string-to-number (match-string 1 ,a)))
       (if (apply ,fn (list ,b))
           (progn
             (setq num-b (string-to-number (match-string 1 ,b)))
             (setq result (if (apply ,compfn (list num-a num-b))
                              -1
                            1)))
         (setq result -1)))
      ((apply ,fn (list ,b))
       (setq result 1))
      (t nil)))

  (defun bh/is-not-scheduled-or-deadline (date-str)
    (and (not (bh/is-deadline date-str))
         (not (bh/is-scheduled date-str))))

  (defun bh/is-due-deadline (date-str)
    (string-match "Deadline:" date-str))

  (defun bh/is-late-deadline (date-str)
    (string-match "\\([0-9]*\\) d\. ago:" date-str))

  (defun bh/is-pending-deadline (date-str)
    (string-match "In \\([^-]*\\)d\.:" date-str))

  (defun bh/is-deadline (date-str)
    (or (bh/is-due-deadline date-str)
        (bh/is-late-deadline date-str)
        (bh/is-pending-deadline date-str)))

  (defun bh/is-scheduled (date-str)
    (or (bh/is-scheduled-today date-str)
        (bh/is-scheduled-late date-str)))

  (defun bh/is-scheduled-today (date-str)
    (string-match "Scheduled:" date-str))

  (defun bh/is-scheduled-late (date-str)
    (string-match "Sched\.\\(.*\\)x:" date-str))

  ;; Use sticky agenda's so they persist
  (setq org-agenda-sticky t))

(defun org/init-blocking ()
  (setq org-enforce-todo-dependencies t))

(defun org/init-attachments ()
  (setq org-id-method (quote uuidgen)))

(defun org/init-deadlines ()
  (setq org-deadline-warning-days 30))

(defun org/init-csv-export ()
  (setq org-table-export-default-format "orgtbl-to-csv"))

(defun org/init-frame-minimization ()
  (setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                     (gnus . org-gnus-no-new-news)
                                     (file . find-file))))

  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window))

(defun org/init-logging ()
  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil))

(defun org/init-clock-time-limit ()
  (setq org-clock-sound t))

(defun org/init-habits ()
  
  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)

  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))))

(defun org/init-timestamps () 

  (defvar bh/insert-inactive-timestamp t)

  (defun bh/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
    (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

  (defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

  (defun bh/insert-heading-inactive-timestamp ()
    (save-excursion
      (when bh/insert-inactive-timestamp
        (org-return)
        (org-cycle)
        (bh/insert-inactive-timestamp))))

  ;; (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

  (setq org-export-with-timestamps nil))

(defun org/init-meeting-notes ()

  (defun bh/prepare-meeting-notes ()
    "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
    (interactive)
    (let (prefix)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (untabify (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
            (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
          (goto-char (point-min))
          (kill-ring-save (point-min) (point-max)))))))

(defun org/init-future-dates ()
  ;; Times before now (with no date specified) will default to tomorrow
  (setq org-read-date-prefer-future 'time))

(defun org/init-list-bullets ()
  (setq org-list-demote-modify-bullet (quote (("-" . "+")
                                              ("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-")
                                              ("A)" . "-")
                                              ("B)" . "-")
                                              ("a)" . "-")
                                              ("b)" . "-")
                                              ("A." . "-")
                                              ("B." . "-")
                                              ("a." . "-")
                                              ("b." . "-")))))

(defun org/init-misc ()

  (setq org-src-fontify-natively t)

  ;; Remove indentation on tag views
  (setq org-tags-match-list-sublevels t)

  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)

  ;; Only use "NEXT" for tasks and not projects
  (defun bh/mark-next-parent-tasks-todo ()
    "Visit each parent task and change NEXT states to TODO"
    (let ((mystate (or (and (fboundp 'org-state)
                            state)
                       (nth 2 (org-heading-components)))))
      (when mystate
        (save-excursion
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) (list "NEXT"))
              (org-todo "TODO")))))))

  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

  ;; Start up in folded view
  (setq org-startup-folded t)

  ;; Support alphabetical plain text lists
  (setq org-alphabetical-lists t)

  ;; Prevent editing invisible text
  (setq org-catch-invisible-edits 'error)

  ;; Keep clock time in hours, not days
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  ;; Create a unique id when linking
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-export-backends '(ascii html icalendar latex md)))

(use-package ob-core)
(use-package ox-md)

(use-package org-habit)

(use-package org
  :ensure t

  :init
  
  (unbind-key "s-o")

  (setq org-src-fontify-natively t)
  
  (setq org-agenda-files (quote ("~/org"
                                 "~/org/clients"
                                 "~/.emacs.d/README.org")))

  (setq org-modules (quote (org-docview
                            org-bbdb
                            org-gnus
                            org-id
                            org-info
                            org-habit
                            org-inlinetask
                            org-protocol
                            org-rmail)))

  (setq org-mobile-inbox-for-pull "~/org/refile.org")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

  :bind (("C-c l" . org-store-link)
         ("s-o a" . org-agenda)
         ("s-o b" . org-switchb)
         ("C-c b" . org-switchb)
         ("s-o c" . org-capture)
         ("C-c c" . org-capture)
         ("s-o i" . org-clock-in)
         ("s-o j" . org-clock-goto)
         ("s-o l" . bh/clock-in-last-task)
         ("s-o s" . bh/make-org-scratch)
         ("s-o I" . bh/punch-in)
         ("s-o O" . bh/punch-out))

  :config

  (unbind-key "C-c <SPC>" org-mode-map) ; Using it in helm
  (unbind-key "C-'" org-mode-map) ; Toggle-quotes

  (org/init-tasks)
  (org/init-capture)
  (org/init-refile)
  (org/init-agenda-views)
  (org/init-archive)
  (org/init-clock)
  (org/init-structure)
  (org/init-speed-commands)
  (org/init-tags)
  (org/init-gtd)
  (org/init-focus)
  (org/init-customize-agenda-views)
  (org/init-blocking)
  (org/init-attachments)
  (org/init-deadlines)
  (org/init-csv-export)
  (org/init-frame-minimization)
  (org/init-logging)
  (org/init-clock-time-limit)
  (org/init-habits)
  (org/init-timestamps)
  (org/init-meeting-notes)
  (org/init-future-dates)
  (org/init-list-bullets)
  (org/init-misc)

  (eval-after-load 'org-indent
    '(diminish 'org-indent-mode nil))

  (defun bh/make-org-scratch ()
    (interactive)
    (find-file "~/tmp/org/scratch.org")
    (gnus-make-directory "~/tmp/org")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq
   projectile-keymap-prefix (kbd "C-c p")
   projectile-completion-system 'default
   projectile-enable-caching t)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq
   helm-projectile-fuzzy-match t))

(use-package smartparens
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode +1)

  :bind (("M-n" . sp-next-sexp)
         ("M-p" . sp-previous-sexp)
         ("M-f" . sp-forward-sexp)
         ("M-b" . sp-backward-sexp))

  :diminish smartparens-mode

  :config
  ;; Enable smartparens everywhere
  (use-package smartparens-config)

  ;; Require and disable paredit because some packages rely on it.  
  (use-package paredit)
  (disable-paredit-mode)

  (setq
   smartparens-strict-mode t
   sp-autoinsert-if-followed-by-word t
   sp-autoskip-closing-pair 'always
   sp-base-key-bindings 'paredit
   sp-hybrid-kill-entire-symbol nil)

  (sp-use-paredit-bindings)
  
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  (sp-with-modes '(html-mode sgml-mode web-mode)
    (sp-local-pair "<" ">"))

  ;; Close a backtick with another backtick in clojure-mode
  (sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))

  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))

;; (use-package tagedit
;;   :ensure t
;;   :config
;;   (add-hook 'web-mode-hook 'tagedit-mode)
;;   (tagedit-add-paredit-like-keybindings))

(use-package toggle-quotes
  :ensure t
  :bind (("C-'" . toggle-quotes)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :ensure t
  :init
  
  (setq jsx-indent-level 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing nil)
  
  :config

  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

  (setq find-npm-module-binding "C-c m")

  (bind-key find-npm-module-binding 'find-npm-module web-mode-map)

  (eval-after-load "js2-mode"
    '(bind-key find-npm-module-binding 'find-npm-module js2-mode-map))
  
  (eval-after-load "js-mode"
    '(bind-key find-npm-module-binding 'find-npm-module js-mode-map))
  
  (eval-after-load "js-mode"
     '(bind-key find-npm-module-binding 'find-npm-module js-mode-map)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml$" "\\.raml$"))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config

  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

  (setq yas-expand-only-for-last-commands '(self-insert-command))

  (yas-global-mode 1)
  
  (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)
  
  (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))
  
  ;; Prevent conflict between company mode and yasnippet
  (defun company-yasnippet-or-completion ()
    (interactive)
    (if (yas/expansion-at-point)
        (progn
          (company-abort)
          (yas/expand))
      (company-complete-common)))
   
  (defun yas/expansion-at-point ()
    "Tested with v0.6.1. Extracted from `yas/expand-1'"
    (first (yas--templates-for-key-at-point)))
  
  ;; (bind-key "<tab>" 'hippie-expand yas-minor-mode-map)
  (bind-key "<tab>" 'company-yasnippet-or-completion company-active-map))

(use-package helm-c-yasnippet
  :ensure t)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; (defun my/frame-make-transparent ()
;;   (set-frame-parameter (selected-frame) 'alpha '(85 50)))

(defun my/frame-make-opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;; (defun my/frame-transparency ()
;;   "Toggle frame transparency"
;;   (interactive)
;;   (setq trans (cdr (frame-parameter (selected-frame) 'alpha)))
;;   (setq check (list 100))

;;   (if (cl-equalp trans check)
;;       (my/frame-make-transparent)
;;     (my/frame-make-opaque)))

(defun my/restore-frame-config ()
  "Restore the opacity of the window"
  (remove-hook 'pre-command-hook 'my/restore-frame-config)
  (remove-hook 'focus-out-hook 'my/restore-frame-config)
  (jump-to-register :save-with-transparency))

(defun my/make-transparent ()
  "Save the current frame config to a register, become
  transparent, and add a hook to restore before the next command
  or when the frame loses focus"
  (interactive)
  (frame-configuration-to-register :save-with-transparency)  
  (set-frame-parameter (selected-frame) 'alpha '(5 100))
  (add-hook 'pre-command-hook 'my/restore-frame-config)
  (add-hook 'focus-out-hook 'my/restore-frame-config))

(defun my/save-with-transparency ()
  "Save the current buffer and immediately become transparent.
  Helpful for situations where you'd like to see the impact of
  your change on another window such as a browser behind your
  frame."
  (interactive)
  (save-buffer)
  (my/make-transparent))

(bind-key "s-s" 'my/save-with-transparency)
(bind-key "s-t" 'my/make-transparent)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (text-mode)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when (display-graphic-p)
  (if is-mac
      (menu-bar-mode 1)))

(exec-path-from-shell-initialize)

(server-start)