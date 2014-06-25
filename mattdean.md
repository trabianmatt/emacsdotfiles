- [Document this a bit better &#x2013; was mostly copied/pasted from existing init.el](#document-this-a-bit-better-&#x2013;-was-mostly-copied/pasted-from-existing-init.el)
- [Configuration](#configuration)
  - [About this configuration](#about-this-configuration)
  - [Emacs initialization](#emacs-initialization)
    - [Helper functions](#helper-functions)
  - [General configuration](#general-configuration)
    - [Themes](#themes)
    - [Misc. packages](#misc.-packages)
      - [better-defaults](#better-defaults)
    - [Keyboard customizations](#keyboard-customizations)
    - [Backups and intermediate files](#backups-and-intermediate-files)
  - [Mode/package-specific configuration](#mode/package-specific-configuration)
    - [org-mode](#org-mode)
      - [Read [org-mode manual](http://orgmode.org/org.html) and add keystrokes to Anki (I suspect this will be a long-lived TODO)](#read-[org-mode-manual](http://orgmode.org/org.html)-and-add-keystrokes-to-anki-(i-suspect-this-will-be-a-long-lived-todo))
    - [magit](#magit)
    - [paredit and other lisp helpers](#paredit-and-other-lisp-helpers)
      - [Add paredit keystrokes to Anki](#add-paredit-keystrokes-to-anki)
    - [Clojure](#clojure)
    - [Coffeescript](#coffeescript)
    - [Projectile](#projectile)
    - [Whitespace](#whitespace)

Note: Due to some unknown (to me) issue, org-mode needs to be reloaded
via `M-x org-reload` after starting Emacs

# TODO Document this a bit better &#x2013; was mostly copied/pasted from existing init.el<a id="sec-1"></a>

# Configuration<a id="sec-2"></a>

## About this configuration<a id="sec-2-1"></a>

This configuration is inspired by the [Sacha Chua's literate config
file](http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html#sec-1) and is partially an exploration into the idea of literate
programming, particularly via org-mode.

## Emacs initialization<a id="sec-2-2"></a>

Load common package archives.

```lisp
(require 'package)
(setq package-archives '())             ; Start fresh
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))
```

### Helper functions<a id="sec-2-2-1"></a>

The following function will install a package if needed. It is taken from [Sacha
Chua's config](http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html#sec-1-3-5).

```lisp
(defun my/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
      (package-install package))))
```

## General configuration<a id="sec-2-3"></a>

We don't need no stinkin' startup message

```lisp
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
```

### Themes<a id="sec-2-3-1"></a>

```lisp
(my/package-install 'solarized-theme)

(load-theme 'solarized-dark t)
```

### Misc. packages<a id="sec-2-3-2"></a>

#### better-defaults<a id="sec-2-3-2-1"></a>

> &#x2026; this package focuses a few changes that have near-universal appeal, lovingly hand-selected by inhabitants of the #emacs channel on Freenode.

[GitHub Repo](https://github.com/technomancy/better-defaults)

```lisp
(my/package-install 'better-defaults)
```

### Keyboard customizations<a id="sec-2-3-3"></a>

```lisp
;; Remap right alt/option key to 'super' (s-)
(setq ns-right-option-modifier 'super)

;; This is almost alway the intended behavior
(define-key global-map (kbd "RET") 'newline-and-indent)
```

### Backups and intermediate files<a id="sec-2-3-4"></a>

The following will store backups in `~/.emacs.d/backups` instead of in
the same directory as the original. This is particularly important
when using file watchers (such as grunt) that will be triggered
unnecessarily when these files are created or updated.

```lisp
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))  

(setq create-lockfiles nil)
```

## Mode/package-specific configuration<a id="sec-2-4"></a>

### org-mode<a id="sec-2-4-1"></a>

#### TODO Read [org-mode manual](http://orgmode.org/org.html) and add keystrokes to Anki (I suspect this will be a long-lived TODO)<a id="sec-2-4-1-1"></a>

-   [ ] [Document Structure](http://orgmode.org/org.html#Document-Structure)
-   [ ] [Tables](http://orgmode.org/org.html#Tables)
-   [ ] [Hyperlinks](http://orgmode.org/org.html#Hyperlinks)
-   [ ] [TODO Items](http://orgmode.org/org.html#TODO-Items)
-   [ ] [Tags](http://orgmode.org/org.html#Tags)
-   [ ] [Properties and Columns](http://orgmode.org/org.html#Properties-and-Columns)
-   [ ] [Dates and Times](http://orgmode.org/org.html#Dates-and-Times)
-   [ ] [Capture - Refile - Archive](http://orgmode.org/org.html#Capture-_002d-Refile-_002d-Archive)
-   [ ] [Agenda Views](http://orgmode.org/org.html#Agenda-Views)
-   [ ] [Markup for rich export](http://orgmode.org/org.html#Markup)
-   [ ] [Exporting](http://orgmode.org/org.html#Exporting)
-   [ ] [Publishing](http://orgmode.org/org.html#Publishing)
-   [ ] [Working with source code](http://orgmode.org/org.html#Working-With-Source-Code)
-   [ ] [Miscellaneous](http://orgmode.org/org.html#Miscellaneous)
-   [ ] [Appendix A: Hacking](http://orgmode.org/org.html#Hacking)
-   [ ] [Appendix B: MobileOrg](http://orgmode.org/org.html#MobileOrg)

```lisp
(add-to-list 'load-path "~/.emacs.d/submodules/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/submodules/org-mode/contrib/lisp")

(require 'ox-gfm)                       ; Support export to GitHub Flavored Markdown

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(setq org-hide-leading-stars t)
```

### magit<a id="sec-2-4-2"></a>

```lisp
(my/package-install 'magit)
```

### paredit and other lisp helpers<a id="sec-2-4-3"></a>

-   [Paredit Keystrokes](http://mumble.net/~campbell/emacs/paredit.html)

#### TODO Add paredit keystrokes to Anki<a id="sec-2-4-3-1"></a>

```lisp
(my/package-install 'paredit)

(my/package-install 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
```

### Clojure<a id="sec-2-4-4"></a>

1.  TODO Add clojure-mode, clojure-test-mode, clj-refactor, and cider keystrokes to Anki

    ```lisp
    (my/package-install 'clojure-mode)
    (my/package-install 'clojure-test-mode)
    (my/package-install 'cider)
    (my/package-install 'clj-refactor)
    
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    
    (add-hook 'clojure-mode-hook 'paredit-mode)
    
    (require 'clj-refactor)
    
    (setq cljr-sort-comparator 'cljr-semantic-comparator)
    
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)
                                   (cljr-add-keybindings-with-prefix "C-c C-m")))
    ```

### Coffeescript<a id="sec-2-4-5"></a>

```lisp
(my/package-install 'coffee-mode)

(setq coffee-tab-width 2)
(setq coffee-args-compile '("-c" "--bare"))

(add-hook 'coffee-mode-hook 'whitespace-mode)
```

### Projectile<a id="sec-2-4-6"></a>

```lisp
(my/package-install 'projectile)

(projectile-global-mode)
```

### Whitespace<a id="sec-2-4-7"></a>

```lisp
(setq whitespace-action '(auto-cleanup))

(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
```
