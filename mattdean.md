- [Configuration](#configuration)
  - [About this configuration](#about-this-configuration)
  - [Emacs initialization](#emacs-initialization)
    - [Helper functions](#helper-functions)
  - [General configuration](#general-configuration)
    - [Backups](#backups)
  - [Mode/package-specific configuration](#mode/package-specific-configuration)
    - [org-mode](#org-mode)
      - [Learn org-mode (I suspect this will be a long-lived TODO)](#learn-org-mode-(i-suspect-this-will-be-a-long-lived-todo))
    - [magit](#magit)
    - [Install packages](#install-packages)
      - [better-defaults](#better-defaults)
      - [Clojure modes and tools](#clojure-modes-and-tools)
      - [General lisp editing](#general-lisp-editing)


# Configuration<a id="sec-1"></a>

## About this configuration<a id="sec-1-1"></a>

This configuration is inspired by the [Sacha Chua's literate config
file](http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html#sec-1) and is partially an exploration into the idea of literate
programming, particularly via org-mode.

## Emacs initialization<a id="sec-1-2"></a>

Load common package archives.

```lisp
(require 'package)
(setq package-archives '())             ; Start fresh
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))
```

### Helper functions<a id="sec-1-2-1"></a>

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

## General configuration<a id="sec-1-3"></a>

### Backups<a id="sec-1-3-1"></a>

The following will store backups in `~/.emacs.d/backups` instead of in the same
directory as the original.

```lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))
```

## Mode/package-specific configuration<a id="sec-1-4"></a>

### org-mode<a id="sec-1-4-1"></a>

#### TODO Learn org-mode (I suspect this will be a long-lived TODO)<a id="sec-1-4-1-1"></a>

```lisp
(add-to-list 'load-path "~/.emacs.d/submodules/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/submodules/org-mode/contrib/lisp")

(require 'ox-gfm)                       ; Support export to GitHub
					; Flavored Markdown
```

### magit<a id="sec-1-4-2"></a>

```lisp
(my/package-install 'magit)
```

### Install packages<a id="sec-1-4-3"></a>

```lisp
(defvar my-packages '())
```

#### better-defaults<a id="sec-1-4-3-1"></a>

> &#x2026; this package focuses a few changes that have near-universal appeal, lovingly hand-selected by inhabitants of the #emacs channel on Freenode.

[GitHub Repo](https://github.com/technomancy/better-defaults)

```lisp
(add-to-list 'my-packages 'better-defaults)
```

#### Clojure modes and tools<a id="sec-1-4-3-2"></a>

1.  TODO Add clojure-mode, clojure-test-mode, and cider keystrokes to Anki

    ```lisp
    (add-to-list 'my-packages 'clojure-mode t)
    (add-to-list 'my-packages 'clojure-test-mode t)
    (add-to-list 'my-packages 'cider t)
    (add-to-list 'my-packages 'clj-refactor t)
    ```

#### General lisp editing<a id="sec-1-4-3-3"></a>
