;;; Initialize
;;  -----------------------------------------------------------------------------

(package-initialize)
;(package-refresh-contents)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(setq package-check-signature nil)  ; because GNU ELPA keeps choking on the sigs
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)


;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(eval-when-compile (require 'use-package))
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

; Increase Garbage Collector size, improves startup speed
(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 1000000)
	    (message "gc-cons-threshold restored to %S"
		     gc-cons-threshold)))

;; Get user PATH
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; make sure theme can be loaded
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")

;;; Preferences
;;  ----------------------------------------------------------------------------

(setq user-full-name "Sourabh Cheedella"
      user-mail-address "cheedella.sourabh@gmail.com")

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; Remove useless whitespace before saving a file
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Auto-revert to disk on file change
(global-auto-revert-mode t)

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; hide DOS ^M line-endings
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

;; Make ibuffer default instead of list-buffers
(defalias 'list-buffers 'ibuffer)

;; Disable backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil)

;;; Interface
;;  ----------------------------------------------------------------------------

;; Fullscreen maximized frame in GUI mode
(modify-all-frames-parameters '((fullscreen . maximized)))

;; Undo and redo window configurations C-c left and C-c right
(winner-mode 1)

;; Word wrap on vertical split
(setq truncate-partial-width-windows nil)

;; disable toolbar-mode in GUI
(tool-bar-mode -1)

;; switch-window
(use-package switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; Scroll-bar? What's that?
(when (display-graphic-p)
  (set-scroll-bar-mode nil))

;; [DHA] I *never* use the stupid thing..
(menu-bar-mode -1)

;; split window vertically
;; (split-window-right)

;; [DHA] Show the time in the mode line
;;(display-time)                              ; how late am I?

;; [DHA] Don't show the 'startup screen'
(setq inhibit-startup-message t)            ; ok I've seen the copyleft &c

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Use these commands w/o confirmation
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; remove that dastardly annoying middle button paste from X11
;; paste like civilized people - C-y
(global-unset-key [mouse-2])


;;; Appearance
;;  ----------------------------------------------------------------------------

(setq frame-title-format
      '("" (:eval (if (buffer-file-name)
		      (abbreviate-file-name (buffer-file-name))
		    "[%b]"))))


;; use zenburn as the default theme
(load-theme 'zenburn t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "dark sea green" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:background "DarkSeaGreen1" :foreground "dim gray" :box nil)))))

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 110)

;;; Packages
;;  ----------------------------------------------------------------------------

(setq package-list '(ag
		     auto-yasnippet
;		     autopair
		     dumb-jump
		     lsp-mode ;eglot
;		     ein-mumamo
;		     ein
		     auto-complete
		     elpy
;		     company
		     exec-path-from-shell
;		     fill-column-indicator
;		     find-file-in-project
		     flycheck
		     flymake
;		     git-commit-mode
		     helm-ag
		     helm-projectile
;		     helm-sage
		     helm-system-packages
		     helm
		     helm-core
		     highlight-indentation
;		     htmlize
		     ido-hacks
;		     ido-vertical-mode
		     ivy
		     json-mode
		     json-reformat
		     json-snatcher
		     jsonrpc
		     magit
		     git-commit
		     magit-popup
;		     mmm-mode
		     multiple-cursors
		     ob-ipython
		     dash-functional
		     org
;		     org-ehtml
		     php-mode
		     popup
		     project-explorer
		     es-windows
		     es-lib
		     pyvenv
		     pos-tip
		     request
		     markdown-mode
		     projectile
		     pkg-info
		     epl
		     f
		     dash
		     s
		     deferred
		     shell-switcher
		     spinner
;		     sr-speedbar ; https://www.emacswiki.org/emacs/SrSpeedbar
		     switch-window
		     telephone-line
		     use-package
		     bind-key
		     web-server
		     websocket
		     with-editor
		     async
		     xterm-color
		     yaml-mode
		     yasnippet))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Smartparens - keep parentheses balanced (from Jamie's)
(use-package smartparens
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; Highlight nested parentheses (from Jamie's)
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Color comprehension
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package flycheck-inline
  :config
  (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))

;; Auto-complete
(use-package auto-complete
  :init
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  :config
  (ac-config-default))

;; Multiple-cursors
(use-package multiple-cursors
  :bind (("H-e" . mc/edit-lines)
	 ("H-k" . mc/mark-next-like-this)
	 ("H-l" . mc/mark-previous-like-this)
	 ("C-S-k" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Yasnippet
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :bind (:map yas-minor-mode-map
	      ("<tab>" . nil)
	      ("TAB" . nil)
	      ("<C-tab>" . yas-expand)
	      ("C-j" . yas-next-field))
  :config
  (yas-global-mode 1))

;; Magit, of course
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (use-package magit-popup))

;; Silversearcher support - faster-than-grep
(use-package ag)

;;; Navigation and projects
;;  ----------------------------------------------------------------------------

;; IDO
(use-package ido
  :config
  (use-package ido-vertical-mode)
  (use-package ido-hacks)
  (ido-mode t)
  (ido-vertical-mode))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key (kbd "C-S-x C-S-f") 'ido-find-file-in-tag-files)

;; Projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

;; Helm - incremental completions and narrowing
(use-package helm
  :config
  (use-package helm-projectile)
  (use-package helm-ag)
  (use-package helm-sage))

;; Dumb jump
(use-package dumb-jump
  :config
  (dumb-jump-mode)
  :bind (("H-g g" . dumb-jump-go)
	 ("H-g b" . dumb-jump-back)))

;;; Python
;;  ----------------------------------------------------------------------------

;; Basic python-mode config. I've been using this for years with no problems.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("/usr/bin/ipython3" . python-mode)
  :hook (python-mode . fci-mode)
  :config
  (use-package pyvenv))

;; Elpy makes Emacs a full Python IDE. Do I want that? I dunno yet. Guess I'll try it...
(use-package elpy
  :config
  (elpy-enable))

;;; Markdown (from Jamie's)
;;  ----------------------------------------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Yaml
;;  ----------------------------------------------------------------------------

(use-package yaml-mode
  :init
  (setq indent-tabs-mode nil)
  :mode "\\.yml\\'"
  :bind (:map yaml-mode-map
	      ("C-m" . newline-and-indent)))

;;; Emacs iPython Notebook (EIN) with Jupyter support
;;  ----------------------------------------------------------------------------

;; (use-package ein
;;   :config
;;  (use-package ein-notebook)
;;  (use-package ein-subpackages)
;;  (use-package ein-mumamo))

;;; Go
;;  ----------------------------------------------------------------------------

(use-package go-mode)

;;; LaTeX
;;  ----------------------------------------------------------------------------

;;(load "auctex.el" nil t )
;;(load "preview-latex.el" nil t t)

;;; Ace-Jump-Mode
;;  ----------------------------------------------------------------------------
(use-package ace-jump-mode
  :bind (("H-SPC" . ace-jump-mode)))

;;; Highlight-Indentation
;; -----------------------------------------------------------------------------
(use-package highlight-indentation
  :init
  (set-face-background 'highlight-indentation-face "#2e3838")
  (set-face-background 'highlight-indentation-current-column-face "#3d4a4b")
  (highlight-indentation-mode 1))

;;; Telephone Line (powerline)
;;  ----------------------------------------------------------------------------
(use-package telephone-line
  :init
  (setq telephone-line-primary-right-separator 'telephone-line-halfcos-left
	telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-left
	telephone-line-primary-left-separator 'telephone-line-halfcos-left
	telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left)
  (telephone-line-mode 1))

;;; Org-Mode
;;  ----------------------------------------------------------------------------
;; (use-package org
;;   :ensure org-plus-contrib
;;   :init
;;   (setq org-agenda-files (quote ("~/notes/events.org" "~/notes/homework.org" "~/notes/personal.org")))
;;   (setq org-agenda-skip-scheduled-if-done t)
;;   (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;;   :bind (("C-c l" . org-store-link)
;;	 ("C-c a" . org-agenda)
;;	 ("C-c c" . org-capture)
;;	 ("C-c b" . org-switchb)))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-files (quote ("~/docs/org-files/yearlyevents.org"
			       "~/docs/org-files/events.org"
			       "~/docs/org-files/skillRequirements.org"
			       "~/docs/org-files/jobStatus.org"
			       "~/docs/org-files/geicoJavaDeveloper.org"
			       "~/docs/org-files/agenda.org")))


(bind-key "C-c l" 'org-store-link)
(bind-key "C-c a" 'org-agend)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c b" 'org-switchb)


;;; aggressive-indent-mode
;;  ---------------------------------------------------------------------------
(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (global-aggressive-indent-mode 1))

;;; guru-mode
;;  ---------------------------------------------------------------------------
(use-package guru-mode
  :diminish guru-mode
  :config (guru-global-mode 1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-jump-mode go-mode yaml-mode xterm-color web-server use-package telephone-line switch-window smartparens shell-switcher rainbow-mode rainbow-delimiters project-explorer pos-tip php-mode ob-ipython multiple-cursors markdown-mode magit-popup magit lsp-mode jsonrpc json-mode ido-vertical-mode ido-hacks helm-system-packages helm-sage helm-projectile helm-ag flycheck-inline exec-path-from-shell elpy ein-mumamo dumb-jump auto-yasnippet auto-compile ag))))
