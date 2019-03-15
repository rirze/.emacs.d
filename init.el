;;; init.el --- my emacs configuration
;;; Initialize
;;  -----------------------------------------------------------------------------
;;; Commentary:
;;; Code:
(package-initialize)
;;(package-refresh-contents)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(setq package-check-signature nil)  ; because GNU ELPA keeps choking on the sigs
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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

;; Increase Garbage Collector size, improves startup speed
(setq gc-cons-threshold 50000000)

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

;; test out Save Place
(save-place-mode 1)

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

;; [DHA] I *never* use the stupid thing..
(menu-bar-mode -1)
(mouse-wheel-mode -1)
(scroll-bar-mode -1)


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

;; SPACES
(setq-default indent-tabs-mode nil)

;; Don't write lock-files, I'm the only one here
(setq create-lockfiles nil)

;;; Appearance
;;  ----------------------------------------------------------------------------

(setq frame-title-format
      '("" invocation-name (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "[%b]"))))


;; use zenburn as the default theme
(use-package zenburn-theme)
(load-theme 'zenburn t)

;; highlight the current line
(global-hl-line-mode +1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "dark sea green" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:background "DarkSeaGreen1" :foreground "dim gray" :box nil)))))

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 110)

;;; Custom Key Bindings
;;  ---------------------------------------------------------------------------
(use-package bind-key)

;; use-package-hydra
(use-package hydra)
(use-package use-package-hydra
  :ensure t)


(bind-key "H-K" 'kill-this-buffer)
(bind-key "H-c c" 'comment-or-uncomment-region)
(bind-key "H-a" 'align-current)
(bind-key "H-w" 'eval-region)
(bind-key "H-L" 'display-line-numbers-mode)
(bind-key "M-g l" 'goto-line)
(bind-key "<C-M-backspace>" 'backward-kill-sexp)
(bind-key "C-c m b" 'eval-buffer)
(bind-key "C-c m e" 'eval-last-sexp)
(bind-key "C-c m i" 'eval-expression)
(bind-key "C-c m d" 'eval-defun)
(bind-key "C-c m n" 'eval-print-last-sexp)
(bind-key "C-c m r" 'eval-region)




(defhydra frame-resize-hydra (:hint nil)
  "
        ^Vertical^                    ^Horizontal^
--------------------------------------------------------------
[_a_] Shrink  [_s_] Enlarge    [_d_] Shrink  [_f_] Enlarge

"
  ("a" shrink-window)
  ("s" enlarge-window)
  ("d" shrink-window-horizontally)
  ("f" enlarge-window-horizontally)
  )
(bind-key "H-f" 'frame-resize-hydra/body)

;; select current line
(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(bind-key "H-l l" 'select-current-line)

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Source:  http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;;; Default Emacs Packages Configuration
;;  ---------------------------------------------------------------------------

;; wind move - move across frames with <mod> + arrow keys
(require 'windmove)
(windmove-default-keybindings 'hyper)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

;; configure sh-mode
;; recognize bash config files as sh scripts
;; TODO: make this more elegant
(add-to-list 'auto-mode-alist '(".*\\.bash.*\\'" . sh-mode))

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Tramp Settings
(setq vc-handled-backends '(Git))
(setq tramp-verbose 1)

;; dired settings
(setq dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-listing-switches "-alhv")
(setq dired-omit-files "^\\.\\|^#.*#$")
(setq dired-omit-verbose nil)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-subtree-line-prefix " ")

;;; Packages
;;  ----------------------------------------------------------------------------

(setq package-list '(ag
                     auto-yasnippet
                     ;;autopair
                     dumb-jump
                     diminish
                     lsp-mode ;eglot
                     ;;ein-mumamo
                     ;;ein
                     auto-complete
                     elpy
                     ;;company
                     exec-path-from-shell
                     fill-column-indicator
                     ;;find-file-in-project
                     flycheck
                     flymake
                     ;;git-commit-mode
                     helm-ag
                     helm-projectile
                     ;;helm-sage
                     helm-system-packages
                     helm
                     helm-core
                     highlight-indentation
                     ;;htmlize
                     ido-hacks
                     ;;ido-vertical-mode
                     ivy
                     json-mode
                     json-reformat
                     json-snatcher
                     jsonrpc
                     magit
                     git-commit
                     magit-popup
                     ;;mmm-mode
                     multiple-cursors
                     ob-ipython
                     dash-functional
                     org
                     ;;org-ehtml
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
                     ;;sr-speedbar ; https://www.emacswiki.org/emacs/SrSpeedbar
                     switch-window
                     ;; telephone-line
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
;; (use-package auto-complete
;;   :init
;;   (require 'auto-complete-config)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;   :config
;;   (ac-config-default))

;; Multiple-cursors
(use-package multiple-cursors
  :after hydra
  ;; :bind (("H-m ^"     . mc/edit-beginnings-of-lines)
  ;;     ("H-m a"     . mc/edit-beginnings-of-lines)
  ;;     ("H-m $"     . mc/edit-ends-of-lines)
  ;;     ("H-m e"     . mc/edit-ends-of-lines)
  ;;     ("H-m R"     . mc/reverse-regions)
  ;;     ("H-m S"     . mc/sort-regions)
  ;;     ("H-m W"     . mc/mark-all-words-like-this)
  ;;     ("H-m Y"     . mc/mark-all-symbols-like-this)
  ;;     ("H-m A"     . mc/mark-all-like-this-dwim)
  ;;     ("H-m c"     . mc/mark-all-dwim)
  ;;     ("H-m l"     . mc/insert-letters)
  ;;     ("H-m n"     . mc/insert-numbers)
  ;;     ("H-m r"     . mc/mark-all-in-region)
  ;;     ("H-m s"     . set-rectangular-region-anchor)
  ;;     ("H-m %"     . mc/mark-all-in-region-regexp)
  ;;     ("H-m t"     . mc/mark-sgml-tag-pair)
  ;;     ("H-m w"     . mc/mark-next-like-this-word)
  ;;     ("H-m x"     . mc/mark-more-like-this-extended)
  ;;     ("H-m y"     . mc/mark-next-like-this-symbol)
  ;;     ("H-m C-x"   . reactivate-mark)
  ;;     ("H-m C-SPC" . mc/mark-pop)
  ;;     ("H-m ("     . mc/mark-all-symbols-like-this-in-defun)
  ;;     ("H-m C-("   . mc/mark-all-words-like-this-in-defun)
  ;;     ("H-m M-("   . mc/mark-all-like-this-in-defun)
  ;;     ("H-m ["     . mc/vertical-align-with-space)
  ;;     ("H-m {"     . mc/vertical-align))
  ;; :bind (:map region-bindings-mode-map
  ;;          ("c"   . mc/edit-lines)
  ;;          ("."   . mc/mark-next-like-this)
  ;;          ("<"   . mc/unmark-next-like-this)
  ;;          ("C->" . mc/skip-to-next-like-this)
  ;;          (","   . mc/mark-previous-like-this)
  ;;          (">"   . mc/unmark-previous-like-this)
  ;;          ("C-<" . mc/skip-to-previous-like-this)
  ;;          ("y"   . mc/mark-next-symbol-like-this)
  ;;          ("Y"   . mc/mark-previous-symbol-like-this)
  ;;          ("w"   . mc/mark-next-word-like-this)
  ;;          ("W"   . mc/mark-previous-word-like-this))

  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))

(defhydra hydra-mc (:hint nil)
  "
      ^Up^            ^Down^        ^All^                ^Lines^               ^Edit^                 ^Other^
-----------------------------------------------------------------------------------------------------------------------
[_p_]   Next    [_n_]   Next    [_A_] All like this  [_E_] Edit lines      [_i_] Insert numbers   [_t_] Tag pair
[_P_]   Skip    [_N_]   Skip    [_r_] All by regexp  [_a_] Edit line beg.  [_s_] Sort regions     [_._] Set Rect Region
[_M-p_] Unmark  [_M-n_] Unmark  [_d_] All DWIM       [_e_] Edit line ends. [_R_] Reverse regions  [_q_] Quit
"
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)

  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)

  ("A" mc/mark-all-like-this :exit t)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("d" mc/mark-all-dwim :exit t)

  ("E" mc/edit-lines :exit t)
  ("a" mc/edit-beginnings-of-lines :exit t)
  ("e" mc/edit-ends-of-lines :exit t)

  ("i" mc/insert-numbers)
  ("s" mc/sort-regions)
  ("R" mc/reverse-regions)

  ("t" mc/mark-sgml-tag-pair)
  ("q" nil)
  ("." set-rectangular-region-anchor)

  ;;  ("x" er/expand)
  ("<mouse-1>" mc/add-cursor-on-click)
  )
(bind-key "H-m" 'hydra-mc/body)

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
  (use-package magit-popup)
  :init
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil))

(use-package forge)

;; Silversearcher support - faster-than-grep
(use-package ag)

;;; Navigation and projects
;;  ----------------------------------------------------------------------------

;; IDO
;; (use-package ido
;;   :config
;;   (use-package ido-vertical-mode)
;;   (use-package ido-hacks)
;;   (ido-mode t)
;;   (ido-vertical-mode))

;; (defun ido-find-file-in-tag-files ()
;;   (interactive)
;;   (save-excursion
;;     (let ((enable-recursive-minibuffers t))
;;       (visit-tags-table-buffer))
;;     (find-file
;;      (expand-file-name
;;       (ido-completing-read
;;        "Project file: " (tags-table-files) nil t)))))

;; (global-set-key (kbd "C-S-x C-S-f") 'ido-find-file-in-tag-files)

;; Projectile
(use-package projectile
  :bind-keymap
  ("H-p" . projectile-command-map)
  :init
  (setq projectile-file-exists-remote-cache-expire nil)
  ;; (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
  (setq projectile-globally-ignored-directories
        (quote
         (".idea" ".eunit" ".git" ".hg" ".svn" ".fslckout" ".bzr" "_darcs" ".tox" "build" "target")))
  :config
  (projectile-mode +1))

;; Helm - incremental completions and narrowing
(use-package helm
  :config
  (use-package helm-projectile)
  (use-package helm-ag)
  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-z" . helm-select-action)
         ("M-y" . helm-show-kill-ring)))

;; Dumb jump
(use-package dumb-jump
  :config
  ;;  (setq dumb-jump-selector 'helm)
  (dumb-jump-mode)
  :bind (("H-g g" . dumb-jump-go)
         ("H-g b" . dumb-jump-back)
         ("H-g o" . dumb-jump-go-other-window)))

;;; Python
;;  ----------------------------------------------------------------------------

;; Basic python-mode config. I've been using this for years with no problems.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("/usr/bin/ipython3" . python-mode)
  :hook
  (python-mode . fci-mode)
  (python-mode . subword-mode)
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
(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))


;;; Ace-Jump-Mode
;;  ----------------------------------------------------------------------------
(use-package ace-jump-mode
  :init
  (setq ace-jump-mode-scope 'window)
  (setq ace-jump-mode-case-fold t)

  :bind (("H-SPC" . ace-jump-char-mode)
         ("H-j w" . ace-jump-word-mode)
         ("H-j c" . ace-jump-char-mode)
         ("H-j l" . ace-jump-line-mode)
         ("H-j v" . ace-jump-mode)
         )
  )

;;; Highlight-Indentation

;; -----------------------------------------------------------------------------
(use-package highlight-indentation
  :init
  (set-face-background 'highlight-indentation-face "#4F4F4F")
  (set-face-background 'highlight-indentation-current-column-face "#5F5F5F")
  ;; (highlight-indentation-mode 0)
  :bind (("H-h i" . highlight-indentation-mode))
  )


;;; Telephone Line (powerline)
;;  ----------------------------------------------------------------------------
;; (use-package telephone-line
;;   :init
;;   (setq telephone-line-primary-right-separator 'telephone-line-halfcos-left
;;      telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-left
;;      telephone-line-primary-left-separator 'telephone-line-halfcos-left
;;      telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left)
;;   (telephone-line-mode 1))

;;; Org-Mode
;;  ----------------------------------------------------------------------------
;; (use-package org
;;   :ensure org-plus-contrib
;;   :init
;;   (setq org-agenda-files (quote ("~/notes/events.org" "~/notes/homework.org" "~/notes/personal.org")))
;;   (setq org-agenda-skip-scheduled-if-done t)
;;   (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;;   :bind (("C-c l" . org-store-link)
;;       ("C-c a" . org-agenda)
;;       ("C-c c" . org-capture)
;;       ("C-c b" . org-switchb)))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-files (quote ("~/docs/org-files/yearlyevents.org"
                               "~/docs/org-files/events.org"
                               "~/docs/org-files/skillRequirements.org"
                               "~/docs/org-files/jobStatus.org"
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
  ;; (global-aggressive-indent-mode 1)
  )

;;; guru-mode
;;  ---------------------------------------------------------------------------
(use-package guru-mode
  :diminish guru-mode
  :config (guru-global-mode 1))

;;; move-text
;;  ---------------------------------------------------------------------------
(use-package move-text
  ;;:config (move-text-default-bindings)) ;; uses M-up and M-down
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

;;; operate-on-number
;;  ---------------------------------------------------------------------------
(use-package operate-on-number
  :bind (("H-=" . operate-on-number-at-point)))

;;; discover-my-major
;;  ---------------------------------------------------------------------------
(use-package discover-my-major)
;; invoke M-x discover-my-major


;;; which-key
;;  ---------------------------------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;;; crux
;;  ---------------------------------------------------------------------------
;; DONE: add key bindings for commands [https://github.com/bbatsov/crux]
(use-package crux
  :config
  (crux-reopen-as-root-mode)
  (crux-with-region-or-line comment-or-uncomment-region)
  :bind (("H-c i" . crux-find-user-init-file)
         ("H-u"   . crux-kill-line-backwards)
         ("H-c o" . crux-open-with)
         ("<H-return>" . crux-smart-open-line)
         ("<H-S-return>" . crux-smart-open-line-above)
         ("H-c n" . crux-cleanup-buffer-or-region)
         ("H-c 2" . crux-transpose-windows)
         ("H-c D" . crux-delete-file-and-buffer)
         ("H-c C" . crux-copy-file-preserve-attributes)
         ("H-c C-c" . crux-duplicate-and-comment-current-line-or-region)
         ("H-c r" . crux-rename-file-and-buffer)
         ("H-c t" . crux-visit-term-buffer)
         ("H-c K" . crux-kill-other-buffers)
         ("H-c S" . crux-find-shell-init-file)
         ("H-6" . crux-top-join-line)
         ("H-k" . crux-kill-whole-line)
         ("H-c u" . crux-upcase-region)
         ("H-c l" . crux-downcase-region)
         ("H-c M-c" . crux-capitalize-region)
         ))

;;; ace-window
;;  ---------------------------------------------------------------------------
(use-package ace-window
  :bind (("H-o" . ace-window)))

;;; tiny
;;  ---------------------------------------------------------------------------
(use-package tiny
  ;;  :init  (tiny-setup-default)
  :bind (("H-;" . tiny-expand)))

;; Examples of Tiny
;; m5 10*xx -> 25 36 49 64 81 100
;; m5 10*xx|0x%x -> 0x19 0x24 0x31 0x40 0x51 0x64
;; m10+x?a%c -> a b c d e f g h i j k
;; m10+x?A%c -> A B C D E F G H I J K
;; m97,105stringx -> a,b,c,d,e,f,g,h,i
;; m97,102stringxupcasex -> aA,bB,cC,dD,eE,fF
;; m,3|%(+ x x) and %(* x x) and %s -> 0 and 0 and 0,2 and 1 and 1,4 and 4 and 2,6 and 9 and 3

;;; company
;;  ---------------------------------------------------------------------------
(use-package company
  :defer t
  :ensure
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  )

;;; terraform-mode and company-terraform
;;  ---------------------------------------------------------------------------
(use-package terraform-mode
  :ensure
  :config
  (use-package company)
  (use-package company-quickhelp
    :config (company-quickhelp-mode))
  (use-package company-terraform
    :ensure
    :defer (company-terraform-init)
    :hook (terraform-mode . company-terraform-init)
    ))

;;; company-tabnine
;;  ---------------------------------------------------------------------------
(use-package company-tabnine :ensure t)

;;; restart-emacs
;;  ---------------------------------------------------------------------------
(use-package restart-emacs)

;;; powershell.el
;;  ---------------------------------------------------------------------------
(use-package powershell)

;;; string-inflection
;;  ---------------------------------------------------------------------------
(use-package string-inflection
  :bind (("H-q q" . string-inflection-all-cycle)
         ("H-q p" . string-inflection-python-style-cycle)
         ("H-q r" . string-inflection-ruby-style-cycle)
         ("H-q j" . string-infleciton-java-style-cycle))
  )

;;; phi-search
;;  ---------------------------------------------------------------------------
(use-package phi-search
  :init
  (require 'phi-replace)
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)
         ("H-r" . phi-replace-query)))

;;; expand-region
;;  ---------------------------------------------------------------------------
(use-package expand-region
  :defer t
  :bind (("H-[" . er/expand-region)
         ("H-]" . er/contract-region)))

(use-package smart-hungry-delete
  :ensure t
  :bind (("H-d" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (forge phi-search string-inflection nlinum powershell fill-column-indicator fci-mode company-tabnine restart-emacs company-mode company-terraform ace-jump-mode go-mode yaml-mode xterm-color web-server use-package telephone-line switch-window smartparens shell-switcher rainbow-mode rainbow-delimiters project-explorer pos-tip php-mode ob-ipython multiple-cursors markdown-mode magit-popup magit lsp-mode jsonrpc json-mode ido-vertical-mode ido-hacks helm-system-packages helm-sage helm-projectile helm-ag flycheck-inline exec-path-from-shell elpy ein-mumamo dumb-jump auto-yasnippet auto-compile ag))))
