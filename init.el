;;; init.el --- my emacs configuration
;;; Initialize
;;  -----------------------------------------------------------------------------
;;; Commentary:
;;; Code:
(package-initialize)
;;(package-refresh-contents)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/cart")

(setq package-check-signature nil)  ; because GNU ELPA keeps choking on the sigs
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't auto-initialize
(setq package-enable-at-startup nil
      ;; don't add that `custom-set-variables' block to my init!
      package--init-file-ensured t)


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


;; quelpa and quelpa-use-package
(use-package quelpa
  :config
  (setq quelpa-upgrade-p t))
(use-package quelpa-use-package)

;; Increase Garbage Collector size, improves startup speed
(setq gc-cons-threshold 50000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 10000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Get user PATH
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; check if we're on WSL (Windows Subsystem Linux)
(defconst my/wsl (not (null
                    (string-match ".*Microsoft$"
                                  (with-temp-buffer (insert-file-contents "/proc/sys/kernel/osrelease") (buffer-string))))))

(if my/wsl
    (progn
      (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "wslview")))

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

;; get that outta here!
(setq custom-file (make-temp-file "emacs-custom"))

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
(use-package switch-window
  :config
  (setq switch-window-minibuffer-shortcut ?z)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-preferred 'helm)
  :bind
  (("H-o w" . switch-window)
   ("H-o m" . switch-window-then-maximize)
   ("H-o v" . switch-window-then-split-below)
   ("H-o h" . switch-window-then-split-right)
   ("H-o r" . switch-window-then-delete)
   ("H-o d" . switch-window-then-dired)
   ("H-o f" . switch-window-then-find-file)
   ("H-o M" . switch-window-then-compose-mail)
   ("H-o n" . switch-window-then-find-file-read-only)
   ("H-o H-f" . switch-window-then-find-file)
   ("H-o H-o" . switch-window-then-display-buffer))
  )

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

;; save minibuffer history across sessions
(savehist-mode 1)

;; disable scroll lock because it keeps getting stuck and making noises.... on WSL atleast
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

;;; Appearance
;;  ----------------------------------------------------------------------------

(setq frame-title-format
      '("" invocation-name (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "[%b]"))))


;; use zenburn as the default theme
(use-package zenburn-theme
  :config
  (let ((line (face-attribute 'mode-line :underline)))
    (zenburn-with-color-variables
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)
      (set-face-attribute 'mode-line          nil :background zenburn-bg-2)
      (set-face-attribute 'mode-line-inactive nil :background zenburn-bg-1)
      (set-face-attribute 'mode-line          nil :foreground zenburn-green+4)
      (set-face-attribute 'mode-line-inactive nil :foreground "gray70")))
  :init
  (load-theme 'zenburn t)
  )

;; highlight the current line
(global-hl-line-mode +1)

;; (defun my/set-font ()
;;   "Function to manually set the font."
;;   (interactive)
;;   (set-face-attribute 'default nil :font "Source Code Pro SemiBold" :height 90)
;;   )

;; (my/set-font)

;; (setq default-frame-alist '((font . "Inconsolata-dz-15")))
;; (set-face-attribute 'bold nil :font "Operator Mono Bold" : :height 90)
;; (set-frame-font "Operator Mono Book-9" nil t) ;; this messes up my dpi? either way the text looks realllly small when i use emacs-daemon


;;; Custom Key Bindings
;;  ---------------------------------------------------------------------------
(use-package bind-key)

(use-package general)

;; use-package-hydra
(use-package hydra)
(use-package use-package-hydra)


(bind-key "H-K" 'kill-this-buffer)
(bind-key "H-c c" 'comment-or-uncomment-region)
(bind-key "H-a" 'align-current)
(bind-key "H-w" 'eval-region)
(bind-key "H-L" 'display-line-numbers-mode)
(bind-key "H-l g" 'goto-line)
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
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(global-set-key [remap backward-delete-char-untabify]
                'my/backward-delete-char)

(defun my/backward-delete-char ()
  (interactive)
  (cond ((bolp)
         (delete-char -1)
         (indent-according-to-mode)
         (when (looking-at "\\([ \t]+\\)[^ \t]")
           (delete-region (point) (match-end 1))))
        ((<= (point) (save-excursion (back-to-indentation) (point)))
         (let ((backward-delete-char-untabify-method 'hungry))
           (call-interactively 'backward-delete-char-untabify)
           (delete-char -1))
         (indent-according-to-mode))
        (t
         (let ((backward-delete-char-untabify-method 'hungry))
           (call-interactively 'backward-delete-char-untabify)))))

;; (require 'stagger-mode)
;; (bind-key "A-s" 'stagger-mode)

;; TODO: Remap move-paragraph ( M-{ , M-} ) to M-[ M-]

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
(use-package dired-avfs)
(use-package dired-subtree)
(use-package dired-collapse)
(add-hook 'dired-mode 'dired-collapse-mode)

;;; Packages
;;  ----------------------------------------------------------------------------

(setq package-list '(ag
                     auto-yasnippet
                     ;;autopair
                     ;; dumb-jump
                     diminish
                     ;;lsp-mode ;eglot
                     ;;ein-mumamo
                     ;;ein
                     auto-complete
                     ;; elpy
                     ;;company
                     ;; exec-path-from-shell
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
                     ;; highlight-indentation
                     ;;htmlize
                     ;; ido-hacks
                     ;;ido-vertical-mode
                     ;; ivy
                     json-mode
                     json-reformat
                     json-snatcher
                     jsonrpc
                     git-commit
                     ;; magit-popup
                     ;;mmm-mode
                     ;; multiple-cursors
                     ob-ipython
                     dash-functional
                     ;; org
                     ;;org-ehtml
                     php-mode
                     popup
                     project-explorer
                     es-windows
                     es-lib
                     pyvenv
                     request
                     markdown-mode
                     pkg-info
                     epl
                     f
                     dash
                     s
                     deferred
                     shell-switcher
                     spinner
                     ;;sr-speedbar ; https://www.emacswiki.org/emacs/SrSpeedbar
                     ;; switch-window
                     ;; telephone-line
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
    (use-package package)))


;; Smartparens - keep parentheses balanced (from Jamie's)
(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; Highlight nested parentheses (from Jamie's)
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Color comprehension
(use-package rainbow-mode
  :diminish rainbow-mode
  :custom
  (rainbow-x-colors nil)
  :hook prog-mode
  )

;; Flycheck
(use-package flycheck
  :custom
  (flycheck-python-flake8-executable "python3")
  (flycheck-python-pylint-executable "python3")
  (flycheck-python-mypy-executable   "python3")
  :config
  (global-flycheck-mode))
(use-package flycheck-inline
  :hook
  (flycheck-mode . turn-on-flycheck-inline))

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
  :init
  (use-package mc-extras)
  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))

(defhydra hydra-mc (:hint nil)
  "
      ^Up^            ^Down^        ^All^                ^Lines^               ^Edit^                 ^Other^
-----------------------------------------------------------------------------------------------------------------------
[_p_]   Prev    [_n_]   Next    [_A_] All like this  [_E_] Edit lines      [_i_] Insert numbers   [_t_] Tag pair
[_P_]   Skip    [_N_]   Skip    [_r_] All by regexp  [_a_] Edit line beg.  [_s_] Sort regions     [_._] Set Rect Region
[_M-p_] Unmark  [_M-n_] Unmark  [_d_] All DWIM       [_e_] Edit line ends. [_R_] Reverse regions  [_q_] Quit
[_{_] Prev Sexp [_}_] Next Sexp [_<_] All Above      [_|_] Align w/ Space  [_c_] Insert letters
                                [_>_] All Below      [_/_] Align w/ Char   [_=_] Compare chars
                                                     [_?_] Move to Column
"
  ("p"   mc/mark-previous-like-this)
  ("P"   mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("{"   mc/mark-previous-sexps)

  ("n"   mc/mark-next-like-this)
  ("N"   mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("}"   mc/mark-next-sexps)

  ("A"   mc/mark-all-like-this)
  ("r"   mc/mark-all-in-region-regexp)
  ("d"   mc/mark-all-dwim)
  ("<"   mc/mark-all-above)
  (">"   mc/mark-all-below)

  ("E"   mc/edit-lines)
  ("a"   mc/edit-beginnings-of-lines)
  ("e"   mc/edit-ends-of-lines)
  ("|"   mc/vertical-align-with-space)
  ("/"   mc/vertical-align)
  ("?"   mc/move-to-column)

  ("i"   mc/insert-numbers)
  ("s"   mc/sort-regions)
  ("R"   mc/reverse-regions)
  ("c"   mc/insert-letters)
  ("="   mc/compare-chars)

  ("t"   mc/mark-sgml-tag-pair)
  ("q"   nil)
  ("."   set-rectangular-region-anchor)

  ;;  ("x" er/expand)
  ("<mouse-1>" mc/add-cursor-on-click)
  )
(bind-key "H-m" 'hydra-mc/body)

;; Yasnippet
(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
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
  (use-package magit-todos
    :custom
    (magit-todos-insert-at 'unpushed)
    :config
    (use-package hl-todo
      :config (global-hl-todo-mode))
    (magit-todos-mode)
    )
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-untracked-files nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-ignored-files   nil t)
  :custom
  (magit-stage-all-confirm nil)
  (magit-unstage-all-confirm nil)
  (magit-completing-read-function 'helm--completing-read-default)
  )


(use-package forge
  :defer t)

;; hydra for convienent smerge commands
;; hydra stolen from alphapapa's unpackaged repository
(use-package smerge-mode
  :after hydra
  :config
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (smerge-hydra/body)))))

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
  :diminish projectile-mode
  :bind-keymap
  ("H-p" . projectile-command-map)
  :custom
  (projectile-file-exists-remote-cache-expire nil)
  ;; (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
  (projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".svn" ".fslckout" ".bzr" "_darcs" ".tox" "build" "target"))
  :config
  (use-package persp-projectile
    :config
    (persp-mode))
  (projectile-mode +1))

;; Helm - incremental completions and narrowing
(use-package helm
  :diminish helm-mode
  :config
  (use-package helm-projectile
    :config
    (helm-projectile-on))
  (use-package helm-ag)
  (use-package helm-company
    :config
    (progn
     (define-key company-mode-map (kbd "H-;") 'helm-company)
     (define-key company-active-map (kbd "H-;") 'helm-company)))
  :custom
  (helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
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

;; Basic python-mode config.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("/usr/bin/ipython3" . python-mode)
  :hook
  (python-mode . subword-mode)
  :config
  (use-package pyvenv))

;; Elpy makes Emacs a full Python IDE.
(use-package elpy
  :custom
  (elpy-rpc-python-command "python3")
  (elpy-modules '(elpy-module-company))
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter "-i --simple-prompt")
  :config
  (elpy-enable))

;;; Markdown (from Jamie's)
;;  ----------------------------------------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

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
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))


;;; Ace-Jump-Mode
;;  ----------------------------------------------------------------------------
(use-package ace-jump-mode
  :custom
  (ace-jump-mode-scope 'window)
  (ace-jump-mode-case-fold t)

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
  ;; :disabled
  :custom
  (highlight-indentation-face "#4F4F4F")
  (highlight-indentation-current-column-face "#5F5F5F")
  ;; (highlight-indentation-mode 0)
  :bind (("H-h i" . highlight-indentation-mode))
  )


;;; Telephone Line (powerline)
;;  ----------------------------------------------------------------------------
(use-package telephone-line
  :disabled
  :init
  (setq telephone-line-primary-right-separator 'telephone-line-halfcos-left
     telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-left
     telephone-line-primary-left-separator 'telephone-line-halfcos-left
     telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left)
  (telephone-line-mode 1))

;;; Org-Mode
;;  ----------------------------------------------------------------------------
(use-package org-mode
  :quelpa (org-mode :fetcher git :url "https://code.orgmode.org/bzg/org-mode.git" :branch "maint")
  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-files '("~/org-files"))
  :bind
  (("H-r l" . org-store-link)
   ("H-r a" . org-agenda)
   ("H-r c" . org-capture)
   ("H-r b" . org-switchb)
   )
  :hook (org-mode . visual-line-mode)
  )

(use-package org-jira
  :custom
  (jiralib-url "https://code.integrity-apps.com:8443/jira")
  (org-jira-jira-status-to-org-keyword-alist
   '(("In Progress" . "INPROGRESS")
     ("To Do" . "TODO")
     ("In Review" . "REVIEW")
     ("Done" . "DONE")))
  (org-todo-keyword-faces
   '(("TODO" . "orange")
     ("INPROGRESS" . "yellow")
     ("REVIEW" . "blue")))
  ;; (org-jira-custom-jqls
  ;;  '(:jql "assignee = currentUser() AND resolution IN (Unresolved, Incomplete) ORDER BY priority DESC, updated DESC, created ASC"))
  ;; "<option value="">Unresolved</option>"
  )


;;; aggressive-indent-mode
;;  ---------------------------------------------------------------------------
(use-package aggressive-indent
  :disabled
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
  (crux-with-region-or-line eval-region)
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
;; (use-package ace-window
;;   :bind (("H-o" . ace-window)))

;;; tiny
;;  ---------------------------------------------------------------------------
(use-package tiny
  ;;  :init  (tiny-setup-default)
  :bind (("H-'" . tiny-expand)))

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
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (company-frontends
   '(company-tng-frontend
     company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  )

;;; terraform-mode and company-terraform
;;  ---------------------------------------------------------------------------
(use-package terraform-mode
  :defer t
  :config
  (use-package company)
  (use-package company-quickhelp
    :config (company-quickhelp-mode))
  (use-package company-terraform
    :defer (company-terraform-init)
    :hook (terraform-mode . company-terraform-init)
    ))

;;; company-tabnine
;;  ---------------------------------------------------------------------------
(use-package company-tabnine)

;;; company-lsp
;;  ---------------------------------------------------------------------------
(use-package lsp-mode
  :config
  (use-package company-lsp
    :config
    (push 'company-lsp company-backends)
    )
)
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
  :custom
  (phi-search-limit 10000)
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)
         ("H-R" . phi-replace-query)))

;;; expand-region
;;  ---------------------------------------------------------------------------
(use-package expand-region
  :defer t
  :bind (("H-[" . er/expand-region)
         ("H-]" . er/contract-region)))

(use-package smart-hungry-delete
  :disabled
  :bind (("H-d" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )

(use-package tide
  :defer t
  :after (typescript-mode company flycheck)
  :config
  (use-package js-mode
    :custom (js-indent-level 8))
  :custom
  (tide-format-options '(:tabSize 8
                                  :indentSize 8))

  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         )
  )

(use-package objed
  :quelpa (objed :fetcher git :url "https://github.com/clemera/objed" :branch "master")
  :config
  (use-package avy)
  :bind ("H-e" . objed-activate))

(use-package scrollkeeper
  :general ([remap scroll-up-command]   #'scrollkeeper-contents-up
            [remap scroll-down-command] #'scrollkeeper-contents-down)
  :custom
  (scrollkeeper-scroll-distance 0.9)
  (scrollkeeper-scroll-steps 2)
  :custom-face
  (scrollkeeper-guideline-highlight ((t (:background "#BFEBBF"))))
  )

(use-package torus
  :disabled
  :bind-keymap ("H-t" . torus-map)
  :bind (:map torus-map
              ("t" . torus-copy-to-circle))
  :hook ((emacs-startup . torus-start)
         (save-buffers-kill-emacs . torus-quit)
         (kill-emacs . torus-quit))
  :custom ((torus-prefix-key "H-t")
           (torus-binding-level 3)
           (torus-verbosity 1)
           (torus-dirname (concat user-emacs-directory (file-name-as-directory "torus")))
           (torus-load-on-startup t)
           (torus-save-on-exit t)
           (torus-autoread-file (concat torus-dirname "last.el"))
           (torus-autowrite-file torus-autoread-file)
           (torus-backup-number 10)
           (torus-history-maximum-elements 30)
           (torus-maximum-horizontal-split 4)
           (torus-maximum-vertical-split 4)
           (torus-display-tab-bar t)
           (torus-separator-torus-circle " >> ")
           (torus-separator-circle-location " > ")
           (torus-prefix-separator "/")
           (torus-join-separator " & "))
  :config
  ;; (torus-init)
  (torus-install-default-bindings))

(use-package helm-spotify-plus
  :disabled
  :bind (
         ("H-s s" . helm-spotify-plus)  ;; s for SEARCH
         ("H-s f" . helm-spotify-plus-next)
         ("H-s b" . helm-spotify-plus-previous)
         ("H-s p" . helm-spotify-plus-play)
         ("H-s g" . helm-spotify-plus-pause)
         ))

(use-package paradox
  :custom
  (paradox-github-token t)
  (paradox-execute-asynchronously t))

(use-package helm-posframe
  :disabled
  :custom
  (helm-posframe-poshandler 'posframe-poshandler-frame-center))

(use-package web-mode
  :mode "\\.j2\\'")

;;; init.el ends here
