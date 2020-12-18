;;; init.el --- my emacs configuration  -*- lexical-binding: t; -*-

;;; Initialize
;;  -----------------------------------------------------------------------------
;;; Commentary:
;;; Code:
(package-initialize)
;;(package-refresh-contents)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/cart")
  (add-to-list 'load-path "~/.emacs.d/elisp/")
  )

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

;; Bootstrap straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(watch-files find-when-checking))


;; Increase Garbage Collector size, improves startup speed
(setq gc-cons-threshold 50000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 10000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; improve process parsing speed (JSON, language servers, etc)
(setq read-process-output-max (* 2048 2048)) ;; 2mb


;; Get user PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; The :ensure-system-package keyword allows you to ensure system binaries exist alongside your package declarations.
(use-package use-package-ensure-system-package)


;; check if we're on WSL (Windows Subsystem Linux)
;; (defconst my/wsl (not (null
                    ;; (string-match ".*Microsoft$"
                    ;;               (with-temp-buffer (insert-file-contents "/proc/sys/kernel/osrelease") (buffer-string))))))

(defconst my/wsl (not (null (string-match "Linux.*Microsoft" (shell-command-to-string "uname -a")))))

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
  :custom
  (switch-window-minibuffer-shortcut ?z)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-preferred 'helm)
  :bind
  (("H-o w" . switch-window)
   ("H-o m" . switch-window-then-maximize)
   ("H-o v" . switch-window-then-split-below)
   ("H-o h" . switch-window-then-split-right)
   ("H-o r" . switch-window-then-delete)
   ("H-o d" . switch-window-then-dired)
   ("H-o f" . switch-window-then-find-file)
   ("H-o H-f" . switch-window-then-find-file)
   ("H-o M" . switch-window-then-compose-mail)
   ("H-o n" . switch-window-then-find-file-read-only)
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
(setq echo-keystrokes 0.01)

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

;; enable local variables
(setq enable-local-variables :all)

;;; Appearance
;;  ----------------------------------------------------------------------------

(setq frame-title-format
      '("" invocation-name (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "[%b]"))))


;; use zenburn as the default theme
(use-package zenburn-theme
  :straight (zenburn-theme :host github :repo "bbatsov/zenburn-emacs"
                           :fork (:host github :repo "rirze/zenburn-emacs"))
  :defines (z-variable-pitch
            z-class
            zenburn-fg-1
            zenburn-fg-05
            zenburn-fg
            zenburn-fg+1
            zenburn-fg+2
            zenburn-bg-2
            zenburn-bg-1
            zenburn-bg-08
            zenburn-bg-05
            zenburn-bg
            zenburn-bg+05
            zenburn-bg+1
            zenburn-bg+2
            zenburn-bg+3
            zenburn-red-6
            zenburn-red-5
            zenburn-red-4
            zenburn-red-3
            zenburn-red-2
            zenburn-red-1
            zenburn-red
            zenburn-red+1
            zenburn-red+2
            zenburn-orange
            zenburn-yellow-2
            zenburn-yellow-1
            zenburn-yellow
            zenburn-green-5
            zenburn-green-4
            zenburn-green-3
            zenburn-green-2
            zenburn-green-1
            zenburn-green
            zenburn-green+1
            zenburn-green+2
            zenburn-green+3
            zenburn-green+4
            zenburn-cyan
            zenburn-blue+3
            zenburn-blue+2
            zenburn-blue+1
            zenburn-blue
            zenburn-blue-1
            zenburn-blue-2
            zenburn-blue-3
            zenburn-blue-4
            zenburn-blue-5
            zenburn-magenta
            )
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
(bind-key "H-E" 'eval-region)
(bind-key "H-L" 'display-line-numbers-mode)
(bind-key "H-g l" 'goto-line)
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


(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "C-c o" 'switch-to-minibuffer)


;; select current line
(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(bind-key "H-c L" 'select-current-line)

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
  "Testing deleting multiple empty spaces with backspace."
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


(require 'stagger-mode)
(bind-key "A-s" 'stagger-mode)

(require 'increment-chars)
(bind-key "H-i l" 'increment-char-at-point)
(bind-key "H-i h" 'decrement-char-at-point)
(bind-key "H-i j" 'increment-number-or-char-at-point)
(bind-key "H-i k" 'decrement-number-or-char-at-point)

(defun my/backward-capitalize-word (number)
  "Capitalize the word that is before the point.  Optionally provide (NUMBER) for multiple consecutive targets."
  (interactive "p")
  (capitalize-word (- number)))

(bind-key "M-c" 'my/backward-capitalize-word)

;; Remap move-paragraph ( M-{ , M-} ) to something more reachable
(bind-key "C->" 'forward-paragraph)
(bind-key "C-<" 'backward-paragraph)

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

;; (setq package-list '(ag
;;                      auto-yasnippet
;;                      ;;autopair
;;                      ;; dumb-jump
;;                      diminish
;;                      ;;lsp-mode ;eglot
;;                      ;;ein-mumamo
;;                      ;;ein
;;                      auto-complete
;;                      ;; elpy
;;                      ;;company
;;                      ;; exec-path-from-shell
;;                      fill-column-indicator
;;                      ;;find-file-in-project
;;                      flycheck
;;                      flymake
;;                      ;;git-commit-mode
;;                      helm-ag
;;                      helm-projectile
;;                      ;;helm-sage
;;                      helm-system-packages
;;                      helm
;;                      helm-core
;;                      ;; highlight-indentation
;;                      ;;htmlize
;;                      ;; ido-hacks
;;                      ;;ido-vertical-mode
;;                      ;; ivy
;;                      json-mode
;;                      json-reformat
;;                      json-snatcher
;;                      jsonrpc
;;                      git-commit
;;                      ;; magit-popup
;;                      ;;mmm-mode
;;                      ;; multiple-cursors
;;                      ob-ipython
;;                      dash-functional
;;                      ;; org
;;                      ;;org-ehtml
;;                      php-mode
;;                      popup
;;                      project-explorer
;;                      es-windows
;;                      es-lib
;;                      pyvenv
;;                      request
;;                      markdown-mode
;;                      pkg-info
;;                      epl
;;                      f
;;                      dash
;;                      s
;;                      deferred
;;                      shell-switcher
;;                      spinner
;;                      ;;sr-speedbar ; https://www.emacswiki.org/emacs/SrSpeedbar
;;                      ;; switch-window
;;                      ;; telephone-line
;;                      web-server
;;                      websocket
;;                      with-editor
;;                      async
;;                      xterm-color
;;                      yaml-mode
;;                      yasnippet))

;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (use-package package)))

;;; company
;;  ---------------------------------------------------------------------------
(use-package company
  :defer t
  :diminish company-mode
  :hook (after-init-hook . global-company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :custom
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  ;; (company-tng-configure-default)
  (company-frontends
   '(;; company-tng-frontend               ;
     company-pseudo-tooltip-frontend
     company-echo-metadata-frontend
     ))

  )


(use-package diminish)

(diminish 'eldoc-mode)
(diminish 'subword-mode)

;; Smartparens - keep parentheses balanced
(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :bind ("H-w" . sp-rewrap-sexp))

;; Highlight nested parentheses
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
  ;(flycheck-python-mypy-executable   "python3")
  (flycheck-python-pyright-executable   "python3")
  :config
  (global-flycheck-mode))

(use-package flycheck-inline
  :hook
  (flycheck-mode . turn-on-flycheck-inline))

(use-package flyspell
  :diminish flyspell-mode
  :hook
  (prog-mode . flyspell-prog-mode))

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
  (use-package mc-extras
    :straight (mc-extras :host github :repo "knu/mc-extras.el"
                         :fork (:host github
                                      :repo "rirze/mc-extras.el")))
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
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets)
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
(use-package ag
  :ensure-system-package (ag . silversearcher-ag))

(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :init
  (use-package prescient
    :straight (selectrum-prescient :host github :repo "raxod502/prescient.el"
                                   :files ("selectrum-prescient.el"))
    :config
    ;; to save your command history on disk, so the sorting gets more
    ;; intelligent over time
    (prescient-persist-mode +1))
  :config
  ;; integrate the two
  (selectrum-prescient-mode +1)
  )

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
    :bind-keymap ("H-x" . perspective-map)
    :commands (persp-mode))
  (projectile-mode +1))


;; Helm - incremental completions and narrowing
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  :custom
  ;;; (helm-linum-relative-mode 1)
  (helm-completion-style 'emacs)
  (completion-styles '(helm-flex)) ;'(helm-flex))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-z" . helm-select-action)
         ("M-y" . helm-show-kill-ring)
         ("H-;" . helm-semantic-or-imenu)
         ("H-'" . helm-occur)
         )
  )

(use-package helm-fzf
  :after helm
  :straight (helm-fzf :type git :host github :repo "ofnhwx/helm-fzf")
  :ensure-system-package
  ((fd . "cargo install fd-find")
   (sk . "cargo install skim"))
  :custom
  (helm-fzf-executable "fd | sk")
  ;; '(helm-fzf-args '())
  :config
  (defun fzf-from-helm-session ()
    "run fzf from a helm session."
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit
       'helm-fzf
       helm-ff-default-directory)))
  ;; (helm-add-action-to-source "Switch to fzf" #'my/helm-run-fzf helm-source-find-files)
  ;; (map-put helm-find-files-actions '"fff `C-,'" 'fzf-from-helm-session)
  ;; (setq helm-find-files-actions (append helm-find-files-actions "fff `C-,'" 'fzf-from-helm-session))
  :bind
  (:map helm-find-files-map
        ("C-," . fzf-from-helm-session)
        )
  )

(use-package fuz
  :diminish helm-fuz-mode
  :after helm
  :straight (fuz :host github :repo "rustify-emacs/fuz.el" :files (:defaults "Cargo*" "src"))
  :init
  (setq completion-styles '())
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod))
  (require 'fuz-core)
  (require 'helm-fuz)
  (helm-fuz-mode)
  )


(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure-system-package ag)

(use-package helm-company
  :config
  (progn
    (define-key company-mode-map (kbd "H-;") 'helm-company)
    (define-key company-active-map (kbd "H-;") 'helm-company)))

(use-package helm-flycheck
  :straight (helm-flycheck :type git :host github :repo "cslux/helm-flycheck")
  :bind (
         ("C-c h" . helm-flycheck))
  )

(use-package ace-jump-helm-line
  :straight (ace-jump-helm-line :host github :repo "cute-jumper/ace-jump-helm-line")
  :bind (:map helm-map
              ("C-;" . ace-jump-helm-line)))

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
  :diminish subword-mode
  :interpreter ("/usr/bin/ipython3" . python-mode)
  :hook
  (python-mode . subword-mode)
  :custom
  (python-shell-interpreter "python")
  :config
  (use-package pyvenv))

;; Elpy makes Emacs a full Python IDE.
(use-package elpy
  :disabled
  :custom
  (elpy-rpc-python-command "python3")
  (elpy-modules '(elpy-module-company))
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter-args "-i --simple-prompt")
  :config
  (elpy-enable))

(use-package pip-requirements)

(defun my/setup-ms-python-lsp ()
  "Setup us Microsoft Python Language Server with custom Flycheck module since mspyls doesn't have a syntax checker."
  (progn
    (setq-local lsp-flycheck-live-reporting nil) ; mspyls doesn't report errors, so disable lsp waiting for errors
    (require 'lsp-python-ms)
    (lsp)
    (my/setup-python-flycheck)
    )
  )

(defun my/setup-python-flycheck ()
  "Setup the default python Flycheck syntax checker."
  (progn
    (setq-local flycheck-checker 'python-flake8)
    )
  )

(use-package lsp-python-ms
  :disabled
  :ensure t
  :hook (python-mode . my/setup-ms-python-lsp)
  :custom
  (lsp-python-ms-python-executable-cmd "python3")
  (lsp-disabled-clients '(pyls)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))  ; or lsp-deferred
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  )

(use-package flycheck-pycheckers
  :after flycheck
  :straight (flycheck-pycheckers :host github :repo "msherry/flycheck-pycheckers")
  :hook (flycheck-mode . flycheck-pycheckers-setup)
  :custom
  (flycheck-pycheckers-command "~/.emacs.d/straight/repos/flycheck-pycheckers/bin/pycheckers.py")
  (flycheck-pycheckers-checkers '(flake8)))

(use-package company-jedi             ;;; company-mode completion back-end for Python JEDI
  :disabled
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

(use-package py-isort)
;;; Java
;;  ----------------------------------------------------------------------------
(use-package lsp-java
  :defer t
  :after lsp
  :hook (java-mode . lsp))

;;; Kotlin
;;  ----------------------------------------------------------------------------
(use-package kotlin-mode
  :defer t)

;; (use-package lsp-kotlin
;;   :after lsp
;;   :straight (lsp-kotlin :type git :host github :repo "whily/lsp-kotlin"))

(add-to-list 'exec-path "~/cart/kotlin-language-server/server/build/scripts")

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

(use-package go-mode
  :defer t)

;;; C/C++
;;  ----------------------------------------------------------------------------

(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

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
  :bind (("H-d" . ace-jump-char-mode)
         ("H-j w" . ace-jump-word-mode)
         ("H-j c" . ace-jump-char-mode)
         ("H-j l" . ace-jump-line-mode)
         ("H-j v" . ace-jump-mode)
         )
  )

(use-package ace-link
  :config
  (ace-link-setup-default)
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
;; NOTE: Enabling/disabling this, before running Emacs or downloading
;; org-plus-contrib doesn't change anything (visibly).
(setq-default straight-fix-org t)
(use-package org
  :defines (org-log-states)
  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-files '("~/org"))
  (org-odt-preferred-output-format "docx")
  ;; (org-hierarchical-todo-statistics nil)
  ;; (org-checkbox-hierarchical-statistics nil)
  :bind
  (("H-r l" . org-store-link)
   ("H-r a" . org-agenda)
   ("H-r c" . org-capture)
   ("H-r b" . org-switchb)
   )
  :config
  (defun org-summary-todo (_n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  :hook
  (org-mode . visual-line-mode)
  (org-after-todo-statistics . org-summary-todo)

  )

(use-package org-jira
  :disabled
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

(use-package org-roam
  :diminish org-roam-mode
  :after org
  :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")

  :hook
  (org-mode . org-roam-mode)
  (after-init . org-roam--build-cache-async) ;; optional!

  :custom
  (org-roam-directory "~/org")

  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

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
  :disabled
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
  :bind (("H-i =" . operate-on-number-at-point)))

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
         ("H-c d" . crux-duplicate-current-line-or-region)
         ("H-c r" . crux-rename-file-and-buffer)
         ("H-c t" . crux-visit-term-buffer)
         ("H-c K" . crux-kill-other-buffers)
         ("H-c S" . crux-find-shell-init-file)
         ("H-6" . crux-top-join-line)
         ("H-k" . crux-kill-whole-line)
         ("H-c u" . crux-upcase-region)
         ("H-c l" . crux-downcase-region)
         ("H-c M-c" . crux-capitalize-region)
         )
  )

;;; ace-window
;;  ---------------------------------------------------------------------------
;; (use-package ace-window
;;   :bind (("H-o" . ace-window)))

;;; tiny
;;  ---------------------------------------------------------------------------
(use-package tiny
  ;;  :init  (tiny-setup-default)
  :bind (("H-\"" . tiny-expand)))

;; Examples of Tiny
;; m5 10*xx -> 25 36 49 64 81 100
;; m5 10*xx|0x%x -> 0x19 0x24 0x31 0x40 0x51 0x64
;; m10+x?a%c -> a b c d e f g h i j k
;; m10+x?A%c -> A B C D E F G H I J K
;; m97,105stringx -> a,b,c,d,e,f,g,h,i
;; m97,102stringxupcasex -> aA,bB,cC,dD,eE,fF
;; m,3|%(+ x x) and %(* x x) and %s -> 0 and 0 and 0,2 and 1 and 1,4 and 4 and 2,6 and 9 and 3

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

;;; company-lsp
;;  ---------------------------------------------------------------------------
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "H-l")
  :hook
  (sh-mode . lsp)
  :config
  (advice-add #'lsp--spinner-start :around #'ignore)
  (advice-add #'lsp--spinner-stop :around #'ignore)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  )

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (defun lsp--sort-completions (completions)
  (lsp-completion--sort-completions completions))

  (defun lsp--annotate (item)
    (lsp-completion--annotate item))

  (defun lsp--resolve-completion (item)
    (lsp-completion--resolve item))
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
  :disabled
  )

(use-package lsp-ui
  :disabled
  :custom
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-max-width 50)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-always-show t)
  )

;;; company-tabnine
;;  ---------------------------------------------------------------------------
(use-package company-tabnine
  :disabled
  :init
  (add-to-list 'company-backends #'company-tabnine))


;;; restart-emacs
;;  ---------------------------------------------------------------------------
(use-package restart-emacs)

;;; powershell.el
;;  ---------------------------------------------------------------------------
(use-package powershell
  :defer t)

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
;; (use-package phi-search
;;   :init
;;   (require 'phi-replace)
;;   :custom
;;   (phi-search-limit 10000)
;;   :bind (("C-s" . phi-search)
;;          ("C-r" . phi-search-backward)
;;          ("H-R" . phi-replace-query)))


;;; rustic
;;  ---------------------------------------------------------------------------
(use-package rustic
  :defer t
  )


;;; expand-region
;;  ---------------------------------------------------------------------------
(use-package expand-region
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
  :straight (objed :type git :host github :repo "clemera/objed")
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
  :bind (
         ("H-s s" . helm-spotify-plus)  ;; s for SEARCH
         ("H-s f" . helm-spotify-plus-next)
         ("H-s b" . helm-spotify-plus-previous)
         ("H-s p" . helm-spotify-plus-toggle-play-pause)
         ))

(use-package helm-dash
  :config
  (defun set-python-docs ()
    (interactive)
    (setq-local dash-docs-docsets '("Python 3" "NumPy")))
  :hook (python-mode . set-python-docs)
  :custom
  (helm-dash-docsets-path "~/.docsets"))

(use-package helm-github-stars
  :defer t
  :custom (helm-github-stars-username "rirze"))

(use-package helm-gitignore)

(use-package helm-system-packages)

(use-package helm-tramp)

(use-package paradox
  :disabled
  :custom
  (paradox-github-token t)
  (paradox-execute-asynchronously t))

(use-package helm-posframe
  :disabled
  :custom
  (helm-posframe-poshandler 'posframe-poshandler-frame-center))

(use-package web-mode
  :defer t
  :mode "\\.j2\\'")

;; Ansible
(use-package ansible)
(use-package company-ansible)

;; json tools
(use-package json-mode
  :straight (json-mode :type git :host github :repo "DoMiNeLa10/json-mode")
  :custom
  ;; (json-reformat:indent-level 2)
  ;; (js2-basic-offset 2)
  (js-indent-level 2)
  )



;;; Kakoune
;;  ---------------------------------------------------------------------------
(use-package kakoune
  :defer t
  ;; Having a non-chord way to escape is important, since key-chords don't work in macros
  :bind ("C-z" . ryo-modal-mode)
  :hook (after-init . my/kakoune-setup)
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (defun ryo-enter () "Enter normal mode" (interactive) (ryo-modal-mode 1))
  (defun my/kakoune-setup ()
    "Call kakoune-setup-keybinds and then add some personal config."
    (kakoune-setup-keybinds)
    (setq ryo-modal-cursor-type 'box)
    (setq ryo-modal-cursor-color "PaleGreen")
    ;; (add-hook 'prog-mode-hook #'ryo-enter)
    (define-key ryo-modal-mode-map (kbd "SPC h") 'help-command)
    ;; Access all C-x bindings easily
    (define-key ryo-modal-mode-map (kbd "z") ctl-x-map)
    (ryo-modal-keys
     ("," save-buffer)
     ;; ("P" counsel-yank-pop)
     ("m" mc/mark-next-like-this)
     ("M" mc/skip-to-next-like-this)
     ("n" mc/mark-previous-like-this)
     ("N" mc/skip-to-previous-like-this)
     ("M-m" mc/edit-lines)
     ("*" mc/mark-all-like-this)
     ("v" er/expand-region)
     ("C-v" set-rectangular-region-anchor)
     ("M-s" mc/split-region)
     (";" (("q" delete-window)
           ("v" split-window-horizontally)
           ("s" split-window-vertically)))
     ("C-h" windmove-left)
     ("C-j" windmove-down)
     ("C-k" windmove-up)
     ("C-l" windmove-right)
     ("C-u" scroll-down-command :first '(deactivate-mark))
     ("C-d" scroll-up-command :first '(deactivate-mark)))))

;; This overrides the default mark-in-region with a prettier-looking one,
;; and provides a couple extra commands
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace))
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace))

(use-package visual-regexp-steroids
  :after visual-regexp)

;; Probably the first thing you'd miss is undo and redo, which requires an extra package
;; to work like it does in kakoune (and almost every other editor).
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ryo
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("SPC u" undo-tree-visualize)
  :bind (("C-x u" . undo-tree-visualize)
         :map undo-tree-visualizer-mode-map
         ("h" . undo-tree-visualize-switch-branch-left)
         ("j" . undo-tree-visualize-redo)
         ("k" . undo-tree-visualize-undo)
         ("l" . undo-tree-visualize-switch-branch-right)
         ("C-g" . undo-tree-visualizer-quit)))


(use-package ssh-config-mode
  :custom
  (ssh-config-mode-indent 4))

;; use tree-sitter

(use-package tree-sitter
  ;;:disabled
  ;:load-path "/home/chronos/.emacs.d/straight/repos/emacs-tree-sitter"
  ;; :straight (tree-sitter :type git :host github :repo "ubolonton/emacs-tree-sitter")
  )

(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
)

;; (defun my/pre-tree-sitter-install ()
;;   "Function to run before tree-sitter install."
;;   (require 'tree-sitter-langs)
;;   (tree-sitter-download-dyn-module)
;;   (tree-sitter-langs-install)
;;   )

;; ;; (add-to-list 'load-path "/home/chronos/.emacs.d/straight/repos/emacs-tree-sitter/")
;; (eval-when-compile
;;   (add-to-list 'load-path "/home/chronos/.emacs.d/straight/repos/emacs-tree-sitter/lisp"))
;; (require 'tree-sitter)
;; (diminish 'tree-sitter-mode)

;; (setq tree-sitter-highlight-query-dir "/home/chronos/cart")
;; (require 'tree-sitter-highlight)
;; (diminish 'tree-sitter-highlight-mode)

;; (defun enable-ts-hl ()
;;   "Function for enabling tree-sitter-highlight-mode in buffers."
;;   (font-lock-mode -1) ;; Disable font-lock mode
;;   (tree-sitter-highlight-mode))

;;(add-to-list 'tree-sitter-major-mode-language-alist '((python-mode . python)))
;;(add-hook 'python-mode-hook #'enable-ts-hl)
;; (add-hook 'js-mode-hook 'enable-ts-hl)
;; (add-hook 'rustic-mode-hook 'enable-ts-hl)


(require 'cfn-lint)

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

;; impatient mode
;; See the effect of your HTML as you type it.
;;  * [YouTube demo](http://youtu.be/QV6XVyXjBO8)
(use-package impatient-mode)

(use-package vterm
  :straight (;vterm :type git :host github :repo "akermu/emacs-libvterm"
             :post-build (let ((vterm-always-compile-module t))
                           (require 'vterm)))
  :bind (("C-c v" . vterm)
         :map vterm-mode-map
         ("C-g" . vterm--self-insert)
         ("C-u" . vterm--self-insert)
         ("C-k" . vterm--self-insert)
         ("C-v" . vterm--self-insert)
         ("C-2" . vterm--self-insert))
  )

(use-package vterm-toggle
  :disabled
  :straight (vterm :type git :host github :repo "jixiuf/vterm-toggle")
  :custom
  (vterm-toggle-fullscreen-p nil)
  :bind (
         ("H-t t" . vterm-toggle)
         ("H-T" . vterm-toggle-cd)
         ("H-t d" . vterm-toggle-insert-cd)
         ("H-t n" . vterm-toggle-forward)
         ("H-t p" . vterm-toggle-forward)
         )
  )

(use-package parrot
  :bind (
         ("H-," . parrot-rotate-next-word-at-point)
         ("H-." . parrot-rotate-prev-word-at-point))
  :config
  (setq parrot-rotate-dict
        '(
          (:rot ("alpha" "beta") :caps t :lower nil)
          ;; => rotations are "Alpha" "Beta"

          (:rot ("yes" "no") :caps t :upcase t)
          ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"
          (:rot ("true" "false") :caps t :upcase t)
          ;; => rotations are "true" "false", "True" "False", "TRUE" "FALSE"

          (:rot ("&" "|"))
          ;; => rotations are "&" "|"

          (:rot ("(" "[" "{"))
          (:rot (")" "]" "}"))
          (:rot ("is" "is not"))
          (:rot ("and" "or") :caps t :upcase t)
          ;; default dictionary starts here ('v')
          (:rot ("begin" "end") :caps t :upcase t)
          (:rot ("enable" "disable") :caps t :upcase t)
          (:rot ("enter" "exit") :caps t :upcase t)
          (:rot ("forward" "backward") :caps t :upcase t)
          (:rot ("front" "rear" "back") :caps t :upcase t)
          (:rot ("get" "set") :caps t :upcase t)
          (:rot ("high" "low") :caps t :upcase t)
          (:rot ("in" "out") :caps t :upcase t)
          (:rot ("left" "right") :caps t :upcase t)
          (:rot ("min" "max") :caps t :upcase t)
          (:rot ("on" "off") :caps t :upcase t)
          (:rot ("prev" "next"))
          (:rot ("start" "stop") :caps t :upcase t)
          (:rot ("&&" "||"))
          (:rot ("==" "!="))
          (:rot ("<" ">"))
          (:rot ("." "->"))
          (:rot ("if" "else" "elif"))
          (:rot ("ifdef" "ifndef"))
          (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
          (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
          (:rot ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
          (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
          ))
  )


(use-package piper
  :straight (emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper"))


(use-package elcord
  :init
  (elcord-mode))

(use-package vlf
  :straight (vlf :type git :host github :repo "m00natic/vlfi")
  :config
  (require 'vlf-setup))


(use-package intero
  :defer t
  :hook (haskell-mode . intero-mode)
  )

(use-package helm-aws
  :straight (helm-aws :type git :host github :repo "istib/helm-aws")
  )


(use-package easy-kill
  :bind*
  ("C-." . easy-kill)
  ("C-," . easy-mark))

(use-package easy-kill-extras
  :after easy-kill
  :bind (
         ("C-?" . easy-mark-to-char)
         )
  :init
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?h buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))
  )


(use-package ctrlf
  :straight (ctrlf :type git :host github :repo "raxod502/ctrlf")
  :config
  (ctrlf-mode))


;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
