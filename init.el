;;; init.el --- my emacs configuration  -*- lexical-binding: t; -*-
(setopt default-frame-alist '((undecorated . t)))

(setq-default inhibit-startup-message t ;; dont show startup message
              ring-bell-function 'ignore ;; disable all visual and audible bells
              indent-tabs-mode nil ;; uses spaces and not tabs
              create-lockfiles nil ;; do not create lockfiles
              truncate-lines t;;nil ;; truncate lines by default
              ;;truncate-partial-width-windows nil;;t
              ;; disable until we actually call it
              recentf-auto-cleanup 'never
              ;; Resizing the Emacs frame can be a terribly expensive part of changing the
              ;; font. By inhibiting this, we halve startup times, particularly when we use
              ;; fonts that are larger than the system default (which would resize the frame).
              frame-inhibit-implied-resize t
              ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
              idle-update-delay 1.0
              ;; Font compacting can be terribly expensive, especially for rendering icon
              ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
              ;; hasn't been determined, but do it there anyway, just in case. This increases
              ;; memory usage, however!
              inhibit-compacting-font-caches nil
              ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
              ;; receiving input, which should help a little with scrolling performance.
              redisplay-skip-fontification-on-input t
              ;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
              ;; is more than enough.
              inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              inhibit-default-init t
              ;; Shave seconds off startup time by starting the scratch buffer in
              ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
              ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
              ;; scratch buffer anyway.
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil
              help-window-select t
              ;; use short answers (y/n) when asking yes/no questions
              use-short-answers t
              nuke-trailing-whitespace-p t
              ;; Show keystrokes in progress
              echo-keystrokes 0.01
              ;; Real emacs knights don't use shift to mark things
              shift-select-mode nil
              ;; Don't write lock-files, I'm the only one here
              create-lockfiles nil
              ;; disable most warning messages
              warning-minimum-level :error
              )


;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq-default frame-title-format '("%n %b"))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package no-littering)

;; Block until current queue processed.
(elpaca-wait)


(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(use-package transient)

(use-package magit
  :after transient
  :bind (("C-x g" . magit-status))
  :config
  (use-package magit-popup)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-untracked-files nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-ignored-files   nil t)
  :custom
  (magit-stage-all-confirm nil)
  (magit-unstage-all-confirm nil)
  (magit-completing-read-function 'helm--completing-read-default)
  )

(use-package forge
  :after magit
  )

;; Helm - incremental completions and narrowing
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  :custom
  ;;; (helm-linum-relative-mode 1)
  (helm-completion-style 'emacs)
  (completion-styles '(flex)) ;'(helm-flex))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-z" . helm-select-action)
         ("M-y" . helm-show-kill-ring)
         ("H-;" . helm-semantic-or-imenu)
         ("H-'" . helm-occur)
         )
  )

(use-package diminish)

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

(use-package lsp-bridge
    :after (markdown-mode yasnippet)
    :init (yas-global-mode 1)
    :elpaca '(lsp-bridge
              :host github
              :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources" "icons")
              :build (:not compile))
    :config
    (setq lsp-bridge-python-command "~/.local/bin/python3")
    (setq lsp-bridge-enable-org-babel t)
    (setq lsp-bridge-enable-log nil)
    (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
    (global-lsp-bridge-mode))

(use-package yaml-mode)
(use-package yaml-pro)

(use-package helm-projectile
  :config
  (helm-projectile-on))

;;; move-text
;;  ---------------------------------------------------------------------------
(use-package move-text
  ;;:config (move-text-default-bindings)) ;; uses M-up and M-down
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

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

(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (tree-sitter-require 'typescript)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
)

(use-package ts-fold
  :elpaca (:host github :repo "jcs090218/ts-fold")
  :bind (
         ("H-f c" . ts-fold-close)
         ("H-f o" . ts-fold-open)
         ("H-f r" . ts-fold-open-recursively)
         ("H-f O" . ts-fold-open-all)
         ("H-f C" . ts-fold-close-all)
         ("H-f f" . ts-fold-toggle)
         )
  )

(setq vterm-always-compile-module t)
(use-package vterm
  :bind (("C-c v" . vterm-new)
         :map vterm-mode-map
         ("C-g" . vterm--self-insert)
         ("C-r" . vterm-send-C-r)
         ("C-s" . vterm-send-C-s)
         ("<C-backspace>" .  (lambda () (interactive)
                               (vterm-send-key (kbd "C-w"))))
         )
  :elpaca (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
  :commands (vterm vterm-other-window)
  )


(use-package vterm-toggle
  :elpaca (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle")
  :custom
  (vterm-toggle-fullscreen-p nil)
  :bind (
         ("H-t t" . vterm-toggle)
         ("H-T" . vterm-toggle-cd)
         ("H-t d" . vterm-toggle-insert-cd)
         ("H-t n" . vterm-toggle-forward)
         ("H-t p" . vterm-toggle-backward)
         )
  )

(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))


(use-package indent-bars
  :elpaca (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode)
  )
