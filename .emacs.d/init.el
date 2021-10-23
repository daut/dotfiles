(defun daut/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'daut/display-startup-time)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms			 
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  ;; interval in days
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Save all of the custom data in custom.el
(use-package no-littering)

(use-package exec-path-from-shell)

;; Hide startup message  
(setq inhibit-startup-message t)

;; set line-spacing
(setq default-text-properties '(line-spacing 0.05 line-height 1.1))

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)   ; Disable menu bar

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; highlight current cursor line
(global-hl-line-mode +1)

;; change cursor type
(setq-default cursor-type 'bar)

;; open in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

 ;; font size variable
 (defvar daut/default-font-size 150)
 ;; setup font type and size
 (set-face-attribute 'default nil :font "JetBrains Mono" :height daut/default-font-size)

;; scroll up/down one line
(global-set-key (kbd "C-s-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "C-s-p") (kbd "C-u 1 M-v"))

;; Make ESC quit promps
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; backward kill behave more like VS Code 
(defun backward-kill-char-or-word ()
  (interactive)
  (cond 
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

(define-key (current-global-map) [remap backward-kill-word] 'backward-kill-char-or-word)

;; page up/down like functionality
(global-set-key (kbd "C-s-,")
                (lambda () (interactive) (forward-line -30)))
(global-set-key (kbd "C-s-.")
                (lambda () (interactive) (forward-line 30)))

;; beginning/end of a buffer
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

(use-package general
  :config
  (general-create-definer daut/leader-keys
    :prefix "C-C")
  (daut/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/projects/dotfiles/Emacs.org")) :which-key "open Emacs.org")))

(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region))

(delete-selection-mode t)

(setq tab-width 2)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

(setq require-final-newline t)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package multiple-cursors
  :bind ("s-d" . mc/mark-next-like-this-word))

(use-package move-text
  :bind
  ("C-s-j" . 'move-text-down)
  ("C-s-k" . 'move-text-up))

(use-package command-log-mode)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; better mini-buffer completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  :init (ivy-mode 1))

;; ivy-rich get extra information about commands
;; like description and keybinding
;; works only with counsel
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; package used to do search inside file
(use-package swiper)

;; better UI for the M-x command, C-x b etc.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . find-file)
         :map minibuffer-local-map ;; minibuffer only mapping
         ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; Helpful is an alternative to emacs builtin help
;; which provides much more contextual information and
;; better user experience
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(daut/leader-keys
  "ts" '(hydra-text-scale :which-key "scale text"))

(defun daut/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . daut/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-agenda-files
        '("~/projects/emacs-from-scratch/org-files/Tasks.org"
          "~/projects/emacs-from-scratch/org-files/Birthdays.org")))

;; same effect for `tab' as in the language major mode buffer
(setq
 org-src-preserve-indentation t 
 org-src-tab-acts-natively t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun daut/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . daut/org-mode-visual-fill))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle Emacs.org config file on save
(defun daut/org-babel-tangle-configuration ()
  (when (string-equal (buffer-file-name)
		          (expand-file-name "~/projects/dotfiles/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'daut/org-babel-tangle-configuration)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("s-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; integrate counsel with projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

;; add options to magit like create PR, track issues etc.
(use-package forge
  :after magit)

;; install rainbow delimiters and hook them to any prog-mode (programming language mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-column))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-dealy 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; yasnippet
(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-global-mode t))

;; dired-sidebar uses these
(use-package vscode-icon)

(use-package dired-sidebar
  :bind (("s-b" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-display-alist '((side . right))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; enhanced ui e.g. documentation popup
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-deferred)
  :config
  (setq lua-indent-level 2))

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (before-save . gofmt-before-save)
  (go-mode . (lambda () (setq tab-width 2))))

(use-package term
  :config
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package eshell-git-prompt)

(defun daut/configure-eshell ()
  ;; save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-history-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . daut/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Backup files directory path
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package dired
  :ensure nil
  :bind
  ([remap dired-find-file] . dired-find-alternate-file)
  :config
  (when (string= system-type "darwin")
    (setq insert-directory-program "/opt/homebrew/bin/gls"))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (setq delete-by-moving-to-trash t))

;; (use-package dired-open
;;   :config
;;   (setq dired-open-extensions '(("png" . "open"))))
