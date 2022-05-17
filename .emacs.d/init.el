;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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
(setq use-package-verbose t) ;; write useful information about package loading

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
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(use-package no-littering
  :config
  (setq create-lockfiles nil))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOPRIVATE"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(server-start)

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

(defun daut/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive)
  (delete-region (point) (progn (backward-word arg) (point))))

(defun daut/delete-word (arg)
  "Delete characters forwards until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun daut/backward-delete-char-or-word ()
  "backward delete behave more like VS Code"
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (daut/backward-delete-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

(define-key (current-global-map) [remap backward-kill-word] 'daut/backward-delete-char-or-word)
(define-key (current-global-map) [remap kill-word] 'daut/delete-word)

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
    "o"  '(:ignore t :which-key "org-files")
    "s"  '(:ignore t :which-key "shell/sql")
    "f"  '(:ignore t :which-key "files or folders")
    "h"  '(:ignore t :which-key "hydra")
    "fd" '(:ignore t :which-key "directories")
    "fdp" '((lambda () (interactive) (dired "~/projects")) :which-key "projects")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "se" '(eshell :which-key "eshell")
    "sE" '((lambda () (interactive) (eshell t)) :which-key "New eshell")
    "sc" '(sql-connect :which-key "sql-connect")

    "oc" '(org-capture t :which-key "org-capture")
    "oa" '(org-agenda t :which-key "org-agenda")
    "oi" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/gtd/inbox.org"))) :which-key "Inbox.org")
    "oe" '((lambda () (interactive) (find-file (expand-file-name "~/projects/dotfiles/Emacs.org"))) :which-key "Emacs.org")
    "ot" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Tasks.org"))) :which-key "Tasks.org")
    "od" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Daily.org"))) :which-key "Daily.org")))

(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region))

(delete-selection-mode t)

(setq-default tab-width 2)

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
  :bind ("s-d" . mc/mark-next-like-this-symbol))

(use-package move-text
  :bind
  ("C-s-j" . 'move-text-down)
  ("C-s-k" . 'move-text-up))

;; (use-package origami
;;   :bind (:map origami-mode-map
;;          ("C-s-[" . origami-close-node)
;;          ("C-s-]" . origami-open-node))
;;   :hook (prog-mode . origami-mode)
;;   :init (setq origami-show-fold-header t))

(use-package hideshow
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode)
  (restclient-mode . hs-minor-mode)
  :bind
  ("C-s-[" . hs-hide-block)
  ("C-s-]" . hs-show-block))

(use-package minimap
  :defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 15))

(use-package aggressive-indent
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; defer loading of the package until command-log-mode is invoked
(use-package command-log-mode
  :commands command-log-mode)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; better mini-buffer completion
(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
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
         ("C-d" . ivy-reverse-i-search-kill)))

;; ivy-rich get extra information about commands
;; like description and keybinding
;; works only with counsel
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; package used to do search inside file
(use-package swiper
  :after ivy)

;; better UI for the M-x command, C-x b etc.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . persp-counsel-switch-buffer)
         ("C-x C-f" . find-file)
         ("C-s-f" . counsel-git-grep)
         ("s-F" . counsel-git-grep)
         :map minibuffer-local-map ;; minibuffer only mapping
         ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

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
  :commands (org-capture org-agenda)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-tag-alist '(("@errands" . ?e)
                       ("@home" . ?h)
                       ("@shop" . ?s)))
  (setq org-agenda-files
        '("~/projects/org/gtd/inbox.org"
          "~/projects/org/gtd/gtd.org"
          "~/projects/org/gtd/tickler.org"))
  (setq org-refile-targets '(("~/projects/org/gtd/gtd.org" :maxlevel . 1)
                             ("~/projects/org/gtd/someday.org" :level . 1)
                             ("~/projects/org/gtd/tickler.org" :maxlevel . 1)))
  (setq org-capture-templates '(("t" "TODO [inbox]" entry
                                 (file+headline "~/projects/org/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/projects/org/gtd/tickler.org" "Tickler")
                                 "* %i% \n %U"))))

;; same effect for `tab' as in the language major mode buffer
(setq
 org-src-preserve-indentation t
 org-src-tab-acts-natively t)

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun daut/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . daut/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)))

  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql")))

;; Automatically tangle Emacs.org config file on save
(defun daut/org-babel-tangle-configuration ()
  (when (string-equal (buffer-file-name)
		          (expand-file-name "~/projects/dotfiles/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'daut/org-babel-tangle-configuration)))

(use-package projectile
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :bind
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-find-file)
  :init
  (setq projectile-sort-order 'recentf)
  ;; (setq projectile-enable-caching t)
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-git-submodule-command nil)
  (setq projectile-use-git-grep t))

;; integrate counsel with projectile
;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status)

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
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  :init
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; yasnippet
(use-package yasnippet
  :disabled)
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
  (dired-sidebar-display-alist '((side . right)))
  :config
  (setq dired-sidebar-theme 'vscode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; enhanced ui e.g. documentation popup
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-max-width 80))

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  :bind
  ("C-c h d" . dap-hydra)
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (setq dap-print-io t)
  (dap-ui-mode 1)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

(use-package typescript-mode
  :mode "\\.ts[x]\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

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
  (go-mode . (lambda () (setq tab-width 2)))
  :config
  (require 'dap-go)
  (dap-go-setup))

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . lsp-deferred))

(use-package jq-mode)

;;; load restclient-jq - allow restclient mode to use jq to process JSON results.
;; (fetch it from remote url if it's already there)
(let
    ((restclient-jq-filename "~/.emacs.d/restclient-jq.el")
     (restclient-jq-url
      "https://raw.githubusercontent.com/pashky/restclient.el/master/restclient-jq.el"))
  (progn
    (unless (file-exists-p restclient-jq-filename)
      (url-copy-file restclient-jq-url restclient-jq-filename))
    (load "~/.emacs.d/restclient-jq.el")
    ))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'restclient-jq)
  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient))))

(use-package yaml-mode
  :mode "\\.y[a]ml\\'")

;; vue-language-server should be installed too. npm i -g vls
(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . lsp-deferred)
  :custom
  (setq lsp-vetur-emmet "inMarkupAndStylesheetFilesOnly"))

(use-package eslint-mode
  :hook (eslint-mode . lsp-deferred))

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char)
         ("C-c ." . avy-goto-word-or-subword-1)
         ("C-c ," . avy-goto-char)
         ("M-g f" . avy-goto-line))
  :config
  (setq avy-background t))

(use-package term
  :commands term
  :config
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package eshell-git-prompt
  :after eshell)

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

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Backup files directory path
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (when (string= system-type "darwin")
    (setq insert-directory-program (executable-find "gls")))
  (setq dired-kill-when-opening-new-dired-buffer t)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (setq delete-by-moving-to-trash t))

;; (use-package dired-open
;;   :config
;;   (setq dired-open-extensions '(("png" . "open"))))

(use-package ace-window
  :bind 
  (("s-[" . (lambda () (interactive) (other-window -1)))
  ("s-]" . (lambda () (interactive) (other-window 1)))))

(use-package perspective
  :init (persp-mode)
  :bind (("C-x k" . persp-kill-buffer*)
         ("s-}" . persp-next)
         ("s-{" . persp-prev))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")))

;; make garbage collection pauses faster by decreasing the memory consumption threshold
;; this basically reverts threshold increase at the beginning of the file (which helps with load time)
(setq gc-cons-threshold (* 2 1000 1000))

;; Should make working with long lines faster https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(global-so-long-mode 1)
;; this package looks interesting
;; https://github.com/emacsmirror/gcmh/blob/master/gcmh.el
