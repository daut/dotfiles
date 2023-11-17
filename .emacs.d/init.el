;; TODO: Bug in Emacs 28.2 remove once updated https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
(add-to-list 'image-types 'svg)

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Improve lsp perf https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

(use-package restart-emacs
  :bind ("C-c r" . restart-emacs))

;; Save all of the custom data in custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(use-package no-littering
  :config
  (setq create-lockfiles nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOPRIVATE" "PYTHONPATH" "NODE_PATH" "RUSTUP_HOME" "CARGO_HOME"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; (server-start)

(use-package emacs
  :config
  (defvar daut/default-font-size 150)
  (set-face-attribute 'default nil :font "JetBrains Mono" :height daut/default-font-size))

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
         ("TAB" . ivy-partial-or-done)
         ("C-l" . ivy-immediate-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-immediate-done)
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

(use-package ivy-posframe
  :hook (after-init . ivy-posframe-mode)
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-width 120))

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
    "w"  '(:ignore t :which-key "window")
    "fd" '(:ignore t :which-key "directories")
    "fdp" '((lambda () (interactive) (dired "~/projects")) :which-key "projects")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "se" '(eshell :which-key "eshell")
    "sE" '((lambda () (interactive) (eshell t)) :which-key "New eshell")
    "sc" '(sql-connect :which-key "sql-connect")

    "oc" '(org-capture t :which-key "org-capture")
    "oa" '(org-agenda t :which-key "org-agenda")
    "oi" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/gtd/inbox.org"))) :which-key "inbox.org")
    "og" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/gtd/gtd.org"))) :which-key "gtd.org")
    "oe" '((lambda () (interactive) (find-file (expand-file-name "~/projects/dotfiles/Emacs.org"))) :which-key "Emacs.org")
    "ot" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Tasks.org"))) :which-key "Tasks.org")
    "od" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Daily.org"))) :which-key "Daily.org"))

  (general-define-key
   :keymaps 'global-map
   "C-s-n" (kbd "C-u 1 C-v")
   "C-s-p" (kbd "C-u 1 M-v")

   "<escape>" 'keyboard-escape-quit

   [remap backward-kill-word] 'daut/backward-delete-char-or-word
   [remap kill-word] 'daut/delete-word

   "C-s-," (lambda () (interactive) (forward-line -30))
   "C-s-." (lambda () (interactive) (forward-line 30))

   "s-<" #'beginning-of-buffer
   "s->" #'end-of-buffer))

(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c b s" . crux-create-scratch-buffer))

;; Hide startup message
(setq inhibit-startup-message t)

;; set line-spacing
(setq default-text-properties '(line-spacing 0.05 line-height 1.1))

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 5)  ; Give some breathing room

(menu-bar-mode -1)   ; Disable menu bar

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; highlight current cursor line
(global-hl-line-mode +1)

;; change cursor type
(setq-default cursor-type 'bar)

;; open in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; remove cursor from non-focused windows
(setq-default cursor-in-non-selected-windows nil)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-items '((projects . 4)
                          (recents . 4)
                          (bookmarks . 4)
                          (agenda . 4))
        dashboard-set-file-icons t
        dashboard-set-heading-icons t))

;; Make certain buffers different in color
;; e.g. popups, sidebars, terminals, etc.
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;; defer loading of the package until command-log-mode is invoked
(use-package command-log-mode
  :commands command-log-mode)

(use-package zenburn-theme)

(use-package doom-themes
  :config
  ;; (load-theme 'doom-zenburn t)
  (load-theme 'doom-challenger-deep t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package nerd-icons)

;; Hide modelline in some major modes
(use-package hide-mode-line
  :hook (((eshell-mode shell-mode
           term-mode vterm-mode
           ;; embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . hide-mode-line-mode)))

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
  ("f" nil "cancel" :exit t))

(defhydra hydra-window-scale (:timeout 4)
  "scale window horizontally"
  ("j" (enlarge-window-horizontally 5) "enlarge horizontally")
  ("k" (shrink-window-horizontally 5) "shrink horizontally")
  ("p" (enlarge-window 5) "enlarge vertically")
  ("n" (shrink-window 5) "shrink vertically")
  ("f" nil "cancel" :exit t))

(daut/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "ws" '(hydra-window-scale/body :which-key "horizontally scale window"))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-suppress-auto-error t))

;; When you visit a file, point goes to the last place
;; where it was when you previously visited the same file.
(use-package save-place
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recentf is a minor mode that builds a list of recently opened files.
;; This list is automatically saved across sessions on exiting
;; Emacs - you can then access this list through a command or the menu.
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
	            recentf-exclude
	            '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; Simple
(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
	       (text-mode . visual-line-mode)
	       ((prog-mode markdown-mode conf-mode restclient-mode) . enable-delete-trailing-whitespace))
  :init
  (setq column-number-mode t
	      line-number-mode t)
  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-delete-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; Enable short answers
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default indent-tabs-mode nil) ; Permanently indent with spaces, never with TABs

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook ((markdown-mode text-mode outline-mode) . flyspell-mode))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

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

(use-package hideshow
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode)
  (restclient-mode . hs-minor-mode)
  (nxml-mode . hs-minor-mode)
  (web-mode . hs-minor-mode)
  (html-mode .hs-minor-mode)
  :bind
  ("C-s-[" . hs-hide-block)
  ("C-s-]" . hs-show-block)
  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

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

(defvar daut/killed-file-list nil
  "List of recently killed files")

(defun daut/add-file-to-killed-file-list ()
  (when buffer-file-name
    (push buffer-file-name daut/killed-file-list)))

(add-hook 'kill-buffer-hook #'daut/add-file-to-killed-file-list)

(defun daut/reopen-killed-file ()
  (interactive)
  (when daut/killed-file-list
    (find-file (pop daut/killed-file-list))))

(global-set-key (kbd "s-T") 'daut/reopen-killed-file)

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package olivetti)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup))
  ;; (meow-global-mode t))

(defun daut/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . daut/org-mode-setup)
  :commands (org-capture org-agenda)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
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
     (sql . t)
     (js . t)))

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

(use-package org-roam
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(setq org-clock-sound t)

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
    (setq projectile-project-search-path '(("~/projects" . 2))))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-git-submodule-command nil)
  (setq projectile-use-git-grep t))

;; integrate counsel with projectile
;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode))

;; install rainbow delimiters and hook them to any prog-mode (programming language mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  ;; (:map lsp-mode-map
  ;;       ("<tab>" . company-indent-or-complete-column))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.15)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-keywords-ignore-case t)
  (setq company-dabbrev-downcase t)
  (setq completion-ignore-case t)
  (setq company-transformers '(delete-consecutive-dups
                             company-sort-by-occurrence
                             company-sort-by-backend-importance))
  :init
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; dired-sidebar uses these
;; (use-package vscode-icon)

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
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-provider :none)
  (add-to-list 'lsp-disabled-clients '(typescript-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(js-mode . vue-semantic-server)))

;; enhanced ui e.g. documentation popup
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-max-width 80)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "u" '(:ignore t :wk "lsp ui")
   "ui" '(lsp-ui-imenu t :which-key "imenu")))

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
   "d" '(dap-hydra t :which-key "debugger")))

(use-package typescript-mode
  :mode "\\.ts[x]\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(defun daut/js-standard-fix-file ()
  (interactive)
  (when (eq major-mode 'js-mode)
    (shell-command (concat "standard --fix " (buffer-file-name)))
    (revert-buffer t t)))

(use-package js-mode
  :ensure nil
  :mode "\\.js[x]\\'"
  :hook
  (js-mode . lsp-deferred)
  ;; (after-save . daut/js-standard-fix-file)
  :bind
  ("C-c /" . daut/js-standard-fix-file)
  :config
  (setq js-indent-level 2))

;; Adds node_modules/.bin directory to `exec_path'
;; This allows Emacs to find project based installs of e.g. eslint.
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

;; npm i -g eslint
;; M-x lsp-install-server RET eslint
;; (use-package js2-mode
;;   :mode "\\.js\\'"
;;   :hook (js2-mode . lsp-deferred)
;;   :config
;;   (setq js-indent-level 2)
;;   (with-eval-after-load 'flycheck
;;     ;; https://github.com/mantoni/eslint_d.js
;;     ;; Install: npm -i -g eslint_d
;;     (when (executable-find "eslint")
;;       (setq flycheck-javascript-eslint-executable "eslint"))))

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

(use-package go-playground
  :after go-mode)

(use-package gotest
  :after go-mode)

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))

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

(use-package verb)

(use-package yaml-mode
  :mode "\\.y[a]ml\\'")

;; Major mode for editing web templates
(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :init
  ;; If the user has installed `vue-mode' then, by appending this to
  ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; priority over this one.
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  :mode "\\.vue\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; CSS mode
(use-package css-mode
  :ensure nil
  :hook (css-mode . lsp-deferred)
  :init (setq css-indent-offset 2))

;; vue-language-server should be installed too. npm i -g vls
;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :hook (vue-mode . lsp-deferred)
;;   :config
;;   (setq mmm-submode-decoration-level 2))
  ;; :custom
  ;; (setq lsp-vetur-emmet "inMarkupAndStylesheetFilesOnly"))

(use-package elixir-mode
  :mode "\\.exs\\'"
  :hook (elixir-mode . lsp-deferred))

;; Needs sqls installed and sqlint would be nice also
;; sqls: go get github.com/lighttiger2505/sqls
;; sqlint: gem install sqlint
(use-package sql
  :hook
  (sql-mode . lsp)
  :config
  (setq lsp-sqls-timeout 10)
  (setq lsp-sqls-workspace-config-path "root"))

(use-package markdown-mode
  :hook (markdown-mode . olivetti-mode))

(use-package mermaid-mode
  :mode "\\.mermaid\\'")

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

(use-package simple-httpd)

(use-package rg)

(use-package gptel)

(use-package magit
  :commands magit-status)

;; add options to magit like create PR, track issues etc.
(use-package forge
  :after magit)

(use-package git-gutter
  :config (global-git-gutter-mode t))
;; try hl-mode (dired-mode . diff-hl-dired-mode)

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

;; Make buffer list usable after previous changes
;; https://github.com/syl20bnr/spacemacs/issues/7661
;; https://github.com/syl20bnr/spacemacs/issues/2667#issuecomment-136155556
(add-hook 'Buffer-menu-mode-hook 
          (lambda ()
            (setq-local revert-buffer-function
                        (lambda (&rest args)))))

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

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :diminish
  ;; :when (icons-displayable-p)
  ;; :custom-face
  ;; (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package winner-mode
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package ace-window
  :bind 
  (("s-[" . (lambda () (interactive) (other-window -1)))
  ("s-]" . (lambda () (interactive) (other-window 1)))))

(defun daut/persp-misc ()
  (interactive)
  (persp-switch "misc"))
(use-package perspective
  :hook (kill-emacs . persp-state-save)
  :init
  (persp-mode)
  ;; create misc persp
  (persp-switch "misc")
  ;; switch back to main persp
  (persp-switch "main")
  :bind (("C-x k" . persp-kill-buffer*)
         ("s-}" . persp-next)
         ("s-{" . persp-prev))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (setq persp-state-default-file (concat user-emacs-directory "persp.el")))

;; make garbage collection pauses faster by decreasing the memory consumption threshold
;; this basically reverts threshold increase at the beginning of the file (which helps with load time)
(setq gc-cons-threshold (* 2 1000 1000))

;; Should make working with long lines faster https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(global-so-long-mode 1)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB
