;; Improve lsp perf https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun daut/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'daut/display-startup-time)

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
(setq use-package-compute-statistics t) ;; report how much time a package needs to load

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package auto-package-update
  :custom
  ;; interval in days
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package restart-emacs)

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

(add-to-list 'Info-directory-list
             (expand-file-name "./books" user-emacs-directory))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (pixel-scroll-precision-mode t)
  :config
  (defvar daut/default-font-size 150)
  (set-face-attribute 'default nil :font "JetBrains Mono" :height daut/default-font-size)
  ;; necessary as a fallback for org-modern mode
  (set-fontset-font "fontset-default" '(#x2BC6 . #x2BC6)
                    (font-spec :family "Iosevka Aile") nil 'append))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(defun daut/minibuffer-backward-kill (arg)
  (interactive "p")
  (if (and minibuffer-completing-file-name
           (eq (char-before) ?/))
      (zap-up-to-char (- arg) ?/)
    (delete-backward-char arg)))

(use-package vertico
  :init (vertico-mode)
  :bind ("<backspace>" . daut/minibuffer-backward-kill))

(use-package vertico-posframe
  :init (vertico-posframe-mode)
  :config
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (consult-ripgrep (:not posframe))
          (t posframe)))
  (vertico-multiform-mode t))

(defun daut/selected-region-or-symbol-at-point ()
  "Return the selected region, otherwise return the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))
;; TODO: https://www.reddit.com/r/emacs/comments/16g08me/killbuffer_from_the_minibuffer_after_mx/
(use-package consult
  :bind
  ("C-s"   . consult-line)
  ("C-x b" . consult-buffer)
  ("s-F"   . consult-ripgrep)
  ;; goto
  ("M-g i" . consult-imenu)
  ;; search
  ("M-s d" . consult-find)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :initial (daut/selected-region-or-symbol-at-point)))

(use-package consult-flycheck
  :bind
  ("M-g f" . consult-flycheck))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)))

;; If you use the grepping commands from the Consult package, consult-grep, consult-git-grep or consult-ripgrep, then you should install the embark-consult package, which adds support for exporting a list of grep results to an honest grep-mode buffer, on which you can even use wgrep if you wish.
(use-package embark-consult)

(use-package wgrep)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; improved completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode astro-ts-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package beacon
  :init
  (beacon-mode)
  :config
  (setq beacon-color "#335533"))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner (concat user-emacs-directory "themes/emacs.txt"))
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-footer-messages "Person who say it cannot be done should not interrupt person doing it.")
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

(defun daut/print-current-theme ()
  "Print the currently active theme."
  (interactive)
  (if custom-enabled-themes
      (message "Current theme: %s" (car custom-enabled-themes))
    (message "No theme is currently active")))

(defun daut/random-color-theme ()
  "Load a random theme from the available themes."
  (interactive)
  (let ((themes (custom-available-themes)))
    (random t)
    (let ((selected-theme (nth (random (length themes)) themes)))
      (load-theme selected-theme t)
      (message "Selected theme: %s" selected-theme))))

(use-package ef-themes)
  ;; :config
  ;; (load-theme 'ef-autumn))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;; (load-theme 'miasma t)
(use-package miasma-theme
  :vc (:fetcher github :repo daut/miasma-theme.el)
  :config (load-theme 'miasma t))

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
:bind
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable))

(use-package highlight-indent-guides
  :hook ((prog-mode astro-ts-mode) . highlight-indent-guides-mode)
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
  :hook ((org-mode markdown-mode text-mode outline-mode) . flyspell-mode))

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
  (electric-pair-mode +1)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

(use-package multiple-cursors
  :bind ("s-d" . mc/mark-next-like-this-symbol))

(use-package move-text
  :bind
  ("C-s-j" . 'move-text-down)
  ("C-s-k" . 'move-text-up))

(defun daut/html-forward (arg)
  (interactive "P")
  (pcase (get-text-property (point) `mhtml-submode)
    (`nil (sgml-skip-tag-forward 1))
    (submode (forward-sexp))))

(use-package hideshow
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode)
  (restclient-mode . hs-minor-mode)
  (nxml-mode . hs-minor-mode)
  (web-mode . hs-minor-mode)
  (html-mode . hs-minor-mode)
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

;; (use-package editorconfig
;;   :hook (after-init . editorconfig-mode))

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

(use-package dtrt-indent)

(defun daut/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook ((org-mode . daut/org-mode-setup)
         (org-mode . olivetti-mode))
  :commands (org-capture org-agenda)
  :bind (:map org-mode-map
              ("M-RET" . org-insert-item))
  :config
  ;; https://github.com/minad/consult/issues/1153
  (setq org-fold-core-style 'overlays)
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-agenda-files
        '("~/projects/org/gtd/inbox.org"
          "~/projects/org/gtd/gtd.org"
          "~/projects/org/gtd/tickler.org"))
  (setq org-refile-targets '(("~/projects/org/gtd/gtd.org" :maxlevel . 2)
                             ("~/projects/org/gtd/someday.org" :level . 1)
                             ("~/projects/org/gtd/tickler.org" :maxlevel . 1)))
  (setq org-capture-templates '(("t" "TODO [inbox]" entry
                                 (file+headline "~/projects/org/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/projects/org/gtd/tickler.org" "Tickler")
                                 "* %i% \n %U"))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; same effect for `tab' as in the language major mode buffer
(setq
 org-src-preserve-indentation t
 org-src-tab-acts-natively t)

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

(use-package org-roam-ui
  :after org-roam)

(setq org-clock-sound t)

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind(("C-c C-r" . recompile)
        ("C-c c" . switch-to-compilation-buffer))
  :config
  (defun switch-to-compilation-buffer ()
    (interactive)
    (switch-to-buffer "*compilation*"))
  (setq compilation-scroll-output t)
  (setopt compilation-ask-about-save nil))

(use-package projectile
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :bind
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-find-file)
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-auto-discover nil)
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '(("~/projects" . 3))))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-git-submodule-command nil)
  (setq projectile-use-git-grep t))

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
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.15)
  (setq company-transformers '(delete-consecutive-dups
                             company-sort-by-occurrence
                             company-sort-prefer-same-case-prefix))
  :init
  (setq company-backends '((company-capf :with company-yasnippet company-dabbrev-code)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :hook (after-init . company-statistics-mode))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-provider :none)
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-disabled-clients '(typescript-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(typescript-ts-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(js-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(astro-ts-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(css-mode . vue-semantic-server))
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2915#issuecomment-855156802
  (add-to-list 'lsp-language-id-configuration '(".*\\.liquid" . "html"))
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))
  ;; turn off lsp diagnostics to let flycheck do the job
  ;; (setq lsp-diagnostics-provider :none))

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

(use-package dape
  :defer t
  :config
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-inlay-hints t)
  (setq dape-cwd-fn 'projectile-project-root))

;; (use-package astro-ts-mode
;;   :mode "\\.astro\\'"
;;   :hook (astro-ts-mode . lsp-deferred))

(use-package bash-ts-mode
  :ensure nil
  :mode "\\.sh\\'"
  :mode "\\.bash\\'"
  :hook (bash-ts-mode . lsp-deferred)
  :config
  (setq sh-basic-offset 2))
(add-to-list 'interpreter-mode-alist '("bash" . bash-ts-mode))

(use-package typescript-mode
  :mode "\\.ts[x]\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package js-mode
  :ensure nil
  :mode "\\.[c|m]js[x]\\'"
  :hook
  (js-mode . lsp-deferred)
  (js-mode . dtrt-indent-mode)
  :config
  (setq js-indent-level 2))

;; Adds node_modules/.bin directory to `exec_path'
;; This allows Emacs to find project based installs of e.g. eslint.
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-deferred)
  :config
  (setq lua-indent-level 2))

(defun daut/go-fold-imports ()
  "Fold import statements in go file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^import" nil t)
      (let ((start (point)))
        (forward-line)
        (while (looking-at "^[ \t]+\"")
          (forward-line))
        (let ((end (point)))
          (hs-hide-block))
        (goto-char start)))))

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (before-save . gofmt-before-save)
  (go-mode . (lambda () (setq tab-width 2)))
  (go-mode . daut/go-fold-imports))

(use-package go-playground
  :after go-mode)

(use-package gotest
  :after go-mode)

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package js-json-mode
  :ensure nil
  :mode "\\.json\\'"
  :hook
  (js-json-mode . lsp-deferred)
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

(use-package yaml-mode
  :mode "\\.y[a]ml\\'"
  :mode "\\.y[a]ml\\.j2\\'")

(defvar web-mode-electric-pairs '((?' . ?')) "Electric pairs for org-mode.")

(defun web-mode-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs web-mode-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
;; Major mode for editing web templates
(use-package web-mode
  :hook
  (web-mode . lsp-deferred)
  (web-mode . web-mode-add-electric-pairs)
  (web-mode . dtrt-indent-mode)
  (web-mode . (lambda ()
                (setq yas-after-exit-snippet-hook nil)))
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
  :mode "\\.vue\\'"
  :mode "\\.tmpl\\'"
  :mode "\\.gotmpl\\'"
  :mode "\\.gohtml\\'"
  :mode "\\.astro\\'"
  :mode "\\.liquid\\'"
  :config
  (setq web-mode-enable-auto-indent nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-engines-alist
        '(("go" . "\\.tmpl\\'")
          ("liquid" . "\\.liquid\\'"))))

;; CSS mode
(use-package css-mode
  :ensure nil
  :hook (css-mode . lsp-deferred)
  :init (setq css-indent-offset 2))

;; https://github.com/emacs-lsp/lsp-mode/issues/4313#issuecomment-2051461893
(with-eval-after-load 'lsp-volar
  (lsp-dependency 'typescript
                  '(:npm :package "typescript"
                         :path "tsserver")))

(use-package elixir-mode
  :mode "\\.exs\\'"
  :hook (elixir-mode . lsp-deferred))

;; Needs sqls installed and sqlint would be nice also
;; sqls: go get github.com/lighttiger2505/sqls
;; sqlint: gem install sqlint
(use-package sql
  :hook
  ((sql-mode . lsp)
   (sql-interactive-mode . (lambda () (toggle-truncate-lines t))))
  :config
  (setq lsp-sqls-timeout 10)
  (setq lsp-sqls-workspace-config-path "root"))

(use-package markdown-mode
  :hook ((markdown-mode elfeed-show-mode) . olivetti-mode))

(use-package mermaid-mode
  :mode "\\.mermaid\\'")

(use-package php-mode
  :hook (php-mode . lsp-deferred))

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

(use-package dockerfile-mode)

(use-package geiser-guile)

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package apheleia
  :vc (:fetcher github :repo radian-software/apheleia)
  :hook (after-init . apheleia-global-mode)
  :config
  (cl-pushnew '(eslint . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file))
              apheleia-formatters
              :test #'equal)
  (cl-pushnew '(prettier-liquid . ("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=html"))
              apheleia-formatters
              :test #'equal))

(use-package avy
  :bind (("s-." . avy-goto-char-timer)
         ("s-," . avy-goto-char)
         ("C-c ." . avy-goto-char-timer)
         ("C-c ," . avy-goto-char)
         ("M-g f" . avy-goto-line))
  :config
  (setq avy-background t)
  (setq avy-timeout-seconds 0.4))

(use-package simple-httpd)

(use-package rg)

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((astro "https://github.com/virchau13/tree-sitter-astro")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (vue "https://github.com/ikatyang/tree-sitter-vue")))
  (setq treesit-font-lock-level 4))

(use-package devdocs
  :defer t)

(use-package gptel
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key gptel-api-key))
  (setq gptel-model 'claude-3-5-sonnet-20241022)
  (add-to-list 'gptel-directives '(proofreader . "I want you act as a proofreader. I will provide you texts and I would like you to review them for any spelling, grammar, or punctuation errors. Once you have finished reviewing the text, provide me with any necessary corrections or suggestions to improve the text.")))

(use-package elysium
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(use-package copilot
  :hook ((prog-mode restclient-mode eshell-mode yaml-mode) . copilot-mode)
  :bind
  ("C-<tab>" . copilot-complete)
  :config
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000)
  (setq copilot-idle-delay nil)
  (defun daut/activate-copilot ()
    (if (> (buffer-size) copilot-max-char)
        ;; Or don't even warn to get rid of it.
        (warn "Buffer size exceeds copilot max char limit. Copilot will not be activated.")
      (copilot-mode))))

(use-package magit
  :commands magit-status
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; add options to magit like create PR, track issues etc.
(use-package forge
  :after magit)

(use-package git-gutter
  :config (global-git-gutter-mode t))
;; try hl-mode (dired-mode . diff-hl-dired-mode)

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

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

  ;; better color support
  (add-hook 'eshell-mode-hook (lambda() (setenv "TERM" "xterm-256color")))

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

(use-package dired-sidebar
  :bind (("s-b" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-display-alist '((side . right)))
  :config
  (setq dired-sidebar-theme 'nerd))

(defun 0x0-upload-file (file-path)
  "Upload a file at FILE-PATH to 0x0.st and copy the URL to the kill ring."
  (interactive "fSelect a file to upload: ")
  (message "Sending %s to 0x0.st..." file-path)
  (let ((url (string-trim-right
              (shell-command-to-string
               (format "curl -s -F'file=@%s' https://0x0.st" (expand-file-name file-path))))))
    (message "0x0.st URL: %s" url)
    (kill-new url)))

(setq switch-to-buffer-obey-display-actions t)

(defun daut/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(defun daut/toggle-window-size-fixed ()
  (interactive)
  (setq-default window-size-fixed (not window-size-fixed))
  (message "Window size fixed: %s" window-size-fixed))

(defvar-local daut/original-lock nil
  "Holds the original value of window-size-fixed.")

(defun daut/interactive-window-resize ()
  (interactive)
  (setq daut/original-lock window-size-fixed)
  (setq-default window-size-fixed nil)
  (hydra-window-scale/body))

(use-package winner-mode
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo))
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

(use-package transpose-frame)

(use-package ace-window
  :bind
  (("C-x o" . ace-window)
   ("s-[" . (lambda () (interactive) (other-window -1)))
   ("s-]" . (lambda () (interactive) (other-window 1))))
  :config
  (setq aw-dispatch-always t))

(use-package perspective
  :hook (kill-emacs . persp-save-default)
  :init (persp-mode)
  :bind (("C-x k" . persp-kill-buffer*)
         ("s-}" . persp-next)
         ("s-{" . persp-prev))
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (defun persp-save-default ()
    (let ((current-prefix-arg '(4)))
      (persp-state-save (concat user-emacs-directory "persp.el"))))
  (defun persp-create-aux ()
    "Create a new auxilliary perspective."
    (interactive)
    (let ((current-persp (persp-current-name)))
      (persp-switch "aux")
      (persp-switch current-persp)))
  (setq persp-state-default-file (concat user-emacs-directory "persp-")))

(use-package elfeed
  :defer t
  :config
  (setq elfeed-feeds
        '(("https://world.hey.com/dhh/feed.atom" dhh)
          ("https://forum.systemcrafters.net/posts.rss" sc)
          ("https://karthinks.com/index.xml" karthinks)
          ("https://protesilaos.com/codelog.xml" prot)
          ("https://www.masteringemacs.org/feed" masteringemacs))))

;; scroll up/down one line
(global-set-key (kbd "C-s-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "C-s-p") (kbd "C-u 1 M-v"))

;; Make ESC quit prompts
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

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "cancel" :exit t))

(defhydra hydra-window-scale (:timeout 4
                                       :post (setq-default window-size-fixed daut/original-lock))
  "scale window horizontally"
  ("j" (enlarge-window-horizontally 5) "enlarge horizontally")
  ("k" (shrink-window-horizontally 5) "shrink horizontally")
  ("p" (enlarge-window 5) "enlarge vertically")
  ("n" (shrink-window 5) "shrink vertically")
  ("f" nil "cancel" :exit t))

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
    "tt" '(consult-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "se" '(eshell :which-key "eshell")
    "sE" '((lambda () (interactive) (eshell t)) :which-key "New eshell")
    "sc" '(sql-connect :which-key "sql-connect")

    "oc" '(org-capture t :which-key "org-capture")
    "oa" '(org-agenda t :which-key "org-agenda")
    "oi" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/gtd/inbox.org"))) :which-key "inbox.org")
    "og" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/gtd/gtd.org"))) :which-key "gtd.org")
    "oe" '((lambda () (interactive) (find-file (expand-file-name "~/projects/dotfiles/Emacs.org"))) :which-key "Emacs.org")
    "ot" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Tasks.org"))) :which-key "Tasks.org")
    "od" '((lambda () (interactive) (find-file (expand-file-name "~/projects/org/Daily.org"))) :which-key "Daily.org")
    "wl" '((lambda () (interactive) (daut/toggle-window-size-fixed)) :which-key "Toggle window size fixed")
    "ws" '((lambda () (interactive) (daut/interactive-window-resize)) :which-key "horizontally scale window")
    "wt" '((lambda () (interactive) (transpose-frame)) :which-key "change window split direction"))

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
   "s->" #'end-of-buffer
   "C-s-f" 'toggle-frame-fullscreen))

(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c b s" . crux-create-scratch-buffer))
