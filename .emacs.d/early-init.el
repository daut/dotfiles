;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Prevent package.el from activating packages early
;; This allows use-package to control when packages are loaded
;; which significantly improves startup time
(setq package-enable-at-startup nil)

;; Disable site-run-file (site-start.el)
;; Skip loading the site-start.el file which is rarely used
;; but can slow down startup if present
(setq site-run-file nil)

;; Temporarily disable file handlers for faster startup
;; File handlers process special file names (like .gz or remote files)
;; Disabling them during startup significantly speeds up loading Emacs Lisp files
(defvar daut/file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;; open current frame in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Hide startup message
(setq inhibit-startup-message t)

;; set line-spacing
(setq default-text-properties '(line-spacing 0.05 line-height 1.1))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode '(8 . 8))  ; Set fringes

(menu-bar-mode -1)   ; Disable menu bar

;; highlight current cursor line
(global-hl-line-mode +1)

;; change cursor type
(setq-default cursor-type 'bar)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; remove cursor from non-focused windows
(setq-default cursor-in-non-selected-windows nil)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

;; increase garbage collection threshold to speed up initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore normal values after startup is complete
;; Set garbage collection threshold to a reasonable value for normal use
;; Too high causes memory bloat, too low causes frequent pauses
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000) ;; 2MB is Emacs default
                  gc-cons-percentage 0.1)
            ;; Restore file name handlers that were disabled during startup
            (setq file-name-handler-alist daut/file-name-handler-alist-original)))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'miasma t)

;;; early-init.el ends here
