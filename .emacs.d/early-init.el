;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

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

;; restore to normal value
;; make garbage collection pauses faster by decreasing the memory consumption threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)
                  gc-cons-percentage 0.1)))

;;; early-init.el ends here
