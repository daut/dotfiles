(in-package :lem-user)

;;;
;;; Instant completion
;;; Show completion candidates immediately without pressing TAB
;;;

(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))

;;;
;;; Auto-format on save
;;;

(setf lem:*auto-format* t)

;;;
;;; Line numbers
;;;

(lem/line-numbers:toggle-line-numbers)

;;;
;;; Font (SDL2 only)
;;; Match JetBrains Mono from Emacs/Ghostty setup
;;;

#+lem-sdl2
(lem-core:set-font-name "JetBrains Mono")

;;;
;;; Theme - warm earthy dark palette inspired by miasma
;;;
;;; Uses the base16 theme system. Colors approximated from miasma's palette.
;;; base00: background, base01: status bar/line numbers, base02: selection,
;;; base03: comments, base05: foreground, base08-0f: syntax colors
;;;

(lem-base16-themes::define-base16-color-theme "miasma"
  :base00 "#222222"  ;; background
  :base01 "#333333"  ;; status bar, line numbers
  :base02 "#3a3a3a"  ;; selection
  :base03 "#666666"  ;; comments
  :base04 "#8a8a7a"  ;; dark foreground
  :base05 "#C2C2B0"  ;; foreground
  :base06 "#d4d4c2"  ;; light foreground
  :base07 "#e0e0d0"  ;; lightest foreground
  :base08 "#C9A554"  ;; constants, errors
  :base09 "#C2B790"  ;; strings (secondary)
  :base0a "#D9A441"  ;; warnings, search
  :base0b "#78824B"  ;; strings, success
  :base0c "#C2B790"  ;; escape chars, regex
  :base0d "#78824B"  ;; functions
  :base0e "#B8BB26"  ;; keywords
  :base0f "#685742") ;; deprecated, embeddings

(lem-core:load-theme "miasma")
