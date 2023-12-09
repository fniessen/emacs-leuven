;; ESS: Emacs Speaks Statistics
(add-to-list 'package-selected-packages 'ess)

(autoload 'R "ess-site"
  "Call 'R', the 'GNU S' system from the R Foundation." t)

(autoload 'R-mode "ess-site"
  "Major mode for editing R source." t)

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

;; Start R in current working directory, don't ask user.
(setq ess-ask-for-ess-directory nil)

;; New inferior ESS process appears in another window in the current frame.
(setq inferior-ess-same-window nil)

(when (eq system-type 'cygwin)          ; Running a Cygwin version of Emacs.

  ;; Safe 8.3 name for 32-bit programs.
  (setq ess-program-files "c:/PROGRA~2")

  ;; Safe 8.3 name for 64-bit programs.
  (setq ess-program-files-64 "c:/PROGRA~1")

  ;; Program name for invoking an inferior ESS with `M-x R'.
  (setq inferior-R-program-name "R")) ; [Default: Rterm].

;; Accented characters on graphics.
(add-to-list 'process-coding-system-alist
             '("R.*" . iso-latin-1))

;; ;; Display input commands in the process buffer.
;; (setq ess-eval-visibly 'nowait)      ; But avoid Emacs hanging on large
;;                                      ; evaluations.

;; Default ESS indentation style.
(setq ess-default-style 'DEFAULT)

(with-eval-after-load "ess-site"

  ;; Code folding in ESS mode.
  (add-hook 'ess-mode-hook #'hs-minor-mode)

  ;; Suffix appended by `ac-source-R-args' to candidates.
  (setq ess-ac-R-argument-suffix "=")

  ;; Font-lock keywords for the R mode.
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t) ; Default.
          (ess-R-fl-keyword:fun-defs . t) ; Default.
          (ess-R-fl-keyword:keywords . t) ; Default.
          (ess-R-fl-keyword:assign-ops . t) ; Default.
          (ess-R-fl-keyword:constants . t) ; Default.
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))

  ;; Font-lock patterns used in inferior-R-mode buffers.
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t) ; Default.
          (ess-R-fl-keyword:messages . t) ; Default.
          (ess-R-fl-keyword:modifiers . t) ; Default.
          (ess-R-fl-keyword:fun-defs . t) ; Default.
          (ess-R-fl-keyword:keywords . t) ; Default.
          (ess-R-fl-keyword:assign-ops . t) ; Default.
          (ess-R-fl-keyword:constants . t) ; Default.
          (ess-fl-keyword:matrix-labels . t) ; Default.
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))

  ;; Prototype object browser for R, looks like dired mode.
  (autoload 'ess-rdired "ess-rdired"
    "View *R* objects in a dired-like buffer." t)

  )

(provide 'emacs-leuven-ess)

;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode))
;; flycheck-emacs-lisp-initialize-packages: t
;; flycheck-mode: nil
;; ispell-local-dictionary: "american"
;; End:

;;; ess-leuven.el ends here
