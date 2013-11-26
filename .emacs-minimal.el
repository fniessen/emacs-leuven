(message "Loading Minimal Emacs...")

(defconst em/emacs-load-time-start (float-time))

;;* --[ Constant part ]-------------------------------------------------------

;; activate debugging
(setq debug-on-error t)
(setq debug-on-quit t)

;; no limit when printing values
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; allow input of accented characters (for terminals that use 8-bit charsets)
(set-input-mode nil nil 1)

;; enable visualization of matching parens
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq show-paren-ring-bell-on-mismatch t)

;; title bar display of visible frames
(setq frame-title-format
      (format "Minimal Emacs %s rev:%s of %s    PID:%d"
              ;; (capitalize (symbol-name system-type))
              emacs-version
              (ignore-errors
                (replace-regexp-in-string " .*" "" emacs-bzr-version))
              (format-time-string "%Y-%m-%d" emacs-build-time)
              (emacs-pid)))

;; Org-mode (reverse order, so that the Org lisp directory will be found
;; before the Org contrib lisp directory)
(add-to-list 'load-path (expand-file-name "~/Public/Repositories/org-mode/testing"))
(add-to-list 'load-path (expand-file-name "~/Public/Repositories/org-mode/contrib/lisp"))
(add-to-list 'load-path (expand-file-name "~/Public/Repositories/org-mode/lisp"))
                                        ; change the pathnames appropriately!

;; getting started
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode))
(if (locate-library "org-loaddefs")
    (require 'org-loaddefs)
  (require 'org-install))               ; obsolete since Emacs 24.3

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;* --[ Variable part Under Test ]--------------------------------------------
