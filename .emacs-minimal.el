;;; .emacs-minimal --- Minimal configuration file for Emacs

;;; Commentary:

;;; Code:

(message "Loading Minimal Emacs...")

(defconst em/emacs-load-time-start (float-time))

;;* --[ Constant part ]-------------------------------------------------------

;; Activate debugging.
(setq debug-on-error t)
(setq debug-on-quit t)

;; No limit when printing values.
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Allow input of accented characters (for terminals that use 8-bit charsets).
(set-input-mode nil nil 1)

;; Enable visualization of matching parens.
(require 'paren)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq show-paren-ring-bell-on-mismatch t)

;; Title bar display of visible frames.
(setq frame-title-format
      (format "Minimal Emacs %s%s of %s - PID: %d"
              emacs-version
              (if (and (boundp 'emacs-repository-version)
                       emacs-repository-version)
                  (concat " (r"
                          (replace-regexp-in-string " .*" ""
                                                    emacs-repository-version)
                          ")")
                "")
              (format-time-string "%Y-%m-%d" emacs-build-time)
              (emacs-pid)))

;; Org-mode (reverse order, so that the Org lisp directory will be found
;; before the Org contrib lisp directory).
;; (add-to-list 'load-path "~/Public/Repositories/org-mode/testing")
;; (add-to-list 'load-path "~/Public/Repositories/org-mode/contrib/lisp") ; htmlize
(add-to-list 'load-path "~/Public/Repositories/org-mode/lisp")
                                        ; Modify the paths to suit your
                                        ; environment!

;; Getting started.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode))
(if (locate-library "org-loaddefs")
    (require 'org-loaddefs)
  (require 'org-install))               ; Obsolete since Emacs 24.3.

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;* --[ Variable part Under Test ]--------------------------------------------

;; Place your test code here.

(message "Loading Minimal Emacs... Done (in %.2f s)"
         (- (float-time) em/emacs-load-time-start))

(provide '.emacs-minimal)

;;; .emacs-minimal.el ends here
