;;; .emacs-minimal --- Minimal configuration file for Emacs

;;; Commentary:

;;; Code:

(message "Loading Minimal Emacs...")

(defconst em/emacs-load-time-start (float-time))

;;* --[ Constant part ]-------------------------------------------------------

;; Activate debugging.
(setq debug-on-error t)
(setq debug-on-quit t)

;; Default values for frame creation.
(setq default-frame-alist '((tool-bar-lines . 0)))

;; Title bar display of visible frames.
(setq frame-title-format
      (format "Minimal %s Emacs %s%s of %s - PID: %d"
              (capitalize (symbol-name system-type))
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

;; Allow input of accented characters (for terminals that use 8-bit charsets).
(set-input-mode nil nil 1)

;; Enable visualization of matching parens.
(require 'paren)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq show-paren-ring-bell-on-mismatch t)

;; No limit when printing values.
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Don't display the "Welcome to GNU Emacs" buffer on startup.
(setq inhibit-startup-screen t)

;; Initial message displayed in *scratch* buffer at startup.
(setq initial-scratch-message
      ";;----------------------------------------------------------------------
;; This is Emacs running with a minimal configuration file.
;;----------------------------------------------------------------------

")

(defun try-require (feature)
  "Attempt to load a FEATURE (or library).
    Return true if the library given as argument is successfully loaded.  If
    not, just print a message."
  (condition-case err
      (progn
        (if (stringp feature)
            (load-library feature)
          (require feature))
        t)                              ; Necessary for correct behavior in
                                        ; conditional expressions.
    (file-error
     (message "Requiring `%s'... missing" feature)
     nil)))

;; Org-mode (reverse order, so that the Org lisp directory will be found
;; before the Org contrib lisp directory).
;; (add-to-list 'load-path "~/Public/Repositories/org-mode/testing")
;; (add-to-list 'load-path "~/Public/Repositories/org-mode/contrib/lisp") ; htmlize
(add-to-list 'load-path "~/Public/Repositories/org-mode/lisp")
                                        ; Modify the paths to suit your
                                        ; environment!

;; Getting started.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode))
(when (locate-library "org-loaddefs")
  (require 'org-loaddefs))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;* --[ Variable part Under Test ]--------------------------------------------

;; Place your test code here.

(setq ispell-program-name
      (cond ((eq system-type 'cygwin)
             "/cygdrive/c/Program Files (x86)/Aspell/bin/aspell.exe")
            ((eq system-type 'windows-nt)
             "c:/Program Files (x86)/Aspell/bin/aspell.exe")))

;; Enable on-the-fly spell checking.
(add-hook 'org-mode-hook 'flyspell-mode)

(with-eval-after-load "helm-autoloads"
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

(message "Loading Minimal Emacs... Done (in %.2f s)"
         (- (float-time) em/emacs-load-time-start))

(provide '.emacs-minimal)

;;; .emacs-minimal.el ends here
