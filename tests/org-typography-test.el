;;; org-typography-test.el --- Org save typography tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(ert-deftest leuven-org-save-preserves-typographic-characters ()
  (should-not (fboundp 'boost--org-clean-typography))
  (let ((file (make-temp-file "leuven-typography-" nil ".org"))
        (text (concat (apply #'concat (make-list 48 "\u201cquoted\u201d "))
                      "\u2014 \u2013 \u201f \u2018 \u2019 \u201b \u2026\n"))
        (make-backup-files nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (set-visited-file-name file)
            (insert text)
            (save-buffer)
            (should (equal (buffer-string) text)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) text))))
      (delete-file file))))

(ert-deftest leuven-org-personal-typography-cleanup-is-local ()
  (let ((org-mode-hook org-mode-hook)
        (before-save-hook before-save-hook))
    (unwind-protect
        (progn
          (load "emacs-leuven-org-fni" nil t)
          (should-not (memq #'boost--org-clean-typography before-save-hook))
          (with-temp-buffer
            (org-mode)
            (setq buffer-file-name "/tmp/leuven-typography.org")
            (should (local-variable-p 'before-save-hook))
            (should (memq #'boost--org-clean-typography before-save-hook))
            (insert "\u2026 \u2019 \u201d \u201c \u2013 \u2014\n")
            (run-hooks 'before-save-hook)
            (should (equal (buffer-string) "... ' \" \" - --\n")))
          (dolist (name boost--org-clean-typography-excluded-files)
            (with-temp-buffer
              (org-mode)
              (setq buffer-file-name (concat "/tmp/" name))
              (insert "\u201cquoted\u201d\n")
              (run-hooks 'before-save-hook)
              (should (equal (buffer-string) "\u201cquoted\u201d\n"))))
          (with-temp-buffer
            (text-mode)
            (setq buffer-file-name "/tmp/leuven-typography.txt")
            (insert "\u201cquoted\u201d\n")
            (boost--org-clean-typography)
            (should (equal (buffer-string) "\u201cquoted\u201d\n"))))
      (fmakunbound 'boost--org-clean-typography)
      (fmakunbound 'fni-org-enable-typography-cleanup)
      (makunbound 'boost--org-clean-typography-excluded-files))))

;;; org-typography-test.el ends here
