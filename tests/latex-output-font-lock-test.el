;;; latex-output-font-lock-test.el --- LaTeX output font-lock tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'compile)

(ert-deftest boost-latex-output-unmatched-quote-does-not-start-a-string ()
  (with-temp-buffer
    (compilation-mode)
    (let ((inhibit-read-only t))
      (insert "Escape char \"\nFollowing output line\n"))
    (boost--configure-latex-output)
    (goto-char (point-min))
    (search-forward "Following output line")
    (should (eq (char-syntax ?\") ?.))
    (should-not (nth 3 (syntax-ppss)))))

(ert-deftest boost-latex-output-highlights-class-warnings ()
  (with-temp-buffer
    (compilation-mode)
    (let ((inhibit-read-only t))
      (insert "Class mcreport Warning: `noweb' package not loaded -> some symbols will fail on input line 1287.\n"))
    (boost--configure-latex-output)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'boost-latex-warning-face))))

(ert-deftest boost-latex-warning-face-has-visible-foreground ()
  (should (equal (face-attribute 'boost-latex-warning-face :foreground)
                 "#EA8820")))

(ert-deftest boost-latex-box-face-has-visible-foreground ()
  (should (equal (face-attribute 'boost-latex-box-face :foreground)
                 "#1F4E79")))

(provide 'latex-output-font-lock-test)

;;; latex-output-font-lock-test.el ends here
