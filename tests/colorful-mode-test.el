;;; colorful-mode-test.el --- Colorful prefix tests -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest leuven-colorful-prefix-shows-hex-colors ()
  (skip-unless (and (featurep 'colorful-mode)
                    (fboundp 'boost--colorful-prefix-box)))
  (dolist (mode '(emacs-lisp-mode css-mode html-mode latex-mode text-mode))
    (dolist (alignment '(left right))
      (let ((colorful-use-prefix t)
            (colorful-prefix-alignment alignment))
        (with-temp-buffer
          (funcall mode)
          (should colorful-mode)
          (insert "#ff0000")
          (colorful-mode-fontify-region (point-min) (point-max))
          (let* ((overlay (seq-find
                           (lambda (ov) (overlay-get ov 'colorful--overlay))
                           (overlays-at (point-min))))
                 (prefix (and overlay
                              (overlay-get overlay
                                           (if (eq alignment 'left)
                                               'before-string
                                             'after-string))))
                 (glyph (if (display-graphic-p)
                            prefix
                          (and prefix (get-text-property 0 'display prefix))))
                 (face (and (stringp glyph) (get-text-property 0 'face glyph))))
            (should (equal glyph "■"))
            (should (equal face '(:foreground "#ff0000")))
            (when (and (display-graphic-p) (image-type-available-p 'pbm))
              (let* ((image (get-text-property 0 'display glyph))
                     (size (max 3 (round (* 1.3 (frame-char-width)))))
                     (lines (split-string (plist-get (cdr image) :data) "\n" t)))
                (should (eq (car image) 'image))
                (should (eq (plist-get (cdr image) :type) 'pbm))
                (should (equal (plist-get (cdr image) :foreground) "black"))
                (should (equal (plist-get (cdr image) :background) "#ff0000"))
                (should (equal (image-size image t) (cons size size)))
                (should (equal (car lines) "P1"))
                (should (equal (cadr lines) (format "%d %d" size size)))
                (should (equal (nth 2 lines) (make-string size ?1)))
                (should (equal (car (last lines)) (make-string size ?1)))
                (dolist (row (butlast (nthcdr 3 lines)))
                  (should (equal row (concat "1" (make-string (- size 2) ?0)
                                             "1"))))))))))))

(ert-deftest leuven-colorful-without-prefix-keeps-color-overlay ()
  (skip-unless (and (featurep 'colorful-mode)
                    (fboundp 'boost--colorful-prefix-box)))
  (let ((colorful-use-prefix nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "#ff0000")
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((overlay (seq-find
                      (lambda (ov) (overlay-get ov 'colorful--overlay))
                      (overlays-at (point-min)))))
        (should overlay)
        (should-not (overlay-get overlay 'before-string))
        (should (overlay-get overlay 'face))))))

(ert-deftest leuven-colorful-css-uses-prefix-without-color-background ()
  (skip-unless (and (featurep 'colorful-mode)
                    (fboundp 'boost--colorful-prefix-box)))
  (dolist (mode '(css-mode web-mode))
    (when (or (not (eq mode 'web-mode)) (require 'web-mode nil t))
      (with-temp-buffer
        (insert (if (eq mode 'web-mode)
                    "<style>body { color: #2980b9; background: rgb(255, 0, 0); border-color: blue; }</style>"
                  "body { color: #2980b9; background: rgb(255, 0, 0); border-color: blue; }"))
        (funcall mode)
        (jit-lock-fontify-now (point-min) (point-max))
        (dolist (color '("#2980b9" "rgb(255, 0, 0)" "blue"))
          (goto-char (point-min))
          (search-forward color)
          (let ((overlay (seq-find
                          (lambda (ov) (overlay-get ov 'colorful--overlay))
                          (overlays-at (match-beginning 0)))))
            (should overlay)
            (should (overlay-get overlay 'before-string))
            (should-not (overlay-get overlay 'face))
            (should-not (plist-get (get-text-property (match-beginning 0) 'face)
                                   :background))))))))

(ert-deftest leuven-show-hidden-text-preserves-colorful-prefixes ()
  (skip-unless (featurep 'colorful-mode))
  (with-temp-buffer
    (insert "body { color: #2980b9; }\n")
    (css-mode)
    (hs-minor-mode 1)
    (jit-lock-fontify-now (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "#2980b9")
    (let ((color-overlay
           (seq-find (lambda (ov) (overlay-get ov 'colorful--overlay))
                     (overlays-at (match-beginning 0))))
          (hidden-overlay (make-overlay (point-min) (1+ (point-min))))
          (other-overlay (make-overlay (point-min) (1+ (point-min)))))
      (overlay-put hidden-overlay 'invisible t)
      (should color-overlay)
      (boost-show-hidden-text)
      (should-not (overlay-buffer hidden-overlay))
      (should (overlay-buffer other-overlay))
      (should (overlay-buffer color-overlay))
      (should (overlay-get color-overlay 'before-string)))))

(ert-deftest leuven-colorful-latex-definitions-use-prefix ()
  (skip-unless (and (featurep 'colorful-mode)
                    (fboundp 'boost--colorful-prefix-box)))
  (dolist (mode (if (require 'tex-site nil t)
                    '(latex-mode LaTeX-mode plain-TeX-mode)
                  '(latex-mode)))
    (with-temp-buffer
      (insert "\\definecolor{a}{rgb}{1,0,0}\n"
              "\\definecolor{b}{RGB}{255,0,0}\n"
              "\\definecolor{c}{HTML}{FF0000}\n"
              "\\definecolor{d}{gray}{0.5}\n")
      (funcall mode)
      (jit-lock-fontify-now (point-min) (point-max))
      (dolist (definition '("{rgb}{1,0,0}" "{RGB}{255,0,0}"
                            "{HTML}{FF0000}" "{gray}{0.5}"))
        (goto-char (point-min))
        (search-forward definition)
        (let ((overlay (seq-find
                        (lambda (ov) (overlay-get ov 'colorful--overlay))
                        (overlays-at (match-beginning 0)))))
          (should overlay)
          (should (overlay-get overlay 'before-string))
          (should-not (overlay-get overlay 'face)))))))

(ert-deftest leuven-colorful-refreshes-already-open-buffers ()
  (skip-unless (and (featurep 'colorful-mode)
                    (fboundp 'boost--colorful-refresh-existing-buffers)))
  (let ((css (generate-new-buffer " *colorful-css-test*"))
        (tex (generate-new-buffer " *colorful-latex-test*"))
        (prog-mode-hook (remove #'colorful-mode (copy-sequence prog-mode-hook)))
        (text-mode-hook (remove #'colorful-mode (copy-sequence text-mode-hook))))
    (unwind-protect
        (progn
          (with-current-buffer css
            (insert "body { color: #2980b9; }")
            (css-mode)
            (should-not colorful-mode))
          (with-current-buffer tex
            (insert "\\definecolor{example}{HTML}{2980B9}")
            (latex-mode)
            (should-not colorful-mode))
          (boost--colorful-refresh-existing-buffers)
          (dolist (buffer (list css tex))
            (with-current-buffer buffer
              (should colorful-mode)
              (should (memq #'colorful-mode-fontify-region jit-lock-functions))
              (jit-lock-fontify-now (point-min) (point-max))
              (should (seq-find
                       (lambda (ov)
                         (and (overlay-get ov 'colorful--overlay)
                              (overlay-get ov 'before-string)))
                       (overlays-in (point-min) (point-max)))))))
      (kill-buffer css)
      (kill-buffer tex))))

(provide 'colorful-mode-test)

;;; colorful-mode-test.el ends here
