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

(provide 'colorful-mode-test)

;;; colorful-mode-test.el ends here
