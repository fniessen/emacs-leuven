;;; rnw-full-line-highlights.el --- Full-width line highlighting for Rnw -*- lexical-binding: t; -*-

(require 'jit-lock)
(require 'cl-lib)

(defun boost--hl-full-lines (regexp face)
  "Highlight full lines containing REGEXP with FACE in the current buffer."
  (let* ((key (intern (format "my--hl-lines-%s" regexp))))
    (fset key
          (lambda ()
            (remove-overlays nil nil 'my-hl-lines-key key)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward regexp nil t)
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-end-position))))
                  (overlay-put ov 'my-hl-lines-key key)
                  (overlay-put ov 'face face)
                  (overlay-put ov 'priority 100)
                  ;; Extend highlight to the right edge of the window.
                  (overlay-put ov 'wrap-prefix
                               (propertize " " 'display '(space :align-to right)
                                           'face face))
                  (overlay-put ov 'line-prefix
                               (propertize "" 'face face))
                  (overlay-put ov 'after-string
                               (propertize " " 'display '(space :align-to right)
                                           'face face)))))))
    (jit-lock-register key t)
    (funcall key)))

;;; Rnw integration

(defun boost--setup-rnw-highlights ()
  "Activate full-line highlights for Sweave/Rnw chunk delimiters."
  (boost--hl-full-lines "^<<.*>>=$" 'org-block-begin-line)
  (boost--hl-full-lines "^@$"       'org-block-end-line))

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . LaTeX-mode))

(defun boost--rnw-find-file-setup ()
  "Setup Rnw highlights when visiting an .Rnw file."
  (when (and buffer-file-name
             (string-match-p "\\.Rnw\\'" buffer-file-name))
    (boost--setup-rnw-highlights)))

(add-hook 'find-file-hook #'boost--rnw-find-file-setup)

(provide 'rnw-full-line-highlights)

;;; rnw-full-line-highlights.el ends here
