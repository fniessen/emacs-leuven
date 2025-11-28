(defun eboost-highlight-full-lines-matching-regexp (regexp &optional face-bg)
  "Colorie entièrement (jusqu’au bord droit de la fenêtre) toutes les lignes
contenant REGEXP. Aucun bug, même en édition."
  (interactive "sRegexp à highlighter: \nsCouleur de fond (ex: red, #ff4444): ")
  (let* ((bg      (or face-bg "#ff4444"))
         (face    `(:background ,bg))
         (key     (intern (format "my--hl-lines-%s" regexp))))

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
                  (overlay-put ov 'wrap-prefix
                               (propertize " " 'display '(space :align-to right)
                                           'face face))
                  (overlay-put ov 'line-prefix
                               (propertize " " 'face face))
                  (overlay-put ov 'after-string
                               (propertize " " 'display '(space :align-to right)
                                           'face face)))))))
    (jit-lock-register key t)
    (funcall key)))

(eboost-highlight-full-lines-matching-regexp "^<<.*>>=" "#E2E1D5")
(eboost-highlight-full-lines-matching-regexp "^@$" "#E2E1D5")

(defun my/setup-rnw-highlights ()
  "Active les surlignages complets pour les chunks Sweave/Rnw"
  (eboost-highlight-full-lines-matching-regexp "^<<.*>>=$" "#E2E1D5")
  (eboost-highlight-full-lines-matching-regexp "^@$""#E2E1D5"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "\\.Rnw\\'" buffer-file-name))
              (my/setup-rnw-highlights))))
