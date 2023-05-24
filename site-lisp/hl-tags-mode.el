;;; hl-tags-mode --- Highlight the current SGML tag context

;; Copyright (c) 2011 Mike Spindel <deactivated@gmail.com>
;; Modified by Amit J Patel <amitp@cs.stanford.edu> for nxml-mode
;; Modified by Fabrice Niessen to fix warnings

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; hl-tags-mode is a minor mode for SGML and XML editing that
;; highlights the current start and end tag.
;;
;; To use hl-tags-mode, add the following to your .emacs:
;;
;;   (require 'hl-tags-mode)
;;   (add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
;;   (add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;;; Code:

(eval-when-compile (require 'cl))

(defgroup hl-tags nil
  "Highlight the current tag pair in XML and SGML modes."
  :group 'convenience)

(defface hl-tags-face
  '((t :inherit highlight))
  "Face used to highlight matching tags."
  :group 'hl-tags)


(defvar hl-tags-start-overlay nil)
(make-variable-buffer-local 'hl-tags-start-overlay)

(defvar hl-tags-end-overlay nil)
(make-variable-buffer-local 'hl-tags-end-overlay)


(defun hl-tags-sgml-get-context ()
  (save-excursion (car (last (sgml-get-context)))))

(defun hl-tags-sgml-pair (ctx)
  (if ctx (cons (sgml-tag-start ctx) (sgml-tag-end ctx))
    '(1 . 1)))

(defun hl-tags-context-sgml-mode ()
  (save-excursion
    (when (looking-at "<") (forward-char 1))
    (let* ((ctx (hl-tags-sgml-get-context))
           (boundaries
            (and ctx (cl-case (sgml-tag-type ctx)
                       (empty (cons ctx nil))
                       (close
                        (goto-char (sgml-tag-start ctx))
                        (cons (hl-tags-sgml-get-context) ctx))
                       (open
                        (goto-char (sgml-tag-start ctx))
                        (sgml-skip-tag-forward 1)
                        (backward-char 1)
                        (cons ctx (hl-tags-sgml-get-context)))))))
      (when boundaries
        (cons (hl-tags-sgml-pair (car boundaries))
              (hl-tags-sgml-pair (cdr boundaries)))))))

(defun hl-tags-context-nxml-mode ()
  (condition-case nil
      (save-excursion
        (let (start1 end1 start2 end2)
          (when (looking-at "<") (forward-char))
          (nxml-up-element 1)
          (setq end2 (point))

          (nxml-backward-single-balanced-item)
          (setq start2 (point))

          (nxml-up-element -1)
          (setq end1 (point))

          (nxml-forward-single-balanced-item)
          (setq start1 (point))

          (cons (cons start1 end1) (cons start2 end2))))
    (error nil)))

(defun hl-tags-context ()
  "Return a pair ((start . end) . (start . end)) containing the
boundaries of the current start and end tag , or nil."
  (if (eq major-mode 'nxml-mode)
      (hl-tags-context-nxml-mode)
    (hl-tags-context-sgml-mode)))

(defun hl-tags-update ()
  (let ((ctx (hl-tags-context)))
    (if (null ctx)
        (hl-tags-hide)
      (hl-tags-show)
      (move-overlay hl-tags-start-overlay (caar ctx) (cdar ctx))
      (move-overlay hl-tags-end-overlay (cadr ctx) (cddr ctx)))))

(defun hl-tags-show ()
  (unless hl-tags-start-overlay
    (setq hl-tags-start-overlay (make-overlay 1 1)
          hl-tags-end-overlay (make-overlay 1 1))
    (overlay-put hl-tags-start-overlay 'face 'hl-tags-face)
    (overlay-put hl-tags-end-overlay 'face 'hl-tags-face)))

(defun hl-tags-hide ()
  (when hl-tags-start-overlay
    (delete-overlay hl-tags-start-overlay)
    (delete-overlay hl-tags-end-overlay)))

(define-minor-mode hl-tags-mode
  "Toggle hl-tags-mode."
  :init-value nil
  :lighter ""
  :global nil
  (if hl-tags-mode
      (progn
        (add-hook 'post-command-hook #'hl-tags-update nil t)
        (add-hook 'change-major-mode-hook #'hl-tags-hide nil t))
    (remove-hook 'post-command-hook #'hl-tags-update t)
    (remove-hook 'change-major-mode-hook #'hl-tags-hide t)
    (hl-tags-hide)))


(provide 'hl-tags-mode)
