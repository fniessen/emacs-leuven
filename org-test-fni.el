
;;;; org-test-fni.el --- Extra tests for Org mode

;; Copyright (C) 2014-2016 Fabrice Niessen

;; Author: Fabrice Niessen

;; This file is not part of GNU Emacs.

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

;; Command:
;; emacs -Q --batch -L lisp/ -L testing/ -l ~/src/emacs-leuven/org-test-fni.el --eval '(setq org-confirm-babel-evaluate nil)' -f ert-run-tests-batch-and-exit

;;; Code:

;; Org mode (reverse order, so that the Org lisp directory will be found
;; before the Org contrib lisp directory).
(add-to-list 'load-path "~/Public/Repositories/org-mode/testing")
(add-to-list 'load-path "~/Public/Repositories/org-mode/contrib/lisp") ; htmlize
(add-to-list 'load-path "~/Public/Repositories/org-mode/lisp")
;; XXX This shoud be on the command-line!

(require 'ert)
(require 'ox)

;;; Functions for writing tests.

(defun compare-org-html-export-files (org-file)
  "Compare current export of ORG-FILE with HTML file already present on disk."
  (require 'ox-html)
  (let* ((html-file (concat (file-name-directory org-file)
                            (file-name-base org-file) ".html"))
         html-contents)
    ;; Should have a `.html' file.
    (should
     (file-exists-p html-file))
    ;; Should have the same `.html' exported file.
    (should
     (equal
      ;; New export.
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq html-contents
              (let ((org-export-allow-bind-keywords t))
                (org-export-as 'html)))
        (delete-region (point-min) (point-max))
        (insert html-contents)
        (buffer-string))
      ;; Old export.
      (with-temp-buffer
        (insert-file-contents html-file)
        (buffer-string))))))

;;; Internal tests.

(ert-deftest test-org-export/export-html-backend-test-file ()
  "Compare current export of ORG-FILE with HTML file already present on disk."
  (compare-org-html-export-files "~/src/emacs-leuven/org-test-sample.org"))

;; (ert 'test-org-export/export-html-backend-test-file)

(provide 'org-test-fni)

;;; org-test-fni.el ends here
