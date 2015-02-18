;;;; org-test-fni.el --- Extra tests for Org mode

;; Copyright (C) 2014-2015  Fabrice Niessen

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

(require 'ert)
(require 'ox)

;;; Functions for writing tests

(defun compare-org-html-export-files (org-file)
  "Compare current export of ORG-FILE with HTML file already present on disk."
  (require 'ox-html)
  (let* ((html-file (concat (file-name-directory org-file)
                            (file-name-base org-file) ".html"))
         html-contents)
    ;; should have a .html file
    (should
     (file-exists-p html-file))
    ;; should have the same .html exported file
    (should
     (equal
      ;; new export
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq html-contents (org-export-as 'html))
        (delete-region (point-min) (point-max))
        (insert html-contents)
        (buffer-string))
      ;; old export
      (with-temp-buffer
        (insert-file-contents html-file)
        (buffer-string))))))

;;; Internal Tests

(ert-deftest test-org-export/export-html-backend-test-file ()
  "Compare current export of ORG-FILE with HTML file already present on disk."
  (compare-org-html-export-files "~/src/emacs-leuven/org-test-sample.org"))

;; (ert 'test-org-export/export-html-backend-test-file)

(provide 'org-test-fni)

;;; org-test-fni.el ends here
