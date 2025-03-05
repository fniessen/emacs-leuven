;;;; org-test-fni.el --- Extra tests for Org mode

;; Copyright (C) 2014-2025 Fabrice Niessen. All rights reserved.

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

;; This file provides tests for Org mode exports using ERT.
;;
;; Run with: emacs -Q --batch -L lisp/ -L testing/ -l org-test-fni.el \
;;   --eval '(setq org-confirm-babel-evaluate nil)' -f ert-run-tests-batch-and-exit

;;; Code:

;; Org mode (reverse order, so that the Org lisp directory will be found
;; before the Org contrib lisp directory).
(add-to-list 'load-path "~/Public/Repositories/org-mode/testing")
(add-to-list 'load-path "~/Public/Repositories/org-mode/contrib/lisp") ; htmlize
(add-to-list 'load-path "~/Public/Repositories/org-mode/lisp")
;; XXX This should be on the command-line!

(require 'ert)
(require 'ox)

;;; Functions for writing tests.

(defun compare-org-export-files (org-file backend &optional update-reference)
  "Compare ORG-FILE export with its reference file for BACKEND.
If UPDATE-REFERENCE is non-nil, generate and save the reference file."
  (require (intern (format "ox-%s" backend)))
  (let* ((full-org-file (org-test-fni-full-path org-file))
         (ext-file (concat (file-name-sans-extension full-org-file)
                           (if (eq backend 'html) ".html" (format ".%s" backend))))
         export-contents)
    (with-temp-buffer
      (should (file-readable-p full-org-file))
      (insert-file-contents full-org-file)
      (setq export-contents
            (condition-case err
                (let ((org-export-allow-bind-keywords t))
                  (org-export-as backend))
              (error (ert-fail (format "Export failed: %s" err)))))
      (if update-reference
          (write-region export-contents nil ext-file)
        (should (file-exists-p ext-file))
        ;; Should have the same exported file.
        (should
         (equal
          ;; New export.
          export-contents
          ;; Old export.
          (with-temp-buffer
            (insert-file-contents ext-file)
            (buffer-string))))))))

;;; Internal tests.

(defvar org-test-fni-test-files
  '(("org-test-sample.org" . "~/src/emacs-leuven/")
    ("example.txt"         . "~/src/org-style/")
    ("ERT-refcard.txt"     . "~/src/reference-cards/"))
  "Alist of test files and their base directories for Org export tests.")

(defun org-test-fni-full-path (filename)
  "Return the full path for FILENAME based on `org-test-fni-test-files`."
  (let ((entry (assoc filename org-test-fni-test-files)))
    (if entry
        (expand-file-name filename (cdr entry))
      (error "No base directory defined for %s" filename))))

(ert-deftest test-org-export/html-sample-file ()
  "Test that HTML export of `org-test-sample.org` matches its reference file."
  (compare-org-export-files (org-test-fni-full-path "org-test-sample.org") 'html))

;; (ert 'test-org-export/html-sample-file)

(provide 'org-test-fni)

;;; org-test-fni.el ends here
