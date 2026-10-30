;;; electric-pair-test.el --- Automatic parentheses tests -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest leuven-electric-pair-inserts-and-skips-closing-parenthesis ()
  (should (bound-and-true-p electric-pair-mode))
  (dolist (mode '(text-mode emacs-lisp-mode org-mode))
    (with-temp-buffer
      (let ((text-mode-hook '(leuven--text-electric-pairs))
            (org-mode-hook nil))
        (funcall mode))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro "(")
        (should (equal (buffer-string) "()"))
        (should (= (point) 2))
        (execute-kbd-macro ")")
        (should (equal (buffer-string) "()"))
        (should (= (point) 3))))))

(ert-deftest leuven-electric-pair-text-only-pairs-parentheses ()
  (dolist (mode '(text-mode org-mode))
    (dolist (char '(?\[ ?\{ ?\" ?\' ?~ ?* ?/ ?_ ?= ?<))
      (with-temp-buffer
        (let ((text-mode-hook '(leuven--text-electric-pairs))
              (org-mode-hook nil))
          (funcall mode))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (string char))
          (should (equal (buffer-string) (string char)))
          (should (= (point) 2)))))))

(ert-deftest leuven-electric-pair-programming-retains-other-pairs ()
  (dolist (pair '((?\[ . ?\]) (?\" . ?\")))
    (with-temp-buffer
      (delay-mode-hooks (emacs-lisp-mode))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (string (car pair)))
        (should (equal (buffer-string) (string (car pair) (cdr pair))))
        (should (= (point) 2))))))

(provide 'electric-pair-test)

;;; electric-pair-test.el ends here
