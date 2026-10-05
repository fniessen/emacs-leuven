;;; boost-wsl-clipboard-test.el --- WSL clipboard tests -*- lexical-binding: t; -*-

(require 'ert)

(defun boost-wsl-clipboard-test--slick-copy-roundtrip (text &optional region-active)
  "Copy TEXT with the configured M-w command and yank it into a fresh buffer.
When REGION-ACTIVE is non-nil, copy the entire buffer; otherwise copy its
current line."
  (let ((interprogram-cut-function nil)
        (interprogram-paste-function nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when region-active
        (set-mark (point-max))
        (activate-mark))
      (call-interactively #'boost--slick-kill-ring-save))
    (with-temp-buffer
      (yank)
      (buffer-string))))

(ert-deftest boost-wsl-clipboard-emacs-to-emacs-monoline ()
  (should (equal (boost-wsl-clipboard-test--slick-copy-roundtrip "one line" t)
                 "one line")))

(ert-deftest boost-wsl-clipboard-emacs-to-emacs-multiline ()
  (should (equal (boost-wsl-clipboard-test--slick-copy-roundtrip
                  "first\nsecond\n" t)
                 "first\nsecond\n")))

(ert-deftest boost-wsl-clipboard-emacs-to-emacs-current-line-without-region ()
  (should (equal
           (boost-wsl-clipboard-test--slick-copy-roundtrip
            "current line\nnext line\n")
           "current line\n")))

(defun boost-wsl-clipboard-test--copy-to-windows-with-m-w
    (text &optional without-region)
  "Call the configured M-w command with a Windows clipboard adapter.
When WITHOUT-REGION is non-nil, copy the current line instead of a region."
  (let (selected sent process-options eof-process)
    (let ((interprogram-cut-function #'boost--copy-to-windows)
          (interprogram-paste-function nil))
      (cl-letf (((symbol-function 'gui-select-text)
                 (lambda (value) (setq selected value)))
                ((symbol-function 'make-process)
                 (lambda (&rest options)
                   (setq process-options options)
                   'fake-clip-process))
                ((symbol-function 'process-send-string)
                 (lambda (_process value) (setq sent value)))
                ((symbol-function 'process-send-eof)
                 (lambda (process) (setq eof-process process))))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (unless without-region
            (set-mark (point-max))
            (activate-mark))
          (call-interactively #'boost--slick-kill-ring-save))))
    (list selected sent process-options eof-process)))

(ert-deftest boost-wsl-clipboard-emacs-to-windows-monoline ()
  (let ((result
         (boost-wsl-clipboard-test--copy-to-windows-with-m-w "one line")))
    (should (equal (nth 0 result) "one line"))
    (should (equal (nth 1 result) "one line"))
    (should (eq (plist-get (nth 2 result) :coding)
                'utf-16le-with-signature))
    (should (eq (nth 3 result) 'fake-clip-process))))

(ert-deftest boost-wsl-clipboard-emacs-to-windows-multiline ()
  (let ((result
         (boost-wsl-clipboard-test--copy-to-windows-with-m-w
          "first\nsecond\n")))
    (should (equal (nth 0 result) "first\nsecond\n"))
    (should (equal (nth 1 result) "first\r\nsecond\r\n"))
    (should (eq (nth 3 result) 'fake-clip-process))))

(ert-deftest boost-wsl-clipboard-emacs-to-windows-current-line-without-region ()
  (let ((result
         (boost-wsl-clipboard-test--copy-to-windows-with-m-w
          "current line\nnext line\n" t)))
    (should (equal (nth 0 result) "current line\n"))
    (should (equal (nth 1 result) "current line\r\n"))
    (should (eq (nth 3 result) 'fake-clip-process))))

(defun boost-wsl-clipboard-test--paste-from-windows (text)
  "Return TEXT as though it were read from the Windows clipboard."
  (cl-letf (((symbol-function 'boost-wsl--interop-available-p)
             (lambda () t))
            ((symbol-function 'call-process)
             (lambda (program _infile destination _display &rest _arguments)
               (should (equal program "powershell.exe"))
               (should (eq destination t))
               (insert text)
               0)))
    (boost-wsl-paste-from-windows)))

(defun boost-wsl-clipboard-test--yank-windows-at-point (text)
  "Yank Windows clipboard TEXT at point without an active region."
  (let ((interprogram-paste-function #'boost-wsl-paste-from-windows)
        (interprogram-cut-function nil)
        (kill-ring nil))
    (cl-letf (((symbol-function 'boost-wsl--interop-available-p)
               (lambda () t))
              ((symbol-function 'call-process)
               (lambda (program _infile destination _display &rest _arguments)
                 (should (equal program "powershell.exe"))
                 (should (eq destination t))
                 (insert text)
                 0)))
      (with-temp-buffer
        (insert "prefix: ")
        (goto-char (point-max))
        (setq mark-active nil)
        (should-not (use-region-p))
        (yank)
        (buffer-string)))))

(ert-deftest boost-wsl-clipboard-windows-to-emacs-monoline ()
  (should (equal
           (boost-wsl-clipboard-test--paste-from-windows "\uFEFFone line\r\n")
           "one line")))

(ert-deftest boost-wsl-clipboard-windows-to-emacs-multiline ()
  (should (equal
           (boost-wsl-clipboard-test--paste-from-windows
            "\uFEFFfirst\r\nsecond\r\n")
           "first\nsecond")))

(ert-deftest boost-wsl-clipboard-windows-to-emacs-without-region ()
  (should (equal
           (boost-wsl-clipboard-test--yank-windows-at-point "clipboard\r\n")
           "prefix: clipboard")))

(provide 'boost-wsl-clipboard-test)

;;; boost-wsl-clipboard-test.el ends here