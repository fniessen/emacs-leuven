;;; boost-wsl-clipboard-test.el --- WSL clipboard tests -*- lexical-binding: t; -*-

(require 'ert)

(defun boost-wsl-clipboard-test--emacs-roundtrip (text)
  "Copy TEXT in Emacs and yank it into a fresh buffer."
  (let ((interprogram-cut-function nil)
        (interprogram-paste-function nil))
    (kill-new text)
    (with-temp-buffer
      (yank)
      (buffer-string))))

(ert-deftest boost-wsl-clipboard-emacs-to-emacs-monoline ()
  (should (equal (boost-wsl-clipboard-test--emacs-roundtrip "one line")
                 "one line")))

(ert-deftest boost-wsl-clipboard-emacs-to-emacs-multiline ()
  (should (equal (boost-wsl-clipboard-test--emacs-roundtrip "first\nsecond\n")
                 "first\nsecond\n")))

(defun boost-wsl-clipboard-test--copy-to-windows (text)
  "Call the Windows copy adapter for TEXT and return its effects."
  (let (selected sent process-options eof-process)
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
      (boost--copy-to-windows text))
    (list selected sent process-options eof-process)))

(ert-deftest boost-wsl-clipboard-emacs-to-windows-monoline ()
  (let ((result (boost-wsl-clipboard-test--copy-to-windows "one line")))
    (should (equal (nth 0 result) "one line"))
    (should (equal (nth 1 result) "one line"))
    (should (eq (plist-get (nth 2 result) :coding)
                'utf-16le-with-signature))
    (should (eq (nth 3 result) 'fake-clip-process))))

(ert-deftest boost-wsl-clipboard-emacs-to-windows-multiline ()
  (let ((result
         (boost-wsl-clipboard-test--copy-to-windows "first\nsecond\n")))
    (should (equal (nth 0 result) "first\nsecond\n"))
    (should (equal (nth 1 result) "first\r\nsecond\r\n"))
    (should (eq (nth 3 result) 'fake-clip-process))))

(defun boost-wsl-clipboard-test--paste-from-windows (text)
  "Return TEXT as though it were read from the Windows clipboard."
  (cl-letf (((symbol-function 'boost-wsl--interop-available-p)
             (lambda () t))
            ((symbol-function 'shell-command-to-string)
             (lambda (command)
               (should (equal command
                              "powershell.exe -NoProfile -Command 'Get-Clipboard -Raw'"))
               text)))
    (boost-wsl-paste-from-windows)))

(ert-deftest boost-wsl-clipboard-windows-to-emacs-monoline ()
  (should (equal (boost-wsl-clipboard-test--paste-from-windows "one line\r\n")
                 "one line\n")))

(ert-deftest boost-wsl-clipboard-windows-to-emacs-multiline ()
  (should (equal
           (boost-wsl-clipboard-test--paste-from-windows "first\r\nsecond\r\n")
           "first\nsecond\n")))

(provide 'boost-wsl-clipboard-test)

;;; boost-wsl-clipboard-test.el ends here