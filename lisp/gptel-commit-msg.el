;;; gptel-commit-message.el --- Generate commit messages from Diff with GPTel -*- lexical-binding: t -*-

(require 'gptel)
(require 'subr-x)

(defgroup gptel-commit nil
  "Generate Git commit messages using GPTel."
  :group 'gptel)

(defcustom gptel-commit-prompt-file
  (expand-file-name "write-commit-message.txt"
                    user-emacs-directory)
  "System prompt file for commit-message generation."
  :type 'file
  :group 'gptel-commit)

;;;###autoload
(defun gptel-write-commit-message ()
  "Generate a Git commit message from the current diff region or buffer.

The result is shown in *Commit Message* and copied to the kill ring.

If `gptel-commit-prompt-file' exists, its contents are used
as the system prompt."
  (interactive)
  (unless (or (use-region-p)
              (> (buffer-size) 0))
    (user-error "[No content to analyze]"))

  (let* ((prompt-file gptel-commit-prompt-file)
         (default-prompt
          "Write a Git commit message for the supplied diff. Use the imperative mood and limit the subject line to 72 characters.")
         (system-prompt
          (if (file-readable-p prompt-file)
              (with-temp-buffer
                (insert-file-contents prompt-file)
                (buffer-string))
            default-prompt))
         (diff-text
          (if (use-region-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (buffer-substring-no-properties
             (point-min)
             (point-max)))))
    ;; Notify user that the process has started.
    (message "[Writing commit message...]")

    ;; Create and clear the buffer initially.
    (with-current-buffer
        (get-buffer-create "*Commit Message*")
      (erase-buffer))

    ;; Send request without menu.
    (gptel-request
        diff-text
      :system system-prompt
      :callback
      (lambda (response info)
        (if (stringp response)
            (let ((output-buffer
                   (get-buffer-create "*Commit Message*")))
                                      ; Create a new reference to the buffer
                                      ; to avoid closure dependency.
              (with-current-buffer output-buffer
                (erase-buffer)

                (let ((msg (string-trim response)))
                  ;; Strip Markdown fences.
                  (setq msg
                        (replace-regexp-in-string
                         "\\````[^\n]*\n?"
                         ""
                         msg))

                  (setq msg
                        (replace-regexp-in-string
                         "\n?```\\'"
                         ""
                         msg))

                  ;; Use normal quotes.
                  (setq msg
                        (replace-regexp-in-string "`" "'" msg))

                  (kill-new msg) ; Add to kill ring.
                  (insert msg)

                  (message
                   "[Commit message copied to kill ring.]")))

              (display-buffer
               output-buffer
               '((display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (inhibit-same-window . t))))

          (message
           "[Failed to generate commit message: %s]"
           (plist-get info :status)))))))

(defvar gptel-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'gptel-write-commit-message)
    map)
  "Prefix keymap for GPTel commit utilities.")

;;;###autoload
(global-set-key (kbd "C-c g") gptel-commit-mode-map)

;;;###autoload
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map
              (kbd "w")
              #'gptel-write-commit-message))

;;;###autoload
(global-set-key (kbd "C-x v w")
                #'gptel-write-commit-message)

(provide 'gptel-commit-message)

;;; gptel-commit-message.el ends here
