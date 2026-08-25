;; Commit-prompt file customization.
(defcustom boost-gptel-commit-prompt-file
  (expand-file-name "write-commit-message.txt"
                    boost-gptel-directives-directory)
  "System prompt file for commit-message generation."
  :type 'file :group 'boost-gptel)

;;;###autoload
  (defun boost-gptel-write-commit-message ()
    "Generate a Git commit message from the current diff region or buffer.
The result is shown in *Commit Message* and copied to the kill ring.
If ~/ai-prompts/write-commit-message.txt exists, use its contents as the system prompt."
    (interactive)
    (unless (or (use-region-p) (> (buffer-size) 0))
      (user-error "[No content to analyze]"))
    (let* ((prompt-file boost-gptel-commit-prompt-file)
           (default-prompt
            "Write a Git commit message for the supplied diff. Use the imperative
mood and limit the subject line to 72 characters.")
           (system-prompt
            (if (file-readable-p prompt-file)
                (with-temp-buffer
                  (insert-file-contents prompt-file)
                  (buffer-string))
              default-prompt))
           (diff-text
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Notify user that the process has started.
      (message "[Writing commit message...]")
      ;; Create and clear the buffer initially.
      (with-current-buffer (get-buffer-create "*Commit Message*")
        (erase-buffer))
      ;; Send request without menu.
      (gptel-request diff-text
        :system system-prompt
        :callback (lambda (response info)
                    (if (stringp response)
                        (let ((output-buffer (get-buffer-create "*Commit Message*")))
                                        ; Create a new reference to the buffer
                                        ; to avoid closure dependency.
                          (with-current-buffer output-buffer
                            (erase-buffer)
                            (let ((msg (string-trim response)))
                              ;; Strip ``` fences if present.
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
                              ;; Optional: convert backticks-quotes.
                              (setq msg (replace-regexp-in-string "`" "'" msg))
                              (kill-new msg) ; Add to kill ring.
                              (insert msg)
                              (message "[Commit message copied to kill ring.]")))
                          (display-buffer output-buffer
                           '((display-buffer-reuse-window display-buffer-pop-up-window)
                             (inhibit-same-window . t))))
                      (message "[Failed to generate commit message: %s]"
                               (plist-get info :status)))))))

(defvar boost-gptel-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Custom commands.
    (define-key map "w" #'boost-gptel-write-commit-message)

    map)
  "Prefix keymap for GPTel commands.")

(global-set-key (kbd "C-c g") boost-gptel-mode-map)

  ;; Diff mode keybinding (only if free).
  (with-eval-after-load 'diff-mode
    (boost--set-key-if-free 'diff-mode-map (kbd "w")
                             #'boost-gptel-write-commit-message "diff-mode"))

  ;; Global keybinding (only if free).
  (boost--set-key-if-free global-map (kbd "C-x v w")
                           #'boost-gptel-write-commit-message "global map")
