;; Require a feature/library if available; if not, fail silently.
(unless (fboundp 'try-require)
  (defun try-require (feature)
    "Attempt to load FEATURE, returning t on success or nil on failure.
FEATURE should be a symbol representing an Emacs library or feature.
On failure, issue a warning with the error details."
    (condition-case err
        (progn
          (require feature)
          t)                          ; Return t for success in conditionals.
      (error
       (display-warning 'eboost
                        (format "Failed to load feature `%s': %s"
                                feature
                                (error-message-string err))
                        :warning)
       nil))))

;; Set OpenAI API key.
(defvar eboost-openai-api-key
  (let* ((api-key (or (getenv "OPENAI_API_KEY")
                      (when (file-exists-p (expand-file-name "~/.openai_api_key"))
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name "~/.openai_api_key"))
                          (string-trim (buffer-string))))))
         (trimmed-api-key (string-trim api-key)))
    (if (and trimmed-api-key (not (string-empty-p trimmed-api-key)))
        (progn
          (message "[OpenAI API key successfully loaded.]")
          trimmed-api-key)
      (display-warning 'eboost
                       "No valid OpenAI API key found!"
                       :warning)))
  "Load OpenAI API key from environment variable or file.")

;; Load gptel.
(when (try-require 'gptel)

  ;; Set OpenAI API key.
  (when (boundp 'eboost-openai-api-key)
    (setq gptel-api-key eboost-openai-api-key))

  ;; Controls randomness (lower = more deterministic).
  (setq gptel-temperature 0.7)

  ;; Limit response length.
  (setq gptel-max-tokens 1000)

  ;; Set default mode for response buffer.
  (setq gptel-default-mode 'org-mode)

  (defvar eboost-gptel-prompt-prefix "** --- User prompt ---\n\n"
    "Custom prompt prefix for GPTel in Org mode.")

  (defvar eboost-gptel-response-prefix "** --- AI response ---\n\n"
    "Custom response prefix for GPTel in Org mode.")

  ;; Association list mapping modes to prompt prefixes for GPTel.
  (add-to-list 'gptel-prompt-prefix-alist
               `(org-mode . ,eboost-gptel-prompt-prefix))

  ;; Association list mapping modes to response prefixes for GPTel.
  (add-to-list 'gptel-response-prefix-alist
               `(org-mode . ,eboost-gptel-response-prefix))

  ;; Add auto-scrolling after GPTel stream ends.
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; Automatically move cursor to end of response.
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Coding preset.
  (gptel-make-preset 'gpt4coding
    :description "A preset optimized for coding tasks"
    :backend "ChatGPT"
    :model 'gpt-4.1-mini
    :system
    "You are an expert coding assistant. Your role is to provide
   high-quality code solutions, refactorings, and explanations."
    :tools '("read_buffer" "modify_buffer")
    :temperature 0.7)

  ;; Proofreading Preset.
  (gptel-make-preset 'proofreading
    :description "Preset for proofreading tasks"
    :backend "Claude"
    :model 'claude-sonnet-4-20250514
    :system
    "You are a professional proofreader. Your task is to correct spelling,
   grammar, and improve clarity and style."
    :tools '("read_buffer" "spell_check" "grammar_check")
    :temperature 0.7)

  ;; General-purpose chat preset.
  (gptel-make-preset 'general-chat
    :description "A preset for general-purpose LLM interactions"
    :backend "ChatGPT"
    :model 'o4-mini
    :system
    "You are a helpful assistant providing clear and concise answers to a
   wide range of questions."
    :temperature 0.9)

  ;; Project-specific preset (within '.dir-locals.el').
  (gptel-make-preset 'project-agent
    :description "Preset for project-specific AI tasks"
    :backend "Claude"
    :model 'claude-sonnet-4-20250514
    :system
    "You are an AI assistant for a software project. Provide insights
   based on the project's code and documentation."
    :tools '("read_buffer" "lsp_context"))

  ;; Org-mode specific integration.
  (defun eboost-org-gptel-send-to-chatgpt ()
    "Send selected region or Org subtree to the *ChatGPT* buffer.
    If a region is selected, send its text; otherwise, send the content of the Org subtree.
    Displays the response in the *ChatGPT* buffer."
    (interactive)

    ;; Validate context.
    (unless (or (use-region-p) (org-at-heading-p))
      (user-error "Please place point on an Org heading or select a region"))

    ;; Extract text.
    (let ((text
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (save-excursion
               (org-back-to-heading t)
               (let ((beg (point))
                     (end (org-end-of-subtree t) (point)))
                 (buffer-substring-no-properties beg end))))))

      ;; Prepare output buffer.
      (let ((buffer (get-buffer-create "*ChatGPT*")))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert
           (format-time-string "\n\n* -------------------- GPT Session [%Y-%m-%d %H:%M] --------------------")
           "\n\n" eboost-gptel-prompt-prefix
           text
           "\n\n" eboost-gptel-response-prefix)
          (goto-char (point-max))

          ;; Send to GPTel with error handling.
          (condition-case err
              ;; (gptel-send)
              (gptel-request text :buffer buffer)
            (error
             (message "[Failed to send text to GPTel: %s]"
                      (error-message-string err)))))

        ;; Display the buffer and provide user feedback.
        (pop-to-buffer buffer)
        (message "[GPTel: Prompt sent. Awaiting response...]"))))

  ;; Org mode keybinding.
  (with-eval-after-load 'org
    (let ((existing-binding (lookup-key org-mode-map (kbd "C-c q"))))
      (if (or (null existing-binding) (numberp existing-binding))
          (define-key org-mode-map (kbd "C-c q") #'eboost-org-gptel-send-to-chatgpt)
        (display-warning 'eboost
                         "Keybinding C-c q is already in use in Org mode!"
                         :warning))))

  (defun eboost-gptel-send-diff-for-commit-msg ()
    "Send current region or buffer as a diff to GPT for a commit message.
  Does not show GPTel menu; opens result in a new buffer and adds it to the kill ring."
    (interactive)
    (let* ((diff-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (buffer-string)))
           (prompt (concat "Write a concise git commit message for the following diff:\n\n"
                           diff-text)))
      ;; Notify user that the process has started.
      (message "[Generating commit message...]")
      ;; Create and clear the buffer initially.
      (with-current-buffer (get-buffer-create "*Commit Message*")
        (erase-buffer))
      ;; Send request without menu.
      (gptel-request prompt
        :callback (lambda (response _error)
                    (let ((output-buffer (get-buffer-create "*Commit Message*")))
                                          ; Create a new reference to the buffer
                                          ; to avoid closure dependency.
                      (with-current-buffer output-buffer
                        (erase-buffer)
                        (if response
                            (progn
                              (let ((trimmed-response (string-trim response)))
                                (kill-new trimmed-response) ; Add to kill ring.
                                (insert trimmed-response)
                                (goto-char (point-min))
                                (message "[Commit message generated and copied to kill ring.]")))
                          (message "[Failed to generate commit message.]"))
                        (display-buffer output-buffer)))))))

  ;; Diff mode keybinding.
  (with-eval-after-load 'diff-mode
    (define-key diff-mode-map (kbd "m") 'eboost-gptel-send-diff-for-commit-msg))
  (global-set-key (kbd "C-x v m") 'eboost-gptel-send-diff-for-commit-msg)

  ;; Unbind `C-c RET' in Org mode.
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c RET") nil))

  ;; Keybinding for quick access to gptel-send.
  (global-set-key (kbd "C-c RET") 'gptel-send)

)

;; Load org-ai.
(when (try-require 'org-ai)

  ;; Enable org-ai-mode in Org mode.
  (add-hook 'org-mode-hook #'org-ai-mode)

  ;; Set OpenAI API key.
  (when (boundp 'eboost-openai-api-key)
    (setq org-ai-openai-api-key eboost-openai-api-key)
    (setq org-ai-openai-api-token eboost-openai-api-key))

  ;; Install YASnippet templates for org-ai.
  (when (try-require 'yasnippet)
    (org-ai-install-yasnippets)))

(message "* --[ Loaded Emacs-Leuven AI %s]--" lvn--emacs-version)

(provide 'emacs-leuven-ai)

;;; emacs-leuven-ai.el ends here
