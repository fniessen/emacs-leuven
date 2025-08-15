;;; emacs-leuven-ai.el --- GPTel & org-ai Integration  -*- lexical-binding: t -*-

;; Author: Fabrice Niessen
;; Keywords: tools, ai, convenience
;; URL: https://...
;; Version: 0.1

;;; Commentary:

;; Provides helpers for GPTel and org-ai.

;;; Code:

;; Require a feature/library if available; if not, fail silently.
(defun eboost-try-require (feature)
  "Try to (require FEATURE) silently.
Return t on success, nil on failure. If `init-file-debug' is non-nil,
emit a warning when the feature can't be loaded."
  (if (require feature nil 'noerror)
      t
    (when (bound-and-true-p init-file-debug)
      (display-warning 'eboost
                       (format "Cannot load `%s'" feature)
                       :warning))
    nil))

(defun eboost-define-key-if-free (keymap key command &optional scope)
  "Bind KEY to COMMAND in KEYMAP only if KEY is unbound.
KEYMAP may be the map itself or a symbol naming it.
If already bound, emit a warning mentioning SCOPE (string)."
  (let* ((map (if (keymapp keymap)
                  keymap
                (when (and (symbolp keymap) (boundp keymap))
                  (symbol-value keymap))))
         (existing-binding (and map (lookup-key map key))))
    (cond
     ((not map)
      (display-warning 'eboost "Keymap not available (yet)" :warning))
     ((or (null existing-binding) (numberp existing-binding))
      (define-key map key command))
     (t
      (when init-file-debug
        (display-warning 'eboost
                         (format "Keybinding %s is already in use%s!"
                                 (key-description key)
                                 (if scope (format " in %s" scope) ""))
                         :warning))))))

;; Set OpenAI API key.
(defvar eboost-openai-api-key
  (let* ((api-key (or (getenv "OPENAI_API_KEY")
                      (let ((f (expand-file-name "~/.openai_api_key")))
                        (when (file-exists-p f)
                          (with-temp-buffer
                            (insert-file-contents f)
                            (buffer-string))))))
         (trimmed-api-key (and (stringp api-key) (string-trim api-key))))
    (cond
     ((and (stringp trimmed-api-key) (not (string-empty-p trimmed-api-key)))
      (message "[OpenAI API key loaded]")
      trimmed-api-key)
     (t
      (when init-file-debug
        (display-warning 'eboost "No valid OpenAI API key found!" :warning))
      nil)))
  "OpenAI API key from env or file; nil if unavailable.")

;; Load gptel.
(when (eboost-try-require 'gptel)

  ;; Set OpenAI API key.
  (when (bound-and-true-p eboost-openai-api-key)
    (setq gptel-api-key eboost-openai-api-key))

  ;; Controls randomness (lower = more deterministic).
  (setq gptel-temperature 0.7)

  ;; Limit response length.
  (setq gptel-max-tokens 1000)

  ;; Set default mode for response buffer.
  (setq gptel-default-mode 'org-mode)

  ;; Enable GPTel's expert/power-user commands.
  (setq gptel-expert-commands t)

  (defgroup eboost-gptel nil
    "Eboost tweaks and integration for GPTel."
    :group 'applications
    :prefix "eboost-gptel-")

  (defcustom eboost-gptel-prompt-prefix "** --- User prompt ---\n\n"
    "Prompt prefix inserted before user text in GPTel Org buffers."
    :type 'string
    :group 'eboost-gptel)

  (defcustom eboost-gptel-response-prefix "** --- AI response ---\n\n"
    "Response prefix inserted before AI output in GPTel Org buffers."
    :type 'string
    :group 'eboost-gptel)

  ;; Update or create the org-mode entries (no duplicates).
  (let ((cell (assq 'org-mode gptel-prompt-prefix-alist)))
    (if cell
        (setcdr cell eboost-gptel-prompt-prefix)
      (push (cons 'org-mode eboost-gptel-prompt-prefix)
            gptel-prompt-prefix-alist)))

  (let ((cell (assq 'org-mode gptel-response-prefix-alist)))
    (if cell
        (setcdr cell eboost-gptel-response-prefix)
      (push (cons 'org-mode eboost-gptel-response-prefix)
            gptel-response-prefix-alist)))

  ;; Add auto-scrolling after GPTel stream ends.
  (when (fboundp 'gptel-auto-scroll)
    (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll))

  ;; Automatically move cursor to end of response.
  (when (fboundp 'gptel-end-of-response)
    (add-hook 'gptel-post-response-functions #'gptel-end-of-response))

(require 'subr-x)  ;; string-trim, string-empty-p, etc.

(defcustom eboost-gptel-directives-directory
  (expand-file-name "~/ai-prompts/")
  "Répertoire racine contenant des fichiers .txt pour les directives GPTel (recurse)."
  :type 'directory
  :group 'eboost-gptel)

(defun eboost--gptel-read-directive-file (name directive-file)
  "Read DIRECTIVE-FILE and set NAME entry in `gptel-directives` as a proper alist cell.
Return the directive content, or nil on failure."
  (when (file-readable-p directive-file)
    (with-temp-buffer
      (insert-file-contents directive-file)
      (let ((content (string-trim (buffer-string))))
        ;; Create or update (NAME . CONTENT)
        (setf (alist-get name gptel-directives nil nil #'eq) content)
        content))))

(defun eboost--gptel-read-directives-from-directory (dir)
  "Populate `gptel-directives` from all .txt files under DIR (recursively).
Existing entries with the same NAME are overwritten."
  ;; (setq gptel-directives nil)
  (dolist (f (directory-files-recursively (expand-file-name dir) "\\.txt\\'"))
    (condition-case err
        (let ((name (intern (file-name-base f))))
          (eboost--gptel-read-directive-file name f))
      (error
       (display-warning 'eboost
                        (format "[Failed to read directive %s: %s]"
                                f (error-message-string err))
                        :warning))))
  gptel-directives)

;; Load directives from the specified directory.
(if (file-directory-p eboost-gptel-directives-directory)
    (condition-case err
        (eboost--gptel-read-directives-from-directory eboost-gptel-directives-directory)
      (error (message "[Error reading directives: %s]" (error-message-string err))))
  (message "[Directory %s does not exist.]" eboost-gptel-directives-directory))

  ;; (gptel-make-preset 'gpt4coding2
  ;;   :backend "openai"
  ;;   :model "gpt-4o-mini"
  ;;   :temperature 0.7)

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

;;;###autoload
  (defun eboost-gptel-org-send-to-chatgpt ()
    "Send selected region or Org subtree to the *ChatGPT* buffer.
    If a region is selected, send its text; otherwise, send the content of the Org subtree.
    Displays the response in the *ChatGPT* buffer."
    (interactive)

    ;; Validate context.
    (unless (derived-mode-p 'org-mode)
      (user-error "This command works in Org buffers only"))
    (unless (or (use-region-p) (org-at-heading-p))
      (user-error "Place point on an Org heading or select a region"))

    ;; Extract text.
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (save-excursion
                    (org-back-to-heading t)
                    (let ((beg (point)))
                      (org-end-of-subtree t)
                      (buffer-substring-no-properties beg (point)))))))

      ;; Prepare output buffer.
      (let ((buffer (get-buffer-create "*ChatGPT*")))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (format-time-string
                   "\n\n* -------------------- GPT Session [%Y-%m-%d %H:%M] --------------------")
                  "\n\n" eboost-gptel-prompt-prefix
                  text
                  "\n\n" eboost-gptel-response-prefix)
          (goto-char (point-max)))

        ;; Send to GPTel with error handling.
        (condition-case err
            (gptel-request text :buffer buffer)
          (error (message "[GPTel error: %s]"
                          (error-message-string err))))

        ;; Display the buffer and provide user feedback.
        (pop-to-buffer buffer)
        (message "[GPTel: Prompt sent...]"))))

  ;; Org mode keybinding (only if free).
  (with-eval-after-load 'org
    (eboost-define-key-if-free 'org-mode-map
                               (kbd "C-c q")
                               #'eboost-gptel-org-send-to-chatgpt
                               "Org mode"))

;;;###autoload
  (defun eboost-gptel-write-commit-message ()
    "Generate a Git commit message from the current diff region or buffer.
The result is shown in *Commit Message* and copied to the kill ring."
    (interactive)
    (unless (or (use-region-p) (> (buffer-size) 0))
      (user-error "No content to analyze"))

    (let* ((diff-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (buffer-substring-no-properties (point-min) (point-max))))
           (prompt "Write a Git commit message for the following diff:\n\n"))
      ;; Notify user that the process has started.
      (message "[Generating commit message...]")
      ;; Create and clear the buffer initially.
      (with-current-buffer (get-buffer-create "*Commit Message*")
        (erase-buffer))
      ;; Send request without menu.
      (gptel-request diff-text :system prompt
        :callback (lambda (response info)
                    (if (stringp response)
                        (let ((output-buffer (get-buffer-create "*Commit Message*")))
                                        ; Create a new reference to the buffer
                                        ; to avoid closure dependency.
                          (with-current-buffer output-buffer
                            (erase-buffer)
                            (let ((msg (string-trim response)))
                              ;; Remove leading and trailing backtick wrappers
                              ;; from the response, if any.
                              (setq msg (replace-regexp-in-string "^```\n" "" msg))
                              (setq msg (replace-regexp-in-string "```$" "" msg))
                              ;; Replace backticks with single quotes.
                              (setq msg (replace-regexp-in-string "`" "'" msg))
                              (kill-new msg) ; Add to kill ring.
                              (insert msg)
                              (message "[Commit message copied to kill ring.]"))
                          (display-buffer output-buffer)))
                      (message "[Failed to generate commit message: %s.]"
                               (plist-get info :status)))))))

  ;; Diff mode keybinding (only if free).
  (with-eval-after-load 'diff-mode
    (eboost-define-key-if-free 'diff-mode-map
                               (kbd "w")
                               #'eboost-gptel-write-commit-message
                               "diff-mode"))

  ;; Global keybinding (only if free).
  (eboost-define-key-if-free global-map
                             (kbd "C-x v w")
                             #'eboost-gptel-write-commit-message
                             "global map")

  (defun eboost-ai-generate-contextual-test ()
    "Generate a unit test for the current function by sending its source code to GPTel."
    (interactive)
    (let* ((function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Generate a unit test for the following function:\n\n%s"
                           function-source)))
      (gptel-request prompt)))

  (defun eboost-ai-refactor-function ()
    "Refactor the current function with suggestions from GPTel."
    (interactive)
    (let* ((function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Suggest a refactored, cleaner version of this function:\n\n%s\n\nProvide the refactored code and explain the improvements." function-source)))
      (gptel-request prompt)))

  (defun eboost-ai-generate-docstring ()
    "Generate a docstring for the current function using GPTel."
    (interactive)
    (let* ((function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Generate a detailed docstring for the following function:\n\n%s\n\nFollow Emacs docstring conventions." function-source)))
      (gptel-request prompt)))

  (defun eboost-ai-debug-function ()
    "Analyze the current function for bugs or improvements using GPTel."
    (interactive)
    (let* ((function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Analyze this function for potential bugs or improvements:\n\n%s\n\nProvide a list of issues and suggested fixes." function-source)))
      (gptel-request prompt)))

  (defun eboost-ai-generate-example-usage ()
    "Generate example usage for the current function using GPTel."
    (interactive)
    (let* ((function-name (which-function))
           (function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Provide example usage code for the following function named %s:\n\n%s\n\nInclude a brief explanation of each example."
                           function-name function-source)))
      (gptel-request prompt)))

  (defun eboost-ai-generate-function-from-spec ()
    "Generate an Emacs Lisp function from a user-provided specification using GPTel."
    (interactive)
    (let ((spec (read-string "Enter the function specification (e.g., 'Write a function to reverse a string'): ")))
      (when spec
        (let ((prompt (format "Generate an Emacs Lisp function based on this specification: %s\n\nProvide the function code and a brief explanation of how it works."
                              spec)))
          (gptel-request prompt)))))

  (defun eboost-ai-optimize-function-performance ()
    "Suggest performance optimizations for the current function using GPTel."
    (interactive)
    (let* ((function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Analyze this Emacs Lisp function for performance bottlenecks:\n\n%s\n\nSuggest optimizations with code examples and explain why they improve performance." function-source)))
      (gptel-request prompt)))

  (defvar eboost-ai-code-review-prompt
    "Perform a code review for this Emacs Lisp code:\n\n%s\n\nProvide detailed feedback, including clarity, style, potential bugs, and suggestions for improvement."
    "Prompt template for AI code review.")

  (defun eboost-ai-code-review-function (&optional arg)
    "Perform a code review on the current Emacs Lisp function, region, or buffer using GPTel.
  With no prefix argument, review the current function.
  With one `C-u`, review the active region.
  With two `C-u` (i.e., `C-u C-u`), review the entire buffer.

  The review output is sent to the *Code Review* buffer."
    (interactive "p")
    (unless (fboundp 'gptel-request)
      (error "GPTel package is not loaded or configured"))
    (let* ((function-source
            (cond
             ((= arg 16) ;; C-u C-u
              (buffer-substring-no-properties (point-min) (point-max)))
             ((= arg 4)  ;; C-u
              (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (error "No active region found")))
             (t ;; default: current defun
              (save-excursion
                (unless (beginning-of-defun)
                  (error "No valid function found at point"))
                (buffer-substring-no-properties
                 (point)
                 (progn (end-of-defun) (point)))))))
           (prompt (format eboost-ai-code-review-prompt function-source))
           (output-buffer (get-buffer-create "*Code Review*")))
      (when (string-empty-p function-source)
        (error "No valid code source extracted"))
      (message "Sending code for AI code review...")
      (condition-case err
          (gptel-request
           prompt
           :buffer output-buffer
           :callback
           (lambda (response info)
             (let ((buf (or (plist-get info :buffer) output-buffer)))
               (with-current-buffer buf
                 (erase-buffer)
                 (insert (or response "No response received from GPTel"))
                 (emacs-lisp-mode)
                 (display-buffer buf)))
             (message "Code review completed!")))
        (error (message "Code review failed: %s" err)))))

  (defun eboost-ai-generate-companion-function ()
    "Generate a companion function for the current function using GPTel."
    (interactive)
    (let* ((function-name (which-function))
           (function-source (save-excursion
                              (beginning-of-defun)
                              (buffer-substring-no-properties
                               (point)
                               (progn (end-of-defun) (point)))))
           (prompt (format "Generate a companion function for this Emacs Lisp function named %s:\n\n%s\n\nThe companion could be an inverse operation, a helper function, or something that logically complements it. Provide the code and explain its purpose." function-name function-source)))
      (gptel-request prompt)))

  ;; Unbind `C-c RET' in Org mode.
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c RET") nil))

  ;; Quick access to gptel-send (only if key is free).
  (with-eval-after-load 'gptel
    (eboost-define-key-if-free global-map
                               (kbd "C-c RET")
                               #'gptel-send
                               "global map"))

  (defun eboost-gptel-chat-buffer ()
    "Switch to the GPTel chat buffer, creating it if it doesn't exist."
    (interactive)
    (let ((buffer-name "*ChatGPT*"))
      (if (get-buffer buffer-name)
          (pop-to-buffer buffer-name)
        (progn
          (call-interactively 'gptel)))))

  ;; Global keybinding (only if free).
  (eboost-define-key-if-free global-map
                             (kbd "C-c g")
                             #'eboost-gptel-chat-buffer
                             "global map")

)

;; Load org-ai.
(when (eboost-try-require 'org-ai)

  ;; Enable org-ai-mode in Org mode.
  (add-hook 'org-mode-hook #'org-ai-mode)

  ;; Set OpenAI API key.
  (when (bound-and-true-p eboost-openai-api-key)
    (setq org-ai-openai-api-token eboost-openai-api-key))

  ;; Install YASnippet templates for org-ai.
  (when (eboost-try-require 'yasnippet)
    (org-ai-install-yasnippets)))

(message "* --[ Loaded Emacs-Leuven AI %s ]--"
         (if (boundp 'lvn--emacs-version) lvn--emacs-version ""))

(provide 'emacs-leuven-ai)

;;; emacs-leuven-ai.el ends here
