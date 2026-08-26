;;; boost-gptel.el --- GPTel configuration  -*- lexical-binding: t; -*-

;; This file is generated from emacs-leuven-gptel.txt.
;; Edit the Org source, then tangle it again.

;;; Code:

(boost--try-require 'cl-lib)
(boost--try-require 'seq)
(boost--try-require 'subr-x)
(boost--try-require 'rx)
(boost--try-require 'auth-source)
(boost--try-require 'project)
(boost--try-require 'pp)
(boost--try-require 'org)
(boost--try-require 'gptel)
(boost--try-require 'gptel-context)
(boost--try-require 'gptel-org)
(boost--try-require 'gptel-openai)
(boost--try-require 'gptel-anthropic)

(defgroup boost-gptel nil
  "Personal configuration layered on top of GPTel."
  :group 'gptel
  :prefix "boost-gptel-")

(defcustom boost-gptel-prompt-directory
  (expand-file-name "prompts/gptel/" user-emacs-directory)
  "Directory containing optional external prompt files."
  :type 'directory
  :group 'boost-gptel)

(defcustom boost-gptel-note-directory
  (expand-file-name "gptel-notes/" user-emacs-directory)
  "Directory in which the confirmed create_note tool may write Org files."
  :type 'directory
  :group 'boost-gptel)

(defcustom boost-gptel-private-file
  (expand-file-name "private/local-gptel.el" user-emacs-directory)
  "Optional non-versioned file loaded before backend registration."
  :type 'file
  :group 'boost-gptel)

(defcustom boost-gptel-tool-max-output-chars 50000
  "Maximum number of characters returned by a read tool."
  :type 'natnum
  :group 'boost-gptel)

(defcustom boost-gptel-tool-max-search-files 300
  "Maximum number of project files inspected by search_project."
  :type 'natnum
  :group 'boost-gptel)

(defcustom boost-gptel-tool-max-search-matches 80
  "Maximum number of matches returned by search_project."
  :type 'natnum
  :group 'boost-gptel)

(defcustom boost-gptel-command-max-input-chars 120000
  "Maximum amount of buffer text used by custom one-shot commands."
  :type 'natnum
  :group 'boost-gptel)

(defcustom boost-gptel-move-point-after-response nil
  "Whether to move point to the end of a completed response."
  :type 'boolean
  :group 'boost-gptel)

(defcustom boost-gptel-sensitive-file-regexp
  (rx (seq
       (or string-start "/")
       (or ".env" ".envrc" ".direnv" ".authinfo" ".netrc"
           ".ssh" ".gnupg" "id_rsa" "id_ed25519"
           "credentials" "secret" "secrets")
       (or string-end "/" ".")))
  "Regexp for project paths that GPTel read tools must reject.

This is a conservative example, not a complete secret-detection mechanism."
  :type 'regexp
  :group 'boost-gptel)

(defcustom boost-gptel-sensitive-buffer-regexp
  (rx string-start
      (or " " "*auth-source" "*password" "*secret" "*credentials"))
  "Regexp for buffer names that the read_buffer tool must reject."
  :type 'regexp
  :group 'boost-gptel)

(defcustom boost-gptel-enable-openai t
  "Whether to register the example OpenAI API backend."
  :type 'boolean
  :group 'boost-gptel)

(defcustom boost-gptel-openai-model 'gpt-5-mini
  "Example model identifier for the OpenAI API backend."
  :type 'symbol
  :group 'boost-gptel)

(defcustom boost-gptel-enable-anthropic t
  "Whether to register the example Anthropic backend."
  :type 'boolean
  :group 'boost-gptel)

(defcustom boost-gptel-anthropic-model 'claude-opus-4-8
  "Example model identifier for the Anthropic backend."
  :type 'symbol
  :group 'boost-gptel)

(defcustom boost-gptel-default-provider 'current
  "Provider used as the global default after backend registration.

The value `current' leaves GPTel's existing backend and model unchanged."
  :type '(choice
          (const :tag "Keep the current GPTel default" current)
          (const :tag "OpenAI API" openai)
          (const :tag "Anthropic" anthropic))
  :group 'boost-gptel)

(when (file-readable-p boost-gptel-private-file)
  (load boost-gptel-private-file nil 'nomessage))

(defun boost-gptel-auth-source-secret (host user)
  "Read and return an auth-source secret for HOST and USER."
  (let* ((entry
          (car
           (auth-source-search
            :host host
            :user user
            :require '(:secret)
            :max 1)))
         (secret (plist-get entry :secret)))
    (unless secret
      (user-error
       "No auth-source secret for host %s and user %s"
       host user))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun boost-gptel-auth-source-key (host &optional user)
  "Return a zero-argument credential function for HOST and USER.

USER defaults to \"apikey\".  The result is suitable for a GPTel backend's
=:key= argument and remains valid when this Org block is evaluated directly."
  (apply-partially
   #'boost-gptel-auth-source-secret
   host
   (or user "apikey")))

(defun boost--gptel-api-key-from-file (&optional file)
  "Return a function that reads an API key from FILE.

When FILE is nil, derive the filename from the active backend type.
For example, an `gptel-openai' backend resolves to:

  ~/.openai_api_key

The file must contain only the API key, optionally followed by a
trailing newline.

The returned function performs the lookup when GPTel requests the
credential."
  (lambda ()
    (let* ((key-file
            (or file
                (expand-file-name
                 (format ".%s_api_key"
                         (thread-first
                           (type-of gptel-backend)
                           (symbol-name)
                           (substring 6)
                           (downcase)))
                 "~")))
           (key
            (when (file-readable-p key-file)
              (with-temp-buffer
                (insert-file-contents key-file)
                (string-trim
                 (buffer-substring-no-properties
                  (point-min)
                  (point-max)))))))
      (unless (and key (not (string-empty-p key)))
        (user-error "No API key found in %s" key-file))
      key)))

(defun boost-gptel-project-root (&optional directory)
  "Return the current project root for DIRECTORY, or nil."
  (when-let* ((project (project-current nil directory)))
    (file-name-as-directory
     (expand-file-name (project-root project)))))

(defun boost-gptel-project-name ()
  "Return a short name for the current project."
  (if-let* ((root (boost-gptel-project-root)))
      (file-name-nondirectory (directory-file-name root))
    "no-project"))

(defun boost-gptel-sensitive-file-p (file)
  "Return non-nil when FILE matches the configured sensitive path regexp."
  (let ((case-fold-search t))
    (string-match-p
     boost-gptel-sensitive-file-regexp
     (expand-file-name file))))

(defun boost-gptel-sensitive-buffer-p (buffer)
  "Return non-nil when BUFFER should not be exposed through a read tool."
  (if (not (buffer-live-p buffer))
      t
    (with-current-buffer buffer
      (let ((case-fold-search t))
        (or (string-match-p boost-gptel-sensitive-buffer-regexp (buffer-name))
            (and buffer-file-name
                 (boost-gptel-sensitive-file-p buffer-file-name)))))))

(defun boost-gptel-safe-project-file (relative-path)
  "Return an existing project file identified by RELATIVE-PATH.

The resolved file must remain inside the current project, including after
symbolic links are resolved."
  (when (file-name-absolute-p relative-path)
    (user-error "Expected a path relative to the current project"))
  (let* ((root (or (boost-gptel-project-root)
                   (user-error "No current project")))
         (candidate (expand-file-name relative-path root)))
    (unless (file-exists-p candidate)
      (user-error "Project file does not exist: %s" relative-path))
    (let ((true-root (file-truename root))
          (true-file (file-truename candidate)))
      (unless (file-in-directory-p true-file true-root)
        (user-error "Path escapes the current project: %s" relative-path))
      (when (boost-gptel-sensitive-file-p true-file)
        (user-error "Refusing to read a sensitive project path: %s"
                    relative-path))
      true-file)))

(defun boost-gptel-truncate-string (text limit)
  "Return TEXT truncated to LIMIT characters with a clear marker."
  (if (<= (length text) limit)
      text
    (concat
     (substring text 0 limit)
     (format "\n\n[Output truncated after %d characters.]" limit))))

(defun boost-gptel-buffer-substring-limited (begin end limit)
  "Return buffer text from BEGIN to END without copying more than LIMIT chars."
  (let* ((start (min begin end))
         (finish (max begin end))
         (cutoff (min finish (+ start limit)))
         (text (buffer-substring-no-properties start cutoff)))
    (if (< cutoff finish)
        (concat
         text
         (format "\n\n[Output truncated after %d characters.]" limit))
      text)))

(defun boost-gptel-read-file-limited (file &optional limit)
  "Read FILE and return at most LIMIT characters.

LIMIT defaults to `boost-gptel-tool-max-output-chars'."
  (let ((max-chars (or limit boost-gptel-tool-max-output-chars)))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Not a readable regular file: %s" file))
    (with-temp-buffer
      (insert-file-contents file nil 0 (min (file-attribute-size
                                             (file-attributes file))
                                            (* 4 max-chars)))
      (when (save-excursion
              (goto-char (point-min))
              (search-forward "\0" nil t))
        (user-error "Refusing to read a binary file: %s" file))
      (boost-gptel-truncate-string
       (buffer-substring-no-properties (point-min) (point-max))
       max-chars))))

(defun boost-gptel-read-prompt-file (name &optional fallback)
  "Read prompt NAME from `boost-gptel-prompt-directory'.

NAME is read from NAME.txt.  Return FALLBACK when the file is absent."
  (let ((file (expand-file-name (concat name ".txt")
                                boost-gptel-prompt-directory)))
    (cond
     ((file-readable-p file)
      (string-trim (boost-gptel-read-file-limited file 100000)))
     (fallback fallback)
     (t
      (user-error "Prompt file is not readable: %s" file)))))

(defun boost-gptel-slugify (text)
  "Convert TEXT to a conservative lowercase file-name component."
  (let ((slug (downcase (string-trim text))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    (if (string-empty-p slug) "note" slug)))

(defvar boost-gptel-openai-backend nil)
(defvar boost-gptel-anthropic-backend nil)

(when boost-gptel-enable-openai
  (setq boost-gptel-openai-backend
        (gptel-make-openai
            "OpenAI-API"
          :host "api.openai.com"
          :stream t
          :key (boost--gptel-api-key-from-file)
          :models (list boost-gptel-openai-model))))

(when boost-gptel-enable-anthropic
  (setq boost-gptel-anthropic-backend
        (gptel-make-anthropic
            "Anthropic"
          :stream t
          :key (boost--gptel-api-key-from-file)
          :models (list boost-gptel-anthropic-model))))

(defun boost-gptel-select-default-provider ()
  "Set the global GPTel backend and model from `boost-gptel-default-provider'."
  (pcase boost-gptel-default-provider
    ('current nil)
    ('openai
     (if boost-gptel-openai-backend
         (setq gptel-backend boost-gptel-openai-backend
               gptel-model boost-gptel-openai-model)
       (display-warning
        'boost-gptel
        "OpenAI was selected as default but its backend is disabled.")))
    ('anthropic
     (if boost-gptel-anthropic-backend
         (setq gptel-backend boost-gptel-anthropic-backend
               gptel-model boost-gptel-anthropic-model)
       (display-warning
        'boost-gptel
        "Anthropic was selected as default but its backend is disabled.")))))

(boost-gptel-select-default-provider)

;; Do not include the reasoning at all.
(setq gptel-include-reasoning nil)

(setq gptel-context-restrict-to-project-files t)

(setq gptel-track-media nil)

(setq gptel-log-level nil)

;; Enable GPTel's expert/power-user commands.
(setq gptel-expert-commands t)

(defconst boost-gptel-prompt-default
  (string-join
   '("You are a careful assistant working inside Emacs."
     "Answer the user's actual question directly."
     "Distinguish facts, assumptions, and recommendations."
     "When information is missing, say what is missing instead of inventing it."
     "Prefer clear structure, concrete examples, and concise explanations."
     "Do not claim to have inspected files or executed actions unless a tool result confirms it.")
   "\n"))

(defconst boost-gptel-prompt-precise
  (concat
   boost-gptel-prompt-default
   "\n\n"
   (string-join
    '("Keep the answer compact."
      "Lead with the conclusion."
      "Include only details needed to justify or apply the conclusion.")
    "\n")))

(defconst boost-gptel-prompt-programming
  (string-join
   '("You are a senior software engineer working inside Emacs."
     "Inspect the supplied code and context before proposing changes."
     "Preserve existing behavior unless the user explicitly requests a change."
     "State important assumptions."
     "Prefer small, reviewable changes over broad rewrites."
     "Return complete code when the user asks for code."
     "Mention security, compatibility, and failure-mode implications when relevant."
     "Never claim that code was executed unless a tool result confirms execution.")
   "\n"))

(defconst boost-gptel-prompt-code-review
  (string-join
   '("Act as a rigorous code reviewer."
     "Prioritize correctness, data loss, security, concurrency, and compatibility."
     "Separate confirmed defects from possible risks."
     "For every finding, identify the relevant code and explain the failure scenario."
     "Avoid style-only comments unless they materially affect maintenance or correctness."
     "End with a short assessment of residual risk.")
   "\n"))

(defconst boost-gptel-prompt-writing
  (string-join
   '("You are an exacting writing editor."
     "Preserve the author's meaning, factual claims, and intended audience."
     "Improve clarity, structure, rhythm, and precision."
     "Do not introduce new facts."
     "Keep terminology consistent."
     "When rewriting, return the revised text first.")
   "\n"))

(defconst boost-gptel-prompt-research
  (string-join
   '("You are a cautious research assistant."
     "Base conclusions only on the material supplied in the conversation or by tools."
     "Separate direct evidence from inference."
     "Identify disagreements, missing evidence, and uncertainty."
     "Do not fabricate citations, quotations, dates, or source details."
     "Use a comparison table when it genuinely improves the analysis.")
   "\n"))

(defconst boost-gptel-prompt-summarization
  (string-join
   '("Summarize the supplied material faithfully."
     "Preserve decisions, constraints, numbers, dates, names, and unresolved questions."
     "Do not add information that is not present in the source."
     "Use headings only when they improve navigation."
     "Finish with a short list of open questions when any remain.")
   "\n"))

(defun boost-gptel-project-directive ()
  "Return a programming directive enriched with current Emacs context."
  (format
   "%s\n\nCurrent Emacs context:\n- Project: %s\n- Major mode: %s\n- File: %s"
   boost-gptel-prompt-programming
   (boost-gptel-project-name)
   major-mode
   (if buffer-file-name
       (abbreviate-file-name buffer-file-name)
     "no-file")))

(defun boost-gptel-house-style-directive ()
  "Return the external house-style prompt or a safe built-in fallback."
  (boost-gptel-read-prompt-file
   "house-style"
   boost-gptel-prompt-writing))

(defconst boost-gptel-pair-programming-template
  (list
   boost-gptel-prompt-programming
   "Before changing code, briefly restate the requirement and list material assumptions."
   "Understood. I will first restate the requirement and identify material assumptions, then propose the smallest safe change."))

(dolist
    (entry
     (list
      (cons 'default boost-gptel-prompt-default)
      (cons 'precise boost-gptel-prompt-precise)
      (cons 'programming boost-gptel-prompt-programming)
      (cons 'code-review boost-gptel-prompt-code-review)
      (cons 'writing boost-gptel-prompt-writing)
      (cons 'research boost-gptel-prompt-research)
      (cons 'summarize boost-gptel-prompt-summarization)
      (cons 'project-aware #'boost-gptel-project-directive)
      (cons 'house-style #'boost-gptel-house-style-directive)
      (cons 'pair-programming boost-gptel-pair-programming-template)))
  (setf (alist-get (car entry) gptel-directives) (cdr entry)))

(setq gptel-system-prompt (alist-get 'default gptel-directives))

(defcustom boost-gptel-project-context-files
  '("README.md" "README.org" "CONTRIBUTING.md" "AGENTS.md")
  "Project-relative files considered by `boost-gptel-add-project-context'."
  :type '(repeat string)
  :group 'boost-gptel)

(defun boost-gptel--ensure-local-context ()
  "Ensure that the current buffer has an independent GPTel context."
  (unless (local-variable-p 'gptel-context)
    (setq-local gptel-context nil)))

(defun boost-gptel-add-project-context ()
  "Add existing files from `boost-gptel-project-context-files' to local context."
  (interactive)
  (boost-gptel--ensure-local-context)
  (let ((root (or (boost-gptel-project-root)
                  (user-error "No current project")))
        (added 0))
    (dolist (relative boost-gptel-project-context-files)
      (let ((file (expand-file-name relative root)))
        (when (file-readable-p file)
          (gptel-context-add-file
           (boost-gptel-safe-project-file relative))
          (cl-incf added))))
    (message "[Added %d project context file%s]"
             added
             (if (= added 1) "" "s"))))

(defun boost-gptel-clear-buffer-context ()
  "Remove every GPTel context source local to the current buffer."
  (interactive)
  (boost-gptel--ensure-local-context)
  (gptel-context-remove-all)
  (message "[Cleared buffer-local GPTel context]"))

(defun boost-gptel-show-context ()
  "Display the active GPTel context value in a temporary buffer."
  (interactive)
  (with-help-window "*boost-gptel-context*"
    (princ "Active GPTel context:\n\n")
    (pp gptel-context)))

(defun boost-gptel-tool-current-datetime ()
  "Return the current local date, time, and time-zone offset."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun boost-gptel-tool-read-buffer (buffer-name)
  "Return BUFFER-NAME contents, truncated to the configured limit."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (user-error "No live buffer named %s" buffer-name))
    (when (boost-gptel-sensitive-buffer-p buffer)
      (user-error "Refusing to read a sensitive buffer: %s" buffer-name))
    (with-current-buffer buffer
      (boost-gptel-buffer-substring-limited
       (point-min)
       (point-max)
       boost-gptel-tool-max-output-chars))))

(defun boost-gptel-tool-list-project-files (&optional extension)
  "Return project-relative file names, optionally filtered by EXTENSION."
  (let* ((project (or (project-current nil)
                      (user-error "No current project")))
         (root (file-name-as-directory (project-root project)))
         (files (project-files project))
         (suffix
          (when (and extension (not (string-empty-p extension)))
            (if (string-prefix-p "." extension)
                extension
              (concat "." extension)))))
    (setq files
          (seq-remove
           (lambda (file)
             (boost-gptel-sensitive-file-p
              (expand-file-name file root)))
           files))
    (when suffix
      (setq files
            (seq-filter
             (lambda (file)
               (string-suffix-p suffix file t))
             files)))
    (boost-gptel-truncate-string
     (string-join
      (mapcar (lambda (file) (file-relative-name file root)) files)
      "\n")
     boost-gptel-tool-max-output-chars)))

(defun boost-gptel-tool-read-project-file (relative-path)
  "Return the contents of project file RELATIVE-PATH."
  (boost-gptel-read-file-limited
   (boost-gptel-safe-project-file relative-path)))

(defun boost-gptel-tool-search-project (query)
  "Search project files for literal string QUERY and return matching lines."
  (when (string-empty-p (string-trim query))
    (user-error "Search query must not be empty"))
  (let* ((project (or (project-current nil)
                      (user-error "No current project")))
         (root (file-name-as-directory (project-root project)))
         (true-root (file-truename root))
         (files (seq-take (project-files project)
                          boost-gptel-tool-max-search-files))
         (case-fold-search t)
         (matches nil)
         (match-count 0))
    (catch 'enough-matches
      (dolist (project-file files)
        (let ((file (expand-file-name project-file root)))
          (condition-case nil
              (when (and (file-in-directory-p (file-truename file) true-root)
                         (not (boost-gptel-sensitive-file-p file))
                         (file-regular-p file)
                         (file-readable-p file)
                         (< (file-attribute-size (file-attributes file))
                            1000000))
                (with-temp-buffer
                  (insert-file-contents file)
                  (unless (save-excursion
                            (goto-char (point-min))
                            (search-forward "\0" nil t))
                    (goto-char (point-min))
                    (while (search-forward query nil t)
                      (let* ((line (line-number-at-pos))
                             (text
                              (string-trim
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))))
                        (push
                         (format "%s:%d: %s"
                                 (file-relative-name file root)
                                 line
                                 text)
                         matches)
                        (cl-incf match-count)
                        (when (>= match-count
                                  boost-gptel-tool-max-search-matches)
                          (throw 'enough-matches nil)))))))
            (error nil)))))
    (if matches
        (boost-gptel-truncate-string
         (string-join (nreverse matches) "\n")
         boost-gptel-tool-max-output-chars)
      "No matches found.")))

(defun boost-gptel-tool-create-note (title content)
  "Create an Org note with TITLE and CONTENT in the configured note directory."
  (when (string-empty-p (string-trim title))
    (user-error "Note title must not be empty"))
  (make-directory boost-gptel-note-directory t)
  (let* ((clean-title
          (replace-regexp-in-string "[\r\n]+" " " (string-trim title)))
         (stamp (format-time-string "%Y%m%d-%H%M%S"))
         (slug
          (truncate-string-to-width
           (boost-gptel-slugify clean-title)
           60
           nil
           nil))
         (file
          (make-temp-file
           (expand-file-name
            (format "%s-%s-" stamp slug)
            boost-gptel-note-directory)
           nil
           ".org")))
    (with-temp-file file
      (insert "#+TITLE:     " clean-title "\n")
      (insert "#+DATE:      " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n\n")
      (insert content)
      (unless (string-suffix-p "\n" content)
        (insert "\n")))
    (format "Created note: %s" (abbreviate-file-name file))))

(defvar boost-gptel-tool-current-datetime nil)
(defvar boost-gptel-tool-read-buffer nil)
(defvar boost-gptel-tool-list-project-files nil)
(defvar boost-gptel-tool-read-project-file nil)
(defvar boost-gptel-tool-search-project nil)
(defvar boost-gptel-tool-create-note nil)
(defvar boost-gptel-tools-read-only nil)
(defvar boost-gptel-tools-with-notes nil)

(setq boost-gptel-tool-current-datetime
      (gptel-make-tool
       :name "current_datetime"
       :function #'boost-gptel-tool-current-datetime
       :description
       "Return the current local date, time, weekday, and numeric time-zone offset."
       :args nil
       :category "environment"
       :confirm nil
       :include t))

(setq boost-gptel-tool-read-buffer
      (gptel-make-tool
       :name "read_buffer"
       :function #'boost-gptel-tool-read-buffer
       :description
       "Return the plain-text contents of a currently live Emacs buffer. Sensitive buffers are rejected, the result may be truncated, and the call requires confirmation."
       :args
       (list
        '(:name "buffer_name"
          :type string
          :description "Exact name of the Emacs buffer to read"))
       :category "emacs-read"
       :confirm t
       :include t))

(setq boost-gptel-tool-list-project-files
      (gptel-make-tool
       :name "list_project_files"
       :function #'boost-gptel-tool-list-project-files
       :description
       "List files in the current Emacs project. Optionally filter by a file extension such as el, py, or org."
       :args
       (list
        '(:name "extension"
          :type string
          :description "Optional file extension, with or without a leading dot"
          :optional t))
       :category "project-read"
       :confirm nil
       :include t))

(setq boost-gptel-tool-read-project-file
      (gptel-make-tool
       :name "read_project_file"
       :function #'boost-gptel-tool-read-project-file
       :description
       "Read a text file inside the current Emacs project. The path must be relative to the project root and may not escape it."
       :args
       (list
        '(:name "relative_path"
          :type string
          :description "Path relative to the current project root"))
       :category "project-read"
       :confirm nil
       :include t))

(setq boost-gptel-tool-search-project
      (gptel-make-tool
       :name "search_project"
       :function #'boost-gptel-tool-search-project
       :description
       "Search a bounded set of project text files for a literal, case-insensitive string and return file, line number, and matching line."
       :args
       (list
        '(:name "query"
          :type string
          :description "Non-empty literal text to search for"))
       :category "project-read"
       :confirm nil
       :include t))

(setq boost-gptel-tool-create-note
      (gptel-make-tool
       :name "create_note"
       :function #'boost-gptel-tool-create-note
       :description
       "Create a new timestamped Org note inside the configured GPTel note directory. This tool cannot choose an arbitrary output path."
       :args
       (list
        '(:name "title"
          :type string
          :description "Short note title")
        '(:name "content"
          :type string
          :description "Complete Org-formatted note content"))
       :category "notes-write"
       :confirm t
       :include t))

(setq boost-gptel-tools-read-only
      (list
       boost-gptel-tool-current-datetime
       boost-gptel-tool-read-buffer
       boost-gptel-tool-list-project-files
       boost-gptel-tool-read-project-file
       boost-gptel-tool-search-project))

(setq boost-gptel-tools-with-notes
      (append boost-gptel-tools-read-only
              (list boost-gptel-tool-create-note)))

(defun boost-gptel-pre-tool-policy (call)
  "Apply additional policy to a GPTel tool CALL plist."
  (let ((name (plist-get call :name)))
    (cond
     ((member name '("create_note"))
      '(:confirm t))
     (t nil))))

(add-hook 'gptel-pre-tool-call-functions #'boost-gptel-pre-tool-policy)

(defun boost-gptel-post-tool-log (call)
  "Log completion of a GPTel tool CALL without logging sensitive contents."
  (message "[gptel tool completed: %s]" (plist-get call :name))
  nil)

(add-hook 'gptel-post-tool-call-functions #'boost-gptel-post-tool-log)

(gptel-make-preset 'boost-base
  :description "Conservative defaults with no tools selected."
  :system 'default
  :tools nil
  ;; :temperature 0.2
  :max-tokens nil
  :stream t
  :use-context 'system
  :track-media nil
  :include-reasoning 'ignore
  :confirm-tool-calls 'auto
  :include-tool-results 'auto)

(gptel-make-preset 'boost-precise
  :description "Compact answers with low randomness."
  :parents 'boost-base
  :system 'precise
  ;; :temperature 0.1
  )

(gptel-make-preset 'boost-coding
  :description "Project-aware programming with read-only Emacs and project tools."
  :parents 'boost-base
  :system 'project-aware
  :tools '("read_buffer"
           "list_project_files"
           "read_project_file"
           "search_project")
  ;; :temperature 0.2
  :use-context 'system)

(gptel-make-preset 'boost-code-review
  :description "Rigorous code review using read-only project tools."
  :parents 'boost-coding
  :system 'code-review
  ;; :temperature 0.1
  )

(gptel-make-preset 'boost-pair-programming
  :description "Programming preset with an initial conversation template."
  :parents 'boost-coding
  :system 'pair-programming)

(gptel-make-preset 'boost-writing
  :description "Editing and rewriting with access to the current buffer."
  :parents 'boost-base
  :system 'writing
  :tools '("read_buffer")
  ;; :temperature 0.6
  :use-context 'user)

(gptel-make-preset 'boost-house-style
  :description "Writing with an optional external house-style prompt."
  :parents 'boost-writing
  :system 'house-style
  ;; :temperature 0.4
  )

(gptel-make-preset 'boost-research
  :description "Evidence-focused analysis with bounded read-only tools."
  :parents 'boost-base
  :system 'research
  :tools '("current_datetime"
           "read_buffer"
           "list_project_files"
           "read_project_file"
           "search_project")
  ;; :temperature 0.2
  :use-context 'system)

(gptel-make-preset 'boost-visible-buffers
  :description "Research preset with all visible non-internal buffers as context."
  :parents 'boost-research
  :context
  '(:eval
    (cl-remove-if
     (lambda (buffer)
       (or (string-prefix-p " " (buffer-name buffer))
           (boost-gptel-sensitive-buffer-p buffer)))
     (delete-dups (mapcar #'window-buffer (window-list)))))
  :use-context 'user)

(gptel-make-preset 'boost-note-taking
  :description "Research plus confirmed creation of Org notes."
  :parents 'boost-research
  :tools '("current_datetime"
           "read_buffer"
           "list_project_files"
           "read_project_file"
           "search_project"
           "create_note")
  :confirm-tool-calls 'auto)

(when boost-gptel-openai-backend
  (gptel-make-preset 'boost-openai
    :description "Use the configured OpenAI API backend."
    :parents 'boost-base
    :backend "OpenAI-API"
    :model boost-gptel-openai-model))

(when boost-gptel-anthropic-backend
  (gptel-make-preset 'boost-anthropic
    :description "Use the configured Anthropic backend."
    :parents 'boost-base
    :backend "Anthropic"
    :model boost-gptel-anthropic-model))

;; Use Org mode for GPTel chat buffers.
(setq gptel-default-mode 'org-mode)

(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "Prompt -> ")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "Response <-\n")

(with-eval-after-load 'gptel
  ;; Highlight GPTel responses with a light gray background and a slightly
  ;; darker bar in the left fringe.
  (setq gptel-highlight-methods '(face fringe))

  (set-face-attribute 'gptel-response-highlight nil
                      :background "#F3F3F3"
                      :extend t)

  (set-face-attribute 'gptel-response-fringe-highlight nil
                      :foreground "#BF6896")

  (defvar-local boost-gptel-response-tail-overlays nil
    "Overlays extending GPTel response backgrounds to the next line.

GPTel response regions can end immediately after the final character of the
response, before the terminating newline.  In that situation the `:extend'
attribute of `gptel-response-highlight' cannot paint the remainder of the
visual line.

These overlays cover the terminating newline without modifying GPTel's
`gptel' text property.")

  (defun boost--gptel-delete-response-tail-overlays (&optional begin end)
    "Delete response-tail overlays intersecting BEGIN and END.

BEGIN defaults to `point-min' and END defaults to `point-max'."

    (let ((begin (or begin (point-min)))
          (end   (or end   (point-max))))
      (dolist (overlay (overlays-in begin end))
        (when (overlay-get overlay 'boost-gptel-response-tail-overlay)
          (delete-overlay overlay))))

    ;; Remove references to overlays that no longer exist.
    (setq boost-gptel-response-tail-overlays
          (cl-delete-if-not #'overlay-buffer
                            boost-gptel-response-tail-overlays)))

  (defun boost--gptel-extend-response-background (begin end)
    "Extend a GPTel response background from END to the next line.

BEGIN and END are supplied by `gptel-post-response-functions'.

GPTel can terminate its response overlay immediately after the final response
character.  Since the terminating newline is then outside the overlay,
`:extend t' cannot paint the background through the remainder of the line.

This function adds a background-only overlay from END through the terminating
newline.  It deliberately does not add or modify the `gptel' text property."

    (when (and (< begin end)
               (< end (point-max)))
      (boost--gptel-delete-response-tail-overlays
       end
       (min (1+ end) (point-max)))

      (save-excursion
        (goto-char end)

        ;; Only extend when END is not already positioned after a newline.
        (unless (bolp)
          (let* ((tail-begin end)
                 ;; Include the terminating newline, but do not colour the
                 ;; contents of the following prompt.
                 (tail-end
                  (min (line-beginning-position 2)
                       (point-max)))
                 (overlay
                  (make-overlay tail-begin tail-end nil t nil)))

            (overlay-put overlay
                         'boost-gptel-response-tail-overlay
                         t)

            (overlay-put overlay 'evaporate t)

            ;; Use the same GPTel face.  Because the overlay includes the
            ;; newline, `:extend t' paints the background to the right edge.
            (overlay-put overlay
                         'face
                         'gptel-response-highlight)

            ;; Keep the extension above incidental low-priority overlays
            ;; such as `hl-line'.
            (overlay-put overlay 'priority 90)

            (push overlay
                  boost-gptel-response-tail-overlays))))))

  ;; GPTel calls functions in this abnormal hook with the beginning and end
  ;; positions of the completed response.
  (add-hook 'gptel-post-response-functions
            #'boost--gptel-extend-response-background
            90)

  (defvar-local boost-gptel-org-src-overlays nil
    "Overlays restoring Org source-block backgrounds over GPTel highlighting.")

  (defun boost--gptel-org-delete-src-overlays (&optional begin end)
    "Delete custom source-block overlays between BEGIN and END."
    (let ((begin (or begin (point-min)))
          (end   (or end   (point-max))))
      (dolist (overlay (overlays-in begin end))
        (when (overlay-get overlay 'boost-gptel-org-src-overlay)
          (delete-overlay overlay)))))

  (defun boost--gptel-org-put-src-overlay (begin end face)
    "Put a background-only overlay from BEGIN to END.

FACE is used only to retrieve its background colour.  The overlay
deliberately does not inherit FACE, so that language-specific
font-lock faces remain visible inside Org source blocks."
    (when (and begin end (< begin end))
      (let ((overlay (make-overlay begin end nil t nil)))
        (overlay-put overlay 'boost-gptel-org-src-overlay t)
        (overlay-put overlay 'evaporate t)

        ;; Override only the background applied by GPTel.
        ;;
        ;; Do not use `:inherit FACE' here: inheriting `org-block' would
        ;; override the language-specific font-lock foreground colours.
        (overlay-put
         overlay 'face
         `(:background
           ,(or (face-background face nil t)
                (face-background 'default nil t))
           :extend t))

        ;; Higher than GPTel's response overlay.
        (overlay-put overlay 'priority 100)

        (push overlay boost-gptel-org-src-overlays))))

  (defun boost--gptel-org-src-property-regions (begin end)
    "Return contiguous regions carrying the `src-block' property."
    (let ((position begin)
          regions)
      (while (< position end)
        (if (get-text-property position 'src-block)
            (let ((next
                   (or (next-single-property-change
                        position 'src-block nil end)
                       end)))
              (push (cons position next) regions)
              (setq position next))
          (setq position
                (or (next-single-property-change
                     position 'src-block nil end)
                    end))))
      (nreverse regions)))

  (defun boost--gptel-org-refresh-src-backgrounds (begin end)
    "Restore Org source-block backgrounds in GPTel response BEGIN to END."
    (when (derived-mode-p 'org-mode)
      ;; Org must create `src-block' text properties before we inspect them.
      (font-lock-flush begin end)
      (font-lock-ensure begin end)

      (boost--gptel-org-delete-src-overlays begin end)

      ;; Code contents, identified by Org's own `src-block' property.
      (dolist (region
               (boost--gptel-org-src-property-regions begin end))
        (boost--gptel-org-put-src-overlay
         (car region)
         (cdr region)
         'org-block))

      ;; Delimiter lines do not necessarily carry `src-block'.
      (save-excursion
        (goto-char begin)

        (while (re-search-forward
                "^[ \t]*#\\+begin_src\\(?:[ \t].*\\)?$"
                end t)
          (boost--gptel-org-put-src-overlay
           (line-beginning-position)
           (min (1+ (line-end-position)) end)
           'org-block-begin-line))

        (goto-char begin)

        (while (re-search-forward
                "^[ \t]*#\\+end_src[ \t]*$"
                end t)
          (boost--gptel-org-put-src-overlay
           (line-beginning-position)
           (min (1+ (line-end-position)) end)
           'org-block-end-line)))))

  (add-hook 'gptel-post-response-functions
            #'boost--gptel-org-refresh-src-backgrounds
            95))

(defun boost-gptel-chat-mode-setup ()
  "Configure presentation in buffers managed by `gptel-mode'."
  (visual-line-mode 1)
  (gptel-highlight-mode 1))

(add-hook 'gptel-mode-hook #'boost-gptel-chat-mode-setup)

;; Send the current prompt with C-c C-c in GPTel conversation buffers.
;;
;; This binding belongs to `gptel-mode-map', whose minor-mode binding takes
;; precedence over the major-mode binding in `org-mode-map'.  Ordinary Org
;; buffers where `gptel-mode' is inactive retain `org-ctrl-c-ctrl-c'.
(define-key gptel-mode-map (kbd "C-c C-c") #'gptel-send)

;; Keep the streaming response visible.
(add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

(defun boost-gptel-after-response (begin end)
  "Run lightweight UI actions after a response from BEGIN to END."
  (when (> end begin)
    (when boost-gptel-move-point-after-response
      (gptel-end-of-response begin end))
    (message "[GPTel response completed: %d character%s]"
             (- end begin)
             (if (= (- end begin) 1) "" "s"))))

(add-hook 'gptel-post-response-functions #'boost-gptel-after-response t)

(defun boost-gptel-directive (name)
  "Return directive NAME or signal a user-facing error."
  (or (alist-get name gptel-directives)
      (user-error "Unknown GPTel directive: %s" name)))

(defun boost-gptel--write-result (buffer response info)
  "Write a GPTel RESPONSE and INFO event into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (cond
         ((plist-get info :error)
          (insert
           (format "\nRequest failed: %s\n"
                   (or (plist-get info :error)
                       (plist-get info :status)
                       "unknown error"))))
         ((stringp response)
          (insert response))
         ((eq response t)
          (unless (bolp) (insert "\n"))
          (insert "\nRequest completed.\n"))
         ((eq response 'abort)
          (insert "\nRequest aborted.\n")))))))

(defun boost-gptel-result-callback (buffer)
  "Return a callback that writes GPTel events into BUFFER."
  (apply-partially #'boost-gptel--write-result buffer))

(defun boost-gptel-request-in-new-buffer (title prompt directive)
  "Send PROMPT with DIRECTIVE and display the result under TITLE."
  (let* ((buffer (generate-new-buffer (format "*gptel: %s*" title)))
         (backend gptel-backend)
         (model gptel-model)
         (backend-name
          (when backend
            (gptel-backend-name backend)))
         (gptel-use-context nil)
         (gptel-context nil)
         (gptel-use-tools nil)
         (gptel-tools nil)
         (gptel-include-reasoning nil)
         (gptel-temperature nil))
    (with-current-buffer buffer
      (org-mode)
      (insert "#+TITLE:     " title "\n")
      (insert "#+GPTEL_BACKEND: " (or backend-name "unknown") "\n")
      (insert "#+GPTEL_MODEL: " (format "%s" model) "\n\n")
      (insert "* Response\n\n"))
    (display-buffer buffer)
    (gptel-request
        prompt
      :system (boost-gptel-directive directive)
      :stream nil
      :callback (boost-gptel-result-callback buffer))
    buffer))

(defun boost-gptel-explain-region (begin end)
  "Explain the active region between BEGIN and END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (user-error "Select a region first")))
  (let ((source
         (boost-gptel-buffer-substring-limited
          begin
          end
          boost-gptel-command-max-input-chars)))
    (boost-gptel-request-in-new-buffer
     "Region explanation"
     (format
      "Explain the following material. Describe its purpose, structure, important assumptions, and likely failure modes.\n\n%s"
      source)
     'programming)))

(defun boost-gptel-summarize-buffer ()
  "Summarize the current buffer in a new Org buffer."
  (interactive)
  (let ((source
         (boost-gptel-buffer-substring-limited
          (point-min)
          (point-max)
          boost-gptel-command-max-input-chars)))
    (boost-gptel-request-in-new-buffer
     (format "Summary of %s" (buffer-name))
     (format "Summarize the following source faithfully.\n\n%s" source)
     'summarize)))

(defvar boost-gptel-default-target-languages
  '("French" "Dutch" "English" "Spanish")
  "List of default target languages proposed to `boost-gptel-translate-region'.")

(defun boost-gptel-translate-region (begin end target-language)
  "Translate the active region between BEGIN and END to TARGET-LANGUAGE."
  (interactive
   (if (use-region-p)
       (list
        (region-beginning)
        (region-end)
        (completing-read "Target language: "
                         boost-gptel-default-target-languages
                         nil        ;; predicate
                         nil        ;; require-match (nil = allow custom input)
                         nil        ;; initial-input
                         nil        ;; history
                         "English")) ;; default
     (user-error "Select a region first")))
  (let ((source
         (boost-gptel-buffer-substring-limited
          begin
          end
          boost-gptel-command-max-input-chars)))
    (boost-gptel-request-in-new-buffer
     (format "Translation to %s" target-language)
     (format
      "Translate the following text to %s. Preserve meaning, formatting, names, numbers, and technical terms. Return only the translation.\n\n%s"
      target-language
      source)
     'writing)))

(define-prefix-command 'boost-gptel-prefix-map)
(global-set-key (kbd "C-c g") #'boost-gptel-prefix-map)

(define-key boost-gptel-prefix-map (kbd "g") #'gptel)  ; Chat.
(define-key boost-gptel-prefix-map (kbd "s") #'gptel-send)  ; Send.
(define-key boost-gptel-prefix-map (kbd "m") #'gptel-menu)  ; Change configuration.
(define-key boost-gptel-prefix-map (kbd "r") #'gptel-rewrite)  ; Rewrite this.
(define-key boost-gptel-prefix-map (kbd "a") #'gptel-add)  ; AI, know about this.
(define-key boost-gptel-prefix-map (kbd "f") #'gptel-add-file)

(define-key boost-gptel-prefix-map (kbd "p") #'boost-gptel-add-project-context)
(define-key boost-gptel-prefix-map (kbd "c") #'boost-gptel-clear-buffer-context)
(define-key boost-gptel-prefix-map (kbd "i") #'boost-gptel-show-context)
(define-key boost-gptel-prefix-map (kbd "e") #'boost-gptel-explain-region)
(define-key boost-gptel-prefix-map (kbd "S") #'boost-gptel-summarize-buffer)
(define-key boost-gptel-prefix-map (kbd "t") #'boost-gptel-translate-region)

(defcustom boost-gptel-enable-mcp-integration nil
  "Whether to load GPTel's optional MCP integration library."
  :type 'boolean
  :group 'boost-gptel)

(when boost-gptel-enable-mcp-integration
  (boost--try-require 'gptel-integrations))

(defun boost-gptel-describe-active-configuration ()
  "Display the active GPTel configuration without revealing API keys."
  (interactive)
  (with-help-window "*boost-gptel-configuration*"
    (princ "Active GPTel configuration\n\n")
    (pp
     (list
      :backend
      (when gptel-backend
        (gptel-backend-name gptel-backend))
      :model gptel-model
      :stream gptel-stream
      :temperature gptel-temperature
      :max-tokens gptel-max-tokens
      :system-prompt
      (cond
       ((stringp gptel-system-prompt) "string")
       ((functionp gptel-system-prompt) "function")
       ((listp gptel-system-prompt) "conversation-template")
       (t nil))
      :use-context gptel-use-context
      :context-count (length gptel-context)
      :use-tools gptel-use-tools
      :tools (mapcar #'gptel-tool-name gptel-tools)
      :confirm-tool-calls gptel-confirm-tool-calls
      :include-tool-results gptel-include-tool-results
      :include-reasoning gptel-include-reasoning
      :track-media gptel-track-media
      :log-level gptel-log-level))))

(defun boost-gptel-toggle-debug-logging ()
  "Toggle GPTel debug logging for the current Emacs session."
  (interactive)
  (setq gptel-log-level
        (if (eq gptel-log-level 'debug) nil 'debug))
  (message "[GPTel logging: %s]" (or gptel-log-level "disabled")))

(define-key boost-gptel-prefix-map (kbd "d")
            #'boost-gptel-describe-active-configuration)
(define-key boost-gptel-prefix-map (kbd "l")
            #'boost-gptel-toggle-debug-logging)

(provide 'boost-gptel)

;;; boost-gptel.el ends here

(boost--try-require 'gptel-commit-msg)
