;; -*- lexical-binding: t -*-
;; A module to help with proofreading thanks to gptel
;;

(require 'gptel)
(require 'uuid)

(defvar gptel-proof-gentle-prompt
  (concat
   "Proofread the text.\n\n"

   "Fix spelling, punctuation and grammar only.\n"
   "Do not rewrite for style.\n"
   "Do not change the author's tone.\n"
   "Do not change wording unless required to correct an error.\n\n"

   "Return ONLY the corrected text.\n"
   "Do not explain any change.\n"
   "Do not describe any correction.\n"
   "Do not use markdown.\n"
   "Do not use code fences.\n"
   "Do not add headings.\n"
   "Do not add introductory text.\n"
   "Do not add closing remarks.\n"
   "Do not add comments.\n\n"

   "Preserve the original line breaks whenever possible.\n"
   "Preserve the original whitespace whenever possible.\n"
   "Keep edits to the minimum necessary.\n\n"

   "The output will be compared to the input using Unix diff.\n"
   "Only the corrected text must appear in the response."))

(defvar gptel-proof-aggressive-prompt
  (concat
   "Proofread and improve the text.\n\n"

   "Fix spelling, punctuation and grammar.\n"
   "Rewrite for clarity, brevity and readability.\n"
   "Prefer active voice.\n"
   "Remove unnecessary words.\n"
   "Keep the text natural and conversational.\n"
   "Use language that is easy for a non-technical adult to understand.\n\n"

   "Return ONLY the revised text.\n"
   "Do not explain any change.\n"
   "Do not describe any correction.\n"
   "Do not use markdown.\n"
   "Do not use code fences.\n"
   "Do not add headings.\n"
   "Do not add introductory text.\n"
   "Do not add closing remarks.\n"
   "Do not add comments.\n\n"

   "Preserve the original line breaks whenever possible.\n"
   "Preserve the original whitespace whenever possible.\n"
   "Avoid unnecessary formatting changes.\n\n"

   "The output will be compared to the input using Unix diff.\n"
   "Only the final revised text must appear in the response."))

(defun gptel-proof-apply-fix (buffer marker correction)
  "Apply the changes suggested by GPT."

  (when (stringp correction)
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward marker nil t)
        (let* ((end (point))
               (start (- end (length marker))))
          (delete-region start end)
          (insert correction))))))

(defun gptel-proof (start end &optional aggressive)
  "Proofread either the region using ChatGPT magic."
  (interactive "r\nP")
  (when (not (use-region-p))
    (error "No region selected"))
  (let* ((marker (format "{proof:%s}" (uuid-string)))
         (input (buffer-substring start end))
         (prompt-style (if aggressive "aggressive" "gentle"))
         (start-conflict "<<<<<<< Original\n")
         (sep-conflict "=======\n")
         (end-conflict (format ">>>>>>> Proofread (%s)\n" prompt-style)))
    (save-excursion
      (goto-char start)
      (insert start-conflict)
      (goto-char (+ end (length start-conflict)))
      (insert (concat sep-conflict marker "\n" end-conflict)))
    (gptel-request input
      :callback (lambda (response info)
                  (message "RESPONSE=%S" response)
                  (message "INFO=%S" info)
                  (message "TYPE=%S" (type-of response))
                  (when (stringp response)
                    (gptel-proof-apply-fix
                     (plist-get info :buffer)
                     (plist-get info :context)
                     response)))
      :context marker
      :system (if aggressive
                  gptel-proof-aggressive-prompt
                gptel-proof-gentle-prompt))))

(provide 'gptel-proof)
