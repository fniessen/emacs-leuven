;;; gnus-leuven.el --- my Gnus config file

;; Copyright (C) 2004-2019 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven-theme
;; Version: 20200310.1615
;; Keywords: emacs, gnus, dotfile, config

;;; Code:

;; This file is only provided as an example. Customize it to your own taste!

(message "* Load Gnus Leuven")

;;* Loading

(require 'gnus)

;; List of packages that `try-require' can't find.
(setq leuven--missing-packages nil)

  ;; Require a feature/library if available; if not, fail silently.
  (unless (fboundp 'try-require)
    (defun try-require (feature)
      "Attempt to load a FEATURE (or library).
    Return true if the library given as argument is successfully loaded.  If
    not, just print a message."
      (condition-case err
          (progn
            (if (stringp feature)
                (load-library feature)
              (require feature))
            t)                          ; Necessary for correct behavior in
                                        ; conditional expressions.
        (file-error
         (message "Requiring `%s'... missing" feature)
         nil))))

;;* 1 (info "(gnus)Starting Up") Gnus

  (message "1 Starting Gnus...")

  ;; Support for `.authinfo' file.
  (when (try-require 'auth-source)

    ;; Log debug messages.
    (setq auth-source-debug t))

;;** 1.1 (info "(gnus)Finding the News")

  (if (member (downcase (system-name))
              '("mundaneum" "xiphias" "osboxes")) ; Private config.

      ;; Configure incoming mail.
      (setq gnus-select-method
            '(nnimap "work"
                     (nnimap-address "ssl0.ovh.net")
                     (nnimap-server-port "imaps") ; 993
                     (nnimap-stream ssl)

                     ;; Necessary HERE for fancy splitting in Emacs 25.0!
                     (nnimap-inbox "INBOX")
                     (nnimap-split-methods nnmail-split-fancy) ; NOT QUOTED!!!
                                        ; XXX when (try-require 'bbdb-gnus)...
                     ))

    (message "WARN- SMTP Server NOT configured!")
    ;; Sleep 1.5 s so that you can see the warning.
    (sit-for 1.5)
    (setq gnus-select-method '(nnnil "")))

  ;; Using Gnus for news.
  (setq gnus-secondary-select-methods
        '((nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "eternal-september"
                (nntp-address "news.eternal-september.org"))))

;;** 1.4 (info "(gnus)New Groups")

  ;; Gnus will not check for new newsgroups at startup.
  (setq gnus-check-new-newsgroups nil)  ; Save you some time at startup and when
                                        ; you do the `g' command.

  ;; Gnus will ask you to auto-subscribe on *all* new IMAP groups.
  (setq gnus-subscribe-newsgroup-method #'gnus-subscribe-interactively)

  ;; Don't save the list of killed groups.
  (setq gnus-save-killed-list nil)      ; WARNING -- As Gnus has no record of
                                        ; which groups are new and which are
                                        ; old, the automatic new newsgroups
                                        ; subscription methods becomes
                                        ; meaningless.

;;** 1.6 (info "(gnus)Startup Files")

  ;; Don't save a `.newsrc' file (for using other newsreaders) on exit.
  (setq gnus-save-newsrc-file nil)      ; Speed-up.

  ;; Ignore the `.newsrc' file.
  (setq gnus-read-newsrc-file nil)      ; Speed-up.

  ;; My `.newsrc' file (and the derived .el/.eld files).
  (setq gnus-startup-file (concat gnus-directory ".newsrc"))

;;** 1.7 (info "(gnus)Auto Save")

  ;; Enable showing of [Gmail]/* groups.
  (setq gnus-ignored-newsgroups "")

  ;; Unconditionally read the dribble file.
  (setq gnus-always-read-dribble-file t)

;;** 1.8 (info "(gnus)The Active File")

  ;;  Gnus will only know about the groups in my `.newsrc' file
  (setq gnus-read-active-file nil)      ; Speed-up.

  (message "1 Starting Gnus... Done")

;;* 2 (info "(gnus)The Group Buffer")

  (message "2 Group Buffer...")

;;** 2.1 (info "(gnus)Group Buffer Format")

  (defun gnus-user-format-function-y (headers)
    "Return string for count of unread articles."
    (if (> (string-to-number gnus-tmp-number-of-unread) 0)
        (concat gnus-tmp-number-of-unread " Unread")
      ""))

  ;; (defun gnus-user-format-function-U (headers)
  ;;   "Return string for count of unseen articles."
  ;;   (if (> (gnus-number-of-unseen-articles-in-group gnus-tmp-group) 0)
  ;;       ;; Found in gnus-group.el.
  ;;       (concat (int-to-string
  ;;                (gnus-number-of-unseen-articles-in-group
  ;;                 gnus-tmp-group)) " Unseen")
  ;;     ""))

  (defun gnus-user-format-function-T (headers)
    "Return string for count of ticked articles."
    (if (> (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))) 0)
        (concat (int-to-string
                 (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))))
                " Starred")
      ""))

  ;; Create some faces.
  ;; (defface leuven-gnus-unseen '((t (:weight normal :foreground "#FC7202")))
  ;;   "Face for count of unread articles.")
  ;; (defface leuven-gnus-total '((t (:foreground "#2EAE2C")))
  ;;   "Face for size of article in summary buffer.")

  (setq gnus-face-1 'gnus-summary-normal-ticked)
  (setq gnus-face-2 'gnus-summary-normal-unread)
  ;; (setq gnus-face-3 'leuven-gnus-unseen)
  ;; (setq gnus-face-4 'leuven-gnus-total)

  ;; Format of the group buffer.
  (setq gnus-group-line-format (concat "%1{%M%}"
                                       "%2{%m%}"
                                       "  "
                                       "%(%-42,42g%) "
                                       "%2{%10uy%} "
                                       ;; "%3{%10uU%} "
                                       "%1{%10uT%} "
                                       ;; "%4{%6t Total%}"
                                       "\n"))

;;** 2.3 (info "(gnus)Selecting a Group")

  ;; Groups of 200+ articles are NOT considered big.
  (setq gnus-large-newsgroup nil)

;;** 2.15 (info "(gnus)Exiting Gnus")

  ;; Quit Gnus properly, if it is running ...
  (defun leuven--exit-gnus ()
    "Save and exit Gnus."
    (if (and (fboundp 'gnus-group-exit)
             (gnus-alive-p))
        (with-current-buffer (get-buffer "*Group*")
          (let (gnus-interactive-exit)
            (gnus-group-exit)))))

  ;; ... before exiting Emacs (not leaving auto-save files around).
  (add-hook 'kill-emacs-hook 'leuven--exit-gnus)

;;** 2.16 (info "(gnus)Group Topics")

  ;; Permanently enable the topic mode.
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; Remove the binding of `C-c C-x', used by Org clocking commands.
  (add-hook 'gnus-topic-mode-hook
            #'(lambda ()
              (define-key gnus-topic-mode-map
                (kbd "C-c C-x") nil)))

;;** 2.17 (info "(gnus)Misc Group Stuff")

  ;; Turn off the column number in the group buffer, and remove the binding
  ;; of `C-c C-x', used by Org clocking commands.
  (add-hook 'gnus-group-mode-hook
            #'(lambda ()
              (progn
                (set (make-local-variable 'column-number-mode) nil)
                (define-key gnus-group-mode-map
                  (kbd "C-c C-x") nil))))

  ;; Jump to the first group with unread articles, after getting new news.
  (add-hook 'gnus-after-getting-new-news-hook
            'gnus-group-first-unread-group)

  ;; Keep track of when I last read a group.
  (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

  (message "2 Group Buffer... Done")

;;* 3 (info "(gnus)The Summary Buffer")

  (message "3 Summary Buffer...")

;;** 3.1 (info "(gnus)Summary Buffer Format")

  ;; Date format depending on age of article.
  (setq gnus-user-date-format-alist ;; `user-date'
        '(((gnus-seconds-today) . "Today, %H:%M")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
          (604800 . "%A, %H:%M")
          ((gnus-seconds-month) . "%d %a %H:%M")
          ((gnus-seconds-year) . "%m-%d %a %H:%M")
          (t . "%Y-%m-%d %a %H:%M")))

  ;; Create some faces.
  (defface leuven-gnus-date '((t (:foreground "#FF80BF")))
    "Face for date in summary buffer.")
 (defface leuven-gnus-size '((t (:foreground "#8FBF60")))
    "Face for size of article in summary buffer.")

  (setq gnus-face-7 'gnus-summary-high-unread)
  (setq gnus-face-8 'leuven-gnus-date)
  (setq gnus-face-9 'leuven-gnus-size)

  ;; Format specification of the lines in the summary buffer.
  (setq gnus-summary-line-format
        ;; For the record, the default string is
        ;; `%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n'.
        (concat
         "%U"                           ; "read" status
         "%3{%R%}"                      ; "reply" status
         "%7{%z%} "                     ; score
         "%8{%~(pad-left 20)&user-date; %} " ; date
         "%9{ %4k %}"                   ; size
         "%*" "%-15,15f "               ; cursor before name of the poster
         ;; (if (boundp 'rs-gnus-summary-line-content-type-alist)
         ;;     "%u&attachment; ")
         "%u&r;  "
         "%B"
         "%I%s"
         "\n"))

  ;; String indicating that the current article has the same subject as the
  ;; previous.
  (setq gnus-summary-same-subject "")

  ;; Strings indicating that the current article has the same subject as the
  ;; previous.
  (if (char-displayable-p ?\u2514)      ; Box drawings.
      (progn                            ; Tree layout using Unicode characters.
        (setq gnus-sum-thread-tree-root "")
        (setq gnus-sum-thread-tree-false-root "")
        (setq gnus-sum-thread-tree-single-indent "")
        (setq gnus-sum-thread-tree-indent "    ")
        (setq gnus-sum-thread-tree-vertical "│   ")
        (setq gnus-sum-thread-tree-leaf-with-other "├───")
        (setq gnus-sum-thread-tree-single-leaf "└───"))
    (progn                              ; Tree layout using ASCII characters.
      (setq gnus-sum-thread-tree-root "")
      (setq gnus-sum-thread-tree-false-root "")
      (setq gnus-sum-thread-tree-single-indent "")
      (setq gnus-sum-thread-tree-indent "    ")
      (setq gnus-sum-thread-tree-vertical "|   ")
      (setq gnus-sum-thread-tree-leaf-with-other "+---")
      (setq gnus-sum-thread-tree-single-leaf "+---"))) ; "`---"

  (with-eval-after-load "message"
    ;; Regexp matching alternative email addresses.
    (setq message-alternative-emails
          (concat
           (regexp-quote "johndoe@example.com") "\\|"
           (regexp-quote "janedoe@example.com")))

    ;; `From' headers that may be suppressed in favor of `To' headers.
    (setq gnus-ignored-from-addresses
          (concat
           (regexp-quote user-mail-address) "\\|" message-alternative-emails)))

  ;; Extra headers to parse (to check when matching recipients).
  (when (boundp 'nnmail-extra-headers)
    (add-to-list 'nnmail-extra-headers 'Cc))

  (defun leuven-gnus-count-recipients (header)
    "Given a Gnus message header, returns priority mark.
  If I am the only recipient, return \"!\".
  If I am one of the recipients listed in To:, return \"T\".
  If I am one of a few recipients, return \"C\".
  If I am one of many recipients, return \"*\".
  Else, return \" \"."
    (let* ((to (or (cdr (assoc 'To (mail-header-extra header))) ""))
           (cc (or (cdr (assoc 'Cc (mail-header-extra header))) "")))
      (cond
       ((and (string-match gnus-ignored-from-addresses to)
             (fboundp 'bbdb-split))
        (let ((len (length (bbdb-split to ","))))
          (cond
           ((= len 1) "»")
           (t "T"))))
       ((and (string-match gnus-ignored-from-addresses (concat to ", " cc))
             (fboundp 'bbdb-split))
        (if (< (length (bbdb-split (concat to ", " cc) ",")) 5)
            ;; Number of recipients to consider as large.
            "C"
          "*"))
       (t " "))))

  (defalias 'gnus-user-format-function-r 'leuven-gnus-count-recipients)

  ;; Format specification for the summary mode line.
  (setq gnus-summary-mode-line-format "%V: %%b")

;;** 3.5 (info "(gnus)Reply Followup and Post")

  (defun leuven-gnus-summary-followup-with-original ()
    "Force sending messages to `gnu.emacs.bug' per email."
    (interactive)
    (if (string-match (rx "gnu.emacs.bug") gnus-newsgroup-name)
                                        ; Answer per email.
        (call-interactively 'gnus-summary-wide-reply-with-original)
                                        ; Post via news.
      (call-interactively 'gnus-summary-followup-with-original)))

  (with-eval-after-load "gnus-sum"
    (define-key gnus-summary-mode-map
      (kbd "F") 'leuven-gnus-summary-followup-with-original))

  (when (char-displayable-p ?\u2691)
    (setq gnus-ticked-mark ?⚑))

  (when (char-displayable-p ?\u2690)
    (setq gnus-dormant-mark ?⚐))

  (when (char-displayable-p ?\u2709)
    (setq gnus-unread-mark ?✉))

  (when (char-displayable-p ?\u2717)
    (setq gnus-del-mark ?✗))

  (when (char-displayable-p ?\u2713)
    (setq gnus-read-mark ?✓))

  (setq gnus-ancient-mark ? )

  (when (char-displayable-p ?\u2620)
    (setq gnus-killed-mark ?☠))

  (when (char-displayable-p ?\u2197)
    (setq gnus-canceled-mark ?↗))

  (when (char-displayable-p ?\u267B)
    (setq gnus-expirable-mark ?♻))

  (when (char-displayable-p ?\u21BA)
    (setq gnus-replied-mark ?↺))

  (when (char-displayable-p ?\u21AA)
    (setq gnus-forwarded-mark ?↪))

  (when (char-displayable-p ?\u260D)
    (setq gnus-cached-mark ?☍))

  (when (char-displayable-p ?\u2729)
    (setq gnus-unseen-mark ?✩))

  (when (char-displayable-p ?\u2699)
    (setq gnus-process-mark ?⚙))

  (when (char-displayable-p ?\u2605)
    (setq gnus-recent-mark ?★))

  (when (char-displayable-p ?\u2191)
    (setq gnus-score-over-mark ?↑))

  (when (char-displayable-p ?\u2193)
    (setq gnus-score-below-mark ?↓))

;;** 3.9 (info "(gnus)Threading")

  ;; Gather threads by comparing the Subject header of the articles.
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
        ;; 'gnus-gather-threads-by-references)

  ;; Don't grab old headers to build threads.
  (setq gnus-fetch-old-headers nil)     ; nil to speed up (otherwise, it can
                                        ; seriously impact performance).

  ;; ;; Fill in all the gaps to tie loose threads together.
  ;; (setq gnus-build-sparse-threads 'some)

  ;; ;; Sort the articles within a thread after it has been gathered together.
  ;; (setq gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

;;** 3.18 (info "(gnus)Article Treatment")

  ;; Change a `\205' figure to "...".
  (add-hook 'gnus-part-display-hook 'article-treat-dumbquotes)

  ;; What is to be considered a signature.
  (setq gnus-signature-separator
        '("^-- $"                       ; The standard.
          "^________*$"))               ; A long line of underscores is also
                                        ; popular.

  ;; Limit (in lines, in floating point) to what is considered a signature.
  (setq gnus-signature-limit 20.0)

  (defun leuven-prefix-line ()
    "Prefix the current line with `>'."
    (interactive)
    (beginning-of-line)
    (while
        (progn
          ;; Repeat...
          (if (looking-at "[>\n]")
              (insert ">")
            (insert "> "))
          (forward-line 1)
          (beginning-of-line)
          (not
           ;; ...until blank line found.
           (looking-at " *$")))))

  (global-set-key (kbd "<S-f5>") 'leuven-prefix-line)

;;** 3.19 (info "(gnus)MIME Commands")

  ;; Rewrite file names of MIME parts (delete control characters, delete shell
  ;; gotchas, handle evil white spaces).
  (setq mm-file-name-rewrite-functions
        '(mm-file-name-delete-control
          mm-file-name-delete-gotchas
          mm-file-name-trim-whitespace
          mm-file-name-collapse-whitespace
          mm-file-name-replace-whitespace))

  ;; The color of the stripes is obtained by dimming the frame background color.
  (defvar stripe-intensity 12
    "*Intensity of the shade. Used to compute the color of the stripes.
0 means no shading of the background color, nil means gray80")

  ;; A command that computes the rgb code of the shaded background color.
  (defun shade-color (intensity)
    "print the #rgb color of the background, dimmed according to intensity"
    (interactive "nIntensity of the shade : ")
    (apply 'format "#%02x%02x%02x"
           (mapcar #'(lambda (x)
                     (if (> (lsh x -8) intensity)
                         (- (lsh x -8) intensity)
                       0))
                   (color-values
                    (cdr (assoc 'background-color (frame-parameters)))))))

  ;; The command that actually puts the stripes in the current buffer.
  (defun stripe-alternate ()
    "stripes all down the current buffer"
    (interactive)
    ;; Compute the color of the stripes from the value of stripe-intensity.
    (if stripe-intensity
        (setq stripe-overlay-face (shade-color stripe-intensity))
      (setq stripe-overlay-face "gray80"))
    ;; Put the overlay in the current buffer.
    (save-excursion
      (goto-char (point-min))
      (let (stripe-overlay)
        (while (not (eobp))
          (forward-line)
          (setq stripe-overlay
                (make-overlay (line-beginning-position)
                              (line-beginning-position 2)))
          (overlay-put stripe-overlay 'face
                       (list :background stripe-overlay-face))
          (overlay-put stripe-overlay 'priority -1)
          (forward-line)))))

  ;; Activate the stripes for the mail buffers only.
  (add-hook 'gnus-summary-prepare-hook
            #'(lambda ()
              (with-current-buffer gnus-summary-buffer
                ;; (unless (gnus-news-group-p gnus-newsgroup-name)
                (stripe-alternate))))

  (message "3 Summary Buffer... Done")

;;* 4 (info "(gnus)The Article Buffer")

  (message "4 Article Buffer...")

;;* (info "(emacs-mime)Top")

;;** 1 (info "(emacs-mime)Decoding and Viewing")

;;*** 1.2 (info "(emacs-mime)Non-MIME")

  ;; ;; Regexp of Emacs sources groups.
  ;; (setq mm-uu-emacs-sources-regexp "emacs")

  ;; Regexp matching diff groups.
  (setq mm-uu-diff-groups-regexp ".*")

  ;; ;; Regexp matching TeX groups.
  ;; (setq mm-uu-tex-groups-regexp ".*")

;;*** 1.5 (info "(emacs-mime)Display Customization")

  (with-eval-after-load "mm-decode"
    ;; ;; MIME type that will be displayed externally automatically.
    ;; (add-to-list 'mm-automatic-external-display "text/html")

    ;; Do not treat inline images as real attachments (display them, instead).
    (add-to-list 'mm-attachment-override-types "image/.*")

    ;; Don't render HTML automatically *when plain text alternative is
    ;; available*.
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "text/enriched")
    (add-to-list 'mm-discouraged-alternatives "multipart/related")

    ;; All images fit in the buffer.
    (setq mm-inline-large-images t))

;;*** 1.6 (info "(emacs-mime)Files and Directories")

  ;; Default directory for saving attachments.
  (setq mm-default-directory
        (cond ((or (eq system-type 'windows-nt)
                   (eq system-type 'cygwin))
               "~/")
              (t                        ; Linux
               "~/Desktop/")))

  (when (fboundp 'leuven-make-directory-yes-or-no)
    (leuven-make-directory-yes-or-no mm-default-directory))

  ;; Directory for storing temporary files (opened attachments as well).
  (setq mm-tmp-directory temporary-file-directory)

;;** 4 (info "(emacs-mime)Basic Functions")

;;*** 4.12 (info "(emacs-mime)mailcap")

  ;; Choose the right MIME type when sending an attachment.
  (with-eval-after-load "mailcap"
     (add-to-list 'mailcap-mime-extensions
                  '(".doc" . "application/msword"))
     (add-to-list 'mailcap-mime-extensions
                  '(".ppt" . "application/vnd.ms-powerpoint")))
                                        ; MIME content-types keyed by file ext.

;;** 4.4 (info "(gnus)Customizing Articles")

  (when (try-require 'gnus-art)

    ;; Add buttons.
    (setq gnus-treat-buttonize t)

    ;; Add buttons to the head.
    (setq gnus-treat-buttonize-head 'head))

  ;; From Tassilo Horn, 2014-07-17.
  (setq shr-color-visible-distance-min 10)
  (setq shr-color-visible-luminance-min 60)
  (setq gnus-treat-fill-article 0)

;;** 4.6 (info "(gnus)Misc Article")

  ;; Format specification for the article mode line.
  (setq gnus-article-mode-line-format "%S%m")

  ;; Make `C-c C-f' active from within messages.
  (define-key gnus-article-mode-map
    (kbd "C-c C-f") 'gnus-summary-mail-forward)

  (message "4 Article Buffer... Done")

;;* 5 (info "(gnus)Composing Messages")

  (message "5 Composing Messages...")

;;** 5.1 (info "(gnus)Mail")

  ;; Gnus requests confirmation when replying to news (unlike mail).
  (setq gnus-confirm-mail-reply-to-news t)

;;** 5.5 (info "(gnus)Archived Messages")

  ;; Group in which to save the messages you've written.
  (setq gnus-message-archive-group "nnimap:INBOX.Sent") ; Sent Items?

  ;; The Gcc Header specifies a local mail box that receives a copy of the sent
  ;; article.
  ;; "Gcc:"-chooser, from Christoph Conrad.
  (defvar header-gcc-history nil)
  (defun leuven-choose-gcc()
    (interactive)
    (let* (;; if this "group" is chosen the default "Gcc" remains
           (default "INBOX.Sent")
           ;; if this "group" is chosen the default "Gcc" is deleted
           (delete "INBOX.Trash")
           ;; else the choosen group is inserted as "Gcc:"
           (groups (append gnus-newsrc-alist (list (list default 'dummy)
                                                   (list delete  'dummy))))
           (group (completing-read "Gcc: "
                                   groups
                                   nil t default 'header-gcc-history))
           (completion-ignore-case t))
      ;; input?
      (if (not (equal default group))
          (progn
            (message-remove-header "Gcc" t)
            (if (not (equal delete group))
                (message-add-header (concat "Gcc: " group)))))))

  (add-hook 'message-send-hook 'leuven-choose-gcc)

  ;; Automatically mark Gcc articles (i.e., sent by myself) as read.
  (setq gnus-gcc-mark-as-read t)

;;** 5.6 (info "(gnus)Posting Styles")

  ;; An alternative to `gnus-posting-styles', if you want to change accounts
  ;; on the fly while composing messages.
  (autoload 'gnus-alias-determine-identity "gnus-alias" nil t)
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity)

  (with-eval-after-load "gnus-alias"

    ;; ;; Add gnus-alias call to message mode hook.
    ;; (gnus-alias-init)

    ;; Added one key binding.
    (define-key message-mode-map
      (kbd "C-c x") 'gnus-alias-select-identity)

    ;; set up my identities
    (setq gnus-alias-identity-alist
          '(("Work-ID"
             nil                        ; Does not refer to any other identity.
             "John Doe <j.doe@example.com>" ; Sender address.
             "Example Corp."            ; Organization header.
             (("X-Url" . "http://www.example.com/~john")) ; Extra headers.
             nil                        ; No extra body text.
             "John Doe")                ; Signature.

            ("Home-ID"
             ""
             "Jane Doe <john.doe@example.net>"
             "Jane Doe"
             (("X-Url" . "Under construction..."))
             "\nBest regards,\n  John\n"
             "John")))

    ;; Automatically choose an identity given the message context.
    (setq gnus-alias-identity-rules
          '(("Newsgroups-Rule"
             ("newsgroups" ".*" current)
             "Work-ID")

            ("Work-Rule"
             ("any" "j.doe@example.com" both)
             "Work-ID")

            ("Home-Rule"
             ("any" "john.doe@example.net" both)
             "Home-ID")))

    ;; Identity to use when gnus-alias finds an unknown identity.
    (setq gnus-alias-unknown-identity-rule 'error)

    ;; ;; Default identity (when it isn't able to determine which identity to
    ;; ;; use).
    ;; (setq gnus-alias-default-identity "Work-ID")

    ;; Old identity is completely removed before the new one is added.
    (setq gnus-alias-overlay-identities nil)

    ;; Allow your `Return-Path' to be set properly.
    (setq gnus-alias-override-user-mail-address t)

    ;; After an Identity is used, where should point be moved to?
    (setq gnus-alias-point-position 'start-of-sig)

    ;; `From' header becomes a button that you can click on.
    (setq gnus-alias-use-buttonized-from t)

    ;; Level of verbosity -- message output only (see `*gnus-alias debug*'
    ;; buffer, when maximum verbosity).
    (setq gnus-alias-verbosity 1)

    ;; Set message envelope to content of `From'.
    ;; XXX see `mail-specify-envelope-from'
    (defun leuven-set-msg-envelope-from()
      "Set `mail-envelope-from' to the value in the \"From\" field."
      (let* ((from (message-fetch-field "From" t))
             (first (1+ (string-match "<" from)))
             (last (string-match ">" from)))
        (setq mail-envelope-from (substring from first last))))

    ;; ;; Alternative for FPZ??
    ;; (defun leuven-set-msg-envelope-from ()
    ;;   (let ((from (cadr
    ;;                (mail-extract-address-components
    ;;                 (message-field-value "from")))))
    ;;       (setq mail-envelope-from from)))

    (add-hook 'message-setup-hook 'leuven-set-msg-envelope-from)
    )

  ;; Add certain headers before sending messages.
  (defun leuven-message-add-content ()
    ;; For Gmane address obfuscation.
    (message-add-header "X-Archive: encrypt"))

  (add-hook 'message-send-hook 'leuven-message-add-content)

  (message "5 Composing Messages... Done")

;;* (info "(message)Top")

  (message "Message...")

;;** 1 (info "(message)Interface")

;;*** 1.1 (info "(message)New Mail Message")

  ;; Prepare a mail from the current buffer.
  (defun leuven-mail-current-region-or-buffer ()
    "Insert the current region or buffer into an email sending buffer."
    (interactive)
    (save-excursion
      (if (use-region-p)
          (kill-ring-save (region-beginning) (region-end))
        (kill-ring-save (point-min) (point-max)))
      (mail)
      (end-of-buffer)
      (yank)                            ; insert text
      (beginning-of-buffer)
      (next-line)
      (end-of-line)))

;;*** 1.4 (info "(message)Wide Reply")

  ;; Addresses to prune (disable `Cc:' to myself) when doing wide replies.
  (with-eval-after-load "message"
    (when (boundp 'gnus-ignored-from-addresses)
      (setq message-dont-reply-to-names gnus-ignored-from-addresses)))

;;*** 1.8 (info "(message)Forwarding")

  ;; ;; Delimiter inserted before forwarded messages.
  ;; (setq message-forward-start-separator "-----Original Message-----\n")
  ;;
  ;; ;; Delimiter inserted after forwarded messages.
  ;; (setq message-forward-end-separator "\n")

  ;; When forwarding mail, chop off these headers.
  (setq message-forward-ignored-headers
        (concat "^\\("
                ;; FIXME Rewrite the following in a shorter way.
                ;; (mapconcat 'regexp-quote
                ;;            '("Content-Class" "Content-language" "DKIM-Signature"
                ;;              "Delivered-To" "Disposition-Notification-To")
                ;;            "\\|")

                "Approved\\|Archived-At\\|Authentication-Results\\|"
                "Accept-Language\\|acceptlanguage\\|Auto-Submitted\\|"
                "BCc\\|"
                "Cancel-Lock\\|Content-Class\\|Content-language\\|"
                "DKIM-Signature\\|Delivered-To\\|Delivery-date\\|"
                "Disposition-Notification-To\\|DomainKey-Signature\\|"
                "Envelope-to\\|Errors-To\\|"
                "Face\\|"
                "Importance\\|In-Reply-To\\|"
                "Lines\\|List-.*\\|"
                "Message-Id\\|"
                "NNTP-Posting-Date\\|NNTP-Posting-Host\\|"
                "Organization\\|Original-.*\\|"
                "Path\\|Precedence\\|Priority\\|"
                "Received-SPF\\|Received\\|References\\|Reply-To\\|"
                "Return-Path\\|Return-Receipt-To\\|"
                "Sender\\|Sensitivity\\|spamdiagnosticoutput\\|"
                "spamdiagnosticmetadata\\|"
                "Thread-Index\\|Thread-Topic\\|"
                "User-Agent\\|"
                "X.*"
                "\\):"))

  ;; subject of article with `Fwd:' prepended to it, for forwarded messages
  (setq message-make-forward-subject-function
        'message-forward-subject-fwd)

  ;; forwarded messages will just be copied inline to the new message
  (setq message-forward-as-mime nil)

;;** 2 (info "(message)Commands")

;;*** 2.4 (info "(message)Insertion")

  ;; Name of file containing the signature.
  (setq message-signature-file "~/Mail/signatures/johndoe")
                                        ; this file must exist (otherwise, you
                                        ; get misplaced headers when switching
                                        ; between personalities (see
                                        ; `gnus-alias')

;;*** 2.11 (info "(message)Spelling")

 (add-hook 'message-setup-hook          ; or message-mode-hook?
           #'(lambda ()
             (flyspell-mode 1)))

;;** 3 (info "(message)Variables")

;;*** 3.1 (info "(message)Message Headers")

  ;; Specify how `From' headers should look.
  (setq message-from-style 'angles)

;;*** 3.3 (info "(message)Mail Variables")

  ;; Sending mail -- for Gnus (for `message').
  (setq message-send-mail-function 'message-smtpmail-send-it)

  ;; Limit on the size of messages sent (10 MB).
  (setq message-send-mail-partially-limit (* 10 1000 1024))

;;*** 3.4 (info "(message)News Headers")

  ;; Masquerade domain part of Message-ID.
  (setq message-user-fqdn "example.com") ; (downcase (system-name))

;;*** 3.6 (info "(message)Insertion Variables")

  ;; Strip off the signature when citing the original message.
  (setq message-cite-function 'message-cite-original-without-signature)

;;*** 3.7 (info "(message)Various Message Variables")

  ;; ;; Directory from which all other mail file variables are derived.
  ;; (setq message-directory "~/Mail/")

  ;; Remove the binding of `C-c C-v', used by Org-Babel commands.
  (add-hook 'message-mode-hook
            #'(lambda ()
              (define-key message-mode-map
                (kbd "C-c C-v") nil)))

  ;; Operate on messages you compose.
  (defun leuven--message-mode-hook ()
    "Enable Org minor modes and auto-fill."

    ;; Tab completion for alias in `.mailrc'.
    (local-set-key (kbd "<M-tab>") 'mail-abbrev-complete-alias)

    ;; Enable automatic word-wrap when composing messages.
    (setq-default fill-column 80)
    (auto-fill-mode)

    ;; Turn on the Org mode table editor (in emails).
    (turn-on-orgtbl)

    ;; ;; Turn on Org-like lists in non-Org buffers.
    ;; (when (fboundp 'orgalist-mode)
    ;;   (orgalist-mode 1))

    (when (try-require 'org-footnote)
      ;; Default style used for footnoting is local to the Message being
      ;; written.
      (set (make-local-variable 'org-footnote-auto-label) 'plain)

      ;; No tag marking the beginning of footnote section.
      (set (make-local-variable
            'org-footnote-tag-for-non-org-mode-files) nil)))

    (add-hook 'message-mode-hook 'leuven--message-mode-hook 'append)

;;*** 3.9 (info "(message)Message Buffers")

  ;; Kill message buffer after sending a message.
  (setq message-kill-buffer-on-exit t)

  (message "Message... Done")

;;* 6 (info "(gnus)Select Methods")

  (message "6 Select Methods...")

;;** 6.3 (info "(gnus)Using IMAP")

;;** 6.4 (info "(gnus)Getting Mail")

;;*** 6.4.3 (info "(gnus)Splitting Mail") (in IMAP)

  ;; The first match in `nnmail-split-rule' found will be used.
  (setq nnmail-crosspost nil)

;;*** 6.4.6 (info "(gnus)Fancy Mail Splitting")

  ;; Specify how to split mail.
  (setq nnmail-split-fancy
        '(|                             ; Split to the *first* match.

            ;; (: nnmail-split-fancy-with-parent)

            ;; Mailing lists (in To: or Cc:).
            (to "foo@bar\\.com" "list.foo")

            ;; Catch spam.
            ("X-Spam-Status" "[Yy]es"
             "INBOX.Spam")

            ;; ;; Unmatched mail goes to the catch-all group (default mailbox).
            ;; "INBOX"
            ))                          ; Undecided.

;;*** 6.4.11 (info "(gnus)Duplicates")

;;* 9 (info "(gnus)Various")

  (message "9 Various...")

;;** 9.2 (info "(gnus)Interactive")

  ;; Don't display verbose messages and don't require confirmations.
  (setq gnus-novice-user nil)

;;** 9.4 (info "(gnus)Formatting Variables")

  ;; Assume that fixed width fonts have characters of the same width.
  (setq gnus-use-correct-string-widths nil)

;;** 9.5 (info "(gnus)Window Layout")

  ;; Add a window configuration to `gnus-buffer-configuration'.
  (if (> (frame-width) 160)
      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (summary 0.50 point)
                     ;; `point' positions the cursor properly.
                     (article 1.0))))
    (gnus-add-configuration
     '(article
       (vertical 1.0
                 (summary 0.40 point)
                 (article 1.0)))))

;;** 9.19 Interaction with (info "(gnus)Other modes")

  ;; Attach all marked files from Dired to a new Gnus message.
  (autoload 'gnus-dired-mode "gnus-dired"
    "Attach dired's marked files to a gnus message composition." t)

  (autoload 'gnus-dired-attach "gnus-dired"
    "Attach dired's marked files to a gnus message composition." t)

  (with-eval-after-load "dired"

    (add-hook 'dired-mode-hook 'gnus-dired-mode)

    (define-key dired-mode-map
      (kbd "a") 'gnus-dired-attach))    ; XXX conflict with
                                        ; `dired-find-alternate-file'

;;** 9.20 (info "(gnus)Various Various")

  ;; Display more messages from Gnus.
  (setq gnus-verbose 10)                ; 9 = minimum for helpful debugging.

  ;; Display more messages from the Gnus back-ends.
  (setq gnus-verbose-backends 10)

  (message "9 Various... Done")

  ;; Warn that some packages were missing.
  (dolist (pkg leuven--missing-packages)
    (message "WARN- Package `%s' not found" pkg))

  (message "\n")

(provide 'gnus-leuven)

;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8-unix
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode))
;; flycheck-emacs-lisp-initialize-packages: t
;; flycheck-mode: nil
;; ispell-local-dictionary: "american"
;; End:

;;; gnus-leuven.el ends here
