;;; gnus-leuven.el --- my Gnus config file

;; Copyright (C) 2004-2015 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven-theme
;; Version: 20150214.0149
;; Keywords: emacs, gnus, dotfile, config

;;; Code:

;; This file is only provided as an example. Customize it to your own taste!

(message "* Load Gnus Leuven")

;;* Loading

(require 'gnus)

;; list of packages that `try-require' can't find
(setq leuven--missing-packages nil)

;;* 1 (info "(gnus)Starting Up") Gnus

  (message "1 Starting Gnus...")

  ;; support for `.authinfo' file
  (when (try-require 'auth-source)

    ;; log debug messages
    (setq auth-source-debug t))

;;** 1.1 (info "(gnus)Finding the News")

  ;; configure incoming mail
  (setq gnus-select-method
        '(nnimap "mail"
                 (nnimap-address "mail")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)
                 ;; (nnimap-split-methods default) ; << ABSOLUTELY NEEDED
                 (nnimap-split-methods nnimap-split-fancy) ; <<< NOT QUOTED!!!
                 ;;                        ; XXX when (try-require 'bbdb-gnus)...
                 ))

  ;; ;; allow "hostname NOT matched" in the server certificate
  ;; (setq starttls-extra-arguments '("--insecure"))
  ;;                                   ;! not used in an Emacs 24 with built-in
  ;;                                   ;! gnutls support

  ;; using Gnus for news
  (setq gnus-secondary-select-methods
        '((nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "eternal-september"
                (nntp-address "news.eternal-september.org"))))

  ;; required when posting to an authenticated news server
  (add-hook 'nntp-server-opened-hook 'nntp-send-authinfo)

;;** 1.4 (info "(gnus)New Groups")

  ;; Gnus will not check for new newsgroups at startup
  (setq gnus-check-new-newsgroups nil)
  ;; save you some time at startup and when you do the `g' command

  ;; don't save the list of killed groups
  ;; WARNING As Gnus has no record of which groups are new and which are
  ;; old, the automatic new newsgroups subscription methods becomes
  ;; meaningless
  (setq gnus-save-killed-list nil)

  ;; make all new groups that come from mail back-ends subscribed
  (setq gnus-auto-subscribed-groups
        "^nnml\\|^nnimap")

;;** 1.6 (info "(gnus)Startup Files")

  ;; don't save a `.newsrc' file (for using other newsreaders) on exit
  (setq gnus-save-newsrc-file nil)  ; speed-up

  ;; ignore the `.newsrc' file
  (setq gnus-read-newsrc-file nil)  ; speed-up

  ;; my `.newsrc' file (and the derived .el/.eld files)
  (setq gnus-startup-file (concat gnus-directory ".newsrc"))

;;** 1.7 (info "(gnus)Auto Save")

  ;; enable showing of [Gmail]/* groups
  (setq gnus-ignored-newsgroups "")

  ;; unconditionally read the dribble file
  (setq gnus-always-read-dribble-file t)

;;** 1.8 (info "(gnus)The Active File")

  ;;  Gnus will only know about the groups in my `.newsrc' file
  (setq gnus-read-active-file nil)  ; speed-up

  (message "1 Starting Gnus... Done")

;;* 2 (info "(gnus)The Group Buffer")

  (message "2 Group Buffer...")

;;** 2.1 (info "(gnus)Group Buffer Format")

  (defun gnus-user-format-function-y (headers)
    "Return string for count of unread articles."
    (if (> (string-to-number gnus-tmp-number-of-unread) 0)
        (concat gnus-tmp-number-of-unread " Unread")
      ""))

  (defun gnus-user-format-function-U (headers)
    "Return string for count of unseen articles."
    (if (> (gnus-number-of-unseen-articles-in-group gnus-tmp-group) 0)
        ;; found in gnus-group.el
        (concat (int-to-string
                 (gnus-number-of-unseen-articles-in-group
                  gnus-tmp-group)) " Unseen")
      ""))

  (defun gnus-user-format-function-T (headers)
    "Return string for count of ticked articles."
    (if (> (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))) 0)
        (concat (int-to-string
                 (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))))
                " Ticked")
      ""))

  ;; create some faces
  (defface leuven-gnus-unseen '((t (:weight bold :foreground "blue")))
    "Face for marker of new mail, and count of unseen articles.")
  (defface leuven-gnus-unread '((t (:weight bold :foreground "black")))
    "Face for count of unread articles.")
  (defface leuven-gnus-diredp-file-suffix-7 '((t (:foreground "#2EAE2C")))
    "Face for size of article in summary buffer.")

  (setq gnus-face-1 'gnus-summary-normal-ticked)
  (setq gnus-face-3 'leuven-gnus-unseen)
  (setq gnus-face-4 'leuven-gnus-unread)
  (setq gnus-face-2 'leuven-gnus-diredp-file-suffix-7)

  ;; format of the group buffer
  (setq gnus-group-line-format (concat "%1{%M%}"
                                       "%3{%m%}"
                                       "  "
                                       "%(%-42,42g%) "
                                       "%4{%10uy%} "
                                       "%3{%10uU%} "
                                       "%1{%10uT%} "
                                       "%2{%6t Total%}"
                                       "\n"))

;;** 2.3 (info "(gnus)Selecting a Group")

  ;; no groups are considered big (> 200 articles)
  (setq gnus-large-newsgroup nil)

;;** 2.15 (info "(gnus)Exiting Gnus")

  ;; quit Gnus properly, if it is running ...
  (defun exit-gnus ()
    "Save and exit Gnus."
    (if (and (fboundp 'gnus-group-exit)
             (gnus-alive-p))
        (with-current-buffer (get-buffer "*Group*")
          (let (gnus-interactive-exit)
            (gnus-group-exit)))))

  ;; ... before exiting Emacs (not leaving auto-save files around)
  (add-hook 'kill-emacs-hook 'exit-gnus)

;;** 2.16 (info "(gnus)Group Topics")

  ;; permanently enable the topic mode
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; remove the binding of `C-c C-x', used by Org clocking commands
  (add-hook 'gnus-topic-mode-hook
            (lambda ()
              (define-key gnus-topic-mode-map
                (kbd "C-c C-x") nil)))

  ;; turn off the column number in the group buffer, and remove the binding
  ;; of `C-c C-x', used by Org clocking commands
  (add-hook 'gnus-group-mode-hook
            (lambda ()
              (progn
                (set (make-local-variable 'column-number-mode) nil)
                (define-key gnus-group-mode-map
                  (kbd "C-c C-x") nil))))

  (message "2 Group Buffer... Done")

;;* 3 (info "(gnus)The Summary Buffer")

  (message "3 Summary Buffer...")

;;** 3.1 (info "(gnus)Summary Buffer Format")

  ;; date format depending on age of article
  (setq gnus-user-date-format-alist ;; `user-date'
        '(((gnus-seconds-today) . "               %H:%M")
          ;; (604800 . "           %a %H:%M")
          ((gnus-seconds-month) . "        %d %a %H:%M")
          ((gnus-seconds-year) . "     %m-%d %a %H:%M")
          (t . "%Y-%m-%d %a %H:%M")))

  ;; create some faces
  (defface leuven-gnus-linum '((t (:foreground "#AFB7BA")))
    "Face for markers in summary buffer.")
  (defface leuven-gnus-score '((t (:weight bold :foreground "#2B7E2A" :background "#CCFFCC")))
    "Face for scoring marker in summary buffer.")
  (defface leuven-gnus-diredp-file-suffix '((t (:foreground "#2EAE2C")))
    "Face for size of article in summary buffer.")
  (defface leuven-gnus-org-date '((t (:foreground "purple")))
    "Face for date in summary buffer.")

  (setq gnus-face-5 'leuven-gnus-linum)
  (setq gnus-face-6 'leuven-gnus-score)
  (setq gnus-face-7 'leuven-gnus-diredp-file-suffix)
  (setq gnus-face-8 'leuven-gnus-org-date)

  ;; format specification of the lines in the summary buffer
  (setq gnus-summary-line-format
        ;; for the record, the default string is
        ;; `%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n'
        (concat
         "%5{%U%}"                      ; status
         "%8{%R%}"                      ; reply status
         "%6{%z%} "                     ; score
         "%8{%20&user-date; %} "        ; date
         "%7{ %4k %}"                   ; size
         "%*" "%-15,15f "               ; cursor before name of the poster
         ;; (if (boundp 'rs-gnus-summary-line-content-type-alist)
         ;;     "%u&attachment; ")
         "%u&r; "
         "%5{ %}"
         "%B"
         "%I%s"
         "\n"))

  ;; string indicating that the current article has the same subject as the
  ;; previous
  (setq gnus-summary-same-subject "")

  ;; strings indicating that the current article has the same subject as the
  ;; previous
  (if (char-displayable-p ?\u2514)      ; box drawings
      (progn                            ; tree layout using Unicode characters
        (setq gnus-sum-thread-tree-root "")
        (setq gnus-sum-thread-tree-false-root "")
        (setq gnus-sum-thread-tree-single-indent "")
        (setq gnus-sum-thread-tree-indent "    ")
        (setq gnus-sum-thread-tree-vertical "│   ")
        (setq gnus-sum-thread-tree-leaf-with-other "├───")
        (setq gnus-sum-thread-tree-single-leaf "└───"))
    (progn                              ; tree layout using ASCII characters
      (setq gnus-sum-thread-tree-root "")
      (setq gnus-sum-thread-tree-false-root "")
      (setq gnus-sum-thread-tree-single-indent "")
      (setq gnus-sum-thread-tree-indent "    ")
      (setq gnus-sum-thread-tree-vertical "|   ")
      (setq gnus-sum-thread-tree-leaf-with-other "+---")
      (setq gnus-sum-thread-tree-single-leaf "+---"))) ; "`---"

  (with-eval-after-load "message"
    ;; regexp matching alternative email addresses
    (setq message-alternative-emails
          (concat
           (regexp-quote "johndoe@example.com") "\\|"
           (regexp-quote "janedoe@example.com")))

    ;; `From' headers that may be suppressed in favor of `To' headers
    (setq gnus-ignored-from-addresses
          (concat
           (regexp-quote user-mail-address) "\\|" message-alternative-emails)))

  ;; extra headers to parse (to check when matching recipients)
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
            ;; number of recipients to consider as large
            "C"
          "*"))
       (t " "))))

  (defalias 'gnus-user-format-function-r 'leuven-gnus-count-recipients)

  ;; format specification for the summary mode line
  (setq gnus-summary-mode-line-format "%V: %%b")

;;** 3.5 (info "(gnus)Reply Followup and Post")

  (defun leuven-gnus-summary-followup-with-original ()
    "Force sending messages to `gnu.emacs.bug' per email."
    (interactive)
    (if (string-match (rx "gnu.emacs.bug") gnus-newsgroup-name)
                                        ; answer per email
        (call-interactively 'gnus-summary-wide-reply-with-original)
                                        ; post via news
      (call-interactively 'gnus-summary-followup-with-original)))

  (with-eval-after-load "gnus-sum"
    (define-key gnus-summary-mode-map
      (kbd "F") 'leuven-gnus-summary-followup-with-original))

(setq gnus-ticked-mark ?⚑)
(setq gnus-dormant-mark ?⚐)
(setq gnus-unread-mark ?✉)

(setq gnus-del-mark ?✗)
(setq gnus-read-mark ?✓)
(setq gnus-killed-mark ?☠)
(setq gnus-canceled-mark ?↗)

(setq gnus-score-over-mark ?↑)          ; ↑ ☀
(setq gnus-score-below-mark ?↓)         ; ↓ ☂

(setq gnus-replied-mark ?↺)
(setq gnus-forwarded-mark ?↪)
(setq gnus-cached-mark ?☍)
(setq gnus-unseen-mark ?✩)
(setq gnus-recent-mark ?★)
(setq gnus-process-mark ?⚙)
(setq gnus-expirable-mark ?♻)

;;** 3.9 (info "(gnus)Threading")

  ;; gather threads by comparing the Subject header of the articles
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
        ;; 'gnus-gather-threads-by-references)

  ;; don't grab old headers to build threads
  (setq gnus-fetch-old-headers nil)     ; nil to speed up (otherwise, it can
                                        ; seriously impact performance)

  ;; fill in all the gaps to tie loose threads together
  (setq gnus-build-sparse-threads nil)  ; was 'some

  ;; ;; sort the articles within a thread after it has been gathered together
  ;; (setq gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

  (message "3 Summary Buffer... Done")

;;* 4 (info "(gnus)The Article Buffer")

  (message "4 Article Buffer...")

;;* (info "(emacs-mime)Top")

;;** 1 (info "(emacs-mime)Decoding and Viewing")

;;*** 1.2 (info "(emacs-mime)Non-MIME")

  ;; regexp of Emacs sources groups
  (setq mm-uu-emacs-sources-regexp "emacs")

  ;; regexp matching diff groups
  (setq mm-uu-diff-groups-regexp ".*")

  ;; regexp matching TeX groups
  (setq mm-uu-tex-groups-regexp ".*")

;;*** 1.5 (info "(emacs-mime)Display Customization")

  (with-eval-after-load "mm-decode"
    ;; ;; MIME type that will be displayed externally automatically
    ;; (add-to-list 'mm-automatic-external-display "text/html")

    ;; do not treat inline images as real attachments (display them, instead)
    (add-to-list 'mm-attachment-override-types "image/.*")
                                        ; "text/x-vcard"...

    ;; don't render HTML automatically *when plain text alternative is
    ;; available*
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "text/enriched")
    (add-to-list 'mm-discouraged-alternatives "multipart/related")

    ;; all images fit in the buffer
    (setq mm-inline-large-images t)

    ;; ;; always show HTML mails as attachments (even if they can be
    ;; ;; displayed) use `browse-url-browser-function' (firefox) to render
    ;; ;; HTML mails
    ;; (push "text/html" mm-inline-override-types)


    ;; use `w3m' browser (if installed) to render HTML-only mails
    (setq mm-text-html-renderer
          (cond ((executable-find "w3m") 'w3m) ; or `gnus-w3m'?
                (t 'html2text)))        ; Emacs built-in

    ;; recent Gnusae have a built-in HTML renderer which even (somewhat)
    ;; handles CSS
    (setq mm-text-html-renderer 'shr) ; eww-backend?
    )

  (with-eval-after-load "w3m"
     ;; (setq browse-url-browser-function 'w3m-browse-url)
     (setq mm-inline-text-html-renderer
           'mm-inline-text-html-render-with-w3m)
     (setq gnus-article-wash-function
           'gnus-article-wash-html-with-w3m)

     ;; Whenever you try to browse URLs found in articles, Gnus
     ;; (`emacs-w3m', in fact) complains that "This link is considered
     ;; unsafe...". To turn it off, frob the variables
     ;; `w3m-safe-url-regexp' and `mm-w3m-safe-url-regexp'.
     )

  ;; allow retrieving images in HTML contents with the <img> tags
  (setq mm-inline-text-html-with-images t)

;;*** 1.6 (info "(emacs-mime)Files and Directories")

  ;; default directory for saving attachments
  (setq mm-default-directory
        (cond ((or leuven--win32-p
                   leuven--cygwin-p)
               "~/")
              (t                        ; Linux
               "~/Desktop/")))

  (when (fboundp 'leuven-make-directory-yes-or-no)
    (leuven-make-directory-yes-or-no mm-default-directory))

  ;; directory for storing temporary files (opened attachments as well)
  (setq mm-tmp-directory temporary-file-directory)

;;** 4 (info "(emacs-mime)Basic Functions")

;;*** 4.12 (info "(emacs-mime)mailcap")

  ;; choose the right MIME type when sending an attachment
  (with-eval-after-load "mailcap"
     (add-to-list 'mailcap-mime-extensions
                  '(".doc" . "application/msword"))
     (add-to-list 'mailcap-mime-extensions
                  '(".ppt" . "application/vnd.ms-powerpoint")))
                                        ; MIME content-types keyed by file
                                        ; extensions

  ;; from Tassilo Horn, 2014-07-17
  (setq shr-color-visible-distance-min 10)
  (setq shr-color-visible-luminance-min 60)
  (setq gnus-treat-fill-article 0)

;;** 4.6 (info "(gnus)Misc Article")

  ;; format specification for the article mode line
  (setq gnus-article-mode-line-format "%S%m")

  ;; make `C-c C-f' active from within messages
  (define-key gnus-article-mode-map
    (kbd "C-c C-f") 'gnus-summary-mail-forward)

  (message "4 Article Buffer... Done")

;;* 5 (info "(gnus)Composing Messages")

  (message "5 Composing Messages...")

;;** 5.1 (info "(gnus)Mail")

  ;; Gnus requests confirmation when replying to news (unlike mail)
  (setq gnus-confirm-mail-reply-to-news t)

;;** 5.2 (info "(gnus)Posting Server")

  ;; control the hostname sent in the first EHLO or HELO command sent to the
  ;; server (client sends `EHLO CLARK.smtpmail-local-domain')
  (setq smtpmail-local-domain
        "i-did-not-set--mail-host-address--so-tickle-me")
        ;;              ^^^^^^^^^^^^^^^^^

  ;; make the SMTP library add `@' and the specified domain name to
  ;; recipients specified in the message when they are sent using the RCPT
  ;; TO command (or when we simply enter an email address without its
  ;; domain extension)
  (setq smtpmail-sendto-domain "local")

  ;; print info in buffer *trace of SMTP session to <somewhere>*
  (setq smtpmail-debug-info t)          ; only to debug problems

  ;; send the SMTP VERB command (enabling verbose information from the SMTP
  ;; server)
  (setq smtpmail-debug-verb t)

  ;; group in which to save the messages you've written
  (setq gnus-message-archive-group "nnimap+mail:INBOX.Sent")

;;** 5.5 (info "(gnus)Archived Messages")

  ;; the Gcc Header specifies a local mail box that receives a copy of the
  ;; sent article
  ;; "Gcc:"-chooser, from Christoph Conrad
  (defvar header-gcc-history nil)
  (defun leuven-choose-gcc()
    (interactive)
    (let* (;; if this "group" is chosen the default "Gcc" remains
           (default "nnimap+mail:INBOX")
           ;; if this "group" is chosen the default "Gcc" is deleted
           (delete "nnimap+mail:INBOX.Trash")
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

  ;; automatically mark Gcc articles (i.e., sent by myself) as read
  (setq gnus-gcc-mark-as-read t)

;;** 5.6 (info "(gnus)Posting Styles")

  ;; an alternative to `gnus-posting-styles', if you want to change accounts
  ;; on the fly while composing messages
  (autoload 'gnus-alias-determine-identity "gnus-alias" nil t)
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity)

  (with-eval-after-load "gnus-alias"

    ;; ;; add gnus-alias call to message mode hook
    ;; (gnus-alias-init)

    ;; added one key binding
    (define-key message-mode-map
      (kbd "C-c p") 'gnus-alias-select-identity)

    ;; set up my identities
    (setq gnus-alias-identity-alist
          '(("John-Doe-ID"
             ""
             "John Doe <johndoe@example.com>"
             "John Doe"
             (("X-Url" . "http://www.example.com/~john"))
             "\nJohn\n"
             "John Doe")

            ("Jane-Doe-ID"
             ""
             "Jane Doe <janedoe@example.com>"
             "Jane Doe"
             (("X-Url" . "Under construction..."))
             "\nBest regards,\n  Jane\n"
             "Jane Doe")))

    ;; automatically choose an identity given the message context
    (setq gnus-alias-identity-rules
          '(("Newsgroups-Rule"
             ("newsgroups" ".*" current)
             "John-Doe-ID")

            ("John-Doe-Rule"
             ("any" "johndoe@example.com" both)
             "John-Doe-ID")

            ("Jane-Doe-Rule"
             ("any" "janedoe@example.com" both)
             "Jane-Doe-ID")))

    ;; identity to use when gnus-alias finds an unknown identity
    (setq gnus-alias-unknown-identity-rule 'error)

    ;; ;; default identity (when it isn't able to determine which identity to
    ;; ;; use)
    ;; (setq gnus-alias-default-identity "John-Doe-ID")

    ;; old identity is completely removed before the new one is added
    (setq gnus-alias-overlay-identities nil)

    ;; allow your `Return-Path' to be set properly
    (setq gnus-alias-override-user-mail-address t)

    ;; after an Identity is used, where should point be moved to?
    (setq gnus-alias-point-position 'start-of-sig)

    ;; `From' header becomes a button that you can click on
    (setq gnus-alias-use-buttonized-from t)

    ;; level of verbosity -- message output only (see `*gnus-alias debug*'
    ;; buffer, when maximum verbosity)
    (setq gnus-alias-verbosity 1)

    ;; set message envelope to content of `From'
    ;; XXX see `mail-specify-envelope-from'
    (defun leuven-set-msg-envelope-from()
      "Set `mail-envelope-from' to the value in the \"From\" field."
      (let* ((from (message-fetch-field "From" t))
             (first (1+ (string-match "<" from)))
             (last (string-match ">" from)))
        (setq mail-envelope-from (substring from first last))))

    ;; ;; alternative for FPZ??
    ;; (defun leuven-set-msg-envelope-from ()
    ;;   (let ((from (cadr
    ;;                (mail-extract-address-components
    ;;                 (message-field-value "from")))))
    ;;       (setq mail-envelope-from from)))

    (add-hook 'message-setup-hook 'leuven-set-msg-envelope-from)
    )

  ;; add certain headers before sending messages
  (defun leuven-message-add-content ()
    ;; for Gmane address obfuscation
    (message-add-header "X-Archive: encrypt"))

  (add-hook 'message-send-hook 'leuven-message-add-content)

  (message "5 Composing Messages... Done")

;;* (info "(message)Top")

  ;; Message mode has become the default mail editing mode in Emacs 23.2+
  ;; (used by Gnus and RMail, at least)

  (message "Message...")

;;** 1 (info "(message)Interface")

;;*** 1.1 (info "(message)New Mail Message")

  ;; prepare a mail from the current buffer
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

  ;; addresses to prune (disable `Cc:' to myself) when doing wide replies
  (with-eval-after-load "message"
    (when (boundp 'gnus-ignored-from-addresses)
      (setq message-dont-reply-to-names gnus-ignored-from-addresses)))

;;*** 1.8 (info "(message)Forwarding")

  ;; ;; delimiter inserted before forwarded messages
  ;; (setq message-forward-start-separator "-----Original Message-----\n")
  ;;
  ;; ;; delimiter inserted after forwarded messages
  ;; (setq message-forward-end-separator "\n")

  ;; when forwarding mail, chop off these headers
  (setq message-forward-ignored-headers
        (concat "^\\("
                ;; FIXME Rewrite the following in a shorter way
                ;; (mapconcat 'regexp-quote
                ;;            '("Content-Class" "Content-language" "DKIM-Signature"
                ;;              "Delivered-To" "Disposition-Notification-To")
                ;;            "\\|")

                "Approved\\|Archived-At\\|Authentication-Results\\|"
                "Accept-Language\\|acceptlanguage\\|Auto-Submitted\\|"
                "BCc\\|"
                "Cancel-Lock\\|Content-Class\\|Content-language\\|"
                "DKIM-Signature\\|Delivered-To\\|"
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
                "Sender\\|Sensitivity\\|"
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

  ;; name of file containing the signature
  (setq message-signature-file "~/Mail/signatures/johndoe")
                                        ; this file must exist (otherwise, you
                                        ; get misplaced headers when switching
                                        ; between personalities (see
                                        ; `gnus-alias')

;;*** 2.11 (info "(message)Spelling")

 (add-hook 'message-setup-hook          ; or message-mode-hook?
           (lambda ()
             (flyspell-mode 1)))

;;** 3 (info "(message)Variables")

;;*** 3.1 (info "(message)Message Headers")

  ;; specify how `From' headers should look
  (setq message-from-style 'angles)

;;*** 3.3 (info "(message)Mail Variables")

  ;; sending mail -- for Gnus (for `message')
  (setq message-send-mail-function 'message-smtpmail-send-it)

  ;; limit on the size of messages sent (10 MB)
  (setq message-send-mail-partially-limit (* 10 1000 1024))

;;*** 3.4 (info "(message)News Headers")

  ;; masquerade domain part of Message-ID
  (setq message-user-fqdn "example.com") ; (system-name)

  ;; tell Gnus not to generate a sender header on outgoing posts
  ;; (default behavior since Gnus 5.10.0)
  (with-eval-after-load "message"
    (add-to-list 'message-syntax-checks '(sender . disabled)))

;;*** 3.6 (info "(message)Insertion Variables")

  ;; function for citing an original message
  ;; (stripping off the signature from the original message)
  (setq message-cite-function 'message-cite-original-without-signature)

  ;; (setq message-cite-function 'trivial-cite
  ;;       tc-normal-citemarks ">|:"
  ;;       tc-fill-long-lines nil
  ;;       tc-make-attribution 'tc-fancy-attribution)

  (defun message-insert-citation-line ()
    "A replacement of the original `message-insert-citation-line'."
    (let ((from (replace-regexp-in-string
                 "[()]\\| ?[^ ]*?@[^ ]* ?" ""
                 (mail-header-from message-reply-headers))))
      (insert from " wrote:\n")))

;;*** 3.7 (info "(message)Various Message Variables")

  ;; ;; directory from which all other mail file variables are derived
  ;; (setq message-directory "~/Mail/")

  ;; remove the binding of `C-c C-v', used by Org-Babel commands
  (add-hook 'message-mode-hook
            (lambda ()
              (define-key message-mode-map
                (kbd "C-c C-v") nil)))

  ;; operates on messages you compose
  (defun leuven--message-mode-hook ()
    "Enable Org minor modes and auto-fill."

    ;; ;; prompt for and insert a mail alias
    ;; (local-set-key (kbd "M-a") 'mail-abbrev-insert-alias)

    ;; tab completion for alias in `.mailrc'
    (local-set-key (kbd "<M-tab>") 'mail-abbrev-complete-alias)

    ;; enable automatic word-wrap when composing messages
    (setq-default fill-column 80)
    (auto-fill-mode)

    ;; turn on the Org mode table editor (in emails)
    (turn-on-orgtbl)

    ;; turn on (the enhanced version of) orgstruct-mode
    (turn-on-orgstruct++)

    ;; ;; make `orgstruct-hijacker-command-22' rebind `M-q' to a message
    ;; ;; specific function to fill a paragraph
    ;; (setq fill-paragraph-function 'org-fill-paragraph)

    (when (try-require 'org-footnote)
      ;; default style used for footnoting is local to the Message being
      ;; written
      (set (make-local-variable 'org-footnote-auto-label) 'plain)

      ;; no tag marking the beginning of footnote section
      (set (make-local-variable
      'org-footnote-tag-for-non-org-mode-files) nil))

    ;; (when (try-require 'auto-complete)
    ;;   (auto-complete-mode))
    )

  (add-hook 'message-mode-hook 'leuven--message-mode-hook 'append)

  (add-hook 'message-send-hook 'org-footnote-normalize)

;;*** 3.9 (info "(message)Message Buffers")

  ;; kill message buffer after sending a message
  (setq message-kill-buffer-on-exit t)

  (message "Message... Done")

;;* 6 (info "(gnus)Select Methods")

  (message "6 Select Methods...")

;;** 6.2 (info "(gnus)Getting News")

;;*** 6.2.1 (info "(gnus)NNTP")

  ;; number of seconds to wait before an nntp connection times out
  (setq nntp-connection-timeout 5)      ; 30

  ;; the first match in `nnmail-split-rule' found will be used
  (setq nnmail-crosspost nil)

  ;; name(s) of IMAP mailboxes to split mail from
  (setq nnimap-inbox '("INBOX"))        ; Gnus v5.13
  ;; (setq nnimap-split-inbox '("INBOX")) ; Ma Gnus

  ;; BBDB (Big Brother DataBase) is loaded from my `.emacs' file
  (when (try-require 'bbdb-gnus)

    ;; split function to use (sorting mails into groups using BBDB)
    (setq nnimap-split-methods 'nnimap-split-fancy)

    ;; for records which don't have `gnus-private' set, the rules in
    ;; split-fancy are invoked
    (setq bbdb/gnus-split-default-group nil)

    ;; specify how to split mail
    (setq nnimap-split-fancy            ; XXX vs `nnmail-split-fancy'?
          `(|                           ; split to the *first* match

              ;; mailing lists (in To: or Cc:)
              (to "foo@bar\\.com" "list.foo")

              ;; invoke BBDB            ; XXX with BBDB v2 and v3?
              (: (lambda ()
                   (car (bbdb/gnus-split-method))))

              ;; catch spam
              ("X-Spam-Status" "[Yy]es"
               "INBOX.Spam")

              ;; unmatched mail goes to the catch-all group (default mailbox)
              "INBOX")))

;;*** 6.3.11 (info "(gnus)Duplicates")

  ;; cache of old Message-IDs for every message Gnus sees
  (setq nnmail-message-id-cache-file
        (concat gnus-directory ".nnmail-cache"))

  ;; to use `nnmail-split-fancy-with-parent'
  ;; record the Message-ID of every message it sees
  (setq nnmail-treat-duplicates 'warn)  ; or `delete'

  ;; ensure that the Message-ID are still in the cache
  (setq nnmail-message-id-cache-length 5000)

  ;; record the Message-ID of moved articles
  (setq nnmail-cache-accepted-message-ids t)

;;* 9 (info "(gnus)Various")

  (message "9 Various...")

;;** 9.2 (info "(gnus)Interactive")

  ;; don't display verbose messages and don't require confirmations
  (setq gnus-novice-user nil)

;;** 9.4 (info "(gnus)Formatting Variables")

  ;; assume that fixed width fonts have characters of the same width
  (setq gnus-use-correct-string-widths nil)

;;** 9.5 (info "(gnus)Window Layout")

  ;; add a window configuration to `gnus-buffer-configuration'
  (if (> (frame-width) 160)
      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (summary 0.50 point)
                     ;; `point' positions the cursor properly
                     (article 1.0))))
    (gnus-add-configuration
     '(article
       (vertical 1.0
                 (summary 0.40 point)
                 (article 1.0)))))

;;** 9.20 Interaction with (info "(gnus)Other modes")

  ;; attach all marked files from Dired to a new Gnus message
  (autoload 'gnus-dired-mode "gnus-dired"
    "Attach dired's marked files to a gnus message composition." t)

  (autoload 'gnus-dired-attach "gnus-dired"
    "Attach dired's marked files to a gnus message composition." t)

  (with-eval-after-load "dired"

    (add-hook 'dired-mode-hook 'gnus-dired-mode)

    (define-key dired-mode-map
      (kbd "a") 'gnus-dired-attach))    ; XXX conflict with
                                        ; `dired-find-alternate-file'

;;** 9.21 (info "(gnus)Various Various")

  ;; display more messages from Gnus
  (setq gnus-verbose 10)                ; 9 = minimum for helpful debugging

  ;; display more messages from the Gnus back-ends
  (setq gnus-verbose-backends 10)

  (message "9 Various... Done")

;;* 11 (info "(gnus)Appendices")

  (message "11 Appendices...")

;;** 11.2 (info "(gnus)History")

  (setq canlock-password "secret")
  (setq canlock-password-for-verify "secret")

;;** 11.9 (info "(gnus)Frequently Asked Questions")

  ;; byte-compile the user-defined format specs (things like
  ;; `gnus-summary-line-format')
  (when (fboundp 'gnus-compile)
    (gnus-compile)) ;;  at the bottom

  (message "11 Appendices... Done")

  ;; warn that some packages were missing
  (dolist (pkg leuven--missing-packages)
    (message "(warning) Package `%s' not found" pkg))

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
