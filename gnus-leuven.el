;;; gnus-leuven.el --- my Gnus config file

;; Copyright (C) 2004-2014 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; Keywords: gnus, dotfile, config

;;
;;    __ _ _ __  _   _ ___
;;   / _` | '_ \| | | / __|
;;  | (_| | | | | |_| \__ \
;; (_)__, |_| |_|\__,_|___/
;;   |___/

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;** Help
;;
;; For help on The Gnus Newsreader, see:
;;     (info "(gnus)")  <== C-x C-e here!
;;
;;** BUG
;;
;; Updating info for nnimap does not occurr for group INBOX (well for
;; subgroups) - i.e., mail GCc'ed to INBOX is lost...

;; http://www.emacswiki.org/emacs/GnusTutorial

;; Disconnected from IMAP? In the `*Server*' buffer, hit 'C' to close the
;; server and then 'O' to reopen it.

;; "You should byte-compile Gnus" because the function `gnus' isn't
;; byte-compiled. Check `M-x locate-library RET gnus RET' if it picks up the
;; .el or .elc file.

;;; Code:

;; This file is only provided as an example. Customize it to your own taste!

(message "* Load Gnus Leuven")

(defvar leuven-section-starting-gnus t) ; required
(defvar leuven-section-group-buffer nil)
(defvar leuven-section-summary-buffer nil)
(defvar leuven-section-article-buffer nil)
(defvar leuven-section-composing-messages t)
                                        ; required
(defvar leuven-section-message t)       ; required
(defvar leuven-section-select-methods nil)
(defvar leuven-section-scoring nil)
(defvar leuven-section-various nil)
(defvar leuven-section-appendices nil)

(setq leuven-section-group-buffer t)
(setq leuven-section-summary-buffer t)
(setq leuven-section-article-buffer t)
(setq leuven-section-select-methods t)
(setq leuven-section-scoring t)
(setq leuven-section-various t)
(setq leuven-section-appendices t)

;;* Loading

;; list of packages that `try-require' can't find
(setq leuven--missing-packages nil)

;;* 1 (info "(gnus)Starting Up") Gnus

(when leuven-section-starting-gnus
      (message "1 Starting Gnus...")

      ;; support for `.authinfo' file
      (when (try-require 'auth-source)

        ;; for any host and any protocol, use just one .netrc-like file that
        ;; holds authinfo passwords
        (setq auth-sources
              (if (file-exists-p "~/.authinfo.gpg")
                  '("~/.authinfo.gpg")
                '("~/.authinfo")))

        ;; log debug messages
        (setq auth-source-debug t))

;;** 1.1 (info "(gnus)Finding the News")

      ;; using Gnus for news (see `gnus-secondary-select-methods' as well)
      (setq gnus-select-method
            '(nntp "eternal-september.org"
                   (nntp-address "news.eternal-september.org")
                   (nnir-search-engine nntp))

            ;; ;; empty, read-only back-end (all real servers are secondary or
            ;; ;; foreign)
            ;; '(nnnil)
            )

      ;; required when posting to an authenticated news server
      (add-hook 'nntp-server-opened-hook 'nntp-send-authinfo)

      ;; configure incoming mail
      (setq gnus-secondary-select-methods
            '((nnimap "mail"
                      ;; account label allows for multiple accounts on the
                      ;; same server (see `.authinfo' for logins)

                      (nnimap-address "mail")
                      ;; (nnimap-server-port 993)
                      ;; (nnimap-stream ssl)

                      (nnimap-split-methods 'nnimap-split-fancy)
                                        ; XXX when (try-require 'bbdb-gnus)...

                      )

              (nntp "gmane"
                    (nntp-address "news.gmane.org")
                    (nntp-port-number 119))))

      ;; allow "hostname NOT matched" in the server certificate
      (setq starttls-extra-arguments '("--insecure"))
      ;;! not used in an Emacs 24 with built-in gnutls support

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
            "^nnml\\|^nnimap\\|^nnslashdot")

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

      (message "1 Starting Gnus... Done"))

;;* 2 (info "(gnus)The Group Buffer")

(when leuven-section-group-buffer
      (message "2 Group Buffer...")

;;** 2.1 (info "(gnus)Group Buffer Format")

      ;; create some faces
      (defface leuven-gnus-ticked '((t (:weight bold :foreground "#CF5D60")))
        "Face for count of ticked articles.")
      (defface leuven-gnus-unseen '((t (:weight bold :foreground "blue")))
        "Face for marker of new mail, and count of unseen articles.")
      (defface leuven-gnus-unread '((t (:weight bold :foreground "black")))
        "Face for count of unread articles.")
      (defface leuven-gnus-linum '((t (:foreground "#AFB7BA")))
        "Face for markers in summary buffer.")
      (defface leuven-gnus-score '((t (:weight bold :foreground "#2B7E2A" :background "#CCFFCC")))
        "Face for scoring marker in summary buffer.")
      (defface leuven-gnus-diredp-file-suffix '((t (:foreground "#2EAE2C")))
        "Face for size of article in summary buffer.")
      (defface leuven-gnus-org-date '((t (:foreground "purple")))
        "Face for date in summary buffer.")

      (setq gnus-face-1 'leuven-gnus-ticked)
      (setq gnus-face-3 'leuven-gnus-unseen)
      (setq gnus-face-4 'leuven-gnus-unread)
      (setq gnus-face-5 'leuven-gnus-linum)
      (setq gnus-face-6 'leuven-gnus-score)
      (setq gnus-face-7 'leuven-gnus-diredp-file-suffix)
      (setq gnus-face-8 'leuven-gnus-org-date)

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

      ;; format of the group buffer
      (setq gnus-group-line-format (concat "%1{%M%}"
                                           "%3{%m%}"
                                           "  "
                                           "%(%-42,42g%) "
                                           "%4{%10uy%} "
                                           "%3{%10uU%} "
                                           "%1{%10uT%} "
                                           "%7{%6t Total%}"
                                           "\n"))

;;** 2.3 (info "(gnus)Selecting a Group")

      ;; no groups are considered big
      (setq gnus-large-newsgroup nil)

;;** 2.11 (info "(gnus)Listing Groups")

      ;; groups that should always be listed in the group buffer (even when
      ;; there are no unread articles in the groups)
      (setq gnus-permanently-visible-groups nil) ; = none

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

;;** 2.17 (info "(gnus)Misc Group Stuff")

      (defface highlight-line-gnus
        '((t (:background "#DAEAFC")))
        "Face for highlighting the current line with `highlight-line-gnus'."
        :group 'hl-line)

      (defun leuven--hl-line-highlight ()
        "Enable line highlighting in the current buffer."
        (set (make-local-variable 'hl-line-face) 'highlight-line-gnus)
        (hl-line-mode 1))

      ;; highlight current line in group buffer
      (add-hook 'gnus-group-mode-hook 'leuven--hl-line-highlight)

      ;; jump to the first group with unread articles, after getting new news
      (add-hook 'gnus-after-getting-new-news-hook
                'gnus-group-first-unread-group)

      ;; keep track of when I last read a group
      (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

      ;; turn off the column number in the group buffer, and remove the binding
      ;; of `C-c C-x', used by Org clocking commands
      (add-hook 'gnus-group-mode-hook
                (lambda ()
                  (progn
                    (set (make-local-variable 'column-number-mode) nil)
                    (define-key gnus-group-mode-map
                      (kbd "C-c C-x") nil))))

      (message "2 Group Buffer... Done"))

;;* 3 (info "(gnus)The Summary Buffer")

(when leuven-section-summary-buffer
      (message "3 Summary Buffer...")

;;** 3.1 (info "(gnus)Summary Buffer Format")

      ;; auxiliary summary mode commands for Gnus
      ;; http://theotp1.physik.uni-ulm.de/~ste/comp/emacs/gnus/rs-gnus-summary.el
      (when (try-require 'rs-gnus-summary-XXX)

        ;; summary line indicators
        (setq rs-gnus-summary-line-content-type-alist
              '((".*" " ")
                ("^multipart/mixed" "@")))

        ;; display `@' for message with attachment in summary line
        (defalias 'gnus-user-format-function-attachment
          'rs-gnus-summary-line-content-type)

        ;; alias for the score function:
        (defalias 'gnus-user-format-function-score
          'rs-gnus-summary-line-score))

      ;; date format depending on age of article
      (setq gnus-user-date-format-alist ;; `user-date'
            '(((gnus-seconds-today) . "               %H:%M")
              ;; (604800 . "           %a %H:%M")
              ((gnus-seconds-month) . "        %d %a %H:%M")
              ((gnus-seconds-year) . "     %m-%d %a %H:%M")
              (t . "%Y-%m-%d %a %H:%M")))

      ;; format specification of the lines in the summary buffer
      (setq gnus-summary-line-format
            ;; for the record, the default string is
            ;; `%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n'
            (concat
             "%5{%U%}" ;; status
             "%8{%R%}" ;; reply status
             "%6{%z%} " ;; score
             "%8{%20&user-date; %} " ;; date
             "%7{ %4k %}" ;; size
             "%*" "%-15,15f " ;; cursor before name of the poster
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
      (if (char-displayable-p ?\u2514)  ; box drawings
          (progn                        ; tree layout using Unicode characters
            (setq gnus-sum-thread-tree-root "")
            (setq gnus-sum-thread-tree-false-root "")
            (setq gnus-sum-thread-tree-single-indent "")
            (setq gnus-sum-thread-tree-indent "    ")
            (setq gnus-sum-thread-tree-vertical "│   ")
            (setq gnus-sum-thread-tree-leaf-with-other "├───")
            (setq gnus-sum-thread-tree-single-leaf "└───"))
        (progn                          ; tree layout using ASCII characters
          (setq gnus-sum-thread-tree-root "")
          (setq gnus-sum-thread-tree-false-root "")
          (setq gnus-sum-thread-tree-single-indent "")
          (setq gnus-sum-thread-tree-indent "    ")
          (setq gnus-sum-thread-tree-vertical "|   ")
          (setq gnus-sum-thread-tree-leaf-with-other "+---")
          (setq gnus-sum-thread-tree-single-leaf "+---"))) ; "`---"

      (eval-after-load "message"
        '(progn
           ;; regexp matching alternative email addresses
           (setq message-alternative-emails
                 (concat
                  (regexp-quote "johndoe@example.com") "\\|"
                  (regexp-quote "janedoe@example.com")))

           ;; `From' headers that may be suppressed in favor of `To' headers
           (setq gnus-ignored-from-addresses
                 (concat
                  (regexp-quote user-mail-address) "\\|" message-alternative-emails))))

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

;;** 3.4 (info "(gnus)Paging the Article")

      ;; added some key bindings to the gnus summary mode
      (define-key gnus-summary-mode-map
        (kbd "<M-up>") '(lambda () (scroll-other-window -1)))

      (define-key gnus-summary-mode-map
        (kbd "<M-down>") '(lambda () (scroll-other-window 1)))

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

;;** 3.9 (info "(gnus)Threading")

      ;; gather threads by comparing the Subject header of the articles
      (setq gnus-summary-thread-gathering-function
            'gnus-gather-threads-by-subject)
            ;; 'gnus-gather-threads-by-references)

      ;; don't grab old headers to build threads
      (setq gnus-fetch-old-headers nil)
      ;; nil to speed up (otherwise, it can seriously impact performance)

      ;; fill in all the gaps to tie loose threads together
      (setq gnus-build-sparse-threads nil) ;; was 'some

      ;; ;; sort the articles within a thread after it has been gathered together
      ;; (setq gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

(define-key gnus-summary-mode-map
  (kbd ">") 'gnus-summary-show-thread)
(define-key gnus-summary-mode-map
  (kbd "<") 'gnus-summary-hide-thread)

;;** 3.10 (info "(gnus)Sorting the Summary Buffer")

      ;; sort threads in descending article order
      (setq gnus-thread-sort-functions
            '(gnus-thread-sort-by-number
              (not gnus-thread-sort-by-most-recent-date)
              ;; gnus-thread-sort-by-total-score
              ))

      ;; If needed, look at `gnus-subthread-sort-functions' for sorting
      ;; subthreads in the summary buffer.

;;** 3.12 (info "(gnus)Article Caching")

      ;; use the cache
      (setq gnus-use-cache t)

      ;; ;; local cache
      ;; (setq gnus-cache-directory (concat gnus-directory "cache/"))

      ;; entering of articles from the cache
      (setq gnus-cache-enter-articles '(ticked dormant unread read))
                                        ; simple setup for your convenience, if
                                        ; you are using `nnimap' from home, over
                                        ; a dialup connection

      ;; removing of articles from the cache
      (setq gnus-cache-remove-articles nil)

      ;; cache your nnimap groups
      (setq gnus-cacheable-groups "^nnimap")

      ;; avoid caching your nnml and nnfolder groups
      (setq gnus-uncacheable-groups "^nnml\\|^nnfolder")

;;** 3.18 (info "(gnus)Article Treatment")

      ;; change a `\205' figure to "..."
      (add-hook 'gnus-part-display-hook 'article-treat-dumbquotes)

      ;; full shampoo of messages you read
      (defun leuven-gnus-wash-this-article ()
        "Wash an article."
        (interactive)
        (gnus-article-outlook-deuglify-article)
        (gnus-article-fill-cited-article nil '79)
        (gnus-article-capitalize-sentences)
        (message "Washed this article"))

      (define-key gnus-summary-mode-map
        (kbd "C-c w") 'leuven-gnus-wash-this-article)

      ;; what is to be considered a signature
      (setq gnus-signature-separator
            '("^-- $"         ; the standard
              "^-- *$"        ; a common mangling
              "^________*$")) ; a long line of underscores is also popular

      ;; limit (in lines, in floating point) to what is considered a signature
      (setq gnus-signature-limit 20.0)

      (defun leuven-prefix-line ()
        "Prefix the current line with `>'."
        (interactive)
        (beginning-of-line)
        (while
            (progn
              ;; repeat...
              (if (looking-at "[>\n]")
                  (insert ">")
                (insert "> "))
              (forward-line 1)
              (beginning-of-line)
              (not
               ;; ...until blank line found
               (looking-at " *$")))))

      (global-set-key (kbd "<S-f5>") 'leuven-prefix-line)

      ;; banners to remove
      (setq gnus-article-address-banner-alist
            '(("@yahoo\\.com" .
               "^__________________+\nDo you Yahoo!\\?\n.*\n.*\n")
              ("@hotmail\\.com\\|@msn.com" .
               "^_________________________________________________________________\n.*MSN .*\n.*\n")))

      ;; FIXME This is under test!
      ;; L'affichage des messages
      (setq gnus-article-display-hook
            '(gnus-article-hide-headers-if-wanted
              gnus-article-date-lapsed
              gnus-article-hide-pgp
              gnus-article-treat-overstrike
              gnus-article-de-quoted-unreadable
              gnus-article-strip-leading-blank-lines
              gnus-article-remove-trailing-blank-lines
              gnus-article-strip-multiple-blank-lines
              gnus-article-highlight
              gnus-article-highlight-signature
              gnus-article-emphasize
              gnus-article-fill-cited-article))

;;** 3.19 (info "(gnus)MIME Commands")

      ;; rewrite file names of MIME parts (delete control characters, delete
      ;; shell gotchas, handle evil white spaces)
      (setq mm-file-name-rewrite-functions
            '(mm-file-name-delete-control
              mm-file-name-delete-gotchas
              mm-file-name-trim-whitespace
              mm-file-name-collapse-whitespace
              mm-file-name-replace-whitespace))

;;** 3.20 (info "(gnus)Charsets")

      ;; permitted unencoded charsets for posting
      (setq gnus-group-posting-charset-alist
            '((message-this-is-news nil (iso-8859-15))))

      (defun leuven-show-article-as-charset ()
        (interactive)
        (message "Display in: latin-[1], latin-[9]/15, [u]tf-8, [c]p1252")
        (let ((answer (read-char)))
          (cond
           ((eq answer ?1)
            (gnus-summary-show-article-from-menu-as-charset-iso-8859-1))
           ((eq answer ?9)
            (gnus-summary-show-article-from-menu-as-charset-iso-8859-9))
           ((eq answer ?c)
            (gnus-summary-show-article-from-menu-as-charset-cp1252))
           ((eq answer ?u)
            (gnus-summary-show-article-from-menu-as-charset-utf-8))))
        (gnus-summary-rethread-current))

;;** 3.27 (info "(gnus)Various Summary Stuff")

      ;; highlight current line in summary buffer
      (add-hook 'gnus-summary-mode-hook 'leuven--hl-line-highlight)

      ;; search forward for an article containing a given regexp
      (define-key gnus-summary-mode-map
        (kbd "s") 'gnus-summary-search-article-forward)

      ;; repeat the last search through the articles in the summary buffer
      ;; (without requiring a confirmation of the search string each time)
      (defun gnus-summary-search-article-forward-next ()
        "Repeat the last forward search."
        (interactive)
        (gnus-summary-search-article-forward gnus-last-search-regexp nil))
      ;; TODO `g-s-s-a-f-next' should be the only function to be called by the
      ;; user, whether or not the search is made for the first time or a
      ;; repetition of it

      (define-key gnus-summary-mode-map
        (kbd "M-s") 'gnus-summary-search-article-forward-next)

      ;; turn off the column number in the group buffer
      (add-hook 'gnus-summary-mode-hook
                (lambda ()
                  (set (make-local-variable 'column-number-mode) nil)))

     ;;  ;; the color of the stripes is obtained by dimming the frame background color
     ;;  (defvar stripe-intensity 12
     ;;    "*intensity of the shade. Used to compute the color of the stripes.
     ;; 0 means no shading of the background color, nil means gray80")
     ;;
     ;;  ;; a command that computes the rgb code of the shaded background color
     ;;  (defun shade-color (intensity)
     ;;    "print the #rgb color of the background, dimmed according to intensity"
     ;;    (interactive "nIntensity of the shade : ")
     ;;    (apply 'format "#%02x%02x%02x"
     ;;           (mapcar (lambda (x)
     ;;                     (if (> (lsh x -8) intensity)
     ;;                         (- (lsh x -8) intensity)
     ;;                       0))
     ;;                   (color-values (cdr (assoc 'background-color (frame-parameters)))))))
     ;;
     ;;  ;; the command that actually puts the stripes in the current buffer
     ;;  (defun stripe-alternate ()
     ;;    "stripes all down the current buffer"
     ;;    (interactive)
     ;;    ;; compute the color of the stripes from the value of stripe-intensity
     ;;    (if stripe-intensity
     ;;        (setq stripe-overlay-face (shade-color stripe-intensity))
     ;;      (setq stripe-overlay-face "gray80"))
     ;;    ;; put the overlay in the current buffer
     ;;    (save-excursion
     ;;      (goto-char (point-min))
     ;;      (let (stripe-overlay)
     ;;        (while (not (eobp))
     ;;          (forward-line)
     ;;          (setq stripe-overlay
     ;;                (make-overlay (line-beginning-position)
     ;;                              (line-beginning-position 2)))
     ;;          (overlay-put stripe-overlay 'face (list :background stripe-overlay-face))
     ;;          (overlay-put stripe-overlay 'priority -1)
     ;;          (forward-line)))))
     ;;
     ;;  ;; activate the stripes for the mail buffers only
     ;;  (add-hook 'gnus-summary-prepare-hook (lambda ()
     ;;                                         (with-current-buffer gnus-summary-buffer
     ;;                                           ;; (unless (gnus-news-group-p gnus-newsgroup-name)
     ;;                                             (stripe-alternate))))
     ;;  ;; )

      (message "3 Summary Buffer... Done"))

;;* 4 (info "(gnus)The Article Buffer")

(when leuven-section-article-buffer
      (message "4 Article Buffer...")

;;** 4.1 Hiding headers

      (eval-after-load "gnus-art"
        '(progn
           ;; TODO Could be limited to news headers only
           (setq gnus-visible-headers
                 (concat
                 "^User-Agent:\\|^X-Spam-Level:\\|^X-Report-Spam:\\|"
                 gnus-visible-headers))

           ;; when Gnus, highlight user agent
           (add-to-list
            'gnus-header-face-alist
            (list (concat
                   "^"
                   (regexp-opt '("User-Agent" "X-Mailer" "Newsreader" "X-Newsreader") t)
                   ":.*Gnus.*")
                  nil 'gnus-server-opened))))

      ;; ,----[ (info "(gnus)Choosing Commands") ]
      ;; | `G j'
      ;; | `j'
      ;; |      Ask for an article number or `Message-ID', and then go to that
      ;; |      article (`gnus-summary-goto-article').
      ;; `----

      ;; > At the moment, I am appending the `Message-ID' to
      ;; > `http://groups.google.com/groups?selm=' to get the target URL, but
      ;; > this does not work reliably.

      ;; auxiliary article mode commands for Gnus
      (when (try-require 'rs-gnus-article-XXX)
        ;; FIXME Break my 2-column display for the mails (summary | article)

        ;; initialization
        (rs-gnus-buttons)

        ;; (defun rs-gnus-button-browse-mid (mid)
        ;;   "Browse MID on Google or Gmane."
        ;;   (message "mid=`%s'" mid)
        ;;   (browse-url (concat
        ;;                (if (string-match "\\bgmane\\." gnus-newsgroup-name)
        ;;                    "http://thread.gmane.org/31"
        ;;                  "http://groups.google.com/groups?as_umsgid=32")
        ;;                mid)))

        ;; open the article in the browser when clicking (or using `RET') on a
        ;; mid in `References' and `Message-ID' headers
        (add-to-list
         'gnus-header-button-alist
         '("^\\(References\\|Message-I[Dd]\\):" "<\\([^<>]+\\)>"
           1 (>= gnus-button-message-level 0) rs-gnus-button-browse-mid 1)
         t)) ;; append!

;;** 4.2 (info "(gnus)Using MIME")

      (message "Emacs MIME...")

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

      ;; You can view "text/html" parts of the current article with a WWW
      ;; browser by pressing `K H' (`gnus-article-browse-html-article')

      ;; ;; MIME type that will be displayed externally automatically
      ;; (add-to-list 'mm-automatic-external-display "text/html")

      ;; do not treat inline images as real attachments (display them, instead)
      (add-to-list 'mm-attachment-override-types "image/.*")
                                        ; "text/x-vcard"...

      ;; don't render HTML automatically *when plain text alternative is
      ;; available*
      (eval-after-load "mm-decode"
        '(progn
           (add-to-list 'mm-discouraged-alternatives "text/html")
           (add-to-list 'mm-discouraged-alternatives "text/richtext")
           (add-to-list 'mm-discouraged-alternatives "text/enriched")
           (add-to-list 'mm-discouraged-alternatives "multipart/related")))

      ;; all images fit in the buffer
      (setq mm-inline-large-images t)

      ;; ;; always show HTML mails as attachments (even if they can be
      ;; ;; displayed) use `browse-url-browser-function' (firefox) to render
      ;; ;; HTML mails
      ;; (push "text/html" mm-inline-override-types)


      ;; use `w3m' browser (if installed) to render HTML-only mails
      (setq mm-text-html-renderer
            (cond ((executable-find "w3m") 'w3m) ; or `gnus-w3m'?
                  (t 'html2text)))      ; Emacs built-in

      ;; recent Gnusae have a built-in HTML renderer which even (somewhat)
      ;; handles CSS
      (setq mm-text-html-renderer 'shr) ; eww-backend?


      (eval-after-load "w3m"
        '(progn
           ;; (setq browse-url-browser-function 'w3m-browse-url)
           (setq mm-inline-text-html-renderer
                 'mm-inline-text-html-render-with-w3m)
           (setq gnus-article-wash-function
                 'gnus-article-wash-html-with-w3m)

           ;; Whenever you try to browse URLs found in articles, Gnus
           ;; (`emacs-w3m', in fact) complains that "This link is considered
           ;; unsafe...". To turn it off, frob the variables
           ;; `w3m-safe-url-regexp' and `mm-w3m-safe-url-regexp'.
           ))

      ;; allow retrieving images in HTML contents with the <img> tags
      (setq mm-inline-text-html-with-images t)

      ;; Remove HTML tags from a buffer
      (defun leuven-wash-ugly-html ()
        "Remove ugly HTML tags."
        (interactive)
        (toggle-read-only -1)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "<[^#][^<@>]*>" nil t)
            (replace-match "" nil nil))
          (goto-char (point-min))
          (while (re-search-forward "&gt;" nil t)
            (replace-match ">" nil nil))
          (goto-char (point-min))
          (while (re-search-forward "&lt;" nil t)
            (replace-match "<" nil nil))

          ;; FIXME Do not simply remove accents like it does now
          (goto-char (point-min))
          (while (re-search-forward "&[^;]*;" nil t)
            (replace-match "" nil nil))))

;;*** 1.6 (info "(emacs-mime)Files and Directories")

      ;; default directory for saving attachments
      (setq mm-default-directory "~/Desktop/")
      (setq mm-default-directory "~/") ; FIXME Temp on temp pc

      (when (fboundp 'leuven-make-directory-yes-or-no)
        (leuven-make-directory-yes-or-no mm-default-directory))

      ;; directory for storing temporary files (opened attachments as well)
      (setq mm-tmp-directory temporary-file-directory)

;;** 2 (info "(emacs-mime)Composing")

;;*** 2.4 (info "(emacs-mime)Encoding Customization")

      ;; preferred coding systems for encoding outgoing messages
      (setq mm-coding-system-priorities
            '(utf-8
              iso-latin-9
              iso-latin-1))             ; ensure that utf-8 is chosen over
                                        ; iso-latin-9 and latin-1, if possible

;;** 4 (info "(emacs-mime)Basic Functions")

;;*** 4.12 (info "(emacs-mime)mailcap")

      ;; choose the right MIME type when sending an attachment
      (eval-after-load "mailcap"
        '(progn
           (add-to-list 'mailcap-mime-extensions
                        '(".doc" . "application/msword"))
           (add-to-list 'mailcap-mime-extensions
                        '(".ppt" . "application/vnd.ms-powerpoint"))))
                                        ; MIME content-types keyed by file
                                        ; extensions

      ;; > which settings do you have to see PDF attachments as text,
      ;; > automagically? Does it work as well for DOC, XLS and PPT?
      ;;
      ;; not here, but I think it's customizable. You need to have the
      ;; programs installed, of course. The variable to look at is
      ;; mailcap-mime-data. It is not customizable, but see
      ;; http://www.gnus.org/manual/emacs-mime_30.html

;;** 4.4 (info "(gnus)Customizing Articles")

      ;; (require 'gnus-art)

      ;; add buttons
      (setq gnus-treat-buttonize t)
      (setq gnus-treat-buttonize-head 'head)

      ;; do not display smileys as pictures
      (setq gnus-treat-display-smileys nil)

      ;; sound
      (setq gnus-treat-play-sounds t)

;; from Tassilo Horn, 17/7/14
(setq shr-color-visible-distance-min 10
      shr-color-visible-luminance-min 60)
(setq gnus-treat-fill-article 0)

;;** 4.6 (info "(gnus)Misc Article")

      ;; format specification for the article mode line
      (setq gnus-article-mode-line-format "%S%m")

      ;; make `C-c C-f' active from within messages
      (define-key gnus-article-mode-map
        (kbd "C-c C-f") 'gnus-summary-mail-forward)

      (message "4 Article Buffer... Done"))

;;* 5 (info "(gnus)Composing Messages")

(when leuven-section-composing-messages
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
      ;; only to debug problems
      (setq smtpmail-debug-info t)

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

      (eval-after-load "gnus-alias"
        '(progn

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
        ))

      ;; add certain headers before sending messages
      (defun leuven-message-add-content ()
        ;; for Gmane address obfuscation
        (message-add-header "X-Archive: encrypt"))

      (add-hook 'message-send-hook 'leuven-message-add-content)

      (message "5 Composing Messages... Done"))

;;* (info "(message)Top")

      ;; Message mode has become the default mail editing mode in Emacs 23.2+
      ;; (used by Gnus and RMail, at least)

(when leuven-section-message
      (message "Message...")

;;** 1 (info "(message)Interface")

;;*** 1.1 (info "(message)New Mail Message")

      ;; keyboard macro to prepare a mail from the current buffer
      (fset 'leuven-mail-this-buffer ;; XXX does not work
            "\C-xh\M-w\M-xmail\C-m\C-c\C-t\C-y\C-c\C-f\C-t")

;;*** 1.4 (info "(message)Wide Reply")

      ;; addresses to prune (disable `Cc:' to myself) when doing wide replies
      (eval-after-load "message"
        (when (boundp 'gnus-ignored-from-addresses)
          (setq message-dont-reply-to-names gnus-ignored-from-addresses)))

;;*** 1.8 (info "(message)Forwarding")

      ;; delimiter inserted before forwarded messages
      (setq message-forward-start-separator "-----Original Message-----\n")

      ;; delimiter inserted after forwarded messages
      (setq message-forward-end-separator "\n")

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

     (defun leuven-message-setup-routine ()
       (flyspell-mode 1))
     (add-hook 'message-setup-hook 'leuven-message-setup-routine)

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
      (eval-after-load "message"
        '(add-to-list 'message-syntax-checks '(sender . disabled)))

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

      (message "Message... Done"))

;;* 6 (info "(gnus)Select Methods")

(when leuven-section-select-methods
      (message "6 Select Methods...")

;;** 6.3 (info "(gnus)Getting Mail")

;;*** 6.3.3 (info "(gnus)Splitting Mail") (in IMAP)

      ;; the registry should be installed
      (setq gnus-registry-install t)    ; for `G G' searches

      ;; article registry for Gnus
      (when (try-require 'gnus-registry-XXX)

        ;; use long group names (so that Gnus knows where the article is)
        (setq gnus-registry-use-long-group-names t)

        ;; unlimited number of entries in the registry
        (setq gnus-registry-max-entries nil)

        ;; enable the various registry functions
        (gnus-registry-initialize))

      ;; the first match in `nnmail-split-rule' found will be used
      (setq nnmail-crosspost nil)

      ;; name(s) of IMAP mailboxes to split mail from
      (setq nnimap-inbox '("INBOX"))    ; Gnus v5.13
      (setq nnimap-split-inbox '("INBOX")) ; Ma Gnus

      ;; BBDB (Big Brother DataBase) is loaded from my `.emacs' file
      (when (try-require 'bbdb-gnus)

        ;; split function to use (sorting mails into groups using BBDB)
        (setq nnimap-split-methods 'nnimap-split-fancy)

        ;; for records which don't have `gnus-private' set, the rules in
        ;; split-fancy are invoked
        (setq bbdb/gnus-split-default-group nil)

        ;; specify how to split mail
        (setq nnimap-split-fancy        ; XXX vs `nnmail-split-fancy'?
              `(|                       ; split to the *first* match

                  ;; mailing lists (in To: or Cc:)
                  (to "foo@bar\\.com" "list.foo")

                  ;; invoke BBDB        ; XXX with BBDB v2 and v3?
                  (: (lambda ()
                       (car (bbdb/gnus-split-method))))

                  ;; catch spam
                  ("X-Spam-Status" "[Yy]es" "INBOX.Spam")

                  ;; unmatched mail goes to the catch-all group (default mailbox)
                  "INBOX")))

;;*** 6.3.9 (info "(gnus)Expiring Mail")

      ;; How can I purge a message from an IMAP server using Gnus?
      ;; IMAP calls it "Expunge" of deleted messages, Gnus calls it expiry of
      ;; expirable (marked 'E') messages..

      ;; groups in which to perform expiry of all read articles
      (setq gnus-total-expirable-newsgroups "\\`nnrss:")

      ;; ;; disable the expiry process started on leaving a group (for speed
      ;; ;; reason) before you leave for lunch, `M-x
      ;; ;; gnus-group-expire-all-groups'
      ;; (remove-hook 'gnus-summary-prepare-exit-hook
      ;;              'gnus-summary-expire-articles)

      ;; make the `d' key mean `delete' in mail groups, too (as in many other
      ;; modes), instead of `keep this message for archival purposes'
      (define-key gnus-summary-mode-map
        (kbd "d") 'gnus-summary-mark-as-expirable)

      ;; ;; Email pre-expiry period according to group
      ;; (setq nnmail-expiry-wait-function
      ;;       (lambda (group)
      ;;         (cond ((string= group "Gmail")      90)
      ;;               ((string= group "moley")      30)
      ;;               ((string= group "blackhole") 'immediate)
      ;;               (t                            7))))
      ;;
      ;; ;; Each time I quit the summary buffer of 'blackhole', any read mail
      ;; ;; is immediately expired


      ;; "deletion" versus "expiry"

      ;; ;; messages should be expired as soon as possible (when the mail
      ;; ;; group is left)
      ;; (setq nnmail-expiry-wait 'immediate)

      ;; 1) Only (*only*) articles that are marked as expirable are apt to be
      ;;    expired. (Yes, *only* those. Yes.)

      ;; 2) If you have made a group total-expirable, all read articles are
      ;;    apt to be expired. (Yes, only the read articles. No, not the
      ;;    unread, ticked and dormant articles. Only the read ones.)

      ;; Delete, even in IMAP terms, means "I want this message to go
      ;; away soon."  IMAP has no equivalent to Gnus' expire mark, which means
      ;; "I want this message to go away after expiry-wait days."

      ;; When you `B DEL', the message dies immediately, now and forever.
      ;; There is no recovery.
      ;; This is "delete" as in "delete it from your disk forever and ever,
      ;; never to return again." Use with caution.

      ;; If you want something to be scheduled for death, then hit `E' to mark
      ;; a message as expirable. The default expiry interval is one week, at
      ;; which point it will be deleted automatically, as though you had hit
      ;; it then with `B DEL'. Then you have 7 days to undelete the messages.

      ;; Many mailers have a concept of a "deleted items" folder but Gnus does
      ;; not have that.  It has expiry instead, so that articles due to
      ;; disappear will remain where they belong until their expiry interval
      ;; arrives.

      ;; Deleting a message is normally done via marking as expirable: you hit
      ;; `E' (not `e') to mark a message as expirable, and whenever expiry is
      ;; run (normally when you `q'uit a summary buffer), the old E messages
      ;; are deleted (old == nnmail-expiry-wait days).

      ;; 1. No expiry.  Only articles explicitly marked with 'E' from the
      ;;    summary buffer are expired.  Reading articles marks them 'R'ead.
      ;;
      ;; 2. Auto-expiry.  Only articles marked expirable are expired, but
      ;;    reading articles marks them 'E'xpirable.
      ;;
      ;; 3. Total-expiry.  Articles merely marked 'R'ead are expired, along
      ;;    with articles explicitly marked 'E'xpirable.
      ;;
      ;; Assuming you read every article, auto- and total-expiry will
      ;; eventually expire them all at the same times. It's just a question as
      ;; to whether you'd prefer to mark articles as expirable or read when
      ;; you read them. I tend to use total-expiry for everything; it has
      ;; predictable behavior if I'm switching it on and off for a group, and
      ;; I understand better how it works with scoring.

      ;; delete the article when pressing `Delete'

;; FIXME Does not work... This is the opposite of SPC (page down)!
(eval-after-load "gnus-sum" ;; be sure to override default
      (gnus-define-keys gnus-summary-mode-map
        [delete] gnus-summary-delete-article))

;;*** 6.3.11 (info "(gnus)Duplicates")

      ;; cache of old Message-IDs for every message Gnus sees
      (setq nnmail-message-id-cache-file
            (concat gnus-directory ".nnmail-cache"))

;; to use `nnmail-split-fancy-with-parent'
;; record the Message-ID of every message it sees
(setq nnmail-treat-duplicates 'warn)     ; or `delete'

;; ensure that the Message-ID are still in the cache
(setq nnmail-message-id-cache-length 5000)

;; record the Message-ID of moved articles
(setq nnmail-cache-accepted-message-ids t)

;;** 6.4 (info "(gnus)Browsing the Web")

      ;; use `wget' instead of `url.el' (for nnweb-type google), cause it
      ;; seems to be more reliable (about how the HTML is fetched)

      ;; use external grab program (to fetch the RSS feed, etc.)
      (setq mm-url-use-external t)

      ;; URL grab program
      ;; TODO Protect in case wget is not installed
      ;; (setq mm-url-program "wget") ; make sure you don't use the symbol 'wget

      ;; FYI, wget is run when a link from Org is not found, for example
      ;; because the mail has been moved from one group to another.

      ;; arguments for `mm-url-program'
      (setq mm-url-arguments '("-q" "-O" "-"))

      ;; how to find the parent article
      (when (try-require 'nnweb-XXX)    ; not sure it really works!?
        (setq gnus-refer-article-method
              '(
                ;; first try the current method
                current

                ;; find the Message-ID based on the registry, so that you can
                ;; safely move articles around
                ;; (nnregistry)

                (nnweb "gmane" (nnweb-type gmane))

                ;; then ask Google
                (nnweb "google" (nnweb-type google))

                ;; and finally ask `news.gmane.org' if that fails
                (nntp "news.gmane.org")
                )))

;;** 6.5 (info "(gnus)IMAP")

      ;; log commands (imap session trace) to the `*imap log*' buffer
      (setq nnimap-record-commands t)

;;** 6.8 (info "(gnus)Gnus Unplugged")

      ;; disable the Gnus agent (used for "offline" reading)
      (setq gnus-agent nil)             ; prevent being unable to delete some
                                        ; mails from your inbox

      (message "6 Select Methods... Done"))

;;* 7 (info "(gnus)Scoring")

(when leuven-section-scoring
      (message "7 Scoring...")

      ;; ;; directory where kill and score files will be stored
      ;; (setq gnus-kill-files-directory "~/")

      ;; ;; increase score of messages that are replies to my own postings
      ;; ;; TODO only for newsgroups (and not for mails)!
      ;; (add-hook 'message-sent-hook 'gnus-score-followup-article)
      ;; (add-hook 'message-sent-hook 'gnus-score-followup-thread)

      (message "7 Scoring... Done"))

;;* 9 (info "(gnus)Various")

(when leuven-section-various
      (message "8 Various...")

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
          (kbd "a") 'gnus-dired-attach)) ; XXX conflict with
                                         ; `dired-find-alternate-file'

;;** 9.21 (info "(gnus)Various Various")

      ;; display more messages from Gnus
      (setq gnus-verbose 10) ;; 9 = minimum for helpful debugging

      ;; display more messages from the Gnus back-ends
      (setq gnus-verbose-backends 10)

      (message "8 Various... Done"))

;;* 11 (info "(gnus)Appendices")

(when leuven-section-appendices
      (message "11 Appendices...")

;;** 11.2 (info "(gnus)History")

      (setq canlock-password "secret")
      (setq canlock-password-for-verify "secret")

;;** 11.9 (info "(gnus)Frequently Asked Questions")

      ;; byte-compile the user-defined format specs (things like
      ;; `gnus-summary-line-format')
      (when (fboundp 'gnus-compile)
        (gnus-compile)) ;;  at the bottom

      (message "11 Appendices... Done"))

;; warn that some packages were missing
(dolist (pkg leuven--missing-packages)
  (message "(warning) Package `%s' not found" pkg))

(message "\n")

(provide 'gnus-leuven)

;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8-unix
;; eval: (when (locate-library "rainbow-mode") (require 'rainbow-mode) (rainbow-mode))
;; ispell-local-dictionary: "american"
;; End:

;;; gnus-leuven.el ends here
