;;* (info "(bbdb)Installation")

(unless (ignore-errors (load-library "bbdb-autoloads")) ; "hand-made"
  (autoload 'bbdb "bbdb-com"
    "Insidious Big Brother Database." t)
  (autoload 'bbdb-name "bbdb-com"
    "Insidious Big Brother Database." t)
  (autoload 'bbdb-company "bbdb-com"
    "Insidious Big Brother Database." t)
  (autoload 'bbdb-net "bbdb-com"
    "Insidious Big Brother Database." t)
  (autoload 'bbdb-notes "bbdb-com"
    "Insidious Big Brother Database." t)

  (autoload 'bbdb-insinuate-gnus "bbdb-gnus"
    "Hook BBDB into Gnus.")
  ;; (autoload 'bbdb-insinuate-message "bbdb"
  ;;   "Hook BBDB into `message-mode'.") ; BBDB 2.35
  (autoload 'bbdb-insinuate-message "bbdb-message"
    "Hook BBDB into `message-mode'."))

;; Search the BBDB.
(global-set-key (kbd "<C-f11>") #'bbdb)

(with-eval-after-load "bbdb"

  ;; Coding system used for reading and writing `bbdb-file'.
  (setq bbdb-file-coding-system 'utf-8)

  ;; Ensure `~/.bbdb' never becomes non utf-8 again (it is defined with
  ;; `defconst', so it is reset whenever `bbdb.el' is loaded).
  (add-hook 'bbdb-load-hook
            #'(lambda ()
                (setq bbdb-file-coding-system 'utf-8)))

  ;; Enable the various package-specific BBDB functions.
  (bbdb-initialize 'gnus 'message)
  ;; - Add bindings for the default keys to Gnus and configure Gnus to notify
  ;;   the BBDB when new messages are loaded (required if the BBDB is to be
  ;;   able to display BBDB entries for messages displayed in Gnus).
  ;;
  ;; - Add a binding for `M-TAB' to Message mode.  This will enable completion
  ;;   of addresses based on BBDB records.

  ;; What do we do when invoking bbdb interactively (`:' to display sender).
  (setq bbdb-mua-update-interactive-p '(query . create))

  ;; Update BBDB silently (don't display an auto-updated BBDB window).
  (setq bbdb-mua-pop-up nil)

  ;;* (info "(bbdb)Interfaces")

  ;; Mail aliases (local mailing lists).
  ;; (add-hook 'message-setup-hook #'bbdb-define-all-aliases) ; BBDB 2.35
  (add-hook 'message-setup-hook #'bbdb-mail-aliases) ; BBDB 3

  ;; Always use full name when sending mail.
  ;; (even if User Name has an address of the form <user.name@domain>)
  (setq bbdb-dwim-net-address-allow-redundancy t) ; BBDB 2.35
  (setq bbdb-mail-avoid-redundancy nil) ; BBDB 3

  ;; No popup on auto-complete.
  (setq bbdb-completion-display-record nil)

  ;; Completion is done across the set of all full-names and user-ids.
  (setq bbdb-completion-type nil)

  ;;* (info "(bbdb)Reader-specific Features")

  ;; Marking posters with records in the BBDB.
  (setq bbdb/gnus-summary-mark-known-posters t)

  ;; Mark authors in the Summary Buffer who have records in the BBDB.
  (setq bbdb/gnus-summary-known-poster-mark "B")

  ;; Display the poster's name from the BBDB if we have one.
  (setq bbdb/gnus-summary-prefer-real-names t)

  ;; Replace the information provided in the From header with data from the
  ;; BBDB if we have one.
  (setq bbdb/gnus-summary-prefer-bbdb-data t)

  (setq bbdb/gnus-summary-show-bbdb-names t)

  ;;* (info "(bbdb)Options")

  ;; No default area code to use when prompting for a new phone number.
  (setq bbdb-default-area-code nil)

  ;; Default country to use if none is specified.
  (setq bbdb-default-country "")

  ;; Disable syntax-checking of telephone numbers.
  (setq bbdb-north-american-phone-numbers-p nil) ; BBDB 2.35
  (setq bbdb-phone-style nil)         ; BBDB 3

  ;; Restoration of the window configuration.
  (setq bbdb-electric-p t)            ; BBDB 2.35
  (setq bbdb-electric t)              ; BBDB 3

  ;; Don't display a continuously-updating BBDB window while in GNUS.
  ;; (setq bbdb-use-pop-up nil)       ; BBDB 2.35
  ;; (setq bbdb-pop-up-layout nil)    ; BBDB 3

  ;; Desired number of lines in a GNUS pop-up BBDB window.
  (setq bbdb-pop-up-target-lines 1)   ; BBDB 2.35
  (setq bbdb-pop-up-window-size 1)    ; BBDB 3

  ;; Default display layout.
  (setq bbdb-display-layout 'multi-line)

  ;; Default display layout pop-up BBDB buffers.
  (setq bbdb-pop-up-display-layout 'one-line)

  ;; Omit creation-date and time stamp from BBDB display.
  (setq bbdb-display-layout-alist
        '((one-line          (order     . (phones notes))
                             (name-end  . 24)
                             (toggle    . t)
                             (omit      . (net AKA mail-alias gnus-private
                                               creation-date timestamp)))
          (multi-line        (indention . 14)
                             (toggle    . t)
                             (omit      . (AKA creation-date timestamp)))
          (pop-up-multi-line (indention . 14))))

  ;; Allow cycling of email addresses while completing them.
  (setq bbdb-complete-name-allow-cycling t) ; BBDB 2.35
  (setq bbdb-complete-mail-allow-cycling t) ; BBDB 3

  ;; Save the database without asking (any time it would ask).
  (setq bbdb-offer-save 'auto)

  ;; Automatically add some text to the notes field of the BBDB record.
  (add-hook 'bbdb-notice-hook #'bbdb-auto-notes-hook)

  ;; Capture auto-notes.
  (setq bbdb-auto-notes-alist
        ;; Organization.
        `(("Organization" (".*" Organization 0))

          ;; X-Face bitmaps of the people.
          ("x-face" ,(list (concat "[ \t\n]*\\([^ \t\n]*\\)"
                                   "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                   "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                   "\\([ \t\n]+\\([^ \t\n]+\\)\\)?")
                           'face
                           "\\1\\3\\5\\7")))))

(provide 'bbdb-leuven)

;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8-unix
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode))
;; flycheck-emacs-lisp-initialize-packages: t
;; flycheck-mode: nil
;; ispell-local-dictionary: "american"
;; End:

;;; bbdb-leuven.el ends here
