;; Require a feature/library if available; if not, fail silently.
(unless (fboundp 'try-require)
  (defun try-require (feature)
    "Attempt to load a FEATURE (or library).
Return true if the library given as argument is successfully loaded.
If not, just print a message."
    (condition-case err
        (progn
          (if (stringp feature)
              (load-library feature)
            (require feature))
          t)                            ; Necessary for correct behavior in
                                        ; conditional expressions.
      (file-error
       (message "[Requiring `%s'... missing]" feature)
       nil))))

;; (info "(org)Top") outline-based notes management and organizer

;;* 1 (info "(org)Introduction")

;;** 1.2 (info "(org)Installation")

;; Autoloads.
(try-require 'org-loaddefs)

;; Getting started.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

;; Using links outside Org.
(global-set-key (kbd "C-c L") #'org-insert-link-global)
(global-set-key (kbd "C-c O") #'org-open-at-point-global)

(when (or (not (boundp 'org-agenda-files))
          (null org-agenda-files))
  (message "[Found no entries in `org-agenda-files']")
  (sit-for 1.5))

(with-eval-after-load 'org
  ;; Unbind `C-j' and `C-''.
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-'") nil) ; Unbind `org-cycle-agenda-files'.
  (define-key org-mode-map (kbd "<C-S-down>") nil)
  (define-key org-mode-map (kbd "<C-S-up>") nil)

  ;; Double-clicking on the fringe cycles the corresponding subtree.
  (define-key org-mode-map (kbd "<left-fringe> <double-mouse-1>") #'org-cycle))

;; These variables need to be set before org.el is loaded...

;; ;; Face to be used by `font-lock' for highlighting in Org mode Emacs
;; ;; buffers, and tags to be used to convert emphasis fontifiers for HTML
;; ;; export. XXX Format changed!
;; (setq org-emphasis-alist              ; Remove the strike-through emphasis.
;;       '(("*" bold "<b>" "</b>")
;;         ("/" italic "<i>" "</i>")
;;         ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
;;         ("=" org-verbatim "<code>" "</code>" verbatim)
;;         ("~" org-code "<code>" "</code>" verbatim)))

;; (setq org-emphasis-alist
;;       '(("&" (:weight ultra-bold :foreground "#000000" :background "#FBFF00"))
;;         ;; ("?" (:box t))
;;         ("!" (:weight ultra-bold :foreground "#B40000")) ; = alert in some Wikis

(with-eval-after-load 'org
  ;; Allow both single and double quotes in the border.
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
  (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)))

;; Single character alphabetical bullets (a, b, c, ..., X, Y, Z) are allowed.
(setq org-list-allow-alphabetical t)

;; Libraries that should (always) be loaded along with `org.el'
;; (loaded when opening the first Org file).
(setq org-modules nil)

;; Check that org-checklist is found before adding it!
;;
;;   ;; Set the RESET_CHECK_BOXES and LIST_EXPORT_BASENAME properties in items as
;;   ;; needed.
;;   (add-to-list 'org-modules 'org-checklist) ; From org-contrib.

;; Globally unique ID for Org mode entries (see `org-store-link')
;; (takes care of automatically creating unique targets for internal
;; links, see `C-h v org-id-link-to-org-use-id RET').
(add-to-list 'org-modules 'org-id)

;; Support for links to Gnus groups and messages from within Org mode.
(add-to-list 'org-modules 'ol-gnus)

;; ;; Habit tracking code for Org mode.
;; (add-to-list 'org-modules 'org-habit)

;; Make sure to turn `ol-info' on in order to link to info nodes.
(add-to-list 'org-modules 'ol-info)

(add-hook 'org-mode-hook
          (lambda ()
            ;; ;; Create a binding for `org-show-subtree'.
            ;; (local-set-key (kbd "C-c C-S-s") #'org-show-subtree)
            ;; (local-set-key (kbd "C-c s") #'org-show-subtree)

            ;; (local-set-key (kbd "C-c h") #'hide-other) ; XXX Helm

            ;; Remove some bindings.
            (local-unset-key (kbd "C-c SPC")) ; Used by Ace Jump.
            (local-unset-key (kbd "C-c C-<")) ; Used by Multiple Cursors.
            ;; (local-unset-key (kbd "C-c %")) ; XXX
            ;; (local-unset-key (kbd "C-c &")) ; XXX

            ))

(add-to-list 'package-selected-packages 'helm-org)
(add-to-list 'package-selected-packages 'org-contrib)
(add-to-list 'package-selected-packages 'orgalist)
(add-to-list 'package-selected-packages 'ox-jira)

(with-eval-after-load 'org
  (message "[... Org Introduction]")

  ;;** 1.3 (info "(org)Activation")

  (message "1.3 (org)Activation")

  ;; Insert the first line setting Org mode in empty files.
  (setq org-insert-mode-line-in-empty-file t))

;;* 2 (info "(org)Document Structure")

(with-eval-after-load 'org
  (message "[... Org Document Structure]")

  ;; Define the right-pointing pointer character.
  (defvar lvn-right-pointing-char
    (if (char-displayable-p ?\u25B8)
        " \u25B8"
      nil))

  ;; Define common face attributes.
  (defvar lvn-org-ellipsis-face-attributes
    '((:box "#999999"
       :foreground "#999999"
       :background "#FFF8C0"
       :underline nil)))

  ;; Set the Org mode ellipsis.
  (if lvn-right-pointing-char
      (setq org-ellipsis lvn-right-pointing-char)
    (apply #'set-face-attribute 'org-ellipsis lvn-org-ellipsis-face-attributes)
    (setq org-ellipsis 'org-ellipsis)))

;; RET follows links (except in tables, where you must use `C-c C-o').
(setq org-return-follows-link t)

;; Blank lines.
(setq org-blank-before-new-entry
      '(;; Insert a blank line before new heading.
        (heading . t)

        ;; Try to make an intelligent decision whether to insert a
        ;; blank line or not before a new item.
        (plain-list-item . auto)))

;;** (info "(org)Headlines")

(message "2.2 (org)Headlines")

;; `C-a' and `C-e' behave specially in headlines and items.
(setq org-special-ctrl-a/e 'reversed)

(with-eval-after-load 'org
  (message "[... Org Headlines]")

  ;; Insert an inline task (independent of outline hierarchy).
  (try-require 'org-inlinetask))      ; Needed.

(with-eval-after-load 'org-inlinetask

  ;; Initial state (TODO keyword) of inline tasks.
  (setq org-inlinetask-default-state "TODO")

  ;; ;; Template for inline tasks in HTML exporter.
  ;; (defun leuven--org-html-format-inlinetask
  ;;     (todo todo-type priority text tags contents &optional info)
  ;;   "Format an inline task element for HTML export."
  ;;   (let ((todo-kw
  ;;          (if todo
  ;;              (format "<span class=\"%s %s\">%s</span> " todo-type todo todo)
  ;;            ""))
  ;;         (full-headline-w/o-todo-kw
  ;;          (concat
  ;;           (when priority (format "[#%c] " priority))
  ;;           text
  ;;           (when tags
  ;;             (concat "&nbsp;&nbsp;&nbsp;"
  ;;                     "<span class=\"tag\">"
  ;;                     (mapconcat (lambda (tag)
  ;;                                  (concat "<span class= \"" tag "\">" tag
  ;;                                          "</span>"))
  ;;                                tags
  ;;                                "&nbsp;")
  ;;                     "</span>")))))
  ;;     (concat "<table class=\"inlinetask\" width=\"100%\">"
  ;;               "<tr>"
  ;;                 "<td valign=\"top\"><b>" todo-kw "</b></td>"
  ;;                 "<td width=\"100%\"><b>" full-headline-w/o-todo-kw "</b><br />"
  ;;                   (or contents "") "</td>"
  ;;               "</tr>"
  ;;             "</table>")))
  ;;
  ;; ;; Function called to format an inlinetask in HTML code.
  ;; (setq org-html-format-inlinetask-function
  ;;       'leuven--org-html-format-inlinetask)
  ;;
  ;; ;; Template for inline tasks in LaTeX exporter.
  ;; (defun leuven--org-latex-format-inlinetask
  ;;     (todo todo-type priority text tags contents &optional info)
  ;;   "Format an inline task element for LaTeX export."
  ;;   (let* ((tags-string (format ":%s:" (mapconcat 'identity tags ":")))
  ;;          (opt-color
  ;;           (if tags
  ;;               (cond ((string-match ":info:" tags-string)
  ;;                      "color=yellow!40")
  ;;                     ((string-match ":warning:" tags-string)
  ;;                      "color=orange!40")
  ;;                     ((string-match ":error:" tags-string)
  ;;                      "color=red!40")
  ;;                     (t ""))
  ;;             ""))
  ;;          (full-headline
  ;;           (concat
  ;;            (when todo
  ;;              (format "{\\color{red}\\textbf{\\textsf{\\textsc{%s}}}} "
  ;;                      todo))
  ;;            (when priority
  ;;              (format "\\textsf{\\framebox{\\#%c}} " priority))
  ;;            text
  ;;            (when tags
  ;;              (format "\\hfill{}:%s:"
  ;;                      (mapconcat 'identity tags ":")))))
  ;;          (opt-rule
  ;;           (if contents
  ;;               "\\\\ \\rule[.3em]{\\textwidth}{0.2pt}\n"
  ;;             ""))
  ;;          (opt-contents
  ;;           (or contents "")))
  ;;     ;; This requires the `todonotes' package.
  ;;     (format (concat "\\todo[inline,caption={},%s]{\n"
  ;;                     "  %s\n"
  ;;                     "  %s"
  ;;                     "  %s"
  ;;                     "}")
  ;;             opt-color
  ;;             full-headline
  ;;             opt-rule
  ;;             opt-contents)))
  ;;
  ;; ;; Function called to format an inlinetask in LaTeX code.
  ;; (setq org-latex-format-inlinetask-function
  ;;       'leuven--org-latex-format-inlinetask)
  )                                   ; with-eval-after-load "org-inlinetask" ends here.

;;** (info "(org)Visibility cycling")

(message "2.3 (org)Visibility cycling")

;; Switch to OVERVIEW (fold all) at startup.
(setq org-startup-folded t)

;; Inhibit startup when preparing agenda buffers -- agenda optimization.
(setq org-agenda-inhibit-startup t)   ; XXX

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)       ; Apps key.

(with-eval-after-load 'org
  ;; Create indirect buffer and narrow it to current subtree.
  (define-key org-mode-map (kbd "<H-RET>") #'org-tree-to-indirect-buffer))

;;** (info "(org)Motion")

(message "2.4 (org)Motion")

;; Outline-node based navigation similar to the behavior of paredit-mode in
;; Lisp files.
(add-hook 'org-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "M-n")   #'outline-next-visible-heading)
            ;; (local-set-key (kbd "C-M-n") #'outline-next-visible-heading)

            ;; (local-set-key (kbd "M-p")   #'outline-previous-visible-heading)
            ;; (local-set-key (kbd "C-M-p") #'outline-previous-visible-heading)

            ;; (local-set-key (kbd "C-M-u") #'outline-up-heading)
            ))

;; Headlines in the current buffer are offered via completion
;; (interface also used by the `refile' command).
(setq org-goto-interface 'outline-path-completion)

(with-eval-after-load 'org

  (defun lvn-org-reveal (&optional all-siblings)
    "Show all siblings of the current level.
With a prefix argument (C-u C-c C-r), show all hidden siblings."
    (interactive "P")
    (if all-siblings
        (org-reveal t)
      (org-show-siblings)))

  (define-key org-mode-map (kbd "C-c C-r") #'lvn-org-reveal))

;;** (info "(org)Structure editing")

(message "2.5 (org)Structure editing")

;; Don't adapt indentation to outline node level.
(setq org-adapt-indentation nil)

;; ;; FIXME Choose the right value!
;; (setq org-M-RET-may-split-line nil)

;;** (info "(org)Sparse trees")

(message "2.6 (org)Sparse trees")

(with-eval-after-load 'org

  (when (boundp 'org-show-context-detail)
    ;; (setq org-show-context-detail '((default . local)))
    (add-to-list 'org-show-context-detail '(tags-tree . ancestors))
    (add-to-list 'org-show-context-detail '(occur-tree . ancestors))))

;;** (info "(org)Plain lists")

(message "2.7 (org)Plain lists")

;; Maximum indentation for the second line of a description list.
(setq org-description-max-indent 3)

;; Don't make tab cycle visibility on plain list items.
(setq org-cycle-include-plain-lists nil) ;; 'integrate?

;; (setq org-cycle-separator-lines -2)

;;** (info "(org)Footnotes")

(message "2.10 (org)Footnotes")

;; Use `C-c C-x f' to add a footnote, to go back to the message
;; *and* to go to a footnote.
(global-set-key (kbd "C-c C-x f") #'org-footnote-action)

;;* 3 (info "(org)Tables")

(setq org-table-use-standard-references 'from)

;;** 3.1 The (info "(org)Built-in table editor")

(message "3.1 The (org)Built-in table editor")

;; Default export parameters for `org-table-export'.
(setq org-table-export-default-format "orgtbl-to-csv")

;;** 3.5 (info "(org)The spreadsheet")

(message "3.5 (org)The spreadsheet")

(with-eval-after-load 'org-table
  ;; Some Calc mode settings for use in `calc-eval' for table formulas.
  (setcar (cdr (memq 'calc-float-format org-calc-default-modes))
          '(float 12)))               ; [Default: 8]

;;* 4 (info "(org)Hyperlinks")

;; Don't hexify URL when creating a link.
(setq org-url-hexify-p nil)

(with-eval-after-load 'org
  (message "[... Hyperlinks]")

  ;; ;; Open non-existing files.
  ;; (setq org-open-non-existing-files t)

  ;; Function and arguments to call for following `mailto' links.
  (setq org-link-mailto-program '(compose-mail "%a" "%s")))

;; Support for links to Gnus groups and messages from within Org mode.
(with-eval-after-load 'org-gnus

  ;; Create web links to Google groups or Gmane (instead of Gnus messages).
  (setq org-gnus-prefer-web-links t))

;; Global identifiers for Org mode entries.
(with-eval-after-load 'org-id

  ;; Storing a link to an Org file will use entry IDs.
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id))

(with-eval-after-load 'org
  (message "[... Handling links]")

  ;; 4.4 Show inline images when loading a new Org file.
  (setq org-startup-with-inline-images t) ; Invokes org-display-inline-images.

  ;; 4.4 Try to get the width from an #+ATTR.* keyword and fall back on 320px
  ;; width if none is found.
  ;; (setq org-image-actual-width '(320)) ; crashes Emacs with Org 9?

  (defun leuven-org-search-backlinks ()
    "Show all entries that point to the current node.  Also show the current
node itself.

This makes ID links quasi-bidirectional."
    (interactive)
    (let ((org-agenda-files
           (add-to-list 'org-agenda-files (buffer-file-name))))
      (org-search-view nil (org-entry-get nil "ID" t))))

  ;; Shortcut links.
  (setq org-link-abbrev-alist
        '(("cache" .
           "http://www.google.com/search?q=cache:%s")
          ("dictionary" .
           "http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query=%s")
          ("google" .
           "http://www.google.com/search?q=%s")
          ("googlegroups" .
           "http://groups.google.com/groups?q=%s")
          ("googlemaps" .
           "http://maps.google.com/maps?q=%s")
          ("imdb" .
           "http://us.imdb.com/Title?%s")
          ("openstreetmap" .
           "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
          ("wpen" .
           "http://en.wikipedia.org/wiki/%s")
          ("wpfr" .
           "http://fr.wikipedia.org/wiki/%s"))))

;;* 5 (info "(org)TODO Items")

;;** 5.1 (info "(org)TODO basics") functionality

(message "5.1 (org)TODO basics functionality")

;; 5.1 Select a TODO state and bypass any logging associated with that.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Some commands act upon headlines in the active region.
(setq org-loop-over-headlines-in-active-region 'start-level)

;;** 5.2 Use of (info "(org)TODO extensions")

(message "5.2 Use of (org)TODO extensions")

;; Define TODO keyword sequences with fast access keys and state change logging.
(setq org-todo-keywords
      '((sequence "MAYB(m)"             ; Proposal, idea, someday/maybe, wish.
                  "TODO(t)"             ; Open, not yet started.
                  "STRT(s)"             ; In progress, working on.
                  "WAIT(w@/!)"          ; On hold, needs discussion or feedback.
                  "|"
                  "DONE(d!)"            ; Completed, closed, resolved.
                  "CANX(x@)")))         ; Won't fix, rejected, ignored.

(with-eval-after-load 'org-faces

  ;; Define non-standard faces for specific TODO states.
  (defface leuven-org-mayb-kwd
    '((t :weight bold :box "#BBBBBB"
         :foreground "#BBBBBB" :background "#F0F0F0"))
    "Face used to display state MAYB.")
  (defface leuven-org-strt-kwd
    '((t :weight bold :box "#D9D14A"
         :foreground "#D9D14A" :background "#FCFCDC"))
    "Face used to display state STRT.")
  (defface leuven-org-wait-kwd
    '((t :weight bold :box "#9EB6D4"
         :foreground "#9EB6D4" :background "#E0EFFF"))
    "Face used to display state WAIT.")
  (defface leuven-org-canx-kwd
    '((t :weight bold :box "#BBBBBB"
         :foreground "#BBBBBB" :background "#F0F0F0"))
    "Face used to display state CANX.")

  ;; Set the specific faces for Org TODO keywords.
  (setq org-todo-keyword-faces
        '(("MAYB" . leuven-org-mayb-kwd)
          ("TODO" . org-todo)
          ("STRT" . leuven-org-strt-kwd)
          ("WAIT" . leuven-org-wait-kwd)
          ("DONE" . org-done)
          ("CANX" . leuven-org-canx-kwd)))

  ;; Org standard faces with specific attributes.
  (set-face-attribute 'org-todo nil
                      :weight 'bold :box "#D8ABA7"
                      :foreground "#D8ABA7" :background "#FFE6E4")

  (set-face-attribute 'org-done nil
                      :weight 'bold :box "#89C58F"
                      :foreground "#89C58F" :background "#E2FEDE"))

;; Block switching entries to DONE if
;; 1) there are undone child entries, or
;; 2) the parent has an `:ORDERED:' property and there are prior
;;    siblings not yet done.
(setq org-enforce-todo-dependencies t)

;; 5.2.7 Don't dim blocked tasks in the agenda display -- agenda optimization.
(setq org-agenda-dim-blocked-tasks nil) ; XXX not sure about this one

;; Block switching the parent to DONE if there are unchecked checkboxes.
(setq org-enforce-todo-checkbox-dependencies t)

;;** 5.3 (info "(org)Progress logging")

(message "5.3 (org)Progress logging")

;; 5.3.1 Prevent inserting a CLOSED timestamp each time a TODO entry is marked
;; DONE (the default behavior).
(setq org-log-done nil)

;; 5.3.2 Ensure notes are ordered chronologically.
(setq org-log-states-order-reversed nil)

;; 5.3.3 Insert state change notes and timestamps into the LOGBOOK drawer.
(setq org-log-into-drawer t)    ; This should be the default behavior!

;; ~5.3.2 Heading for state change added to entries.
(with-eval-after-load 'org
  (message "[... Progress logging]")

  ;; Update the 'state' log format to show state transitions like
  ;; 'State "TODO"        ->  "DONE"       [2025-01-26 Sun 10:38]'.
  (setcdr (assq 'state org-log-note-headings)
          "State %-12S  ->  %-12s %t"))

(with-eval-after-load 'org-habit

  ;; Show habits for future days.
  (setq org-habit-show-habits-only-for-today nil)

  ;; Use character "heavy check mark" to show completed days on which a task
  ;; was done.
  (setq org-habit-completed-glyph ?\u2714)

  ;; Use character "heavy quadruple dash vertical" to identify today.
  (setq org-habit-today-glyph ?\u250B))

;;** 5.5 (info "(org)Breaking down tasks")

(message "5.5 (org)Breaking down tasks")

;; Automatically change a TODO entry to DONE when all children are done.
(defun leuven--org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)  ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'leuven--org-summary-todo)

;;* 6 (info "(org)Tags")

;; Column to which tags should be indented in a headline.
(setq org-tags-column -80)

;; 6.2 List of tags ("contexts") allowed in Org mode files.
(setq org-tag-alist '(("FLAGGED"     . ?1) ; = ASAP.
                      (:startgroup  . nil)
                      ("work"       . ?2)
                      ("personal"   . ?3)
                      (:endgroup    . nil)
                      ;; ("now"      . XXX)

                      ("notbillable" . ?B)

                      ("call"        . ?c)
                      ("errands"     . ?e)
                      ;; childcare
                      ;; event
                      ("finance"     . ?f) ; = Bills, statements, receipts.
                      ;; shopping
                      ;; family
                      ;; friends
                      ("mail"        . ?m)

                      ;; ("reading"  . ?r)
                      ;; ("proj"     . ?P)

                      ("ARCHIVE"     . ?a) ; Speed command + action in task list.
                      ("crypt"       . ?C)
                      ))

;; Faces for specific tags.
(setq org-tag-faces
      '(("FLAGGED"                    ; Important.
         (:slant italic :foreground "#FF0000"))
        ("work"
         (:slant italic :foreground "#FF9900"))
        ("personal"
         (:slant italic :foreground "#009900"))
        ("now"                        ; To do.
         (:slant italic :foreground "#3333FF"))
        ("inbox"                      ; Later.
         (:slant italic :foreground "#993399"))
        ("notbillable"
         (:slant italic :foreground "#A9876E"))
        ))

;; 6.2 Exit fast tag selection after first change (toggle this with `C-c').
(setq org-fast-tag-selection-single-key t)

;; Remove redundant tags of headlines (from David Maus).
(defun leuven-org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.
A tag is considered redundant if it is local to a headline and inherited by
a parent headline."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string
                         (or (org-entry-get (point) "ALLTAGS") "")
                         ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited)
               (push tag local)))
           (dolist (tag local)
             (when (member tag inherited)
               (org-toggle-tag tag 'off)))))
       t nil))))

;; ;; Always offer completion for all tags of all agenda files.
;; (setq org-complete-tags-always-offer-all-agenda-tags t)

;;* 7 (info "(org)Properties and Columns")

;;** 7.1 (info "(org)Property syntax")

(message "7.1 (org)Property syntax")

;; List of property/value pairs that can be inherited by any entry.
(setq org-global-properties
      '(("Effort_ALL" .
         "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"
         ;; "0d 1d 2d 3d 4d 5d 6d 7d 8d 10d"
         ;; "0 1:00 4:00 1d 2d 1w 2w"
         )))

;;* 8 (info "(org)Dates and Times")

(message "8 (org)Dates and Times")

;; Insinuate appt if Org mode is loaded.
(with-eval-after-load 'org
  (message "[... Org Dates and Times]")

  (try-require 'appt))

;;** 8.2 (info "(org)Creating timestamps")

(message "8.2 (org)Creating time stamps")

;; Prefer the future for incomplete dates.
(setq org-read-date-prefer-future 'time)

;; ;; Advise `org-read-date' to bury the calendar buffer after selecting a date,
;; ;; so it is out of the way.
;; (defadvice org-read-date
;;   (after leuven-bury-calendar-after-org-read-date
;;          (&optional with-time to-time from-string prompt
;;          default-time default-input) protect)
;;   "Bury the *Calendar* buffer after reading a date."
;;   (bury-buffer "*Calendar*"))
;; (ad-activate 'org-read-date)

;;** 8.3 (info "(org)Deadlines and scheduling")

(message "8.3 (org)Deadlines and scheduling")

;; Information to record when the scheduling date is modified.
(setq org-log-reschedule nil)

;; Information to record when the deadline date is modified.
(setq org-log-redeadline 'time)

;; Number of days before expiration during which a deadline becomes active.
(setq org-deadline-warning-days 7)

;; Skip deadline prewarning (up to 7 days before the actual deadline) when
;; entry is also scheduled.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 7)

;; Don't show deadlines when the corresponding item is done.
(setq org-agenda-skip-deadline-if-done t)

;; Skip scheduling line if same entry shows because of deadline.
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Don't show scheduled items in agenda when they are done.
(setq org-agenda-skip-scheduled-if-done t)

;; ~8.3 Don't select item by time stamp or -range if it is DONE.
(setq org-agenda-skip-timestamp-if-done t)

;; ;; Show all days between the first and the last date.
;; (setq org-timeline-show-empty-dates t)

;; TODO state to which a repeater should return the repeating task.
(setq org-todo-repeat-to-state "TODO")

;;** 8.4 (info "(org)Clocking work time")

(message "8.4 (org)Clocking work time")

(global-set-key (kbd "C-c C-x C-i") #'org-clock-in)
(global-set-key (kbd "C-c C-x C-j") #'org-clock-goto)
(global-set-key (kbd "C-c C-x C-o") #'org-clock-out)

(defun leuven-helm-org-clock-in (marker)
  "Clock into the item at MARKER"
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (org-clock-in)))

;; Add action "Clock into task" directly from helm-org session
(with-eval-after-load 'helm-org
  (nconc helm-org-headings-actions
         (list (cons "Clock into task" #'leuven-helm-org-clock-in))))

;; The time clocking code for Org mode.
;; (require 'org-clock)                 ;! needed for trying to automatically
                                        ;! re-clock at Emacs startup

;; XXX Under test!
(add-hook 'org-mode-hook
          (lambda ()
            (require 'org-clock)
            (setq org-clock-persist t)
            (org-clock-persistence-insinuate)))

;; Org Clock Configuration.
(with-eval-after-load 'org-clock

  ;; ;; 8.4 Enable clock persistence and resume previous session on startup.
  ;; (setq org-clock-persist t)
  ;; (org-clock-persistence-insinuate)

  ;; Resume clocking task on clock-in if the clock is open.
  (setq org-clock-in-resume t)

  ;; Set history length for clocked tasks.
  (setq org-clock-history-length 35)    ; 1-9A-Z

  ;; 8.4.2 Include current clocking task time in reports.
  (setq org-clock-report-include-clocking-task t)

  ;; 8.4.2 Format duration as hours and minutes (h:mm).
  (setq org-duration-format 'h:mm)      ; Avoid showing days.

  ;; Define clock time cell formats.
  (setq org-clock-total-time-cell-format "%s")
  (setq org-clock-file-time-cell-format "%s")

  ;; Function to clock back into an interrupted task.
  (defun lvn-org-clock-in-interrupted-task ()
    "Clock back into the interrupted task, if one exists."
    (interactive)
    (if (and (not org-clock-resolving-clocks-due-to-idleness)
             (marker-buffer org-clock-marker)
             (marker-buffer org-clock-interrupted-task))
        (org-with-point-at org-clock-interrupted-task
          (org-clock-in nil))
      (org-clock-out)))

  (global-set-key (kbd "C-c C-x C-q") #'lvn-org-clock-in-interrupted-task)

  ;; 8.4.3 Set idle time before resolving open clocks.
  (setq org-clock-idle-time 240)

  ;; Function to switch task state to "STRT" when clocking in.
  (defun lvn--org-switch-to-started (kwd)
    "Switch task state to STRT unless it's already in STRT or in capture mode."
    (if (and kwd
             (not (string-equal kwd "STRT"))
             (not (and (boundp 'org-capture-mode) org-capture-mode)))
        "STRT"
      nil))

  ;; 8.4.3 Set task to todo state STRT while clocking it.
  (setq org-clock-in-switch-to-state 'lvn--org-switch-to-started)

  ;; Prevent automatic clock-out when task is marked DONE.
  (setq org-clock-out-when-done nil)

  ;; Configure mode line clock display.
  (setq org-clock-mode-line-total 'all)

  ;; Alert when planned time is over.
  (setq org-clock-sound "/mnt/c/Windows/Media/Alarm01.wav")
  ;; Use `start-process` for non-blocking sound alerts.

  ;; Default clock display range.
  (setq org-clock-display-default-range 'untilnow)

  ;; Remove zero-time clock entries.
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; ;; When clocking into a task with a clock entry which has not been
  ;; ;; closed, resume the clock from that point.
  ;; (setq org-clock-in-resume t)

  ;; Ask user before clocking out on Emacs exit.
  (defun lvn--org-query-clock-out ()
    "Prompt user to clock out before exiting Emacs."
    (if (and (featurep 'org-clock)
             (funcall 'org-clocking-p)
             (y-or-n-p "You are currently clocking time, clock out? "))
        (org-clock-out)
      t))

  (add-hook 'kill-emacs-query-functions #'lvn--org-query-clock-out)

  )                                   ; with-eval-after-load "org-clock" ends here.

;;** 8.5 (info "(org)Effort estimates")

(message "8.5 (org)Effort estimates")

;; Add an effort estimate on the fly when clocking in.
(defun leuven--org-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Estimated time (H:MM): "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(add-hook 'org-clock-in-prepare-hook #'leuven--org-ask-effort)

;;* 9 (info "(org)Capture - Refile - Archive")

(message "9.1 (org)Capture")

;; 9.1.2 Directory with Org files.
(setq org-directory
      (directory-file-name            ; This function removes the final slash.
       (cond ((file-directory-p "~/org/") "~/org/")
             ((file-directory-p "~/org-files/") "~/org-files/")
             (t "~/"))))

;; 9.1.2 Default target for storing notes.
(with-eval-after-load 'org
  (setq org-default-notes-file          ; Inbox for collecting
                                        ; [Default: "~/.notes"].
        (or (and (file-exists-p (concat org-directory "/inbox.org"))
                 (concat org-directory "/inbox.org"))
            (and (file-exists-p (concat org-directory "/refile.org"))
                 (concat org-directory "/refile.org"))
            (and (file-exists-p (concat org-directory "/notes.org"))
                 (concat org-directory "/notes.org"))
            org-default-notes-file)))

;; 9.1.2 templates for the creation of capture buffers

;; ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")

;; Fast note taking in Org mode (the ultimate capture tool).
(with-eval-after-load 'org-capture

  (add-to-list 'org-capture-templates
               `("t" "Task" entry
                 (file+headline ,org-default-notes-file "Tasks")
                 "* MAYB %^{Task}%?

%i"
                 :empty-lines 1) t)

  (add-to-list 'org-capture-templates
               `("T" "Task in current file" entry
                 (file+headline
                  (buffer-file-name (org-capture-get :original-buffer))
                  "Tasks")
                 "* TODO %?
%U %a %n"
                 :prepend t) t)

  (add-to-list 'org-capture-templates
               `("a" "Appt" entry
                 (file+headline ,org-default-notes-file "Events")
                 "* %^{Appointment}%?
%^T

%i"
                 :empty-lines 1) t)
  ;; TODO Prompt only for date, not time...

  (add-to-list 'org-capture-templates
               `("Z" "Refile me!" entry
                 (function leuven--find-location)
                 "** TODO Put this in some other file\n\n"
                 :prepend t) t)

  (defun leuven--find-location ()
    "Find the Inbox file and navigate to a headline in the current buffer."
    (find-file org-default-notes-file)
    (goto-char (point-min))
    (helm-org-in-buffer-headings)
    (org-forward-heading-same-level 1))

  (add-to-list 'org-capture-templates
               `("m" "Email processing") t)

  (add-to-list 'org-capture-templates
               `("mT" "Create a TODO Action + edit" entry
                 (file+headline ,org-default-notes-file "Messages") ; #+FILETAGS: :mail:
                 "* TODO %^{Creating action}%? (from %:fromname)
%:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From the address <%a>"
                 :empty-lines 1) t)

  (add-to-list 'org-capture-templates
               `("mt" "Create a TODO Action" entry
                 (file+headline ,org-default-notes-file "Messages") ; #+FILETAGS: :mail:
                 "* TODO %:subject%? (from %:fromname)
%:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From the address <%a>"
                 :empty-lines 1
                 :immediate-finish t) t)

  (add-to-list 'org-capture-templates
               `("mn" "Create a note" entry
                 (file+headline ,org-default-notes-file "Notes") ; #+FILETAGS: :mail:
                 "* %:subject%? (from %:fromname)
%:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From the address <%a>"
                 :empty-lines 1
                 :immediate-finish t) t)

  (add-to-list 'org-capture-templates
               `("p" "Phone call" entry
                 (file+headline ,org-default-notes-file "Phone calls")
                 "* %?"
                 :clock-in t
                 :clock-resume t
                 :empty-lines 1) t)

  (add-to-list 'org-capture-templates
               `("i" "Interruption" entry
                 (file ,org-default-notes-file)
                 "A TEMPLATE HERE"
                 :clock-in t
                 :clock-resume t) t)

  ;; Thought.
  (add-to-list 'org-capture-templates
               `("n" "Note" entry
                 (file+headline ,org-default-notes-file "Notes")
                 "* %^{Thought}%?

%i"
                 :empty-lines 1) t)

  ;; Shopping list (stuff to buy).
  (add-to-list 'org-capture-templates
               `("b" "Buy" checkitem
                 (file+headline ,org-default-notes-file "Shopping")) t)

  ;; Add a note to the currently clocked task.
  (add-to-list 'org-capture-templates
               `("c" "Clock sibling" entry
                 (clock)
                 "* %^{Title}
%U
%a

%i") t)

  ;;          ("w" "org-protocol" entry
  ;;           (file ,org-default-notes-file)
  ;;           "* TODO Review %c
  ;; %U"
  ;;           :clock-in t
  ;;           :clock-resume t
  ;;           :immediate-finish t)
  ;;
  ;; ("web-clippings" ?w
  ;;  "* %^{Title} %^g \n  :PROPERTIES:\n  :date: %^t\n  :link: %^{link}\n  :END:\n\n %x %?"
  ;;  "~/org/data.org" "Web Clippings")

  ;; Default `org-capture-templates' key to use.
  (setq org-protocol-default-template-key "w")

  )                                   ; with-eval-after-load "org-capture" ends here.

;; bug when C-c C-l
;; ;; 4.6 Shortcut links.
;; (add-to-list 'org-link-abbrev-alist '(("att" . org-attach-expand-link)))

(with-eval-after-load 'org-protocol
  (add-to-list 'org-safe-remote-resources
               "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))

(message "9.4 (org)Protocols")

;; 9.4 Capture from Firefox (to store links and text).
(with-eval-after-load 'org-protocol

  ;; Map online URL to an existing working file.
  (add-to-list 'org-protocol-project-alist
               '("Worg at http://orgmode.org/worg/"
                 :online-suffix ".html"
                 :working-suffix ".org"
                 :base-url "http://orgmode.org/worg/"
                 :working-directory "~/Public/Repositories/worg/") t))

(with-eval-after-load 'org
  (message "[... Org Refile]")

  (defvar leuven-org-refile-extra-files
    (if (file-exists-p "~/org/")
        (file-expand-wildcards "~/org/*.txt")
      nil)
    "List of extra .txt files to be used as targets for refile commands.")

  ;; 9.5 Any headline with level <= 3 is a target.
  (setq org-refile-targets
        `((nil
           :maxlevel . 4)             ; Current file.
          (,(append org-agenda-files leuven-org-refile-extra-files)
           :maxlevel . 2)))

  ;; Cache refile targets to speed up the process.
  (setq org-refile-use-cache t)

  ;; 9.5 Provide refile targets as paths, including the file name (without
  ;; directory) as level 1 of the path.
  (setq org-refile-use-outline-path 'file)

  ;; 9.5 Allow to create new nodes (must be confirmed by the user) as refile
  ;; targets.
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Refile only within the current buffer.
  (defun leuven-org-refile-within-current-buffer ()
    "Move the entry at point to another heading in the current buffer."
    (interactive)
    (let ((org-refile-targets '((nil :maxlevel . 4))))
      (org-refile)))
  ;; FIXME Add a smart key binding

  ;; Exclude DONE state tasks from refile targets.
  (defun bh/verify-refile-target ()
    "Exclude TODO keywords with a DONE state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  (message "9.6 (org)Archiving")

  ;; 9.6.1 Subtrees should be archived in the current file.
  (setq org-archive-location "::* Archive")

  )

(message "10 (org)Agenda Views")

;;* 10 (info "(org)Agenda Views")

(with-eval-after-load 'org-agenda

  ;; Multiple same-day time stamps in entry make multiple agenda lines.
  (setq org-agenda-skip-additional-timestamps-same-entry nil)

  ;; Show outline path in echo area after line motion (though, may bring some
  ;; slowness).
  (setq org-agenda-show-outline-path t)

  ;; 10.0 Restore the window configuration when exiting the agenda.
  (setq org-agenda-restore-windows-after-quit t)

  ;; ;; Speed up agenda by avoiding to update some text properties.
  ;; (setq org-agenda-ignore-properties '(effort category)) ; org.el

  ;; Normally hide the "maybe" (nice-to-have) things.
  ;; (setq org-agenda-filter-preset '("-MAYB"))

  ;;** 10.1 (info "(org)Agenda files")

  (message "10.1 (org)Agenda files")

  (when (boundp 'org-agenda-files)
    (if (null org-agenda-files)
        (progn
          (message "[No Org agenda files currently found]")
          (sit-for 1.5)) ; Wait for 1.5 seconds
      (message "[Found %s entries in `org-agenda-files']"
               (length org-agenda-files))))

  ;;** 10.2 (info "(org)Agenda dispatcher")

  (message "10.2 (org)Agenda dispatcher")

  ;; Enable sticky agenda: `q' key will bury agenda buffers (instead of
  ;; killing).
  (setq org-agenda-sticky t)

  ;;** 10.3 The (info "(org)Built-in agenda views")

  (message "10.3 (org)Built-in agenda views")

  ;; Default duration for appointments that only have a starting time.
  (setq org-agenda-default-appointment-duration nil)

  ;; ;; Duration of an appointment will add to day effort.
  ;; (setq org-agenda-columns-add-appointments-to-effort-sum t)

  ;; Show dated entries in the global `todo' list.
  (setq org-agenda-todo-ignore-with-date nil)
                                        ;!! tricky setting

  ;; Show entries with a time stamp in the global `todo' list.
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; 10.3.2 Don't show scheduled entries in the global `todo' list.
  (setq org-agenda-todo-ignore-scheduled 'future)
                                        ;!! Tricky setting.
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; 10.3.2 Don't show entries scheduled in the future in the global
  ;; `todo' list (until they are within the warning period).
  (setq org-agenda-todo-ignore-deadlines 'near)
                                        ;!! Tricky setting.
  (setq org-agenda-todo-ignore-deadlines nil)

  ;; 10.3.2 Check also the sublevels of a TODO entry for TODO entries,
  ;; resulting in potentially much longer `todo' lists.
  (setq org-agenda-todo-list-sublevels t)

  ;; 10.3.3 Honor `todo' list `org-agenda-todo-ignore...' options also
  ;; in the `tags-todo' list.
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; 10.3.5 List of extra files to be searched by text search commands
  ;; (C-c a s).
  (setq org-agenda-text-search-extra-files nil) ; org.el

  (defvar leuven-org-search-extra-files nil
    "List of extra files to be searched by custom search commands (`R s' and `R S').")

  ;; Turn on individual word search (for Google addicts).
  (setq org-agenda-search-view-always-boolean t
        org-agenda-search-view-search-words-only t)

  ;; Match part of a word.
  (setq org-agenda-search-view-force-full-words nil)

  ;; Don't search headline for a time-of-day (unwanted side effects).
  (setq org-agenda-search-headline-for-time nil)

  ;; 10.3.6 How to identify stuck projects.
  (setq org-stuck-projects
        '("+LEVEL=2/-DONE"            ; Identify a project.
          ("TODO" "STRT")             ; Todo keywords.
          nil ""))                    ; Tags, regexp.

  ;;** 10.4 (info "(org)Presentation and sorting")

  (message "10.4 (org)Presentation and sorting")

  ;; 10.4 Format specifications for the prefix of items in the agenda views.
  (setq org-agenda-prefix-format
        '((agenda   . " %-11s%i %?-12t") ; Agenda.
          (timeline . " % s")         ; Timeline.
          (todo     . " %i %-12:c")   ; Todo, alltodo.
          (tags     . " %i %-12:c")   ; Tags, tags-todo, stuck.
          (search   . " %i %-12:c"))) ; Search.

  ;; Type "(" in agenda and todo buffers to show category name and task
  ;; length for each task.
  (defvar leuven--org-agenda-show-tasks-details nil)
  (defun leuven-org-agenda-toggle-tasks-details ()
    "Hide/show tasks details (category and time estimate) in agenda views."
    (interactive)
    (if leuven--org-agenda-show-tasks-details
        (progn
          (setq leuven--org-agenda-show-tasks-details nil)
          (setq org-agenda-prefix-format
                '((agenda    . " %-11s%i %?-12t")
                  (timeline  . " % s")
                  (todo      . " ")
                  (search    . " ")
                  (tags      . " "))))
      (setq leuven--org-agenda-show-tasks-details t)
      (setq org-agenda-prefix-format
            '((agenda   . " %-11s%i %-12:c%?-12t%7e ")
              (timeline . " % s")
              (todo     . " %i %-12:c")
              (search   . " %i %-12:c")
              (tags     . " %i %-12:c"))))
    (org-agenda-redo))

  (define-key org-agenda-mode-map
    (kbd "(") #'leuven-org-agenda-toggle-tasks-details)

  ;; Text preceding scheduled items in the agenda view.
  (setq org-agenda-scheduled-leaders
        '("Scheduled  "
          "           "))

  ;; Text preceding item pulled into the agenda by inactive time stamps.
  (setq org-agenda-inactive-leader "[")

  ;; Text preceding deadline items in the agenda view.
  (setq org-agenda-deadline-leaders
        '("Deadline   "
          "In %d d"                   ; Or "%d d left".
          "%d d ago"))

  )                                   ; with-eval-after-load "org-agenda" ends here.

(with-eval-after-load 'org-faces

  ;; Faces for showing deadlines in the agenda.
  (setq org-agenda-deadline-faces
        '((1.0001 . leuven-org-deadline-overdue)
          (0.9999 . leuven-org-deadline-today)
          (0.8571 . leuven-org-deadline-tomorrow) ; = 6/7, see `org-deadline-warning-days'
          (0.0000 . leuven-org-deadline-future)))

  ;; See http://www.dgtale.ch/index.php?option=com_content&view=article&id=52&Itemid=61.

  ;; Org non-standard faces.
  (defface leuven-org-deadline-overdue
    '((t :foreground "#F22659"))
    "Face used to highlight tasks whose due date is in the past.")

  (defface leuven-org-deadline-today
    '((t :weight bold :foreground "#4F4A3D" :background "#FFFFCC"))
    "Face used to highlight tasks whose due date is today.")

  (defface leuven-org-deadline-tomorrow
    '((t :foreground "#40A80B"))
    "Face used to highlight tasks whose due date is tomorrow.")

  (defface leuven-org-deadline-future
    '((t :foreground "#40A80B"))
    "Face used to highlight tasks whose due date is for later."))

(with-eval-after-load 'org-agenda

  ;; ;; 10.4 Column to shift tags to (in agenda items).
  ;; (setq org-agenda-tags-column -132)

  ;; Right-justify tags in the agenda buffer.
  (defun leuven--org-agenda-right-justify-tags ()
    "Justify the tags to the right border of the agenda window."
    (let ((org-agenda-tags-column (- 2 (window-width))))
      (org-agenda-align-tags)))
  (add-hook 'org-agenda-finalize-hook #'leuven--org-agenda-right-justify-tags))

;; 10.4.2 Settings for time grid for agenda display.
(setq org-agenda-time-grid '((daily remove-match)
                             ""
                             (0800 1000 1200 1400 1600 1800 2000)))

;; Recent Org-mode.
(setq org-agenda-time-grid '((daily today remove-match)
                             (0800 1000 1200 1400 1600 1800 2000)
                             "...... " ""))

;; String for the current time marker in the agenda.
(setq org-agenda-current-time-string
      ;; Check if character 25C0 is displayable.
      (if (char-displayable-p ?\u25C0)
          "\u25C0── Right now ─────────────────────────────────────────"
        "<── Right now ─────────────────────────────────────────"))

;; 10.4.3 Sorting structure for the agenda items of a single day.
(setq org-agenda-sorting-strategy   ; custom value
      '((agenda time-up category-up priority-down effort-down)
        ;; (agenda priority-down time-up category-up effort-down)
        (todo category-up priority-down effort-down)
        (tags category-up priority-down effort-down)
        (search category-up)))

;; Show agenda in the current window, keeping all other windows.
(setq org-agenda-window-setup 'current-window)

(defun leuven-org-agenda-change-sorting-strategy (strategy)
  "Change the sorting strategy."
  (interactive (list
                (completing-read "Choose a strategy: "
                                 (mapcar 'cdr (cdr org-sorting-choice))
                                 nil t)))
  ;; adjust the following types as needed - e.g., add 'agenda, etc.
  (org-agenda-check-type t 'todo 'tags 'search)
  (let ((org-agenda-sorting-strategy (list (intern strategy))))
    (org-agenda-redo)))

;;** 10.5 (info "(org)Agenda commands")

(message "10.5 (org)Agenda commands")

;; Get a compact view during follow mode in the agenda.
(defun leuven--compact-follow ()
  "Make the view compact, then show the necessary minimum."
  (ignore-errors
    (save-excursion
      (while (org-up-heading-safe))
      (hide-subtree)))
  (let ((org-show-siblings nil)
        (org-show-hierarchy-above t))
    (org-reveal))
  (save-excursion
    (org-back-to-heading t)
    (show-children)))

;; FIXME When this is enabled, clicking on a clock line from `v c'
;; (log check) does not jump to the right line
;; (add-hook 'org-agenda-after-show-hook #'leuven--compact-follow)

;; 10.5 Number of days to include in overview display.
(setq org-agenda-span 'day)

;; Always start the overview on the current day.
(setq org-agenda-start-on-weekday nil)

;; Format string for displaying dates in the daily/weekly agenda
;; and in the timeline.
(setq org-agenda-format-date
      (concat                           ; "\n"
       "%Y-%m-%d" " %a "
       ;; (make-string (1- (window-width)) (string-to-char "_"))))
       (make-string 65 (string-to-char " "))
       "_"
       ;; (make-string 1 ?\u25AE)
       ))

;; 10.5 Only show clocked entries in agenda log mode (no closed
;; entries, no state changes).
(setq org-agenda-log-mode-items '(clock))

;; 10.5 Parameters for the clocktable in clockreport mode.
(setq org-agenda-clockreport-parameter-plist
      '(:link nil :maxlevel 3 :fileskip0 t))
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 3 :fileskip0 t))

;; 10.5 Definition of what constitutes a clocking problem (overlapping
;; clock entries, clocking gaps).
(setq org-agenda-clock-consistency-checks
      '(:max-duration "10:00"
                      :min-duration 0
                      :max-gap "0:00"
                      :gap-ok-around ("4:00")
                      :default-face
                      ((:weight bold
                                :box "#AAEE77"
                                :foreground "black" :background "#BFFA9E"))
                      :gap-face
                      ((:weight bold
                                :box "#BBDDFF"
                                :foreground "black" :background "#D0EDFF"))))

;; 10.5 Text prepended to the entry text in agenda buffers.
(setq org-agenda-entry-text-leaders "               │ ")

;; 10.5 File to which to add new entries with the `i' key in agenda and
;; calendar (org.el).
(setq org-agenda-diary-file "~/org/diary.org")

;; 10.5? Keep filters from one agenda view to the next.
(setq org-agenda-persistent-filter t)

;; Faces for specific Priorities (#A, #B and #C).
(setq org-priority-faces
      '((?A . (:foreground "#CC0000" :background "#FFE3E3"))
        (?B . (:foreground "#64992C" :background "#EBF4DD"))
        (?C . (:foreground "#64992C" :background "#FFFFFF"))))

;; 10.5 Commands in the agenda buffer.
(defun leuven--org-weekday-p ()
  "Return t if current day is between Monday and Friday."
  (let ((dow (nth 6 (decode-time))))
    (and (> dow 0)
         (< dow 6))))

(defun leuven--org-working-p ()
  "Return t if current time is inside normal working hours.
Currently: 08:30-12:30 and 13:30-17:30."
  (let* ((time (decode-time))
         (hour (nth 2 time))
         (mins (nth 1 time)))
    (and (leuven--org-weekday-p)
         (or (or (and (= hour 8) (>= mins 30))
                 (and (< 8 hour) (< hour 12))
                 (and (= hour 12) (<= mins 30)))
             (or (and (= hour 13) (>= mins 30))
                 (and (< 13 hour) (< hour 17))
                 (and (= hour 17) (<= mins 30)))))))

(defun leuven--org-calling-hours-p ()
  "Return t if current time is inside normal calling hours.
Currently: 08:00-21:59."
  (let* ((hour (nth 2 (decode-time))))
    (and (<= 8 hour) (<= hour 21))))

(defun leuven--org-auto-exclude-function (tag)
  "Exclude certain tags from the agenda based on specific conditions."
  (and (cond
        ((string= tag "personal")
         (with-temp-buffer
           (call-process "/sbin/ifconfig" nil t nil "en0" "inet")
           (goto-char (point-min))
           (not (re-search-forward "inet 192\\.168\\.9\\." nil t))))
        ((or (string= tag "errands")
             (string= tag "call"))
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 8) (> hour 21)))))
       (concat "-" tag)))

(defun leuven--org-auto-exclude-function (tag)
  "Exclude certain tags from the agenda based on specific conditions.

This function is designed to be used as the `org-agenda-auto-exclude-function'.
It ensures that tags like ':inbox:' are never excluded!

TAG is the tag to be considered for exclusion.

Returns a string to be used in the agenda filter.

Examples:
- Exclude 'personal' tag during working hours.
- Exclude 'work' tag outside of working hours.
- Exclude 'errands' and 'call' tags outside of calling hours."
  (and (cond
        ((string= tag "personal")
         (leuven--org-working-p))
        ((string= tag "work")
         (not (leuven--org-working-p)))
        ((or (string= tag "errands")
             (string= tag "call"))
         (not (leuven--org-calling-hours-p))))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'leuven--org-auto-exclude-function)

;; Make the block agenda more compact (no agenda span name, no week number, no
;; separator line).
(setq org-agenda-compact-blocks t)
(setq org-agenda-compact-blocks nil)

(setq org-agenda-block-separator
      (propertize (make-string 132 (string-to-char "_"))
                  'face '(:foreground "#59ACE2"))) ; lighter version with #C0E2F4

;;** 10.6 (info "(org)Custom agenda views")

(message "10.6 (org)Custom agenda views")

(with-eval-after-load 'org-agenda
  (let ((leuven-org-agenda-views
         (concat lvn--directory "org-leuven-agenda-views.el")))
    (when (file-exists-p leuven-org-agenda-views)
      (load-file leuven-org-agenda-views))))
                                        ; with-eval-after-load "org-agenda" ends here.

(defun leuven-org-agenda-git-root-todo-list ()
  "Produce an Org agenda view of unscheduled TODO items in the Git root directory."
  (interactive)
  (let* ((git-root (vc-git-root default-directory))
         (org-files (directory-files-recursively git-root "\\.\\(org\\|txt\\)$"))
         (org-agenda-sorting-strategy '(todo-state-up priority-down))
         (org-agenda-overriding-header
          (format "Unscheduled TODO items in directory: %s" git-root))
         (org-agenda-sticky nil)
         (org-agenda-skip-scheduled-if-done t)
         (org-agenda-skip-deadline-if-done t)
         (org-agenda-todo-ignore-scheduled 'future))
    (message "[%s...]" org-agenda-overriding-header)
    (org-todo-list)))

;; "TODO list" without asking for a directory.
(global-set-key (kbd "<M-S-f6>") #'leuven-org-agenda-git-root-todo-list)

;;** 10.7 (info "(org)Exporting Agenda Views")

(message "10.7 (org)Exporting Agenda Views")

;; 10.7 Alist of variable/value pairs that should be active during agenda
;; export.
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)      ; 2?
        (ps-landscape-mode t)
        ;; (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)))

;;** 10.8 (info "(org)Agenda column view")

(message "10.8 (org)Agenda column view")

;; 10.8 Default column format, if no other format has been defined.
(setq org-columns-default-format
      ;; "%65ITEM(Task) %DEADLINE(Due Date) %PRIORITY %6CLOCKSUM(Spent) %6Effort(Estim.){:}")
      ;; "%1BLOCKED %4TODO %CATEGORY %5Effort{:} %50ITEM %20TAGS %21ALLTAGS")
      ;; "%65ITEM(Task) %4TODO %PRIORITY %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
      ;; "%65ITEM(Task) %4TODO %PRIORITY %20TAGS %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
      ;; "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
      "%60ITEM(Details) %5PRIORITY(Prio) %14SCHEDULED(Scheduled) %15TAGS(Context) %7TODO(To Do) %6CLOCKSUM(Clock) %5Effort(Effort){:} ")

;; DUPLICATE Obey `eval' variables -- RISKY!
(setq enable-local-eval t)

(with-eval-after-load 'org-agenda

  (defadvice org-agenda-switch-to
      (after leuven-org-agenda-switch-to activate)
    "Recenter after jumping to the file which contains the item at point."
    (recenter))

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (remove-text-properties (point-min) (point-max)
                                      '(mouse-face t))))

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (org-do-emphasis-faces (point-max)))))

  (defun leuven-org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))

  (define-key org-agenda-mode-map
    (kbd "Z") #'leuven-org-agenda-mark-done-and-add-followup)

  (defun leuven-org-agenda-new ()
    "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-switch-to)
    (org-capture 0))

  ;; ;; New key assignment (overrides `org-agenda-next-item').
  ;; (define-key org-agenda-mode-map (kbd "N") #'leuven-org-agenda-new)

  )

;;* 11 (info "(org)Markup")

(message "11 (org)Markup")

(with-eval-after-load 'org-faces

  ;; Add a face to #+begin_quote and #+begin_verse blocks.
  (setq org-fontify-quote-and-verse-blocks t))

(with-eval-after-load 'org
  (message "[... Org Markup]")

  ;;??? Change the face of a headline (as an additional information) if it is
  ;; marked DONE (to face `org-headline-done').
  (setq org-fontify-done-headline t)

  ;; 11.1 Hide the emphasis marker characters.
  (setq org-hide-emphasis-markers t)  ; Impact on table alignment!

  (defun leuven-org-insert-image-or-take-screenshot (name)
    "Insert a link to an already existing image, or else to a screenshot.
The screenshot is either taken to the given non-existing file name,
or added into the given directory, defaulting to the current one."
    ;; FIXME: Should limit to '("pdf" "jpeg" "jpg" "png" "ps" "eps")
    ;; which is org-export-latex-inline-image-extensions.
    (interactive "GImage name? ")
    (when (file-directory-p name)
      (setq name (concat
                  (make-temp-name
                   (expand-file-name
                    (concat (file-name-as-directory name)
                            (subst-char-in-string
                             "." "-"
                             (file-name-sans-extension
                              (file-name-nondirectory
                               (buffer-file-name)))))))
                  ".png")))
    (unless (file-exists-p name)
      (if (file-writable-p name)
          (progn
            (message "[Taking screenshot into %s]" name)
            (call-process "import" nil nil nil name)
            (message "[Taking screenshot...done]"))
        (error "Cannot create image file")))
    (insert (concat "[[" name "]]"))
    (org-display-inline-images))

  ;; Hide the brackets marking macro calls.
  (setq org-hide-macro-markers t)

  (defun org-macro-insert ()
    "XXX"
    (interactive)
    (let* ((macros (org-macro--collect-macros))
           (macro (completing-read "Insert macro: " (mapcar 'car macros)))
           (args (string-match "$[[:digit:]]" (cdr (assoc macro macros))))
           pos)
      (insert (format  "{{{%s" macro))
      (when args (insert "(") (setq pos (point)) (insert ")"))
      (insert "}}}")
      (when pos (goto-char pos)))))

;; 11.7.1 Define user entities to produce special characters.
(with-eval-after-load 'org-entities

  (add-to-list 'org-entities-user
               '("ok"
                 ;; \definecolor{checkmark}{HTML}{1FAC21}
                 "{\\color{checkmark}\\ding{51}}" nil
                 "<font color='green'>&#x2714;</font>"
                 "OK"
                 "OK" "✔"))

  (add-to-list 'org-entities-user
               '("nok"
                 ;; \usepackage{pifont}
                 "{\\color{red}\\ding{55}}" nil
                 "<font color='red'>&#x2718;</font>"
                 "NOK"
                 "NOK" "✘")))

;; 11.7.2 Interpret "_" and "^" for display when braces are used.
(setq org-use-sub-superscripts '{})

;; ;; 11.7.3 Convert LaTeX fragments to images when exporting to HTML (using MathJax).
;; (setq org-export-with-latex t)

;; Highlight LaTeX and related syntax.
(setq org-highlight-latex-and-related '(latex script entities))

;; Show entities as UTF8 characters.
(setq org-pretty-entities t)          ; emsp, etc.

;; ;; Pretty entity display doesn't include formatting sub/superscripts.
;; (setq org-pretty-entities-include-sub-superscripts nil)

;;* 12 (info "(org)Exporting")

;; Bind the exporter dispatcher to a key sequence.
(with-eval-after-load 'org
  (message "[... Org Exporting]")

  ;; Libraries in this list will be loaded once the export framework is needed.
  (setq org-export-backends '(ascii html icalendar latex odt md))

  (define-key org-mode-map (kbd "C-c C-e") #'org-export-dispatch))

(with-eval-after-load 'org

  (defun org-save-buffer-and-do-related ()
    "Save buffer, execute/tangle code blocks, and export to HTML/PDF."
    (interactive)
    (let* ((orgfile (buffer-file-name))
           (base-name (file-name-base orgfile))
           (mdfile (concat base-name ".md"))
           (htmlfile (concat base-name ".html"))
           (texfile (concat base-name ".tex"))
           (pdffile (concat base-name ".pdf")))
      (save-buffer)                     ; See other commands in
                                        ; `before-save-hook':
                                        ; `org-update-all-dblocks'
                                        ; `org-table-iterate-buffer-tables'.
      (when (derived-mode-p 'org-mode)
        (measure-time "Restarted Org mode" (org-mode-restart))
                                        ; Update information from one of the
                                        ; special #+KEYWORD lines
                                        ; (like `C-c C-c')

        ;; Linting for Org documents.
        (when (try-require "org-lint")
          (measure-time "Linted Org mode"
                        (if (org-lint)
                            (progn
                              (message "[You should run `org-lint'!!!]")
                              (beep)
                              (sit-for 1)))))

        ;; ;; Update the results in the Org buffer.
        ;; (org-babel-execute-buffer)    ; In this case, better than
        ;;                               ; (add-hook 'org-export-first-hook
        ;;                               ;           #'org-babel-execute-buffer):
        ;;                               ; executed only once for both exports.

        ;; It'd make sense to eval all code blocks which have :cache yes or :exports
        ;; results or both... And, before that, to delete all code block results!?
        ;; Well, almost all code blocks: not the ones of "cached" blocks (they may have
        ;; taken a long time to be computed, or may not be computable another time), nor
        ;; the ones with a caption on the results block...

        (measure-time "Buffer saved"
                      (let ((before-save-hook nil))
                        (save-buffer)))
        (measure-time "Buffer tangled"
                      (org-babel-tangle))
        (when (file-exists-p mdfile)
          (if (file-newer-than-file-p orgfile mdfile)
              (measure-time "Buffer exported to Markdown"
                            (org-md-export-to-markdown))
            (message "[Markdown is up to date with Org file]")))
        (when (file-exists-p htmlfile)
          (if (file-newer-than-file-p orgfile htmlfile)
              (measure-time "Buffer exported to HTML"
                            (org-html-export-to-html))
            (message "[HTML is up to date with Org file]")))
        (when (or (file-exists-p texfile) (file-exists-p pdffile))
          (if (or (and (file-exists-p pdffile)
                       (file-newer-than-file-p orgfile pdffile))
                  (and (file-exists-p texfile)
                       (not (file-exists-p pdffile))))
                                        ; Previous PDF export failed.
              (measure-time "Buffer exported to PDF LaTeX"
                            (if (string-match "^#\\+BEAMER_THEME: " (buffer-string))
                                (org-beamer-export-to-pdf)
                              (org-latex-export-to-pdf)))
            (message "[PDF is up to date with Org file]")))
        (beep))))

  (define-key org-mode-map (kbd "<f9>") #'org-save-buffer-and-do-related))

;;** 12.2 (info "(org)Export options")

(message "12.2 (org)Export options")

;; Org generic export engine.
(with-eval-after-load 'ox

  ;; 12.3 Don't insert a time stamp into the exported file.
  (setq org-export-time-stamp-file nil)

  ;; 13.1.5 Export all drawers (including properties).
  ;; (setq org-export-with-drawers t)

  ;; Default language of HTML export (see `org-export-language-setup' XXX).
  (setq org-export-default-language "en")

  ;; Include priority cookies in export.
  (setq org-export-with-priority t)

  ;; Activate smart quotes during export (convert " to \og, \fg in French).
  (setq org-export-with-smart-quotes t) ; curly quotes in HTML

  ;; Interpret "_" and "^" for export when braces are used.
  (setq org-export-with-sub-superscripts '{})

  ;; Allow #+BIND to define local variable values for export.
  (setq org-export-allow-bind-keywords t)

  ;; ;; Exported stuff will not be pushed onto the kill ring.
  ;; (setq org-export-copy-to-kill-ring nil) ; new default since 2014-04-17

  ;; ;; Export and publishing commands will run in background.
  ;; (setq org-export-in-background t)

  ;; ;; Use a non-intrusive export dispatcher.
  ;; (setq org-export-dispatch-use-expert-ui t)

  ;; Export snippet translations.
  (add-to-list 'org-export-snippet-translation-alist
               '("h" . "html"))
  (add-to-list 'org-export-snippet-translation-alist
               '("l" . "latex"))
  (add-to-list 'org-export-snippet-translation-alist
               '("b" . "beamer"))

  )                                   ; with-eval-after-load "ox" ends here.

(defmacro by-backend (&rest body)
  `(case org-export-current-backend ,@body))

;;** 12.3 Export settings

(setq org-export-exclude-tags '("noexport" "crypt"))

;;** 12.5 (info "(org)HTML export")

;; Org HTML export engine.
(with-eval-after-load 'ox-html

  (setq org-html-checkbox-type 'unicode)

  ;; Output type to be used by htmlize when formatting code snippets.
  (setq org-html-htmlize-output-type 'css)

  ;; ;; URL pointing to a CSS file defining text colors for htmlized Emacs
  ;; ;; buffers.
  ;; (setq org-org-htmlized-css-url "style.css")

  ;; ;; XML declaration.
  ;; (setq org-html-xml-declaration
  ;;       '(("html" . "<!-- <xml version=\"1.0\" encoding=\"%s\"> -->")
  ;;         ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
  ;;         ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

  ;; Coding system for HTML export.
  (setq org-html-coding-system 'utf-8)

  ;; ;; Format for the HTML postamble.
  ;; (setq org-html-postamble
  ;;       "  <div id=\"footer\"><div id=\"copyright\">\n    &copy; %d %a\n  </div></div>")

  ;; Disable validation link.
  (setq org-html-validation-link nil)

  ;; ;; Set the creator string to "Emacs Org Mode".
  ;; (setq org-html-creator-string "Emacs Org Mode")

  ;; 13.1.5 Don't include the JavaScript snippets in exported HTML files.
  (setq org-html-head-include-scripts nil)

  ;; 12.5.9 Turn inclusion of the default CSS style off.
  (setq org-html-head-include-default-style nil)

  ;; HTML checkbox output.
  (defun leuven--checkbox-filter (item backend info)
    "XXX"
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string
       "\\`.*\\(<code>\\[\\(X\\|&#xa0;\\|-\\)\\]</code>\\).*$"
       (lambda (rep)
         (let ((check (match-string 2 rep)))
           (cond ((equal check "X") "&#x2611;")
                 ((equal check "-") "&#x2610;")
                 (t "&#x2610;"))))
       item
       nil nil 1)))
  (add-to-list 'org-export-filter-item-functions
               'leuven--checkbox-filter)

  )                                   ; with-eval-after-load "ox-html" ends here.

;;** (info "(emacs-goodies-el)htmlize")

(message "(emacs-goodies-el)htmlize")

;; HTML-ize font-lock buffers.
(autoload 'htmlize-buffer "htmlize"
  "Convert BUFFER to HTML, preserving colors and decorations." t)
(autoload 'htmlize-region "htmlize"
  "Convert the region to HTML, preserving colors and decorations." t)
(autoload 'htmlize-file "htmlize"
  "Load FILE, fontify it, convert it to HTML, and save the result." t)

(with-eval-after-load 'htmlize

  ;; Output type of generated HTML.
  (setq htmlize-output-type 'css)

  ;; XXX Override output type `inline-css' used for htmlizing a region.
  (defun htmlize-region-for-paste (beg end)
    "Htmlize the region and return just the HTML as a string.
This forces the `css' style and only returns the HTML body, but without the
BODY tag.  This should make it useful for inserting the text to another HTML
buffer."
    (let* ((htmlize-output-type 'css)  ; Was `inline-css'.
           (htmlbuf (htmlize-region beg end)))
      (unwind-protect
          (with-current-buffer htmlbuf
            (buffer-substring
             (plist-get htmlize-buffer-places 'content-start)
             (plist-get htmlize-buffer-places 'content-end)))
        (kill-buffer htmlbuf))))

  ;; Charset declared by the resulting HTML documents.
  (setq htmlize-html-charset "utf-8")

  ;; Non-ASCII characters (codes in the 128-255 range) are copied to
  ;; HTML without modification -- if your HTML is in Unicode.
  (setq htmlize-convert-nonascii-to-entities nil)

  ;; Key binding.
  (global-set-key (kbd "M-P") #'htmlize-buffer)

  )                                   ; with-eval-after-load "htmlize" ends here.

;; Quick print preview (to Web browser) with `htmlize-view-buffer'.
(autoload 'htmlize-view-buffer "htmlize-view"
  "Convert buffer to html preserving faces and view in web browser." t)

;; Same key binding as Org export to HTML (open in browser).
(global-set-key (kbd "C-c C-e h o") #'htmlize-view-buffer)

;; View current buffer as html in web browser.
(with-eval-after-load 'htmlize-view

  ;; Add "Quick Print" entry to file menu.
  (htmlize-view-add-to-files-menu))

;;** 12.6 (info "(org)LaTeX and PDF export")

(message "12.6 (org)LaTeX and PDF export")

;; LaTeX back-end.
(with-eval-after-load 'ox-latex

  ;; Markup for TODO keywords and for tags, as a printf format.
  (defun leuven--org-latex-format-headline
      (todo todo-type priority text tags &optional info)
    "Default function for formatting the headline's text."
    (concat (when todo
              (format "{%s\\textbf{\\textsc{\\textsf{%s}}}} "
                      (cond ((equal todo-type 'todo) "\\color{red}")
                            ((equal todo-type 'done) "\\color{teal}")
                            (t "\\color{gray}"))
                      todo))
            (when priority
              (format "\\framebox{\\#%c} " priority))
            text
            (when tags
              (format "\\hfill{}\\fbox{\\textsc{%s}}"
                      ;; XXX source of "undefined control sequence"?
                      (mapconcat 'identity tags ":")))))

  ;; Function for formatting the headline's text.
  (setq org-latex-format-headline-function
        'leuven--org-latex-format-headline)

  ;; Default width for images.
  (setq org-latex-image-default-width ".75\\linewidth")

  ;; Format string for links with unknown path type.
  (setq org-latex-link-with-unknown-path-format "\\colorbox{red}{%s}")

  ;; Default process to convert LaTeX fragments to image files.
  ;; (setq org-preview-latex-default-process 'imagemagick)

  (defun leuven--change-pdflatex-program (backend)
    "Automatically run XeLaTeX, if asked, when exporting to LaTeX."

    (when (equal org-export-current-backend "latex")

      (let* ((org-latex-pdf-engine-full-path
              (cond ((string-match "^#\\+LATEX_CMD: xelatex" (buffer-string))
                     (or (executable-find "xelatex")
                         (error "Please install XeLaTeX.")))
                    (t
                     (or (executable-find "pdflatex")
                         (error "Please install PDFLaTeX.")))))

             (org-latex-pdf-command
              (cond ((executable-find "latexmk")
                     "latexmk")
                    (t
                     (file-name-base org-latex-pdf-engine-full-path))))
                                          ; "xelatex" or "pdflatex".

             (latex-file
              (cond ((string-match "^/usr/bin/" org-latex-pdf-engine-full-path)
                     "$(cygpath -m %f)")
                    (t
                     "%f"))))

        (message "[LaTeX engine: %s]" org-latex-pdf-engine-full-path)
        (message "[LaTeX command: %s]" org-latex-pdf-command)

        (setq org-latex-pdf-process
              (cond ((equal org-latex-pdf-command "latexmk")
                     `(;; "echo f = %f" "echo quotedf = '%f'" "echo cygpath = $(cygpath %f)"
                       "latexmk --version"
                       ,(concat "latexmk -cd -f -pdf -pdflatex=" (file-name-base org-latex-pdf-engine-full-path) " " latex-file
                                " && latexmk -c"))) ; Clean up all nonessential files.
                    ((equal org-latex-pdf-command "xelatex")
                     `(,(concat "xelatex -interaction=nonstopmode -output-directory=%o " latex-file)
                       ,(concat "xelatex -interaction=nonstopmode -output-directory=%o " latex-file)
                       ,(concat "xelatex -interaction=nonstopmode -output-directory=%o " latex-file)))
                    (t
                     `(,(concat "pdflatex -interaction=nonstopmode -output-directory=%o " latex-file)
                       ,(concat "pdflatex -interaction=nonstopmode -output-directory=%o " latex-file)
                       ,(concat "pdflatex -interaction=nonstopmode -output-directory=%o " latex-file)))))
        (message "[Export command: %S]" org-latex-pdf-process)
        )))

  ;; Hook run before parsing an export buffer.
  (add-hook 'org-export-before-parsing-hook #'leuven--change-pdflatex-program)

  ;; Export source code using `listings' (instead of `verbatim').
  (setq org-latex-listings t)

  ;; 12.6.2 Default packages to be inserted in the header.
  ;; Add the `listings' package (for fontified source code) to the end of the
  ;; list.
  (push '("" "listings") org-latex-packages-alist)

  ;; Include the `xcolor' package for colored source code.
  (add-to-list 'org-latex-packages-alist '("" "xcolor") t)

  ;; XXX To sort out...
  (setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                      (code . "\\lstinline|%s|")
                                      (italic . "\\emph{%s}")
                                      (strike-through . "\\sout{%s}")
                                      (underline . "\\uline{%s}")
                                      (verbatim . "\\verb|%s|")))

  ;; Filter for no-break spaces.
  (defun leuven--latex-filter-nbsp (text backend info)
    "Convert no-break spaces when exporting to LaTeX/Beamer."
    (when (memq backend '(latex beamer))
      (replace-regexp-in-string " " "~" text)))

  (add-to-list 'org-export-filter-plain-text-functions
               'leuven--latex-filter-nbsp)

  ;; Include the `babel' package for language-specific hyphenation and
  ;; typography.
  (add-to-list 'org-latex-packages-alist '("french" "babel") t)

  (defun leuven--change-pdflatex-packages (backend)
    "Automatically select the LaTeX packages to include (depending on PDFLaTeX
vs. XeLaTeX) when exporting When exporting to LaTeX."

    ;; Unconditionally remove `inputenc' from all the default packages.
    (setq org-latex-packages-alist
          (delete '("AUTO" "inputenc" t)
                  org-latex-packages-alist))

    ;; Unconditionally remove `fontenc' from all the default packages.
    (setq org-latex-packages-alist
          (delete '("T1" "fontenc" t)
                  org-latex-packages-alist))

    ;; Unconditionally remove `textcomp' from all the default packages.
    (setq org-latex-packages-alist
          (delete '("" "textcomp" t)
                  org-latex-packages-alist))

    (if (string-match "^#\\+LATEX_CMD: xelatex" (buffer-string))
        ;; Packages to include when XeLaTeX is used.
        (setq org-export-latex-packages-alist
              '(("" "fontspec" t)
                ("" "xunicode" t)
                ;; Add here things like `\setmainfont{Georgia}'.
                ))

      ;; Packages to include when PDFLaTeX is used.
      (setq org-export-latex-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1" "fontenc" t)
              ("" "textcomp" t))))

    ;; Packages to always include.
    (add-to-list 'org-export-latex-packages-alist
                 '("frenchb" "babel") t))

  ;; Hook run before parsing an export buffer.
  (add-hook 'org-export-before-parsing-hook #'leuven--change-pdflatex-packages)

  ;; 12.6.5 Default position for LaTeX figures.
  (setq org-latex-default-figure-position "!htbp")

  (defun leuven--org-export-ignore-headlines (data backend info)
    "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
    (org-element-map data 'headline
      (lambda (object)
        (when (member "ignore" (org-element-property :tags object))
          (let ((level-top (org-element-property :level object))
                level-diff)
            (mapc (lambda (el)
                    ;; Recursively promote all nested headlines.
                    (org-element-map el 'headline
                      (lambda (el)
                        (when (equal 'headline (org-element-type el))
                          (unless level-diff
                            (setq level-diff (- (org-element-property :level el)
                                                level-top)))
                          (org-element-put-property el
                                                    :level (- (org-element-property :level el)
                                                              level-diff)))))
                    ;; Insert back into parse tree.
                    (org-element-insert-before el object))
                  (org-element-contents object)))
          (org-element-extract-element object)))
      info nil)
    data)

  (add-hook 'org-export-filter-parse-tree-functions
            #'leuven--org-export-ignore-headlines)

  )                                   ; with-eval-after-load "ox-latex" ends here.

;; 12.6.6 Beamer class export.
;; (require 'ox-beamer)
(with-eval-after-load 'ox-beamer

  ;; Default title of a frame containing an outline.
  (setq org-beamer-outline-frame-title "Plan")) ; [default: "Outline"]

(with-eval-after-load 'ox-odt

  ;; Convert "odt" format to "doc" format.
  (setq org-odt-preferred-output-format "doc")

  (when lvn--cygwin-p
    (setcdr (assoc "LibreOffice" org-odt-convert-processes)
            "soffice --headless --convert-to %f%x --outdir \"$(cygpath -m %d)\" \"$(cygpath -m %i)\"")))

;; major mode for editing Markdown-formatted text.
(with-eval-after-load 'markdown-mode-autoloads
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;;* 13 (info "(org)Publishing")

(message "13 (org)Publishing")

(with-eval-after-load 'ox-publish

  ;; Show message about files *not* published.
  (setq org-publish-list-skipped-files nil)

  ;; ;; 13.2 Always publish all files.
  ;; ;; (do not use time stamp checking for skipping unmodified files)
  ;; (setq org-publish-use-timestamps-flag nil)

  ;; 13.4 Force publishing all files.
  (defun leuven-org-publish-all-force ()
    "XXX"
    (interactive)
    (org-publish-all t)))

;;* 14 (info "(org)Working With Source Code")

(with-eval-after-load 'ob-core

  ;; Make the images in the Emacs buffer automatically refresh after
  ;; execution.

  ;; (add-hook 'org-babel-after-execute-hook
  ;;           (lambda ()
  ;;             (org-display-inline-images nil t))) ; DOESN'T WORK!
  ;;                                       ; More efficient with refresh == t.

  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

;;** 14.2 (info "(org)Editing source code")

(message "14.2 (org)Editing source code")

(with-eval-after-load 'org-src

  ;; Mapping languages to their major mode (for editing the source code block
  ;; with `C-c '') -- when the language name doesn't match exactly the
  ;; language mode.
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("js" . js2)))

;; Display the source code edit buffer in the current window, keeping all
;; other windows.
(setq org-src-window-setup 'current-window)

;; FIXME Bind this to the correct keys.
(defun leuven-org-babel-expand-src-block ()
  "XXX"
  (interactive)
  (let ((org-src-window-setup 'reorganize-frame))
    (org-babel-expand-src-block)))

;; Indent the content of a source code block.
(setq org-edit-src-content-indentation 2)

;; Fontify code in code blocks (highlight syntax in the Org buffer).
(setq org-src-fontify-natively t)       ;! Create overlay
                                        ;! `org-block-background' and remove
                                        ;! text property `org-block'.

;; Preserve spaces and `TAB' characters in source code blocks.
(setq org-src-preserve-indentation t) ; Or add a `-i' flag to you source block.

;; Same effect for `TAB' as in the language major mode buffer (indenting
;; properly when hitting the `TAB' key).
(setq org-src-tab-acts-natively t)


;; (with-eval-after-load 'org
;;   (message "[... Org Editing source code]")
;;
;;   ;; Allow indent region in the code edit buffer (according to language).
;;   (defun leuven-org-indent-region (&optional arg)
;;     (interactive "P")
;;     (or (org-babel-do-key-sequence-in-edit-buffer (kbd "C-M-\\"))
;;         (indent-region arg)))
;;
;;   ;; Make `C-c C-v C-x C-M-\' more convenient.
;;   (define-key org-mode-map (kbd "C-M-\\") #'leuven-org-indent-region))

;; Prevent auto-filling in src blocks.
(setq org-src-prevent-auto-filling t)

;; ;; with-eval-after-load...
;; (add-hook 'org-src-mode-hook
;;           (lambda ()
;;             (define-key org-src-mode-map (kbd "<f2>") #'org-edit-src-save)))

(defvar only-code-overlays nil
  "Overlays hiding non-code blocks.")
(make-variable-buffer-local 'only-code-overlays)

(defun hide-non-code ()
  "Hide non-code-block content of the current Org mode buffer."
  (interactive)
  (add-to-invisibility-spec '(non-code))
  (let (begs ends)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (push (match-beginning 5) begs)
        (push (match-end 5)       ends))
      (map 'list (lambda (beg end)
                   (let ((ov (make-overlay beg end)))
                     (push ov only-code-overlays)
                     (overlay-put ov 'invisible 'non-code)))
           (cons (point-min) (reverse ends))
           (append (reverse begs) (list (point-max)))))))

(defun show-non-code ()
  "Show non-code-block content of the current Org mode buffer."
  (interactive)
  (mapc 'delete-overlay only-code-overlays))

(with-eval-after-load 'org
  (defun lvn-org-copy-current-code-block ()
    "Copy the contents of the current Org mode code block to the kill ring.
A code block in Org mode is identified by the \"#+begin_src\" and \"#+end_src\" markers.
This command copies the code block contents to the kill ring, making it available for pasting.
It does not actually delete or kill the code block.
This function is intended for use within Org mode buffers."
    (interactive)
    (if (not (derived-mode-p 'org-mode))
        (message "[Not in an Org mode buffer]")
      (let ((block-start (save-excursion
                           (re-search-backward "^[ \t]*#\\+begin_src[ \t]+\\([a-zA-Z0-9]+\\)?" nil t)
                           (forward-line)
                           (point)))
            (block-end (save-excursion
                         (re-search-forward "^[ \t]*#\\+end_src" nil t)
                         (beginning-of-line)
                         (point))))
        (copy-region-as-kill block-start block-end)
        (message "[Code block copied to kill ring]"))))

  ;; Copy current code block.
  (define-key org-mode-map (kbd "H-w") #'lvn-org-copy-current-code-block))

;;** 14.5 (info "(org)Evaluating code blocks")

(message "14.5 (org)Evaluating code blocks")

;; I don't want to execute code blocks with `C-c C-c' (evaluate code
;; block only with `C-c C-v e').
(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

;; Languages for which Org-babel will raise literate programming errors when
;; noweb references can not be resolved.

(with-eval-after-load 'ob-core
  (add-to-list 'org-babel-noweb-error-langs "emacs-lisp"))

(with-eval-after-load 'ob-exp
  ;; Template used to export the body of code blocks.
  (setq org-babel-exp-code-template
        ;; (concat "\n=%name=:\n"
        org-babel-exp-code-template)
  ;; )
  )

;; Keep lower-case.
(setq org-babel-results-keyword "results")

;;** 14.7 (info "(org)Languages")

(message "14.7 (org)Languages")

;; FIXME Test executable-find (of Rterm, gnuplot, ruby, etc.) before
;; setting language to yes...

(with-eval-after-load 'org
  (message "[... Org Languages]")

  ;; Configure Babel to support most languages.
  (add-to-list 'org-babel-load-languages '(R        . t)) ; Requires R and ess-mode.
  (add-to-list 'org-babel-load-languages '(awk      . t))
  (add-to-list 'org-babel-load-languages '(ditaa    . t)) ; Sudo aptitude install openjdk-6-jre.
  (add-to-list 'org-babel-load-languages '(dot      . t))
  (add-to-list 'org-babel-load-languages '(java     . t))
  (add-to-list 'org-babel-load-languages '(latex    . t)) ; Shouldn't you use #+begin/end_latex blocks instead?
  (add-to-list 'org-babel-load-languages '(makefile . t))
  (add-to-list 'org-babel-load-languages '(org      . t))
  (add-to-list 'org-babel-load-languages '(python   . t))
  (add-to-list 'org-babel-load-languages '(shell    . t)) ; Org mode 8.2 (Emacs 26.1).
  (add-to-list 'org-babel-load-languages '(sql      . t))

  (org-babel-do-load-languages        ; Loads org, gnus-sum, etc...
   'org-babel-load-languages org-babel-load-languages)

  ;; ;; Don't use getline for command-line editing and assert interactive use.
  ;; (setq org-babel-R-command
  ;;       (concat org-babel-R-command " --ess"))

  ;; Accented characters on graphics.
  (setq org-babel-R-command
        (concat org-babel-R-command " --encoding=UTF-8"))

  ;; R commands are displayed in the process buffer.
  (setq org-babel-R-eval-visibly t)   ; XXX Under test

  ;; Check for the support of (inline) source block languages.
  (defun org-src-block-check ()
    "XXX"
    (interactive)
    (org-element-map (org-element-parse-buffer)
        '(src-block inline-src-block)
      (lambda (sb)
        (let ((language (org-element-property :language sb)))
          (cond ((null language)
                 (error "Missing language at line %d in %s"
                        (line-number-at-pos
                         (org-element-property :post-affiliated sb))
                        (buffer-name)))
                ;; ((and (not (assoc-string language org-babel-load-languages))
                ;;       (not (assoc-string language org-src-lang-modes))
                ;;       ;; (locate-library (concat language "-mode")) ; would allow `sh-mode'
                ;;       )
                ;;                       ; XXX This should be stricter: must be
                ;;                       ; in org-babel-load-languages for
                ;;                       ; evaluated code blocks. Must be in both
                ;;                       ; other cases for edited code blocks.
                ;;  (error "Unknown language `%s' at line %d in `%s'"
                ;;         language
                ;;         (line-number-at-pos
                ;;          (org-element-property :post-affiliated sb))
                ;;         (buffer-name)))
                ))))

    ;; (message "[Source blocks checked in %s]"
    ;;          (buffer-name (buffer-base-buffer)))
    )

  (add-hook 'org-mode-hook #'org-src-block-check t))
                                        ; Place this at the end to ensure that
                                        ; errors do not stop applying other
                                        ; functions in the `org-mode-hook' (such
                                        ; as switching the dictionary).

;;** 14.6 (info "(org)Library of Babel")

(message "14.6 (org)Library of Babel")

(with-eval-after-load 'org

  ;; Load the NAMED code blocks defined in Org mode files into the library of
  ;; Babel (global `org-babel-library-of-babel' variable).
  (let ((lob-file (concat (file-name-directory (locate-library "org"))
                          "../doc/library-of-babel.org")))
    (when (file-exists-p lob-file)
      (org-babel-lob-ingest lob-file))))

(message "14.11 (org)Key bindings and useful functions")

(with-eval-after-load 'ob-core

  (defadvice org-babel-next-src-block
      (after leuven-org-babel-next-src-block activate)
    "Recenter after jumping to the next source block."
    (recenter))

  (defadvice org-babel-previous-src-block
      (after leuven-org-babel-previous-src-block activate)
    "Recenter after jumping to the previous source block."
    (recenter)))

;;* 15 (info "(org)Miscellaneous")

;; From Dan Davison.
(defun lvn-switch-to-org-scratch ()
  "Switch to a temporary Org buffer. If the region is active, insert its contents."
  (interactive)
  (let ((contents (and (use-region-p)
                       (buffer-substring (region-beginning)
                                         (region-end)))))
    (find-file "/tmp/org-scratch.org")
    (when contents
      (insert contents))))

(defun lvn-org-check-property-drawers ()
  "Check for erroneous 'PROPERTIES' drawers in Org mode headlines."
  (interactive)
  (org-element-map (org-element-parse-buffer 'element) 'headline
    (lambda (h)
      (and (org-element-map h 'drawer
             (lambda (d) (equal (org-element-property :name d) "PROPERTIES"))
             nil t 'headline)
           (let ((begin (org-element-property :begin h)))
             (message "[Entry with erroneous properties drawer at %d]" begin)
             begin)))))

(defun org-repair-property-drawers ()
  "Fix properties drawers in current buffer.
Ignore non Org buffers."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (inline-re (and (featurep 'org-inlinetask)
                           (concat (org-inlinetask-outline-regexp)
                                   "END[ \t]*$"))))
       (org-map-entries
        (lambda ()
          (unless (and inline-re (looking-at-p inline-re))
            (save-excursion
              (let ((end (save-excursion (outline-next-heading) (point))))
                (forward-line)
                (when (looking-at-p org-planning-line-re) ; Org-8.3.
                  (forward-line))
                (when (and (< (point) end)
                           (not (looking-at-p org-property-drawer-re))
                           (save-excursion
                             (and (re-search-forward org-property-drawer-re end t)
                                  (eq (org-element-type
                                       (save-match-data (org-element-at-point)))
                                      'drawer))))
                  (insert (delete-and-extract-region
                           (match-beginning 0)
                           (min (1+ (match-end 0)) end)))
                  (unless (bolp) (insert "\n"))))))))))))

(when (boundp 'org-planning-line-re)
  (add-hook 'org-mode-hook #'org-repair-property-drawers))

(defun lvn--org-switch-dictionary ()
  "Set the language dictionary if Flyspell is enabled and `#+LANGUAGE:' is found within the top 8 lines of the buffer."
  (when (and (boundp 'ispell-dictionary-alist)
             ispell-dictionary-alist)
    (save-excursion
      (goto-char (point-min))
      (forward-line 8)
      (let ((lang-dict-alist '(("en" . "american")
                               ("fr" . "francais"))))
        (when (re-search-backward "#\\+LANGUAGE: +\\([[:alpha:]_]*\\)" 1 t)
          (let* ((lang (match-string 1))
                 (dict (cdr (assoc lang lang-dict-alist))))
            (if dict
                (progn
                  (ispell-change-dictionary dict)
                  (force-mode-line-update))
              (message "[No Ispell dictionary for language `%s' (see file `%s')]"
                       lang (file-name-base))
              (sit-for 1.5))))))))

;; Guess dictionary.
(add-hook 'org-mode-hook #'lvn--org-switch-dictionary)

;;** 15.2 (info "(org)Easy Templates")

(message "15.2 (org)Easy Templates")

(with-eval-after-load 'org
  (message "[... Org Easy Templates]")

  ;; New format in Org 9.2.
  (add-to-list 'org-structure-template-alist '("n" . "note"))
  (add-to-list 'org-structure-template-alist '("w" . "warning"))
  (add-to-list 'org-structure-template-alist '("t" . "tip"))

  ;; Begin/end example markers will be inserted in lower case.
  (setq org-babel-uppercase-example-markers nil)
  )

;;** 15.3 (info "(org)Speed keys")

(message "15.3 (org)Speed keys")

(with-eval-after-load 'org
  (message "[... Org Speek keys]")

  ;; Activate single letter commands at beginning of a headline.
  (setq org-use-speed-commands t)

  (when (boundp 'org-speed-commands)
    (add-to-list 'org-speed-commands '("d" org-todo "DONE"))
    (add-to-list 'org-speed-commands '("y" org-todo-yesterday "DONE"))
    (add-to-list 'org-speed-commands '("s" call-interactively 'org-schedule))
    (add-to-list 'org-speed-commands '("N" org-narrow-to-subtree))
    (add-to-list 'org-speed-commands '("W" widen))
    (add-to-list 'org-speed-commands '("k" org-cut-subtree)))

  ;; Run current line (mapped to H-r).

  ;; Run from beginning of code block to current line (mapped to H-a?).

  ;; Run from current line to end of code block (mapped to H-e?).

  ;; Run current code block.
  (define-key org-mode-map (kbd "H-e") #'org-babel-execute-maybe)

  (defun org-babel-force-execute-src-block ()
    "Force execution of the current source code block."
    (interactive)
    (org-babel-execute-src-block nil nil '((:eval . "yes"))))

  ;; Run current code block (force execution).
  (define-key org-mode-map (kbd "H-f") #'org-babel-force-execute-src-block)

  (define-key org-mode-map (kbd "H-t") #'org-babel-tangle)

  )

;;** 15.4 (info "(org)Code evaluation security") issues

(message "15.4 (org)Code evaluation security issues")

(with-eval-after-load 'ob-core

  ;;!! Don't be prompted on every code block evaluation.
  (setq org-confirm-babel-evaluate nil))

;;** 15.8 A (info "(org)Clean view")

(with-eval-after-load 'org
  (message "[... Org Clean view]")

  ;; 15.8 Don't skip even levels for the outline.
  (setq org-odd-levels-only nil))

;;** 15.10 (info "(org)Interaction")

(message "15.10 (org)Interaction")

;; Keep my encrypted data (like account passwords) in my Org mode files with
;; a special tag instead.
(with-eval-after-load 'org
  (message "[... Org Crypt]")

  (try-require 'org-crypt))           ; Loads org, gnus-sum, etc...

(with-eval-after-load 'org-crypt

  ;; Encrypt all entries before saving.
  (org-crypt-use-before-save-magic)

  ;; Which tag is used to mark headings to be encrypted.
  (setq org-tags-exclude-from-inheritance '("crypt")))

(defun leuven-org-scramble-contents ()
  "XXX"
  (interactive)
  (let ((tree (org-element-parse-buffer)))
    (org-element-map tree
        '(code comment comment-block example-block fixed-width keyword link
               node-property plain-text verbatim)
      (lambda (obj)
        (cl-case (org-element-type obj)
          ((code comment comment-block example-block fixed-width keyword
                 node-property verbatim)
           (let ((value (org-element-property :value obj)))
             (org-element-put-property
              obj :value (replace-regexp-in-string "[[:alnum:]]" "x" value))))
          (link
           (unless (string= (org-element-property :type obj) "radio")
             (org-element-put-property obj :raw-link "http://orgmode.org")))
          (plain-text
           (org-element-set-element
            obj (replace-regexp-in-string "[[:alnum:]]" "x" obj)))))
      nil nil nil t)
    (let ((buffer (get-buffer-create "*Scrambled text*")))
      (with-current-buffer buffer
        (insert (org-element-interpret-data tree))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

;; Don't pad tangled code with newlines.
(setq org-babel-tangle-pad-newline nil)

;; Use relative path names in links from tangled source back the Org file.
(setq org-babel-tangle-use-relative-file-links t)

;; How to combine blocks of the same name during tangling.
(setq org-babel-tangle-named-block-combination 'append)

;; Speed up tangling dramatically (a couple of orders of magnitude).
(setq org-babel-use-quick-and-dirty-noweb-expansion t)
                                        ; :noweb-ref feature must NOT be used!

;; Minimum number of lines for output *block* (placed in a
;; #+begin_example...#+end_example) vs. output marked as literal by
;; inserting a *colon* at the beginning of the lines.
(setq org-babel-min-lines-for-block-output 2)

;; ;; FIXME Make this the default behavior
;; ;; Grab the last line too, when selecting a subtree.
;; (org-end-of-subtree nil t)

;; Backend aware export preprocess hook.
(defun leuven--org-export-preprocess-hook ()
  "Backend-aware export preprocess hook."
  (save-excursion
    (when (eq org-export-current-backend 'latex)
      ;; ignoreheading tag for bibliographies and appendices.
      (let* ((tag "ignoreheading"))
        ;; (goto-char (point-min))
        ;; (while (re-search-forward (concat ":" tag ":") nil t)
        ;; (delete-region (point-at-bol) (point-at-eol)))
        (org-map-entries
         (lambda ()
           (delete-region (point-at-bol) (point-at-eol)))
         (concat ":" tag ":"))))
    (when (eq org-export-current-backend 'html)
      ;; set custom css style class based on matched tag
      (let* ((match "Qn"))
        (org-map-entries
         (lambda ()
           (org-set-property "HTML_CONTAINER_CLASS" "inlinetask"))
         match)))))

(add-hook 'org-export-preprocess-hook #'leuven--org-export-preprocess-hook)

(defun insert-one-equal-or-two ()
  "XXX"
  (interactive)
  (cond
   ((or (bolp) (not (looking-back "=")))
    ;; Insert just one =.
    (self-insert-command 1))
   ((save-excursion
      (backward-char)
      ;; Skip symbol backwards.
      (and (not (zerop (skip-syntax-backward "w_.")))
           (not (looking-back "="))
           (or (insert-and-inherit "=") t))))
   (t
    ;; insert == around following symbol.
    (delete-char -1)
    (unless (looking-back "=") (insert-and-inherit "="))
    (save-excursion
      (skip-syntax-forward "w_.")
      (unless (looking-at "=") (insert-and-inherit "="))))))

;; Must be in eval-after-load "org"?
;; (define-key org-mode-map (kbd "=") #'insert-one-equal-or-two)

;;** A.3 (info "(org)Adding hyperlink types")

;; (with-eval-after-load 'org
;;   (message "[... Org Adding hyperlink types]")
;;
;;   ;; Define a new link type (`latex') whose path argument can hold the name of
;;   ;; any LaTeX command.
;;   (org-link-set-parameters
;;    "latex" nil
;;    (lambda (path desc format)
;;      (cond
;;       ((eq format 'html)
;;        (format "<span class=\"%s\">%s</span>" path desc))
;;       ((eq format 'latex)
;;        (format "\\%s{%s}" path desc)))))
;;
;;   ;; Add background color by using custom links like [[bgcolor:red][Warning!]].
;;   (org-link-set-parameters
;;     "bgcolor" nil
;;     (lambda (path desc format)
;;       (cond
;;        ((eq format 'html)
;;         (format "<span style=\"background-color:%s;\">%s</span>" path desc))
;;        ((eq format 'latex)
;;         (format "\\colorbox{%s}{%s}" path desc))
;;        (t
;;         (format "BGCOLOR LINK (%s): {%s}{%s}" format path desc))))))

(defun lvn-orgtbl-send-all-tables ()
  "Export all Org tables in the current LaTeX document to LaTeX tables."
  (interactive)
  (org-table-map-tables
   (lambda ()
     (orgtbl-send-table 'maybe))))

;;** A.6 (info "(org)Dynamic blocks")

(defun leuven--org-update-buffer-before-save ()
  "Update all dynamic blocks and all tables in the buffer before save."
  (when (derived-mode-p 'org-mode)
    (message "[Update Org buffer %s]"
             (file-name-nondirectory (buffer-file-name)))
    ;; (sit-for 1.5)
    (let ((cache-long-scans nil)        ; Make `forward-line' much faster and
                                        ; thus `org-goto-line', `org-table-sum',
                                        ; etc.
          (fly-state (and (boundp 'flyspell-mode)
                          (if flyspell-mode 1 -1)))
          (buffer-undo-list buffer-undo-list)) ; For goto-chg.
      (and fly-state (flyspell-mode -1))
                                        ; Temporarily disable Flyspell to avoid
                                        ; checking the following modifications
                                        ; of the buffer.
      (measure-time "Realigned all tags" (org-align-all-tags))
      (measure-time "Updated all dynamic blocks" (org-update-all-dblocks))
      (measure-time "Re-applied formulas to all tables"
                    (org-table-iterate-buffer-tables))
      (when (file-exists-p (buffer-file-name (current-buffer)))
        (leuven-org-remove-redundant-tags))
      (and fly-state (flyspell-mode fly-state)))))

;; Make sure that all dynamic blocks and all tables are always up-to-date.
(add-hook 'before-save-hook #'leuven--org-update-buffer-before-save)

;; (with-eval-after-load 'org
;;   (message "[... Org Effectiveness]")
;;
;;   (try-require 'org-effectiveness)
;;   (with-eval-after-load 'org-effectiveness
;;
;;     (add-hook 'org-mode-hook
;;               (lambda ()
;;                 (org-effectiveness-count-todo)
;;                 (sit-for 0.2)))))

;; Add weather forecast in your Org agenda.
(autoload 'org-google-weather "org-google-weather"
  "Return Org entry with the weather for LOCATION in LANGUAGE." t)

(with-eval-after-load 'org-google-weather
  ;; (try-require 'url)

  ;; Add the city.
  (setq org-google-weather-format "%C %i %c, %l°-%h°"))

(with-eval-after-load 'org-modern

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (setq org-modern-label-border 1)

  ;; TODO: Make a choice with leading characters (in order to visualize depth).
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  (setq org-modern-star ["◉" "·○" "··◈" "···◇" "····✳"]) ; OK.
  (setq org-modern-star ["◈" "·◈" "··◇" "···◇" "·····"]) ; OK.

  ;; (setq org-modern-timestamp '("%y-%m-%d" . "%y-%m-%d %H:%M"))

  (setq org-modern-table-vertical 2)
  (setq org-modern-table-horizontal 1)

  (setq org-modern-list '(
                          ;; (?- . "-")
                          (?- . "–")
                          (?+ . "•")
                          ;; (?+ . "○")
                          (?* . "▹")
                          ;; (?* . "◦")
                          ))

  (setq org-moden-checkbox
        '((?X  . #("▢✓" 0 2 (composition ((2)))))
          (?-  . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))

  (setq org-modern-block-name
        '((t . t)
          ("src" "»" "∥")
          ("example" "»–" "∥")
          ("quote" "❝" "❞")))

  (setq org-modern-block-fringe nil)

  ;; See https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
  (setq org-modern-keyword
        '((t . t)
          ("bibliography" . "")
          ("cite_export" . "⮭")
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("header" . "›")
          ("caption" . "☰")
          ("name" . "⁝")
          ("results" . "∴")))
  ;; (setq org-modern-keyword nil)
  )

(provide 'emacs-leuven-org)

;;; emacs-leuven-org.el ends here
