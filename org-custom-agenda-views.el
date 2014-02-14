    ;; custom commands for the agenda -- start with a clean slate
    (setq org-agenda-custom-commands nil)

    (add-to-list 'org-agenda-custom-commands
                 '("A" . "All Tasks...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("Ad" "All Tasks (grouped by Due Date)"
                   ((tags-todo "DEADLINE<\"<+0d>\""
                               ((org-agenda-overriding-header "OVERDUE")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (tags-todo "DEADLINE=\"<+0d>\""
                               ((org-agenda-overriding-header "DUE TODAY")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (tags-todo "DEADLINE=\"<+1d>\""
                               ((org-agenda-overriding-header "DUE TOMORROW")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                               ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                               ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (tags-todo "DEADLINE>\"<+28d>\""
                               ((org-agenda-overriding-header "DUE LATER")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
                    (todo ""
                               ((org-agenda-overriding-header "NO DUE DATE")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))
                    (todo "DONE|CANX"
                          ((org-agenda-overriding-header "COMPLETED"))))
                   ((org-agenda-sorting-strategy '(priority-down)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("A1" "All Tasks (sorted by Due Date)"
                   alltodo ""
                   ((org-agenda-overriding-header "All Tasks (sorted by Due Date)")
                    (org-agenda-sorting-strategy '(deadline-up)))) t) ; XXX sort not OK

    (add-to-list 'org-agenda-custom-commands
                 `("A2" "All active tasks, by due date"
                   ((agenda ""
                            ((org-agenda-overriding-header "Today")
                             ;; FIXME We don't see "timed" DEADLINE
                             (org-agenda-skip-function
                              (lambda ()
                                (let* ((dl (org-entry-get nil "DEADLINE")))
                                  (if (or (not dl)
                                          (equal dl "")
                                          (org-time> dl (org-time-today)))
                                      (progn (outline-next-heading) (point))))))
                             (org-agenda-skip-scheduled-if-deadline-is-shown t)
                             (org-agenda-span 'day)
                             (org-deadline-warning-days 0)))
                    (agenda ""
                            ((org-agenda-entry-types '(:deadline))
                             (org-agenda-overriding-header "Tomorrow")
                             (org-agenda-skip-function
                              '(leuven--skip-entry-unless-deadline-in-n-days-or-more 1))
                             (org-deadline-warning-days 1)))
                    (agenda ""
                            ((org-agenda-overriding-header "Next 5 days")
                             (org-agenda-skip-function
                              '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                             (org-deadline-warning-days 7)))
                    (agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Next 3 weeks")
                             (org-agenda-skip-function
                              '(leuven--skip-entry-unless-deadline-in-n-days-or-more 7))
                             (org-deadline-warning-days 28))))
                   ((org-agenda-deadline-faces '((0.0 . default)))
                    (org-agenda-clockreport-mode nil)
                    (org-agenda-format-date "")
                    (org-agenda-span 'day)
                    (org-agenda-sorting-strategy '(deadline-up))
                    (org-agenda-use-time-grid nil)
                    (org-agenda-write-buffer-name "Reminders"))) t)

    (defun leuven--skip-entry-unless-deadline-in-n-days-or-more (n)
      "Skip entries that have no deadline, or that have a deadline earlier than in N days."
      (let* ((dl (org-entry-get nil "DEADLINE")))
        (if (or (not dl)
                (equal dl "")
                (org-time< dl (+ (org-time-today) (* n 86400))))
            (progn (outline-next-heading) (point)))))

    (defun leuven--skip-entry-unless-overdue-deadline ()
      "Skip entries that have no deadline, or that have a deadline later than or equal to today."
      (let* ((dl (org-entry-get nil "DEADLINE")))
        (if (or (not dl)
                (equal dl "")
                (org-time>= dl (org-time-today)))
            (progn (outline-next-heading) (point)))))

    (defun leuven--skip-entry-if-past-deadline ()
      "Skip entries that have a deadline earlier than today."
      (let* ((dl (org-entry-get nil "DEADLINE")))
        (if (org-time< dl (org-time-today))
            (progn (outline-next-heading) (point)))))

    (defun leuven--skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days (n1 n2)
      "Skip entries that have a deadline in less than N1 days, or that have a
    scheduled date in less than N2 days, or that have no deadline nor scheduled."
      (let* ((dl (org-entry-get nil "DEADLINE"))
             (sd (org-entry-get nil "SCHEDULED")))
        (if (or (and dl
                     (not (equal dl ""))
                     (org-time< dl (+ (org-time-today) (* n1 86400))))
                (and sd
                     (not (equal sd ""))
                     (org-time< sd (+ (org-time-today) (* n2 86400))))
                (and (or (not dl)       ; no deadline
                         (equal dl ""))
                     (or (not sd)       ; nor scheduled
                         (equal sd ""))))
            (progn (outline-next-heading) (point)))))

    (defun leuven--skip-entry-if-deadline-or-schedule ()
      "Skip entries that have a deadline or that have a scheduled date."
      (let* ((dl (org-entry-get nil "DEADLINE"))
             (sd (org-entry-get nil "SCHEDULED")))
        (if (or (and dl
                     (not (equal dl "")))
                (and sd
                     (not (equal sd ""))))
            (progn (outline-next-heading) (point)))))

    (add-to-list 'org-agenda-custom-commands
                 '("A3" "Agenda for all TODO entries"
                   ((agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Past due")
                             (org-agenda-skip-function
                              'leuven--skip-entry-unless-overdue-deadline)
                             (org-deadline-warning-days 0)))
                    (agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Today/tomorrow")
                             (org-agenda-skip-function
                              'leuven--skip-entry-if-past-deadline)
                             (org-agenda-span 2)
                             (org-agenda-use-time-grid t)
                             (org-deadline-warning-days 0)))
                    (agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Next 12 days")
                             (org-agenda-skip-function
                              '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                             (org-deadline-warning-days 14)))
                    (todo ""
                          ((org-agenda-overriding-header "Later")
                           (org-agenda-skip-function
                            '(leuven--skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days 15 2))
                           (org-agenda-sorting-strategy '(ts-up))))
                    (todo ""
                          ((org-agenda-overriding-header "No due date")
                           (org-agenda-skip-function
                            'leuven--skip-entry-if-deadline-or-schedule))))
                   ((org-agenda-clockreport-mode nil)
                    (org-agenda-prefix-format " %i %?-12t% s")
                    (org-agenda-span 'day)
                    (org-agenda-use-time-grid nil)
                    (org-agenda-write-buffer-name "List Review"))
                   "org-agenda-all-todo-entries.html") t)

    (add-to-list 'org-agenda-custom-commands
                 '("Ap" "All Tasks (grouped by Priority)"
                   ((tags-todo "PRIORITY={A}"
                               ((org-agenda-overriding-header "HIGH")))
                    (tags-todo "PRIORITY={B}"
                               ((org-agenda-overriding-header "MEDIUM")))
                    (tags-todo "PRIORITY=\"\""
                               ((org-agenda-overriding-header "NONE"))) ; = medium
                    (tags-todo "PRIORITY={C}"
                               ((org-agenda-overriding-header "LOW")))
                    (todo "DONE|CANX"
                          ((org-agenda-overriding-header "COMPLETED")
                           (org-agenda-sorting-strategy '(priority-down)))))) t)

    ;; CollectBox + Email
    (add-to-list 'org-agenda-custom-commands
                 '("c" "CollectBox"
                   alltodo ""
                   ((org-agenda-files
                     '("~/org/refile.org" "~/org/email.org")))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("." "Today"
                   ;; list of all TODO entries with deadline today
                   ((tags-todo "DEADLINE=\"<+0d>\""
                               ((org-agenda-overriding-header "DUE TODAY")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    ;; list of all TODO entries with deadline before today
                    (tags-todo "DEADLINE<\"<+0d>\""
                               ((org-agenda-overriding-header "OVERDUE")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    ;; list of all TODO entries completed today
                    (todo "DONE|CANX"
                               ((org-agenda-overriding-header "COMPLETED")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "CLOSED: \\[2014-02-13"))
                                (org-agenda-sorting-strategy '(priority-down)))))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("7" "Next 7 Days"
                   ((tags-todo "DEADLINE=\"<+0d>\""
                               ((org-agenda-overriding-header "TODAY")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+1d>\""
                               ((org-agenda-overriding-header "TOMORROW")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+2d>\""
                               ((org-agenda-overriding-header "IN 2 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+3d>\""
                               ((org-agenda-overriding-header "IN 3 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+4d>\""
                               ((org-agenda-overriding-header "IN 4 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+5d>\""
                               ((org-agenda-overriding-header "IN 5 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+6d>\""
                               ((org-agenda-overriding-header "IN 6 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down))))
                    (tags-todo "DEADLINE=\"<+7d>\""
                               ((org-agenda-overriding-header "IN 7 DAYS")
                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                                (org-agenda-sorting-strategy '(priority-down)))))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("N" "Next"
                   tags-todo "TODO<>{SDAY}"
                   ((org-agenda-overriding-header "List of all TODO entries with no due date (no SDAY)")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                    (org-agenda-sorting-strategy '(priority-down)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("Y" "Someday"
                   tags-todo "TODO={SDAY}"
                   ((org-agenda-overriding-header "List of all SDAY entries")
                    (org-agenda-sorting-strategy '(priority-down)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("W" "Waiting for"
                   tags-todo "TODO={WAIT}"
                   ((org-agenda-overriding-header "Waiting for")
                    (org-agenda-sorting-strategy '(deadline-up)))) t) ; XXX does not work

    (add-to-list 'org-agenda-custom-commands
                 '("P" "Projects"
                   tags-todo "project-DONE-CANX"
                   ((org-agenda-overriding-header "Projects (High Level)")
                    (org-agenda-sorting-strategy nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("R" "Recent items (past 7 days)"
                   ;; faster than tags
                   agenda ""
                   ((org-agenda-start-day "-7d")
                    (org-agenda-span 7)
                    (org-agenda-repeating-timestamp-show-all nil)
                    ;; %s is only for agenda views
                    ;; (org-agenda-prefix-format "%s")
                    ;; maybe not make much difference ka
                    ;; (org-agenda-use-tag-inheritance nil)
                    (org-agenda-inactive-leader "Inactive:  ")
                    (org-agenda-include-inactive-timestamps t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("^" . "Calendar...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("^7" "Events and appointments for 7 days"
                   agenda ""
                   ((org-agenda-entry-types '(:timestamp :sexp))
                    ;; (org-agenda-overriding-header "Calendar for 7 days")
                    ;; (org-agenda-repeating-timestamp-show-all t)
                    (org-agenda-span 'week)
                    (org-agenda-time-grid nil))) t)

    ;; calendar view for org-agenda
    (when (locate-library "calfw-org")

      (autoload 'cfw:open-org-calendar "calfw-org"
        "Open an Org schedule calendar." t)

      (add-to-list 'org-agenda-custom-commands
                   '("^m" "Calendar for current month"
                     (lambda (&rest ignore)
                       (cfw:open-org-calendar))) t)

      ;; (defun cfw:open-org-calendar-non-work (&args)
      ;;   (interactive)
      ;;   (let ((org-agenda-skip-function 'org-agenda-skip-work))
      ;;     (cfw:open-org-calendar)))
      ;;
      ;; (add-to-list 'org-agenda-custom-commands
      ;;              '("c" "Calendar (non-work) for current month"
      ;;                cfw:open-org-calendar-non-work) t)

      )

    (add-to-list 'org-agenda-custom-commands
                 '("f" "Like s, but with extra files"
                   search ""
                   ((org-agenda-text-search-extra-files
                     ;; FIXME Add `agenda-archives'
                     leuven-org-search-extra-files))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("n" "Organize thoughts to refile"
                   tags "refile|capture"
                   ((org-agenda-overriding-header "Refile stuff"))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("r" . "4. Review...") t)

    ;; show what happened today
    (add-to-list 'org-agenda-custom-commands
                 '("rL" "Timeline for today"
                   ((agenda ""
                            ((org-agenda-clockreport-mode t)
                             (org-agenda-entry-types '(:timestamp))
                             (org-agenda-log-mode-items '(clock closed))
                             (org-agenda-show-log t)
                             (org-agenda-span 'day))))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rC" "Clock Review"
                   ((agenda ""
                            ((org-agenda-clockreport-mode t)
                             (org-agenda-overriding-header "Clocking Review")
                             (org-agenda-show-log 'clockcheck)
                             (org-agenda-span 'day))))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rn" "Now (undated tasks in progress)"
                   todo "STRT"
                   ((org-agenda-todo-ignore-with-date t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rh" "Hotlist"
                   ;; tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY={A}|FLAGGED"
                   ((tags-todo "DEADLINE<=\"<+0d>\""
                               ((org-agenda-overriding-header "Late")))
                    (tags-todo "DEADLINE>\"<+0d>\"+DEADLINE<=\"<+1w>\""
                               ((org-agenda-overriding-header "Due in next 7 days")))
                    (tags-todo "DEADLINE={}+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                               ((org-agenda-overriding-header "High priority")))
                    (tags-todo "DEADLINE={}+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                               ((org-agenda-overriding-header "Flagged")
                                (org-agenda-skip-function
                                 '(org-agenda-skip-entry-when-regexp-matches))
                                (org-agenda-skip-regexp "\\[#A\\]"))))
                   ((org-agenda-todo-ignore-scheduled 'future)
                    (org-agenda-sorting-strategy '(deadline-down)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rd" "Daily review"
                   ((tags "CATEGORY={@Collect}&LEVEL=2|TODO={NEW}"
                          ((org-agenda-overriding-header "New Tasks")))
                    (tags "LEVEL=2"
                          ((org-agenda-overriding-header "New Emails")
                           (org-agenda-files '("~/org/email.org"))))
                    (agenda ""
                            ((org-agenda-entry-types '(:timestamp :sexp))
                             (org-agenda-overriding-header "Calendar")
                             (org-agenda-span 'day)))
                    (agenda ""
                            ((org-agenda-entry-types '(:deadline))
                             (org-agenda-overriding-header "Due Dates")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-sorting-strategy
                              '(priority-down time-down))
                             (org-agenda-span 'day)
                             (org-agenda-start-on-weekday nil)
                             (org-agenda-time-grid nil)))
                    (agenda ""
                            ((org-agenda-entry-types '(:scheduled))
                             (org-agenda-overriding-header "Scheduled")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-sorting-strategy
                              '(priority-down time-down))
                             (org-agenda-span 'day)
                             (org-agenda-start-on-weekday nil)
                             (org-agenda-time-grid nil)))
                    )
                   ((org-agenda-format-date "")
                    (org-agenda-start-with-clockreport-mode nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rw" "Weekly review"
                   (
                    (tags "CATEGORY={@Collect}&LEVEL=2|TODO={NEW}"
                          ((org-agenda-overriding-header "New Tasks")))

                    (agenda ""
                            ((org-agenda-clockreport-mode t)
                             (org-agenda-format-date
                              (concat "\n"
                                      "%Y-%m-%d" " %a "
                                      (make-string (window-width) ?_)))
                             (org-agenda-overriding-header "Past week")
                             (org-agenda-prefix-format " %?-11t %i %-12:c% s")
                             (org-agenda-show-log 'clockcheck)
                             (org-agenda-span 7)
                             (org-agenda-start-day "-1w") ; recently done
                             (org-deadline-warning-days 0)))

                    (agenda ""
                            ((org-agenda-overriding-header "Next month")
                             (org-agenda-span 'month)
                             (org-agenda-start-day "+0d")
                             (org-deadline-warning-days 0) ; XXX
                             ))

                    (todo "PROJ"
                          ((org-agenda-overriding-header "Project list")))

                    ;; XXX we should show which tasks (don't) have CLOCK lines: archived vs. deleted
                    (todo "DONE|PROJDONE"
                          ((org-agenda-overriding-header
                            "Candidates to be archived")))

                    ;; (stuck ""
                    ;;        ((org-agenda-overriding-header "Stuck projects")))

                    (todo "STRT"
                          ((org-agenda-overriding-header "In progress")
                           (org-agenda-todo-ignore-scheduled nil)))

                    (todo "TODO"        ; don't include items from CollectBox! XXX
                          ((org-agenda-overriding-header "Action list")))

                    ;; ignore scheduled and deadline entries, as they're
                    ;; visible in the above agenda (for the past + for next
                    ;; month) or scheduled/deadline'd for much later...
                    (todo "WAIT"
                          ((org-agenda-format-date "")
                           (org-agenda-overriding-header "Waiting for")
                           (org-agenda-todo-ignore-deadlines 'all) ; future?
                           (org-agenda-todo-ignore-scheduled t)))

                    ;; same reasoning as for WAIT
                    (todo "SDAY"
                          ((org-agenda-format-date "")
                           (org-agenda-overriding-header "Someday")
                           (org-agenda-todo-ignore-deadlines 'all)
                           (org-agenda-todo-ignore-scheduled t)))

                   ;; ((org-agenda-clockreport-mode nil)
                   ;;  (org-agenda-prefix-format " %i %?-12t% s")
                   ;;  (org-agenda-write-buffer-name "Weekly task review"))
                   ;; "~/org-weekly-review.html") t)
                    )) t)

    (add-to-list 'org-agenda-custom-commands
                 '("d" . "5. Do the work...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("de" "Effort less than 1 hour"
                   tags-todo "Effort<>{}+Effort<\"1:00\""
                   ((org-agenda-todo-ignore-scheduled 'future))) t)

    ;; checking tasks that are assigned to me
    (add-to-list 'org-agenda-custom-commands
                 `("dm" "Tasks assigned to me"
                   tags ,(concat "Assignee={" user-login-name "}")
                   ((org-agenda-overriding-header
                     ,(concat "Tasks assigned to " user-login-name)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("E" . "Exported agenda files...") t)

    ;; exporting agenda views
    (add-to-list 'org-agenda-custom-commands
                 '("Ea"
                   agenda ""
                   (;; (org-tag-faces nil)
                    (ps-landscape-mode t)
                    (ps-number-of-columns 1))
                   ("~/org-agenda.txt" "~/org-agenda.html" "~/org-agenda.pdf")) t)

    (add-to-list 'org-agenda-custom-commands
                 '("Ep" "Call list"
                   tags-todo "phone"
                   ((org-agenda-prefix-format " %-20:c [ ] " )
                    (org-agenda-remove-tags t)
                    ;; (org-agenda-with-colors nil)
                    (org-agenda-write-buffer-name
                     "Phone calls that you need to make")
                    (ps-landscape-mode t)
                    (ps-number-of-columns 1))
                   ("~/org-calls.pdf")) t)

    ;; create a sparse tree (current buffer only) with all entries containing
    ;; the word `TODO', `FIXME' or `XXX'
    (add-to-list 'org-agenda-custom-commands
                 '("1" "Task markers (in current buffer)"
                   occur-tree "\\<TODO\\|FIXME\\|XXX\\>") t)
