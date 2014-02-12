    ;; custom commands for the agenda -- start with a clean slate
    (setq org-agenda-custom-commands nil)

    ;; CollectBox + Email
    (add-to-list 'org-agenda-custom-commands
                 '("c" "CollectBox"
                   alltodo ""
                   ((org-agenda-files
                     '("~/org/refile.org" "~/org/email.org")))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("f" "Like s, but with extra files"
                   search ""
                   ((org-agenda-text-search-extra-files
                     ;; FIXME Add `agenda-archives'
                     leuven-org-search-extra-files))) t)

    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("A" . "0. Agenda...") t)
    ;;
    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("AF" "Agenda of upcoming due dates (6 months)"
    ;;                ;; FIXME We don't see DEADLINE with `-1m' (or so)
    ;;                ;; specifications (if they are more than 1m ahead of now)!
    ;;                agenda ""
    ;;                ((org-agenda-skip-function
    ;;                  '(org-agenda-skip-entry-if 'notdeadline))
    ;;                 (org-agenda-span 'day)
    ;;                 (org-agenda-time-grid nil)
    ;;                 (org-deadline-warning-days 183))) t)
    ;;              ;; Some SCHEDULED are shown (when paired with a deadline and
    ;;              ;; scheduled in the past or for today)

    (add-to-list 'org-agenda-custom-commands
                 '("o" . "3. Organize...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("or" "Thoughts to refile"
                   tags "refile|capture"
                   ((org-agenda-overriding-header "Refile stuff"))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("r" . "4. Review...") t)

    ;; all TODO entries
    (add-to-list 'org-agenda-custom-commands
                 '("rT" "List of undated TODO entries"
                   tags-todo ""
                   ((org-agenda-overriding-header
                     "Global list of undated TODO items of all types but SDAY")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
                    (org-agenda-sorting-strategy '(priority-down)))) t)

    ;; show TODO entries that have no time stamps (it also covers scheduled or
    ;; deadline items)
    (add-to-list 'org-agenda-custom-commands
                 '("rs" "Undated TODO items"
                   todo ""
                   ((org-agenda-overriding-header "Undated TODO items: ")
                    (org-agenda-todo-ignore-with-date t))) t)

    ;; calendar view for org-agenda
    (when (locate-library "calfw-org")

      (autoload 'cfw:open-org-calendar "calfw-org"
        "Open an Org schedule calendar." t)

      (add-to-list 'org-agenda-custom-commands
                   '("r^" "Calendar for current month"
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
                 '("r$" "Cleanup"
                   todo "DONE|CANX"
                   ((org-agenda-overriding-header "Old tasks to delete or archive")
                    ;; also show deadlines and scheduled items
                    (org-agenda-todo-ignore-with-date nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rP" "Projects"
                   tags-todo "project-DONE-CANX"
                   ((org-agenda-overriding-header "Projects (High Level)")
                    (org-agenda-sorting-strategy nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rn" "Now (undated tasks in progress)"
                   todo "STRT"
                   ((org-agenda-todo-ignore-with-date t))) t)

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

;;*** Calendar style views

    (add-to-list 'org-agenda-custom-commands
                 '("rc" "Calendar for 7 days"
                   agenda ""
                   ((org-agenda-entry-types '(:timestamp :sexp))
                    (org-agenda-overriding-header "Calendar for 7 days")
                    ;; (org-agenda-repeating-timestamp-show-all t)
                    (org-agenda-span 'week)
                    (org-agenda-time-grid nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rr" "Recent items (past 7 days)"
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
                 '("d" . "5. Do the work...") t)

;;*** Other views

    (add-to-list 'org-agenda-custom-commands
                 '("dh" "Hotlist"
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
                 '("de" "Effort less than 1 hour"
                   tags-todo "Effort<>{}+Effort<\"1:00\""
                   ((org-agenda-todo-ignore-scheduled 'future))) t)

    ;; checking tasks that are assigned to me
    (add-to-list 'org-agenda-custom-commands
                 `("dm" "Tasks assigned to me"
                   tags ,(concat "Assignee={" user-login-name "}")
                   ((org-agenda-overriding-header
                     ,(concat "Tasks assigned to " user-login-name)))) t)

    ;; create a sparse tree (current buffer only) with all entries containing
    ;; the word `TODO', `FIXME' or `XXX'
    (add-to-list 'org-agenda-custom-commands
                 '("1" "Task markers (in current buffer)"
                   occur-tree "\\<TODO\\|FIXME\\|XXX\\>") t)

    (add-to-list 'org-agenda-custom-commands
                 '("E" . "Exported agenda files...") t)

;;*** Printed agenda

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

    (add-to-list 'org-agenda-custom-commands
                 '("Ee" "Print reports (TODO)"
                   ;; See ThinkingRock examples
                   ((agenda ""
                            ((org-agenda-overriding-header "Scheduled TODO's")
                             (org-agenda-prefix-format "%8e ")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if 'regexp "habit"))
                             (org-agenda-sorting-strategy '(todo-state-up))
                             (org-agenda-span 'week)
                             (org-agenda-todo-keyword-format "%-4s")))
                    (tags-todo "thisweek"
                               ((org-agenda-overriding-iding-header
                                 "Unscheduled TODO's; also tasks (from which todo's were generated)")
                                (org-agenda-prefix-format "%-7e")
                                (org-agenda-skip-function
                                 '(org-agenda-skip-entry-if 'scheduled))
                                (org-agenda-sorting-strategy '(todo-state-up))
                                (org-agenda-todo-keyword-format "%-10s"))))
                   ((org-agenda-remove-tags t))
                   ("~/org-agenda-de.html")) t)

    (add-to-list 'org-agenda-custom-commands
                 '("v" . "6. More views...") t)

;;*** Priorities

    ;; priority levels
    (add-to-list 'org-agenda-custom-commands
                 '("v," . "Priorities...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v,," "Actions Grouped by Priority"
                   (;; important things to do
                    (tags-todo "+PRIORITY={A}")
                    ;; medium important things to do
                    (tags-todo "+PRIORITY={B}")
                    ;; other things to do
                    (tags-todo "+PRIORITY={C}"))) t)

    ;; list only priority A tasks for the current day
    (add-to-list 'org-agenda-custom-commands
                 '("v,A" "Priority #A tasks for today"
                   agenda ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))
                    (org-agenda-span 'day)
                    (org-agenda-overriding-header
                     "Today's priority #A tasks: "))) t)

    ;; list priority A and B tasks for the current day
    (add-to-list 'org-agenda-custom-commands
                 '("v,B" "Priority #A and #B tasks for today"
                   agenda ""
                   ((org-agenda-overriding-header
                     "Today's priority #A and #B tasks: ")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]"))
                    (org-agenda-span 'day))) t)

    (add-to-list 'org-agenda-custom-commands
                 `("dd" "All active tasks, by due date"
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
                             (org-deadline-warning-days 28)))
                    (agenda ""
                            ((org-agenda-entry-types '(:deadline))
                             (org-agenda-overriding-header
                              "Unscheduled upcoming due dates:")
                             (org-agenda-skip-entry-if 'scheduled)
                             (org-agenda-span 'day)
                             (org-agenda-time-grid nil)
                             (org-deadline-warning-days 365))))
                   ((org-agenda-clockreport-mode nil)
                    (org-agenda-format-date "")
                    (org-agenda-span 'day)
                    (org-agenda-sorting-strategy '(deadline-up))
                    (org-agenda-use-time-grid nil)
                    (org-agenda-write-buffer-name "Reminders"))) t)

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

    (defun leuven--skip-entry-unless-deadline-in-n-days-or-more (n)
      "Skip entries that have no deadline, or that have a deadline earlier than in N days."
      (let* ((dl (org-entry-get nil "DEADLINE")))
        (if (or (not dl)
                (equal dl "")
                (org-time< dl (+ (org-time-today) (* n 86400))))
            (progn (outline-next-heading) (point)))))

    (add-to-list 'org-agenda-custom-commands
                 `("dt" "Agenda for upcoming TODO entries"
                   ((agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Past due")
                             (org-agenda-skip-function
                              'leuven--skip-entry-unless-overdue-deadline)
                             (org-deadline-warning-days 0)))
                    (agenda ""
                            ((org-agenda-overriding-header "Today/tomorrow")
                             (org-agenda-skip-function
                              'leuven--skip-entry-if-past-deadline)
                             (org-agenda-span 2)
                             (org-agenda-use-time-grid t)
                             (org-deadline-warning-days 0)))
                    (agenda ""
                            ((org-agenda-format-date "")
                             (org-agenda-overriding-header "Next 5 days")
                             (org-agenda-skip-function
                              '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                             (org-deadline-warning-days 7))))
                   ((org-agenda-clockreport-mode nil)
                    (org-agenda-span 'day)
                    (org-agenda-use-time-grid nil)
                    (org-agenda-write-buffer-name "Reminders"))
                   "~/org-agenda-upcoming-todo-entries.html") t)

    (defun leuven--skip-entry-if-deadline-or-schedule ()
      "Skip entries that have a deadline or that have a scheduled date."
      (let* ((dl (org-entry-get nil "DEADLINE"))
             (sd (org-entry-get nil "SCHEDULED")))
        (if (or (and dl
                     (not (equal dl "")))
                (and sd
                     (not (equal sd ""))))
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

    (add-to-list 'org-agenda-custom-commands
                 '("dT" "Agenda for all TODO entries"
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
