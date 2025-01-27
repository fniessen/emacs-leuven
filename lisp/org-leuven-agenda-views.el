(defun lvn-open-org-agenda-for-current-buffer (&optional arg)
  "Open the Org mode agenda with entries restricted based on ARG.
ARG determines the scope:
- No ARG (nil): Restrict to the current buffer's file.
- Single C-u (4): Restrict to the current buffer's file and all .org files
  in the current directory and its subdirectories.
- Double C-u (16): Restrict to the current buffer's file, all .org files,
  and all .txt files in the current directory and its subdirectories.
If the buffer is in a version-controlled project (e.g., vc-dir),
use the project's root directory instead of the current directory."
  (interactive "P")
  (let* ((current-dir
          (or (when (buffer-file-name) (file-name-directory (buffer-file-name)))
                                        ; File's directory.
              (when (vc-root-dir) (vc-root-dir))))
                                        ; Git root directory.
         (org-agenda-files
          (cond
           ((not current-dir)
            (error "Cannot determine a directory for this buffer"))
           ;; No argument: Restrict to the current buffer's file.
           ((not arg)
            (list (buffer-file-name)))
           ;; Single C-u: Current buffer + .org files in current dir/subdirs.
           ((equal arg '(4))
            (delete-dups
             (append (when (buffer-file-name) (list (buffer-file-name)))
                     (directory-files-recursively current-dir ".*\\.org$"))))
           ;; Double C-u: Current buffer + .org and .txt files in current dir/subdirs.
           ((equal arg '(16))
            (delete-dups
             (append (when (buffer-file-name) (list (buffer-file-name)))
                     (directory-files-recursively current-dir ".*\\(\\.org\\|\\.txt\\)$"))))))
         (org-default-notes-file nil))  ; Disable default notes file temporarily
    (org-agenda))                       ; Open the standard agenda view.
    ;; Uncomment below line if custom agenda view is needed:
    ;; (org-agenda nil "f.")               ; Generate a custom Org agenda view.
)

(global-set-key (kbd "<S-f6>")
                (lambda ()
                  (interactive)
                  (lvn-open-org-agenda-for-current-buffer nil)))
                                        ; Without arg: current buffer only.

(global-set-key (kbd "<C-f6>")
                (lambda ()
                  (interactive)
                  (lvn-open-org-agenda-for-current-buffer '(4))))
                                        ; With C-u: current buffer + .org files.

(global-set-key (kbd "<M-f6>")
                (lambda ()
                  (interactive)
                  (lvn-open-org-agenda-for-current-buffer '(16))))
                                        ; With C-u C-u: current buffer + .org + .txt files.

(setq org-agenda-custom-commands
      `(("d" "Dashboard" ; Shows all tasks...
         ;; High Priority Tasks.
         ((tags-todo "+PRIORITY={A}"
                     ((org-agenda-overriding-header "High Priority Tasks:")))
          ;; Tasks Due This Week.
          (tags-todo "+DEADLINE<=\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks Due This Week:")))
          ;; Scheduled Tasks (Next 14 Days).
          (tags-todo "+SCHEDULED<=\"<+14d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Scheduled Tasks (Next 14 Days):")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANX") 'deadline))))
          ;; Tasks with Future Deadlines.
          (tags-todo "+DEADLINE>\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks with Future Deadlines:")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANX")))
                      (org-agenda-sorting-strategy '(deadline-up))))
          ;; Task states: STRT, WAIT, TODO, MAYB.
          ,@(mapcar (lambda (state)
                      `(todo ,state
                             ((org-agenda-overriding-header (concat ,state " Tasks:"))
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                             (org-agenda-skip-entry-if 'regexp "\\[#A\\]"))))))
                    '("STRT" "WAIT" "TODO" "MAYB"))
          ;; Completed or Cancelled Tasks (Inactive tasks with clock entries this week).
          (todo "DONE|CANX"
                ((org-agenda-overriding-header "Completed or Cancelled Tasks (This Week):")
                 ;; Show tasks with clock entries in the current week.
                 (org-agenda-log-mode-items '(clock)) ; Show clocked entries.
                 (org-agenda-span 'week)              ; Filter to current week.
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notregexp "CLOCK:")) ; Only tasks with clock entries.
                 ;; (org-agenda-skip-function
                 ;;  '(org-agenda-skip-entry-if 'notregexp "CLOSED:")) ; Ensure the task is closed (completed or cancelled).
                 )))
         ;; Compact blocks.
         ((org-agenda-compact-blocks t))
         )))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((tags-todo "+PRIORITY={A}"
                     ((org-agenda-overriding-header "High Priority Tasks")))
          (tags-todo "+DEADLINE<=\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks Due This Week")))
          (tags-todo "+SCHEDULED<=\"<+14d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Scheduled Tasks (Next 14 Days)")))
          (tags-todo "+DEADLINE>\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Future Deadlines")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))  ; Skip tasks with SCHEDULED property.
                      (org-agenda-sorting-strategy '(deadline-up))))
          (todo "DONE|CANX"
                ((org-agenda-overriding-header "Completed or Cancelled Tasks")
                 (org-agenda-span 'week))))))) ; No duplicates!!!

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((tags-todo "+PRIORITY={A}"
                     ((org-agenda-overriding-header "High Priority Tasks")))
          (tags-todo "+DEADLINE<=\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks Due This Week")))
          (tags-todo "+SCHEDULED<=\"<+14d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Scheduled Tasks (Next 14 Days)")))
          (tags-todo "+DEADLINE>\"<+7d>\"-SCHEDULED>\"<+14d>\"-PRIORITY={A}" ; Exclude tasks scheduled within the next 14 days.
                     ((org-agenda-overriding-header "Future Deadlines")
                      (org-agenda-sorting-strategy '(deadline-up))))
          (todo "DONE|CANX"
                ((org-agenda-overriding-header "Completed or Cancelled Tasks")
                 (org-agenda-span 'week))))))) ; Duplicates!!!

(setq org-agenda-custom-commands
      '(("h" "High priority tasks"
         ((tags-todo "+PRIORITY={A}-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\""
                     ((org-agenda-overriding-header "High Priority Tasks")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (tags-todo "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\""
                     ((org-agenda-overriding-header "Other Current Tasks")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
         nil)))

(setq org-agenda-custom-commands
      '(("p" . "Priority views")
        ("pa" "A items" tags-todo "+PRIORITY={A}")
        ("pb" "B items" tags-todo "+PRIORITY={B}")
        ("pc" "C items" tags-todo "+PRIORITY={C}")))

(setq org-agenda-custom-commands
      '(("r" "Weekly Review Cgpt"
         ((tags-todo "+PRIORITY={A}"
                     ((org-agenda-overriding-header "High Priority Tasks (Unscheduled or Within 1 Week):")
                      (org-agenda-skip-function
                       '(lvn--org-skip-tasks-scheduled-over-7-days 'agenda))))
                                        ; Use custom skip function.
          ;; Tasks from the past week.
          (tags-todo "+SCHEDULED<\"<+7d>\"+DEADLINE<\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks from the Past Week:")))
          ;; Tasks due this week.
          (tags-todo "+DEADLINE<=\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks Due This Week:")))
          ;; Tasks scheduled for this week (Not due this week).
          (tags-todo "+SCHEDULED<=\"<+7d>\"-DEADLINE<=\"<+7d>\"-PRIORITY={A}"
                     ((org-agenda-overriding-header "Tasks Scheduled for This Week:")))
          ;; Active tasks (TODO, STRT, WAIT).
          (todo "TODO|STRT|WAIT"
                ((org-agenda-overriding-header "Active Tasks:")
                 (org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'deadline '("<+7d>"))
                       (org-agenda-skip-entry-if 'scheduled '("<+7d>"))
                       (org-agenda-skip-entry-if 'regexp "\\[#A\\]")))))
          ;; Completed tasks for review (can be adjusted as needed).
          (todo "DONE|CANX"
                ((org-agenda-overriding-header "Completed or Cancelled Tasks (This Week):")
                 ;; XXX Does not work!
                 (org-agenda-span 'week)))
          )
         ;; ;; Display in a compact view.
         ;; ((org-agenda-compact-blocks t))
         )))

(defun lvn--org-skip-tasks-scheduled-over-7-days (part)
  "Skip entries scheduled more than 7 days in the future."
  (let* ((scheduled-time (org-get-scheduled-time (point)))
         (seven-days-later (time-add (current-time) (* 7 24 60 60)))
         (headline (org-element-at-point)))

    (cond
     ;; Tasks without a scheduled time are never skipped.
     ((and (not scheduled-time))
      nil)

     ;; Tasks scheduled within 7 days are not skipped.
     ((and scheduled-time
           (not (time-less-p seven-days-later scheduled-time)))
      nil)

     ;; All other cases (including regular tasks scheduled within 7 days).
     (t (org-element-property :end headline)))))

(setq org-agenda-custom-commands
      '(("w" "Weekly Review Perp"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-start-day "-1d")
                      (org-agenda-overriding-header "This Week's Schedule:")))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority Tasks:")))
          (tags-todo "+DEADLINE<=\"<+7d>\"-PRIORITY=\"A\""
                     ((org-agenda-overriding-header "Tasks Due This Week:")))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting Tasks:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\[#A\\]"))))
          (todo "STRT"
                ((org-agenda-overriding-header "In-Progress Tasks:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\[#A\\]"))))
          ;; XXX Does not work!
          ;; (todo "DONE|CANX"
          ;;       ((org-agenda-overriding-header "Completed or Cancelled Tasks:")
          ;;        (org-agenda-span 7)))
          )
         ;; ((org-agenda-compact-blocks t))
         )))

(defun lvn-set-org-agenda-files-dotfiles-todo (&optional root-dir)
  "Set `org-agenda-files` to all TODO.org and TODO-xxx.org files
in the root directories of repositories under ROOT-DIR.
If ROOT-DIR is not provided, it defaults to `~/.dotfiles/`."
  (interactive
   (list (read-directory-name "Root directory: " "~/.dotfiles/" nil t)))
                                        ; Prompt interactively for ROOT-DIR,
                                        ; defaulting to `~/.dotfiles/`.
  (let ((todo-files '())) ;; Temporary list to store the found files.
    ;; Set ROOT-DIR to default if not provided.
    (setq root-dir (or root-dir "~/.dotfiles/"))
    ;; Iterate over the immediate subdirectories of ROOT-DIR.
    (dolist (subdir (directory-files root-dir t "^[^.]" t))
                                        ; Exclude hidden files ("." and "..").
      (when (and (file-directory-p subdir) ; Ensure it's a directory.
                 (not (file-symlink-p subdir))) ; Ignore symbolic links.
        ;; Look for files matching the pattern `TODO.org` or `TODO-xxx.org` in
        ;; the subdir.
        (dolist (todo-file (directory-files subdir t "^TODO\\(-.*\\)?\\.org$"))
          (push todo-file todo-files)))) ; Add each matching file to the list.
    ;; Update `org-agenda-files` with the found files.
    (setq org-agenda-files todo-files)
    ;; Message and return the list of files for verification.
    (message "Org agenda files set to: %s" org-agenda-files)
    org-agenda-files))

(defun lvn-set-org-agenda-files ()
  "Set `org-agenda-files` to all `.org` files in `org-directory`."
  (interactive)
  (setq org-agenda-files (directory-files org-directory t "\\.org$"))
  (message "Org agenda files set to: %s" org-agenda-files))

;;; org-leuven-agenda-views.el --- Org customized views

;;; Commentary:

;;; Code:

(require 'org-agenda)

;; Open up the Tasks.
(global-set-key
 (kbd "C-c T") (kbd "C-c a f . / RET"))

;; Display the calendar and tasks for today.
(global-set-key
 (kbd "<f7>") (kbd "C-c a f ."))

;; Display the hotlist.
(global-set-key
 (kbd "<S-f7>") (kbd "C-c a f h"))

;; Display calendar for 7 days.
(global-set-key
 (kbd "<C-f7>") (kbd "C-c a r c 7"))

(defconst leuven-org-completed-date-regexp
  (concat " \\("
          "CLOSED: \\[%Y-%m-%d"
          "\\|"
          "- State \"\\(DONE\\|CANX\\)\" * from .* \\[%Y-%m-%d"
          "\\|"
          "- State .* ->  *\"\\(DONE\\|CANX\\)\" * \\[%Y-%m-%d"
          "\\) ")
  "Matches any completion time stamp.")

;; Custom commands for the agenda -- start with a clean slate.
(setq org-agenda-custom-commands nil)

(add-to-list 'org-agenda-custom-commands
             '("c" . "CLARIFY...") t)

;; Display all tasks with the 'inbox' tag.
(add-to-list 'org-agenda-custom-commands
             `("ci" "Inbox"
               tags-todo "inbox"
               ((org-agenda-overriding-header "Inbox Tasks")))
             t)

(add-to-list 'org-agenda-custom-commands
             '("f" . "FOCUS...") t)

(add-to-list 'org-agenda-custom-commands
             `("f." "Today"
               (
                ;; Events.
                (agenda ""
                        ((org-agenda-entry-types '(:timestamp :sexp))
                         (org-agenda-overriding-header
                          (concat "CALENDAR Today "
                                  (format-time-string "%a %d" (current-time))
                                  ;; #("__________________" 0 12 (face (:foreground "gray")))
                                  ))
                         (org-agenda-span 'day)))
                ;; Unscheduled new tasks (waiting to be prioritized and scheduled).
                (tags-todo "LEVEL=2"
                           ((org-agenda-overriding-header "INBOX (Unscheduled)")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'scheduled))
                            ;; (org-agenda-files (list ,org-default-notes-file))
                            ))
                ;; List of all TODO entries with deadline today.
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                                        ; XXX Timed deadlines NOT shown!!!
                ;; List of all TODO entries with deadline before today.
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                ;; (agenda ""
                ;;         ((org-agenda-entry-types '(:deadline))
                ;;          (org-agenda-overriding-header "DUE DATES")
                ;;          (org-agenda-skip-function
                ;;           '(org-agenda-skip-entry-if 'todo 'done))
                ;;          (org-agenda-sorting-strategy
                ;;           '(priority-down time-down))
                ;;          (org-agenda-span 'day)
                ;;          (org-agenda-start-on-weekday nil)
                ;;          (org-agenda-time-grid nil)))
                (agenda ""
                        ((org-agenda-entry-types '(:scheduled))
                         (org-agenda-overriding-header "SCHEDULED")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-sorting-strategy
                          '(priority-down time-down))
                         (org-agenda-span 'day)
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-time-grid nil)))
                ;; List of all TODO entries completed today.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header "COMPLETED TODAY")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp)))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-start-with-clockreport-mode nil)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("W" "Work"
               ;; tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY={A}|FLAGGED"
               ((tags-todo "work-pirilampo"
                           ((org-agenda-overriding-header "Work")))
                )
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-up))))
             t) ; FIXME sort not OK

(add-to-list 'org-agenda-custom-commands
             '("fh" "Hotlist"
               ;; tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY={A}|FLAGGED"
               ((tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")))
                (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                           ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
                (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH PRIORITY")))
                (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                           ((org-agenda-overriding-header "FLAGGED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-when-regexp-matches))
                            (org-agenda-skip-regexp "\\[#A\\]")))
                ;; (tags-todo "DEADLINE=\"\"+PRIORITY<>{A}+FLAGGED|DEADLINE>\"<+1w>\"+PRIORITY<>{A}+FLAGGED"
                ;;            ((org-agenda-overriding-header "...FLAGGED...")))
                )
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-up))))
             t) ; FIXME sort not OK

(add-to-list 'org-agenda-custom-commands
             '("ff" "Hot N Fast"
               ;; tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY={A}|FLAGGED"
               ((tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")))
                (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                           ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
                (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH PRIORITY")))
                (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                           ((org-agenda-overriding-header "FLAGGED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-when-regexp-matches))
                            (org-agenda-skip-regexp "\\[#A\\]")))
                ;; (tags-todo "DEADLINE=\"\"+PRIORITY<>{A}+FLAGGED|DEADLINE>\"<+1w>\"+PRIORITY<>{A}+FLAGGED"
                ;;            ((org-agenda-overriding-header "...FLAGGED...")))
                )
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-up))))
             t) ; FIXME sort not OK.

(add-to-list 'org-agenda-custom-commands
             '("r" . "REVIEW...") t)

(add-to-list 'org-agenda-custom-commands
             '("ra" . "All Tasks...") t)

(add-to-list 'org-agenda-custom-commands
             '("rad" "All Tasks (grouped by Due Date)"
               ((tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+1d>\""
                           ((org-agenda-overriding-header "DUE TOMORROW")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+28d>\""
                           ((org-agenda-overriding-header "DUE LATER")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))

                ;; (todo ""
                ;;            ((org-agenda-overriding-header "NO DUE DATE")
                ;;             (org-agenda-skip-function
                ;;              '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={STRT}"
                           ((org-agenda-overriding-header "NO DUE DATE / STARTED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO<>{STRT\\|WAIT}"
                           ((org-agenda-overriding-header "NO DUE DATE / NEXT")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={WAIT}"
                           ((org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={MAYB}"
                           ((org-agenda-overriding-header "NO DUE DATE / MAYBE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                )
               ((org-agenda-sorting-strategy '(priority-down))
                (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
               "~/org___all-tasks-by-due-date.pdf") t)

(add-to-list 'org-agenda-custom-commands
             '("ra1" "All Tasks with a due date"
               ((alltodo ""))
               ((org-agenda-overriding-header "All Tasks (sorted by Due Date)")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notdeadline))
                (org-agenda-sorting-strategy '(deadline-up))))
             t)

(add-to-list 'org-agenda-custom-commands
             `("ra2" "All active tasks, by due date"
               ((agenda ""
                        ((org-agenda-overriding-header "Today")
                         ;; FIXME We don't see "timed" DEADLINE.
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
                          '(lvn--skip-entry-unless-deadline-in-n-days-or-more 1))
                         (org-deadline-warning-days 1)))
                (agenda ""
                        ((org-agenda-overriding-header "Next 5 days")
                         (org-agenda-skip-function
                          '(lvn--skip-entry-unless-deadline-in-n-days-or-more 2))
                         (org-deadline-warning-days 7)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Next 3 weeks")
                         (org-agenda-skip-function
                          '(lvn--skip-entry-unless-deadline-in-n-days-or-more 7))
                         (org-deadline-warning-days 28))))
               ((org-agenda-deadline-faces '((0.0 . default)))
                (org-agenda-start-with-clockreport-mode nil)
                (org-agenda-format-date "")
                (org-agenda-span 'day)
                (org-agenda-sorting-strategy '(deadline-up))
                (org-agenda-use-time-grid nil)
                (org-agenda-write-buffer-name "Reminders")))
             t)

(defun lvn--skip-entry-unless-deadline-in-n-days-or-more (n)
  "Skip entries that have no deadline or have a deadline earlier than N days from today."
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (when (or (not deadline)
              (string= deadline "")
              (org-time< deadline (+ (org-time-today) (* n 86400))))
      (outline-next-heading)
      (point))))

(defun lvn--skip-entry-unless-overdue-deadline ()
  "Skip entries that have no deadline or have a deadline later than or equal to today."
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (when (or (not deadline)
              (string= deadline "")
              (org-time>= deadline (org-time-today)))
      (outline-next-heading)
      (point))))

(defun lvn--skip-entry-if-past-deadline ()
  "Skip entries that have a deadline earlier than today."
  (let* ((deadline (org-entry-get nil "DEADLINE")))
    (when (and deadline (org-time< deadline (org-time-today)))
      (outline-next-heading)
      (point))))

(defun lvn--skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days (n1 n2)
  "Skip entries that have a deadline in less than N1 days, or that have a
scheduled date in less than N2 days, or that have no deadline nor scheduled."
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (scheduled (org-entry-get nil "SCHEDULED")))
    (when (or (and deadline
                   (not (string= deadline ""))
                   (org-time< deadline (+ (org-time-today) (* n1 86400))))
              (and scheduled
                   (not (string= scheduled ""))
                   (org-time< scheduled (+ (org-time-today) (* n2 86400))))
              (and (or (not deadline) (string= deadline ""))
                   (or (not scheduled) (string= scheduled ""))))
      (outline-next-heading)
      (point))))

(defun lvn--skip-entry-if-deadline-or-schedule ()
  "Skip entries that have a deadline or that have a scheduled date."
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (scheduled (org-entry-get nil "SCHEDULED")))
    (when (or (and deadline (not (string= deadline "")))
              (and scheduled (not (string= scheduled ""))))
      (outline-next-heading)
      (point))))

(add-to-list 'org-agenda-custom-commands
             '("ra3" "Agenda for all TODO entries"
               ((agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Past due")
                         (org-agenda-skip-function
                          'lvn--skip-entry-unless-overdue-deadline)
                         (org-deadline-warning-days 0)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Today/tomorrow")
                         (org-agenda-skip-function
                          'lvn--skip-entry-if-past-deadline)
                         (org-agenda-span 2)
                         (org-agenda-use-time-grid t)
                         (org-deadline-warning-days 0)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Next 12 days")
                         (org-agenda-skip-function
                          '(lvn--skip-entry-unless-deadline-in-n-days-or-more 2))
                         (org-deadline-warning-days 14)))
                (todo ""
                      ((org-agenda-overriding-header "Later")
                       (org-agenda-skip-function
                        '(lvn--skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days 15 2))
                       (org-agenda-sorting-strategy '(ts-up))))
                (todo ""
                      ((org-agenda-overriding-header "No due date")
                       (org-agenda-skip-function
                        'lvn--skip-entry-if-deadline-or-schedule))))
               ((org-agenda-start-with-clockreport-mode nil)
                (org-agenda-prefix-format " %i %?-12t% s")
                (org-agenda-span 'day)
                (org-agenda-use-time-grid nil)
                (org-agenda-sorting-strategy '(deadline-up)) ; FIXME sort does not work in "Past due", well in "Next 12 days".
                (org-agenda-write-buffer-name "List Review"))
               "~/org___agenda-all-todo-entries.html") t)

(add-to-list 'org-agenda-custom-commands
             '("rap" "All (Unscheduled) Tasks (grouped by Priority)"
               ((tags-todo "PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                (tags-todo "PRIORITY={B}"
                           ((org-agenda-overriding-header "MEDIUM")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                (tags-todo "PRIORITY={C}"
                           ((org-agenda-overriding-header "LOW")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                (todo "DONE|CANX"
                      ((org-agenda-overriding-header "COMPLETED")
                       (org-agenda-sorting-strategy '(priority-down))))))
             t)

(add-to-list 'org-agenda-custom-commands
             '("rt" . "Timesheet...") t)

;; Show what happened today.
(add-to-list 'org-agenda-custom-commands
             '("rtd" "Daily Timesheet"
               ((agenda ""))
               ((org-agenda-log-mode-items '(clock closed))
                (org-agenda-overriding-header "DAILY TIMESHEET")
                (org-agenda-show-log 'clockcheck)
                (org-agenda-span 'day)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil)))
             t)

;; Show what happened this week.
(add-to-list 'org-agenda-custom-commands
             '("rtw" "Weekly Timesheet"
               ((agenda ""))
               (
                ;; (org-agenda-format-date "")
                (org-agenda-overriding-header "WEEKLY TIMESHEET")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                (org-agenda-span 'week)
                (org-agenda-start-on-weekday 1)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("rc" . "Calendar...") t)

(add-to-list 'org-agenda-custom-commands
             '("rc7" "Events and appointments for 7 days"
               ((agenda ""))
               ((org-agenda-entry-types '(:timestamp :sexp))
                ;; (org-agenda-overriding-header "Calendar for 7 days")
                ;; (org-agenda-repeating-timestamp-show-all t)
                (org-agenda-span 'week)
                (org-agenda-format-date "\n%a %d")
                ;; (org-agenda-date-weekend ... new face ...)
                (org-agenda-time-grid nil)))
             t)

;; Calendar view for org-agenda.
(when (locate-library "calfw-org")

  (autoload 'cfw:open-org-calendar "calfw-org"
    "Open an Org schedule calendar." t)

  (add-to-list 'org-agenda-custom-commands
               '("rcm" "Calendar for current month"
                 (lambda (&rest ignore)
                   (cfw:open-org-calendar)))
               t)

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
             `("rC" "Completed view"
               (;; List of all TODO entries completed yesterday.
                (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                      ((org-agenda-overriding-header
                        (concat "YESTERDAY   "
                                (format-time-string "%a %d" (current-time-ndays-ago 1))
                                ;; #("__________________" 0 12 (face (:foreground "gray")))
                                ))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 1))))
                       (org-agenda-sorting-strategy '(priority-down))))
                ;; List of all TODO entries completed 2 days ago.
                (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                      ((org-agenda-overriding-header
                        (concat "2 DAYS AGO  "
                                (format-time-string "%a %d" (current-time-ndays-ago 2))))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 2))))
                       (org-agenda-sorting-strategy '(priority-down))))
                ;; List of all TODO entries completed 3 days ago.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header
                        (concat "3 DAYS AGO  "
                                (format-time-string "%a %d" (current-time-ndays-ago 3))))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 3))))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-start-with-clockreport-mode nil)))
             t)

(defun current-time-ndays-ago (n)
  "Return the current time minus N days."
  (time-subtract (current-time) (days-to-time n)))

(add-to-list 'org-agenda-custom-commands
             '("rx" "Completed tasks with no CLOCK lines"
               ((todo "DONE|CANX"
                      ((org-agenda-overriding-header "Completed tasks with no CLOCK lines")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'regexp
                          (format-time-string "  CLOCK: .*--.* =>  .*")))
                       (org-agenda-sorting-strategy '(priority-down))))))
             t)

(add-to-list 'org-agenda-custom-commands
             '("rr" "Recent items (past 7 days)"
               ;; Faster than tags.
               ((agenda ""))
               ((org-agenda-start-day "-7d")
                (org-agenda-span 7)
                (org-agenda-repeating-timestamp-show-all nil)
                ;; %s is only for agenda views
                ;; (org-agenda-prefix-format "%s")
                ;; maybe not make much difference ka
                ;; (org-agenda-use-tag-inheritance nil)
                (org-agenda-inactive-leader "Inactive:  ")
                (org-agenda-include-inactive-timestamps t)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("rw" "Weekly review"
               ((tags "CATEGORY={@Collect}&LEVEL=2|TODO={MAYB}"
                      ((org-agenda-overriding-header "INBOX (Unscheduled)")))

                (agenda ""
                        ((org-agenda-clockreport-mode t)
                         (org-agenda-format-date
                          (concat "\n"
                                  "%Y-%m-%d" " %a "
                                  (make-string (window-width) ?_)))
                         (org-agenda-overriding-header "PAST WEEK")
                         (org-agenda-prefix-format " %?-11t %i %-12:c% s")
                         (org-agenda-show-log 'clockcheck)
                         (org-agenda-span 7)
                         (org-agenda-start-day "-1w") ; recently done
                         (org-deadline-warning-days 0)))

                (agenda ""
                        ((org-agenda-overriding-header "NEXT MONTH")
                         (org-agenda-span 'month)
                         (org-agenda-start-day "+0d")
                         (org-deadline-warning-days 0) ; XXX
                         ))

                ;; FIXME we should show which tasks (don't) have CLOCK lines: archived vs. deleted.
                (todo "DONE|PROJDONE"
                      ((org-agenda-overriding-header
                        "Candidates to be archived")))

                ;; (stuck ""
                ;;        ((org-agenda-overriding-header "Stuck projects")))

                (todo "STRT"
                      ((org-agenda-overriding-header "IN PROGRESS")
                       (org-agenda-todo-ignore-scheduled nil)))

                (todo "TODO"        ; Don't include items from Inbox! XXX
                      ((org-agenda-overriding-header "ACTION LIST")))

                ;; Ignore scheduled and deadline entries, as they're visible
                ;; in the above agenda (for the past + for next month) or
                ;; scheduled/deadline'd for much later...
                (todo "WAIT"
                      ((org-agenda-format-date "")
                       (org-agenda-overriding-header "WAITING FOR")
                       (org-agenda-todo-ignore-deadlines 'all) ; Future?
                       (org-agenda-todo-ignore-scheduled t)))

                ;; Same reasoning as for WAIT.
                (todo "MAYB"
                      ((org-agenda-format-date "")
                       (org-agenda-overriding-header "MAYBE")
                       (org-agenda-todo-ignore-deadlines 'all)
                       (org-agenda-todo-ignore-scheduled t)
                       (org-agenda-filter-preset '("+MAYB"))))

                ;; ((org-agenda-start-with-clockreport-mode nil)
                ;;  (org-agenda-prefix-format " %i %?-12t% s")
                ;;  (org-agenda-write-buffer-name "Weekly task review"))
                ;; "~/org-weekly-review.html") t)
                ))
             t)

(add-to-list 'org-agenda-custom-commands
             '("rN" "Next"
               ((tags-todo "TODO<>{MAYB}"))
               ((org-agenda-overriding-header "List of all TODO entries with no due date (no MAYB)")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                (org-agenda-sorting-strategy '(priority-down))))
             t)

(add-to-list 'org-agenda-custom-commands
             '("p" "Past and unscheduled TODOs"
               ((todo ""
                      ((org-agenda-overriding-header "Unscheduled and Past Due")
                       (org-agenda-skip-function
                        '(lambda ()
                           (let ((scheduled (org-get-scheduled-time (point))))
                             (if (or (not scheduled) (org-time< scheduled (current-time)))
                                 nil
                               (org-agenda-skip-entry-if 'nottodo 'scheduled))))))))))

(add-to-list 'org-agenda-custom-commands
             '("rW" "Waiting for"
               ((tags-todo "TODO={WAIT}"))
               ((org-agenda-overriding-header "Waiting for")
                (org-agenda-sorting-strategy '(deadline-up))))
             t)
                                        ; FIXME deadline-up does not work.

(add-to-list 'org-agenda-custom-commands
             '("rP" "Projects"
               ((tags-todo "project-DONE-CANX"))
               ((org-agenda-overriding-header "Projects (High Level)")
                (org-agenda-sorting-strategy nil)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("+" . "MORE...") t)

;; Checking tasks that are assigned to me.
(add-to-list 'org-agenda-custom-commands
             `("+a" "Assigned to me"
               ((tags ,(concat "Assignee={" user-login-name "\\|"
                               user-mail-address "}")))
               ((org-agenda-overriding-header "ASSIGNED TO ME")))
             t)

(add-to-list 'org-agenda-custom-commands
             '("E" . "Exported agenda files...") t)

;; Exporting agenda views.
(add-to-list 'org-agenda-custom-commands
             '("Ea"
               ((agenda ""))
               (;; (org-tag-faces nil)
                (ps-landscape-mode t)
                (ps-number-of-columns 1))
               ("~/org-agenda.html" "~/org-agenda.pdf"))
             t)

(add-to-list 'org-agenda-custom-commands
             '("Ep" "Call list"
               ((tags-todo "phone"))
               ((org-agenda-prefix-format " %-20:c [ ] " )
                (org-agenda-remove-tags t)
                ;; (org-agenda-with-colors nil)
                (org-agenda-write-buffer-name
                 "Phone calls that you need to make")
                (ps-landscape-mode t)
                (ps-number-of-columns 1))
               ("~/org___calls.pdf"))
             t)

(add-to-list 'org-agenda-custom-commands
             '("A" . "ARCHIVE...") t)

(add-to-list 'org-agenda-custom-commands
             '("Aa" "Archive"
               ((tags-todo "ARCHIVE"))
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-down))))
             t)

(add-to-list 'org-agenda-custom-commands
             '("R" . "REFERENCE...") t)

(add-to-list 'org-agenda-custom-commands
             '("Rs" "Like s, but with extra files"
               ((search ""))
               ((org-agenda-text-search-extra-files
                 ;; FIXME Add `agenda-archives'
                 leuven-org-search-extra-files)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("RS" "Like s, but only TODO entries"
               ((search ""))
               ((org-agenda-text-search-extra-files
                 ;; FIXME Add `agenda-archives'
                 leuven-org-search-extra-files)))
             t)

(add-to-list 'org-agenda-custom-commands
             '("Rn" "Organize thoughts to refile"
               ((tags "refile|capture"))
               ((org-agenda-overriding-header "Refile stuff")))
             t)

;; Add a custom agenda command to create a sparse tree in the current buffer
;; showing entries with task markers: TODO, FIXME, XXX, or BUG.
(add-to-list 'org-agenda-custom-commands
             '("1" "Task markers (current buffer)"
               ((occur-tree "\\<\\(TODO\\|FIXME\\|XXX\\|BUG\\)\\>")))
             t)

(provide 'org-leuven-agenda-views)

;;; org-leuven-agenda-views.el ends here
