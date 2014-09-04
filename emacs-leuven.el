;;; emacs-leuven.el --- Emacs configuration file with more pleasant defaults

;; Copyright (C) 1999-2014 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven
;; Version: 20140904.1437
;; Keywords: emacs, dotfile, config

;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs configuration file with many packages already enabled and a more
;; pleasant set of defaults.
;;
;; Operating systems: supposed to work both for Windows and for Linux.
;;
;; Minimal .emacs configuration file:
;;
;;     (add-to-list 'load-path "/path/to/emacs-leuven/")
;;     (require 'emacs-leuven)
;;
;; To get more debug info about the packages getting loaded, add the following
;; line before requiring Emacs Leuven.
;;
;;     ;; show messages describing progress of loading Emacs Leuven
;;     (setq leuven-load-verbose t)
;;
;; To avoid be questioned about packages to add to your local Emacs
;; installation (though, I think you should install them), add the following
;; line before requiring Emacs Leuven.
;;
;;     ;; do not (try to) install extra Emacs packages
;;     (setq leuven-elpa-packages nil)
;;
;; For help on the Emacs Editor, see (info "(emacs)")  <== `C-x C-e' here!

;;; Citations:

;; - Show me your ~/.emacs and I will tell you who you are.
;;   -- Bogdan Maryniuk
;;
;; - Emacs is like a laser guided missile.  It only has to be slightly
;;   mis-configured to ruin your whole day.
;;   -- Sean McGrath
;;
;; - While any text editor can save your files, only Emacs can save your soul.
;;   -- Per Abrahamsen

;;; Code:

;; This file is only provided as an example.  Customize it to your own taste!

(message "* --[ Loading Emacs Leuven 20140904.1437]--")

;; turn on Common Lisp support
(eval-when-compile (require 'cl))       ; provide useful things like `setf'

;; uptimes
(when (string-match "XEmacs" (version))
  ;; XEmacs doesn't have `float-time'
  (defun float-time ()
    "Convert `current-time' to a floating point number."
    (multiple-value-bind (s0 s1 s2) (current-time)
      (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))

(defconst leuven-before-time (float-time)
  "Value of `float-time' before loading the Emacs Leuven library.")

;;; User Customizable Internal Variables

(defgroup leuven nil
  "Set of Emacs customizations (better defaults)."
  :group 'convenience
  :group 'text)

(defcustom leuven-load-verbose nil
  "If non-nil, means show messages describing progress of loading Emacs Leuven."
  :group 'emacs-leuven
  :type 'integer)

(when (and (string-match "GNU Emacs" (version))
           leuven-load-verbose)
  (defadvice message (before leuven-when-was-that activate)
    "Add time stamps to `message' output."
    (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T.")
                          (substring (format-time-string "%N") 0 3)
                          (format-time-string "] ")
                          (ad-get-arg 0)))))

;; allow quick include/exclude of setup parts -- DO NOT EDIT the DEFVAR!
(defvar leuven-chapter-0-environment t) ; required
(defvar leuven-chapter-0-loading-libraries t) ; required
(defvar leuven-chapter-0-debugging t)
(defvar leuven-chapter-47-packages t)
(defvar leuven-chapter-1-screen t)
(defvar leuven-chapter-6-exiting t)
(defvar leuven-chapter-7-basic t)
(defvar leuven-chapter-8-minibuffer t)
(defvar leuven-chapter-10-help t)
(defvar leuven-chapter-11-mark t)
(defvar leuven-chapter-12-killing t)
(defvar leuven-chapter-13-registers t)
(defvar leuven-chapter-14-display t)
(defvar leuven-chapter-15-search t)
(defvar leuven-chapter-16-fixit t)
(defvar leuven-chapter-17-keyboard-macros t)
(defvar leuven-chapter-18-files t)
(defvar leuven-chapter-19-buffers t)
(defvar leuven-chapter-20-windows t)
(defvar leuven-chapter-21-frames t)
(defvar leuven-chapter-22-international t)
(defvar leuven-chapter-23-major-and-minor-modes t)
(defvar leuven-chapter-24-indentation t)
(defvar leuven-chapter-25-text t)
(defvar leuven-chapter-25.9-org-mode t)
(defvar leuven-chapter-25.10-tex-mode t)
(defvar leuven-chapter-26-programs t)
(defvar leuven-chapter-27-building t)
(defvar leuven-chapter-28-maintaining t)
(defvar leuven-chapter-29-abbrevs t)
(defvar leuven-chapter-30-dired t)
(defvar leuven-chapter-31-calendar-diary t)
(defvar leuven-chapter-32-sending-mail t)
(defvar leuven-chapter-34-gnus t)
(defvar leuven-chapter-35-document-view t)
(defvar leuven-chapter-36-shell t)
(defvar leuven-chapter-37-emacs-server t)
(defvar leuven-chapter-38-printing t)
(defvar leuven-chapter-39-sorting t)
(defvar leuven-chapter-42-saving-emacs-sessions t)
(defvar leuven-chapter-45-hyperlinking t)
(defvar leuven-chapter-46-amusements t)
(defvar leuven-chapter-48-customization t)
(defvar leuven-chapter-AppG-ms-dos t)
(defvar leuven-chapter-XX-emacs-display t)
(defvar leuven-chapter-99-debugging t)

(defvar leuven--load-times-list nil
  "List of chapters and time to load them.")

(defmacro leuven--chapter (chapterid chaptername &rest body)
  "When CHAPTERID is not nil, report as CHAPTERNAME the evaluation of BODY.
Save execution times in the global list `leuven--load-times-list'."
  `(when ,chapterid
     (let (before-chapter-time
           this-chapter-time)
       (when leuven-load-verbose
         (message "** %s" ,chaptername))
       (setq before-chapter-time (float-time))
       (setq leuven--before-section-time (float-time)) ; init section time
       (progn ,@body)
       (leuven--section (concat "[" ,chaptername " ends here]") 'end-of-chapter)
                                        ; add fake closing section
       (setq this-chapter-time
             (format "%.3f" (- (float-time) before-chapter-time)))
       (add-to-list 'leuven--load-times-list
                    (concat "| " ,chaptername " "
                            "| " this-chapter-time " |")))))

(defvar leuven--before-section-time
  "Value of `float-time' before loading some section.")

(defun leuven--section (sectionname &optional end-of-chapter)
  "Report under SECTIONNAME the time taken since it was last saved.
Last time is saved in global variable `leuven--before-section-time'."
  (let ((this-section-time (- (float-time)
                              leuven--before-section-time)))
    (when leuven-load-verbose
      (when (not (equal this-section-time 0.000))
        (message "    Section time: %.3f s" this-section-time))
      (unless end-of-chapter (message "*** %s" sectionname)))
    ;; for next one
    (setq leuven--before-section-time (float-time))))

;;* Loading Libraries of Lisp Code for Emacs

(leuven--chapter leuven-chapter-0-loading-libraries "0 Loading Libraries"

  ;; load-path enhancement
  (defun leuven-add-to-load-path (this-directory)
    "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
    (when (and this-directory
               (file-directory-p this-directory))
      ;; TODO Add warning if directory does not exist
      (let* ((this-directory (expand-file-name this-directory)))

        ;; directories containing a `.nosearch' file (such as
        ;; `auctex-11.87\style') should not made part of `load-path'.
        ;; TODO `RCS' and `CVS' directories should also be excluded.
        (unless (file-exists-p (concat this-directory "/.nosearch"))
          (add-to-list 'load-path this-directory)
          (when leuven-load-verbose
            (message "(Info) Added `%s' to `load-path'" this-directory))))))

  ;; remember this directory
  (defconst leuven--directory
    (file-name-directory (or load-file-name (buffer-file-name)))
    "Directory path of Emacs Leuven.")

  (leuven-add-to-load-path
   (concat leuven--directory "site-lisp"))

  (defvar leuven-local-repos-directory "~/Public/Repositories/"
    "Directory containing additional Emacs Lisp public repositories.")

  (leuven-add-to-load-path
   (concat leuven-local-repos-directory "babel"))
  (leuven-add-to-load-path
   (concat leuven-local-repos-directory "emacs-bookmark-extension"))

  (defvar leuven-user-lisp-directory "~/.emacs.d/lisp/"
    "Directory containing personal additional Emacs Lisp packages.")

  (leuven-add-to-load-path leuven-user-lisp-directory)

  ;; require a feature/library if available; if not, fail silently
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
            t)                              ; necessary for correct behavior in
                                            ; conditional expressions
        (file-error
         (message "Requiring `%s'... missing" feature)
         nil))))

  ;; TEMPORARY
  (unless (fboundp 'with-eval-after-load)
    ;; wrapper around `eval-after-load' (added in GNU Emacs 24.4)
    (defmacro with-eval-after-load (mode &rest body)
      "`eval-after-load' MODE evaluate BODY."
      (declare (indent defun))
      `(eval-after-load ,mode
         '(progn ,@body))))

)                                       ; chapter 0-loading-libraries ends here

;;* Environment

(leuven--chapter leuven-chapter-0-environment "0 Environment"

;;** Type of OS

  (leuven--section "Type of OS")

  (defconst linuxp
    (eq system-type 'gnu/linux)
    "Running a GNU/Linux version of Emacs.")

  (defconst macp
    (eq system-type 'darwin)
    "Running a Mac OS version of Emacs.")

  (defconst win32p
    (eq system-type 'windows-nt)
    "Running a native Microsoft Windows version of Emacs.")

  (defconst cygwinp
    (eq system-type 'cygwin)
    "Running a Cygwin version of Emacs.")

;;** MS Windows

  ;; FIXME The path is not correct under Cygwin Emacs (gsprint.exe not found)
  (defconst windows-program-files-dir   ; sys-path
    (if win32p
        (file-name-as-directory (getenv "PROGRAMFILES"))
      "/usr/local/bin/")
    "Defines the default Windows Program Files folder.")

;;** Window system

  (leuven--section "Window system")

  (defconst consolep
    (eq window-system nil)
    "Running a text-only terminal.")

  (defconst x-window-p
    (eq window-system 'x)
    "Running a X Window system.")

;;** Testing Emacs versions

  (leuven--section "Emacs version")

  (defconst running-gnu-emacs
    (string-match "GNU Emacs" (version))
    "Running GNU Emacs.")

  (defconst running-xemacs
    (string-match "XEmacs" (version))
    "Running XEmacs.")

  (defmacro GNUEmacs (&rest body)
    "Execute any number of forms if running GNU Emacs."
    (list 'if running-gnu-emacs
          (cons 'progn body)))

  (defmacro GNUEmacs24 (&rest body)
    (list 'if (string-match "GNU Emacs 24" (version))
          (cons 'progn body)))

  (defmacro XEmacs (&rest body)
    "Execute any number of forms if running XEmacs."
    (list 'if running-xemacs
          (cons 'progn body)))

;;** Testing file accessibility

  (defun leuven--file-exists-and-executable-p (file)
    "Make sure the file FILE exists and is executable."
    (if file
        (if (file-executable-p file)
            file
          (message "(warning) Can't find executable `%s'" file)
          ;; sleep 1.5 s so that you can see the warning
          (sit-for 1.5))
      (error "Missing argument to \"leuven--file-exists-and-executable-p\"")))

;;** Init

  (leuven--section "Init")

  (XEmacs
    ;; don't load init file from `~/.xemacs/init.el' (and don't offer its
    ;; migration)
    (setq load-home-init-file t))

  ;; ensure that the echo area is always visible during the early stage of
  ;; startup (useful in case of error)
  (modify-all-frames-parameters
   '((height . 32)))

)                                       ; chapter 0 ends here

;;* Debugging

(leuven--chapter leuven-chapter-0-debugging "0 Debugging"

  ;; get the backtrace when uncaught errors occur
  (setq debug-on-error t)               ; will be unset at the end

  (XEmacs
    (setq stack-trace-on-error t))

  ;; hit `C-g' while it's frozen to get an Emacs Lisp backtrace
  (setq debug-on-quit nil)

)                                       ; chapter 0 ends here

;;* 47 Emacs Lisp (info "(emacs)Packages")

(leuven--chapter leuven-chapter-47-packages "47 Emacs Lisp Packages"

;;** 47.2 Package Installation

  (leuven--section "47.2 Package Installation")

  ;; simple package system for GNU Emacs
  (GNUEmacs
    (try-require 'package)
    (with-eval-after-load "package"

      ;; archives from which to fetch
      (setq package-archives
            (append '(("org"       . "http://orgmode.org/elpa/")
                      ("melpa"     . "http://melpa.milkbox.net/packages/")
                      ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                      ;; ("ELPA"      . "http://tromey.com/elpa/")
                      )
                    package-archives))

      ;; load the latest version of all installed packages, and activate them
      (package-initialize)              ; add ALL ELPA subdirs to `load-path'
                                        ; and load `<pkg>-autoloads.el'

      (defcustom leuven-elpa-packages
        '(ace-jump-mode auctex auto-complete bbdb bookmark+ boxquote calfw circe
          company csv-mode dictionary dired+ dired-single ess
          fill-column-indicator flycheck fuzzy git-commit-mode graphviz-dot-mode
          helm htmlize idle-require info+ interaction-log ledger-mode
          leuven-theme multi-term multiple-cursors pager powerline rainbow-mode
          redo+ tidy unbound yasnippet
          ;; jabber multi-term paredit redshank w3m
          )
        "A list of packages to ensure are installed at Emacs startup."
        :group 'emacs-leuven
        :type '(repeat (string)))

      (defun leuven--missing-elpa-packages ()
        "List packages to install which are neither built-in nor already installed."
        (let (missing-elpa-packages)
          (dolist (pkg leuven-elpa-packages)
            (unless (or (package-installed-p pkg)
                        (locate-library (symbol-name pkg)))
              (push pkg missing-elpa-packages)))
          missing-elpa-packages))

      ;; propose to install all the packages specified in `leuven-elpa-packages'
      ;; which are missing
      (let ((missing-elpa-packages (leuven--missing-elpa-packages)))
        (when missing-elpa-packages
          ;; download once the ELPA archive description
          (package-refresh-contents)    ; Ensure that the list of packages is
                                        ; up-to-date.  Otherwise, new packages
                                        ; (not present in the cache of the ELPA
                                        ; contents) won't install.
          (dolist (pkg missing-elpa-packages)
            (if (yes-or-no-p (format "Install ELPA package `%s'? " pkg))
                (ignore-errors
                  (package-install pkg))
                                        ; must be run after initializing
                                        ; `package-initialize'
              (message (concat "Customize `leuven-elpa-packages' to ignore "
                               "the `%s' package at next startup...") pkg)
              (sit-for 1.5)))))

      ;; don't truncate package names in Emacs package list
      (add-hook 'package-menu-mode-hook
                (lambda ()
                  (setq tabulated-list-format
                        [("Package" 28 package-menu--name-predicate)
                         ("Version" 14 nil)
                         ("Status"  10 package-menu--status-predicate)
                         ("Description" 0 nil)])
                  (tabulated-list-init-header)))))

)                                       ; chapter 47 ends here

  ;; load elisp libraries while Emacs is idle
  (try-require 'idle-require)

  ;; fail-safe for `idle-require'
  (if (not (featurep 'idle-require))
    (defun idle-require (feature &optional file noerror)
      (try-require feature)))

  (with-eval-after-load "idle-require"

    ;; idle time in seconds after which autoload functions will be loaded
    (setq idle-require-idle-delay 5)

    ;; time in seconds between automatically loaded functions
    (setq idle-require-load-break 2))

  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'idle-require-mode)
                ;; starts loading
                (idle-require-mode 1))))

;;* 1 The Organization of the (info "(emacs)Screen")

(leuven--chapter leuven-chapter-1-screen "1 The Organization of the Screen"

;;** 1.2 The (info "(emacs)Echo Area")

  (leuven--section "1.2 (emacs) The Echo Area")

  ;; don't truncate the message log buffer when it becomes large
  (setq message-log-max t)

)                                       ; chapter 1 ends here

;;* 6 (info "(emacs)Exiting") Emacs

(leuven--chapter leuven-chapter-6-exiting "6 Exiting Emacs"

  ;; quit with Alt + F4
  (global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)

)                                       ; chapter 6 ends here

;;* 7 (info "(emacs)Basic") Editing Commands

(leuven--chapter leuven-chapter-7-basic "7 Basic Editing Commands"

;;** 7.1 (info "(emacs)Inserting Text")

  (leuven--section "7.1 (emacs)Inserting Text")

  ;; enter characters by their code in octal (for `C-q NNN <RET>')
  (setq read-quoted-char-radix 8)       ; 16 for hexadecimal (for Unicode char)

;;** 7.2 (info "(emacs)Moving Point") Location

  (leuven--section "7.2 (emacs)Moving Point Location")

  ;; don't add newlines to end of buffer when scrolling
  (setq next-line-add-newlines nil)

  ;; print the current buffer line number
  (global-set-key (kbd "M-G") 'what-line)

;;** 7.4 (info "(emacs)Basic Undo")ing Changes

  (leuven--section "7.4 (emacs)Basic Undoing Changes")

  ;; undo some previous changes
  (global-set-key (kbd "<f11>") 'undo)

  ;; redo the most recent undo
  (when (locate-library "redo+")
    (autoload 'redo "redo+" "Redo the the most recent undo." t)
    (global-set-key (kbd "<S-f11>") 'redo))

)                                       ; chapter 7 ends here

;;* 8 The (info "(emacs)Minibuffer")

(leuven--chapter leuven-chapter-8-minibuffer "8 The Minibuffer"

  ;; how long to display an echo-area message when the minibuffer is active
  (setq minibuffer-message-timeout 0.5)

;;** 8.3 (info "(emacs)Minibuffer Edit")ing

  (leuven--section "8.3 (emacs)Minibuffer Editing")

  ;; minibuffer and echo area windows resize vertically as necessary to fit
  ;; the text displayed in them
  (setq resize-mini-windows t)

;;** 8.4 (info "(emacs)Completion")

  (leuven--section "8.4 (emacs)Completion")

  ;; don't consider case significant in completion (GNU Emacs default)
  (XEmacs
    (setq completion-ignore-case t))

  ;; ignore case when reading a file name
  (setq read-file-name-completion-ignore-case t)

  ;; ignore case when reading a buffer name
  (setq read-buffer-completion-ignore-case t)

  ;; provide the same facility of `ls --color' inside Emacs
  (when (locate-library "dircolors")
    (autoload 'dircolors "dircolors" nil t)
    (add-hook 'completion-list-mode-hook 'dircolors))

)                                       ; chapter 8 ends here

;;* 10 (info "(emacs)Help")

(leuven--chapter leuven-chapter-10-help "10 Help"

;;** 10.1 (info "(emacs)Help Summary")

  (leuven--section "10.1 (emacs)Help Summary")

  ;; find convenient unbound keystrokes (undefined key bindings)
  (autoload 'describe-unbound-keys "unbound"
    "Display a list of unbound keystrokes of complexity no greater than MAX." t)

  ;; avoid the description of all minor modes
  (defun describe-major-mode ()
    "Describe only `major-mode'."
    (interactive)
    (describe-function major-mode))

  ;; look up subject in (the indices of the) Emacs Lisp manual
  (global-set-key (kbd "C-h E") 'elisp-index-search)

;;** 10.4 (info "(emacs)Apropos")

  (leuven--section "10.4 (emacs)Apropos")

  (with-eval-after-load "apropos"

    ;; check all variables and non-interactive functions as well
    (setq apropos-do-all t))

  ;; show variables whose name matches the pattern
  (GNUEmacs
    (global-set-key (kbd "C-h A") 'apropos-variable))

;;** 10.8 (info "(emacs)Misc Help")

  (leuven--section "10.8 (emacs)Misc Help")

  ;; enter Info documentation browser
  (global-set-key (kbd "<f1>") 'info)

  (defun describe-symbol-at-point ()
    "Get help for the symbol at point."
    (interactive)
    (let ((sym (intern-soft (current-word))))
      (unless
          (cond ((null sym))
                ((not (eq t (help-function-arglist sym)))
                 (describe-function sym))
                ((boundp sym)
                 (describe-variable sym)))
        (message "nothing"))))

  (global-set-key (kbd "<f1>") 'describe-symbol-at-point)

  ;; display symbol definitions, as found in the relevant manual
  ;; (for AWK, C, Emacs Lisp, LaTeX, M4, Makefile, Sh and other languages that
  ;; have documentation in Info)
  (global-set-key (kbd "<C-f1>") 'info-lookup-symbol)

  (with-eval-after-load "info"
    ;; list of directories to search for Info documentation files (in the order
    ;; they are listed)
    (when win32p
      (setq Info-directory-list
            `(,(expand-file-name
                (concat (file-name-directory (locate-library "org"))
                        "../doc/"))
              "c:/cygwin/usr/share/info/"
              ,@Info-directory-list)))

    (GNUEmacs
      (try-require 'info+)
      (with-eval-after-load "info+"

        ;; show breadcrumbs in the header line
        (setq Info-breadcrumbs-in-header-flag t)

        ;; don't show breadcrumbs in the mode line
        (setq Info-breadcrumbs-in-mode-line-mode nil)))

    ;; some info related functions
    ;; (to insert links such as `(info "(message)Insertion Variables")')
    (when (locate-library "rs-info")
      (autoload 'rs-info-insert-current-node "rs-info"
        "Insert reference to current Info node using STYPE in buffer." t)
      (autoload 'rs-info-boxquote "rs-info"
        "Yank text (from an info node), box it and use current info node as title." t)
      (autoload 'rs-info-reload "rs-info"
        "Reload current info node." t)
      (autoload 'rs-info-insert-node-for-variable "rs-info"
        "Insert a custom style info node for the top level form at point." t)
      (defalias 'boxquote-info 'rs-info-boxquote))
    )

  ;; get a Unix manual page of the item under point
  (global-set-key (kbd "<S-f1>") 'man-follow)

  (with-eval-after-load "man"
    ;; make the manpage the current buffer in the current window
    (setq Man-notify-method 'pushy))

  ;; alias man to woman
  (defalias 'man 'woman)

  ;; decode and browse Unix man-pages "W.o. (without) Man"
  (with-eval-after-load "woman"
    (defalias 'man 'woman))

)                                       ; chapter 10 ends here

;;* 11 The (info "(emacs)Mark") and the Region

(leuven--chapter leuven-chapter-11-mark "11 The Mark and the Region"

  ;; inserting text while the mark is active causes the text in the region to be
  ;; deleted first
  (delete-selection-mode 1)

;; multiple cursors for Emacs
(try-require 'multiple-cursors)
(with-eval-after-load "multiple-cursors"

  ;; add a cursor to each (continuous) line in the current region
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

  ;; add a cursor and region at the next part of the buffer forwards that
  ;; matches the current region
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)

  ;; add a cursor and region at the next part of the buffer backwards that
  ;; matches the current region
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

  ;; mark all parts of the buffer that matches the current region
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

)                                       ; chapter 11 ends here

;;* 12 (info "(emacs)Killing") and Moving Text

(leuven--chapter leuven-chapter-12-killing "12 Killing and Moving Text"

;;** 12.1 (info "(emacs)Deletion and Killing")

  (leuven--section "12.1 (emacs)Deletion and Killing")

  (GNUEmacs
;; old ([2012-09-07 Fri] remove "compile" after "activate")

    ;; add the ability to copy or cut the current line without marking it
    ;; (no active region) -- idea stolen from SlickEdit
    (defadvice kill-ring-save (before leuven-slick-copy activate)
      "When called with no active region, copy the current line instead."
      (interactive
       (if (use-region-p) (list (region-beginning) (region-end))
         (message "Copied the current line")
         (list (line-beginning-position)
               (line-beginning-position 2)))))

    (defadvice kill-region (before leuven-slick-cut activate)
      "When called with no active region, kill the current line instead."
      (interactive
       (if (use-region-p) (list (region-beginning) (region-end))
         (list (line-beginning-position)
               (line-beginning-position 2))))))

;; new

    ;; (defadvice kill-ring-save (around leuven-slick-copy activate)
    ;;   "When called interactively with no active region, copy a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p 'any)))
    ;;       ad-do-it
    ;;     (kill-new (buffer-substring (line-beginning-position)
    ;;                                 (line-beginning-position 2)))
    ;;     (message "Copied line")))
    ;;
    ;; (defadvice kill-region (around leuven-slick-cut activate)
    ;;   "When called interactively with no active region, kill a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p 'any)))
    ;;       ad-do-it
    ;;     (kill-new (filter-buffer-substring (line-beginning-position)
    ;;                                        (line-beginning-position 2) t))))
    ;;
    ;; (defun yank-line (string)
    ;;   "Insert STRING above the current line."
    ;;   (beginning-of-line)
    ;;   (unless (= (elt string (1- (length string))) ?\n)
    ;;     (save-excursion (insert "\n")))
    ;;   (insert string))

;; XXX perf 2.00 s requiring bytecomp and warnings...

;;** 12.2 (info "(emacs)Yanking")

  (leuven--section "12.2 (emacs)Yanking")

  ;; auto-indentation of pasted code in the listed programming modes (fall back
  ;; to default, non-indented yanking by preceding the yanking command `C-y'
  ;; with `C-u')
  (dolist (command '(yank yank-pop))
    (eval `(defadvice ,command (after leuven-indent-region activate)
             (and (not current-prefix-arg)
                  (derived-mode-p 'prog-mode)
                  (let ((mark-even-if-inactive transient-mark-mode))
                    (indent-region (region-beginning) (region-end) nil))))))

;;** 12.3 (info "(emacs)Cut and Paste")

  (leuven--section "12.3 (emacs)Cut and Paste on Graphical Displays")

  ;; copy/paste with Gnome desktop
  (GNUEmacs
    ;; ;; cutting and pasting uses the clipboard
    ;; (setq x-select-enable-clipboard t) ; default in Emacs 24

    ;; make cut, copy and paste (keys and menu bar items) use the clipboard
    (menu-bar-enable-clipboard))

)                                       ; chapter 12 ends here

;;* 13 (info "(emacs)Registers")

(leuven--chapter leuven-chapter-13-registers "13 Registers"

  ;; ;; Enable position saving through shortcuts.
  ;; ;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
  ;; (global-set-key [C-f1] (lambda () (interactive) (point-to-register ?1)))
  ;; (global-set-key [C-f2] (lambda () (interactive) (point-to-register ?2)))
  ;; (global-set-key [C-f3] (lambda () (interactive) (point-to-register ?3)))
  ;; (global-set-key [C-f4] (lambda () (interactive) (point-to-register ?4)))

  ;; (defun jump-to-register-other (reg)
  ;; (other-window 1)
  ;; (jump-to-register reg)
  ;; (hilit-recenter (/ (window-height) 2)))

  ;; (defun jump-to-register-here (reg)
  ;; (jump-to-register reg)
  ;; (hilit-recenter (/ (window-height) 2)))

  ;; ;; Move to saved position with F1 F2 F3 and F4
  ;; (global-set-key [f1] (lambda () (interactive) (jump-to-register-here ?1)))
  ;; (global-set-key [f2] (lambda () (interactive) (jump-to-register-here ?2)))
  ;; (global-set-key [f3] (lambda () (interactive) (jump-to-register-here ?3)))
  ;; (global-set-key [f4] (lambda () (interactive) (jump-to-register-here ?4)))

;;** 13.7 (info "(emacs)Bookmarks")

  (leuven--section "13.7 (emacs)Bookmarks")

  (with-eval-after-load "bookmark"

    ;; where to save the bookmarks
    (setq bookmark-default-file "~/.emacs.d/bookmarks.bmk")
                                        ;! a .txt extension would load Org at
                                        ;! the time `bookmark' is required!

    ;; each command that sets a bookmark will also save your bookmarks
    (setq bookmark-save-flag 1)

    ;; extensions to standard library `bookmark.el'
    (try-require 'bookmark+)
    (with-eval-after-load "bookmark+"

      ;; automatically highlight bookmarks when set
      (setq bmkp-auto-light-when-set 'any-bookmark)

      ;; automatically highlight bookmarks when jumped to
      (setq bmkp-auto-light-when-jump 'any-bookmark)

      ;; don't propertize bookmark names to hold full bookmark data
      (setq bmkp-propertize-bookmark-names-flag nil)))
                                        ; we will often be going back and forth
                                        ; between using Bookmark+ and using
                                        ; vanilla Emacs

  ;; quickly jump to a position in the current view
  (try-require 'ace-jump-mode)
  (with-eval-after-load "ace-jump-mode"
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

)                                       ; chapter 13 ends here

;;* 14 Controlling the (info "(emacs)Display")

(leuven--chapter leuven-chapter-14-display "14 Controlling the Display"

;;** 14.1 (info "(emacs)Scrolling")

  (leuven--section "14.1 (emacs)Scrolling")

  ;; keep screen position of point when scrolling
  (setq scroll-preserve-screen-position t)

  ;; better scrolling in Emacs (doing a <PageDown> followed by a <PageUp> will
  ;; place the point at the same place)
  (when (locate-library "pager")
    (autoload 'pager-page-up "pager"
      "Like scroll-down, but moves a fixed amount of lines." t)
    (autoload 'pager-page-down "pager"
      "Like scroll-up, but moves a fixed amount of lines." t)

    (global-set-key (kbd "<prior>") 'pager-page-up)
    (global-set-key (kbd "<next>") 'pager-page-down))

;;** 14.3 (info "(emacs)Auto Scrolling")

  (leuven--section "14.3 (emacs)Auto Scrolling")

  ;; scroll only one line at a time (redisplay will never recenter point)
  (setq scroll-conservatively 10000)    ; or `most-positive-fixnum'

  ;; number of lines of margin at the top and bottom of a window
  (setq scroll-margin 1) ; or 3?        ; also for `isearch-forward'

  ;; scrolling down looks much better
  (setq auto-window-vscroll nil)

;;** 14.5 (info "(emacs)Narrowing")

  (leuven--section "14.5 (emacs)Narrowing")

  ;; enable the use of the command `narrow-to-region' without confirmation
  (put 'narrow-to-region 'disabled nil)

;;** 14.12 (info "(emacs)Font Lock")

  (leuven--section "14.12 (emacs)Font Lock")

  (XEmacs
    ;; stop showing that annoying progress bar when fontifying
    (setq progress-feedback-use-echo-area nil)

    ;; enable Font Lock mode
    (font-lock-mode))

  ;; highlight FIXME notes
  (defvar leuven-highlight-keywords
    "\\(TODO\\|FIXME\\|XXX\\|BUG\\)"
    "Patterns to highlight.")

  (defvar leuven-highlight-keywords-in-org
    "\\(FIXME\\|XXX\\|BUG\\)"
    "Patterns to highlight (for Org mode only).
  The goal is to ensure no conflict with the Org mode TODO keyword.")

  (defface leuven-highlight-face
    '((t (:foreground "#CC0000" :background "#FFFF88")))
    "Face for making FIXME and other warnings stand out.")

  ;; add highlighting keywords for selected major modes only
  (dolist (mode '(fundamental-mode
                  text-mode))
    (font-lock-add-keywords mode
     `((,leuven-highlight-keywords 1 'leuven-highlight-face prepend))
     'end))

  ;; add highlighting keywords for Org mode only
  (dolist (mode '(org-mode))
    (font-lock-add-keywords mode
     `((,leuven-highlight-keywords-in-org 1 'leuven-highlight-face prepend))
     'end))

  ;; add highlighting keywords for selected major modes *and* all major modes
  ;; derived from them
  (dolist (hook '(prog-mode-hook
                  ;; text-mode-hook     ; avoid Org
                  css-mode-hook         ; [parent: fundamental]
                  latex-mode-hook
                  shell-mode-hook       ; [parent: fundamental]
                  ssh-config-mode-hook))
    (add-hook hook
     (lambda ()
       (font-lock-add-keywords nil      ; in the current buffer
        `((,leuven-highlight-keywords 1 'leuven-highlight-face prepend)) 'end))))
        ;; FIXME                      0                        t          t

  ;; just-in-time fontification
  (with-eval-after-load "jit-lock"

    ;; stealth fontification should show status messages
    (setq jit-lock-stealth-verbose t))

  ;; colorize color names in buffers
  (GNUEmacs
    (when (locate-library "rainbow-mode")
      (autoload 'rainbow-mode "rainbow-mode"
        "Colorize strings that represent colors." t)))

;;** 14.13 (info "(emacs)Highlight Interactively") by Matching

  (leuven--section "14.13 (emacs)Highlight Interactively by Matching")

  (GNUEmacs
    ;; enable Hi Lock mode for all buffers
    (global-hi-lock-mode 1)

    (defun leuven-highlight-current-word ()
      "Highlight the word that point is on throughout the buffer.
    If already highlighted, unhighlight the word at point."
      (interactive)
      (let ((cword (current-word t)))
        (when cword
          (if (not (equal (get-text-property (point) 'face) 'highlight))
              (highlight-regexp (regexp-quote cword) 'highlight)
            (unhighlight-regexp (regexp-quote cword))))))

    ;; emulation of Vim's `*' search
    (global-set-key (kbd "C-*") 'leuven-highlight-current-word))

;;** 14.15 (info "(emacs)Displaying Boundaries")

  (leuven--section "14.15 (emacs)Displaying Boundaries")

  ;; visually indicate buffer boundaries and scrolling in the fringe
  (setq indicate-buffer-boundaries t)   ; 'left

;;** 14.16 (info "(emacs)Useless Whitespace")

  (leuven--section "14.16 (emacs)Useless Whitespace")

  ;; highlight trailing whitespaces in all modes
  (setq-default show-trailing-whitespace t)

  ;; nuke all trailing whitespaces in the buffer
  (add-hook 'before-save-hook
            (lambda ()
              ;; except for Message mode where "-- " is the signature separator
              ;; (for when using emacsclient to compose emails and doing C-x #)
              (unless (eq major-mode 'message-mode)
                (delete-trailing-whitespace))))

  ;; visually indicate empty lines after the buffer end in the fringe
  (setq-default indicate-empty-lines t)

  ;; ;; control highlighting of non-ASCII space and hyphen chars, using the
  ;; ;; `nobreak-space' or `escape-glyph' face respectively
  ;; (setq nobreak-char-display t)      ; default

  (GNUEmacs
    ;; whitespace mode
    (add-hook 'text-mode-hook 'whitespace-mode)

    (add-hook 'prog-mode-hook 'whitespace-mode)

    (with-eval-after-load "whitespace"

      ;; which kind of blank is visualized
      (setq whitespace-style
            '(face trailing tabs
              ;; lines-tail
              indentation::space space-mark tab-mark))

      ;; column beyond which the line is highlighted
      (setq whitespace-line-column 80)

      ;; mappings for displaying characters
      (setq whitespace-display-mappings
            '((space-mark ?\u00A0 [?\u2423] [?.]) ; nbsp - open box (bottom square bracket)
              (space-mark ?\u202F [?\u00B7] [?.]) ; narrow nbsp - centered dot
              (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))) ; tab - left quote mark
      ))

  ;; ;; show zero-width spaces
  ;; (font-lock-add-keywords nil
  ;;  `((,(format "\\(%c\\)" ?\u200B) ; #\ZERO_WIDTH_SPACE
  ;;     (1 (progn (compose-region (match-beginning 1) (match-end 1)
  ;;                               ?\u2B1B ; #\BLACK_LARGE_SQUARE
  ;;                               'decompose-region)
  ;;               nil)))))

;;** 14.18 (info "(emacs)Optional Mode Line") Features

  (leuven--section "14.18 (emacs)Optional Mode Line Features")

  ;; show the column number in each mode line
  (column-number-mode 1)

  ;; use inactive face for mode line in non-selected windows
  (setq mode-line-in-non-selected-windows t)

  ;; ;; show buffer position like a scroll bar in mode line
  ;; (try-require 'sml-modeline)
  (with-eval-after-load "sml-modeline"

    ;; mode line indicator total length
    (setq sml-modeline-len 10)

    (setq sml/no-confirm-load-theme t)

    (sml-modeline-mode))

  (with-eval-after-load "smart-mode-modeline"

    ;; theme `smart-mode-line' should use
    (setq sml/theme 'respectful))

  (add-hook 'after-init-hook 'powerline-default-theme)

;;** 14.19 How (info "(emacs)Text Display")ed

  (leuven--section "14.19 (emacs)How Text Displayed")

  (defun leuven-dos2unix ()
    "Convert a plain text file in DOS format to Unix format."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
        (replace-match ""))))

  (defun leuven-unix2dos ()
    "Convert a plain text file in Unix format to DOS format."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\r\n"))))

;;** 14.20 The (info "(emacs)Cursor Display")

  (leuven--section "14.20 (emacs)The Cursor Display")

  ;; cursor to use
  (setq-default cursor-type 'bar)

  ;; cursor of the selected window blinks
  (XEmacs
    (blink-cursor-mode))

  (GNUEmacs
    (defvar leuven-default-cursor-color "black"
      "Default cursor color.")

    ;; using cursor color to indicate some modes (read-only, overwrite and
    ;; insert modes)
    (defun leuven--set-cursor-color-according-to-mode ()
      "Change cursor color according to some minor modes."
      (let ((color (if buffer-read-only
                       "purple1"
                     (if overwrite-mode
                         "red"
                       ;; normal insert mode
                       leuven-default-cursor-color))))
        (set-cursor-color color)))

    (add-hook 'post-command-hook 'leuven--set-cursor-color-according-to-mode))

;;** 14.21 (info "(emacs)Line Truncation")

  (leuven--section "14.21 (emacs)Line Truncation")

  ;; switch wrap mode from "wrap long lines to next screen line" (continued
  ;; line) to "non-wrap", or vice-versa
  (global-set-key (kbd "C-c t") 'toggle-truncate-lines)

  ;; respect the value of `truncate-lines' in all windows less than the full
  ;; width of the frame
  (setq truncate-partial-width-windows nil)

;;** 14.23 (info "(emacs)Display Custom")ization

  (leuven--section "14.23 (emacs)Display Customization")

  ;; echo what I'm typing *immediately*
  (setq echo-keystrokes 0.01)

  ;; exhaustive log of interactions with Emacs (display keystrokes, etc.)
  (with-eval-after-load "interaction-log"

    ;; ;; maximum number of lines to keep in the *Emacs Log* buffer
    ;; (setq ilog-log-max 10)

    ;; enable logging of keys, commands, file loads and messages
    (interaction-log-mode 1)

    ;; hotkey for showing the log buffer
    (global-set-key
      (kbd "C-h C-l")
      (lambda ()
        (interactive)
        (display-buffer ilog-buffer-name))))

)                                       ; chapter 14 ends here

;;* 15 (info "(emacs)Search")ing and Replacement

(leuven--chapter leuven-chapter-15-search "15 Searching and Replacement"

;;** 15.1 (info "(emacs)Incremental Search")

  (leuven--section "15.1 (emacs)Incremental Search")

  ;; FIXME Error when selecting search string from kill ring (`M-p')
  ;; ;; always exit searches at the beginning of the expression found
  ;; (add-hook 'isearch-mode-end-hook 'isearch-goto-match-beginning)
  ;;
  ;; (defun isearch-goto-match-beginning ()
  ;;   "Use with isearch hook to end search at first char of match."
  ;;   (when isearch-forward (goto-char isearch-other-end)))

  (GNUEmacs
    ;; ;; incremental search/query-replace will open the contents
    ;; (setq search-invisible 'open)       ; XXX

    ;; don't re-hide an invisible match right away
    (setq isearch-hide-immediately nil)) ; XXX

  ;; scrolling commands are allowed during incremental search (without
  ;; canceling Isearch mode)
  (setq isearch-allow-scroll t)

  (GNUEmacs
    ;; fuzzy matching utilities (a must-have)
    (when (locate-library "fuzzy")

      (add-hook 'isearch-mode-hook
                (lambda ()
                  (require 'fuzzy))))

    (with-eval-after-load "fuzzy"
      (turn-on-fuzzy-isearch)))

;;** 15.5 (info "(emacs)Regexp Search")

  (leuven--section "15.4 (emacs)Regexp Search")

  (defun leuven-buffer-matched-strings ()
    (interactive)
    (mapcar 'leuven--buffer-matched-string-nth '(0 1 2 3 4 5 6 7 8 9)))

  (defun leuven--buffer-matched-string-nth (n)
    "Return the Nth pattern-matched string from the current buffer."
    (if (and (match-beginning n) (match-end n))
        (if (> (match-end n) (match-beginning n))
            (buffer-substring (match-beginning n) (match-end n))
          "")
      nil))

;;** 15.9 (info "(emacs)Search Case")

  (leuven--section "15.8 (emacs)Search Case")

  ;; searches should ignore case by default (in all buffers that do not
  ;; override this)
  (setq-default case-fold-search t)

;;** 15.11 (info "(emacs)Other Repeating Search") Commands

  (leuven--section "15.10 (emacs)Other Repeating Search Commands")

  (defun leuven-isearch-occur ()
    "Invoke `occur' from within `isearch'."
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur
       (if isearch-regexp
           isearch-string
         (regexp-quote isearch-string)))))

  ;; activate `occur' easily while at the `I-search:' prompt
  (define-key isearch-mode-map (kbd "C-o") 'leuven-isearch-occur)

  (when (locate-library "color-moccur")

    ;; multi-buffer occur (grep) mode
    (mapc (function
           (lambda (x)
             (autoload x "color-moccur" nil t)))
          '(moccur
            dmoccur
            dired-do-moccur
            Buffer-menu-moccur
            grep-buffers
            search-buffers
            occur-by-moccur
            isearch-moccur
            moccur-grep
            moccur-grep-find)))

  ;; multi-buffer occur (grep) mode
  (with-eval-after-load "color-moccur"

    ;; input word splited by space
    (setq moccur-split-word t))

)                                       ; chapter 15 ends here

;;* 16 Commands for (info "(emacs)Fixit") Typos

(leuven--chapter leuven-chapter-16-fixit "16 Commands for Fixing Typos"

;;** 16.4 Checking and Correcting (info "(emacs)Spelling")

  (leuven--section "16.4 (emacs)Checking and Correcting Spelling")

  ;; spelling checker program
  (setq ispell-program-name             ; defined in ispell.el
        (or (executable-find "aspell")
            (executable-find "ispell")
            ;; nil                      ; [default: "ispell"]
            ))

  ;; check if `ispell-program-name' seems correct
  (defun ispell-check-program-name ()
    "Ensure that `ispell-program-name' is defined and non-nil."
    (interactive)
    (and (boundp 'ispell-program-name)
         ispell-program-name))

  (when (ispell-check-program-name)

    (defun ispell-region-or-buffer ()
      "Interactively check the current region or buffer for spelling errors."
      (interactive)
      (if mark-active
          (if (< (mark) (point))
              (ispell-region (mark) (point))
              (ispell-region (point) (mark)))
          (ispell-buffer)))

    ;; key bindings (or `C-c i' prefix key binding?)
    (global-set-key (kbd "C-$") 'ispell-region-or-buffer)
    (global-set-key (kbd "C-M-$") 'ispell-change-dictionary)

    ;; ;; default dictionary to use (if `ispell-local-dictionary' is nil, that
    ;; ;; is if there is no local dictionary to use in the buffer)
    ;; (setq ispell-dictionary "american") ; see `sentence-end-double-space'

    ;; enable on-the-fly spell checking
    (add-hook 'org-mode-hook
              (lambda ()
                (if (or (eq (aref (buffer-name) 0) ?\s) ; buffer starting with " *"
                        (and (boundp 'org-babel-exp-reference-buffer)
                             org-babel-exp-reference-buffer)) ; export buffer
                    (message "DON'T TURN ON Flyspell mode in `%s'" (buffer-name))
                  (message "Turn on Flyspell mode in `%s'" (buffer-name))
                  (flyspell-mode))))

    ;; prevent Flyspell from finding mistakes in the code, well in comments and
    ;; strings
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)

    (with-eval-after-load "ispell"

      ;; save the personal dictionary without confirmation
      (setq ispell-silently-savep t)

      ;; extensions and extra switches to pass to the `ispell' program
      (cond

       ((string-match "aspell" ispell-program-name)
        (setq ispell-extra-args '("--sug-mode=ultra" "-C"))
        (setq ispell-really-aspell t)
        (setq ispell-really-hunspell nil))

       ((string-match "ispell" ispell-program-name)
        (setq ispell-extra-args '())
        (setq ispell-really-aspell nil)
        (setq ispell-really-hunspell nil)))

      (setq-default mode-line-format
                    (cons
                     '(:eval
                       (let ((dict (and (featurep 'ispell)
                                        (not buffer-read-only)
                                        (or ispell-local-dictionary
                                            ispell-dictionary
                                            "--" ; default dictionary
                                            ))))
                         (and dict
                              (propertize (concat " " (substring dict 0 2))
                                          'face 'mode-line-highlight))))
                     (default-value 'mode-line-format)))

      )

    ;; don't use `M-TAB' to auto-correct the current word (only use `C-.')
    (setq flyspell-use-meta-tab nil)
    ;; FIXME M-TAB is still bound to `flyspell-auto-correct-word' when this
    ;; chunk of code is placed within (with-eval-after-load "flyspell"...)

    (with-eval-after-load "flyspell"

     ;; don't consider that a word repeated twice is an error
     (setq flyspell-mark-duplications-flag nil)

     ;; fix the "enabling flyspell mode gave an error" bug
     (setq flyspell-issue-welcome-flag nil)

     ;; ;; don't print messages for every word (when checking the entire buffer)
     ;; ;; as it causes a (small) slowdown
     ;; (setq flyspell-issue-message-flag nil)

     ;; dash character (`-') is considered as a word delimiter
     (setq-default flyspell-consider-dash-as-word-delimiter-flag t)
     ;; '("francais" "deutsch8" "norsk")

     (defun leuven-flyspell-toggle-dictionary ()
       "Toggle the local dictionary between French and US English."
       (interactive)
       (let ((dict (or ispell-local-dictionary
                       ispell-dictionary)))
         (setq dict (if (string= dict "francais") "american" "francais"))
         (message "Switched to %S" dict)
         (sit-for 0.5)
         (ispell-change-dictionary dict)
         (force-mode-line-update)
         (when flyspell-mode
           ;; (flyspell-delete-all-overlays)
           ;; if above is executed, the advised `org-mode-flyspell-verify'
           ;; won't work anymore
           (flyspell-buffer))))

     ;; key bindings
     (global-set-key (kbd "C-$") 'flyspell-buffer)
     (global-set-key (kbd "C-M-$") 'leuven-flyspell-toggle-dictionary)

     ;; spell-check your XHTML (by adding `nxml-text-face' to the list of
     ;; faces corresponding to text in programming-mode buffers)
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

  ;; client for rfc2229 dictionary servers
  (try-require "dictionary-autoloads")
  (with-eval-after-load "dictionary-autoloads"

    (global-set-key (kbd "C-c d s") 'dictionary-search)
    (global-set-key (kbd "C-c d l") 'dictionary-lookup-definition)
    (global-set-key (kbd "C-c d m") 'dictionary-match-words))

  (with-eval-after-load "dictionary"

    ;; enable/disable the tooltip support for all buffers
    (if consolep
        (global-dictionary-tooltip-mode 0)
      (global-dictionary-tooltip-mode 1)))

  ;; XXX excellent!
  (defun leuven-answers-define ()
    "Look up the word under cursor in a browser."
    (interactive)
    (browse-url
     (concat "http://www.answers.com/main/ntquery?s=" (find-tag-default))))

  (defun leuven-lookup-word-definition-in-w3m ()
    "Look up the word's definition in a emacs-w3m.
  If a region is active (a phrase), lookup that phrase."
    (interactive)
    (let (word
          url)
      (setq word
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning)
                                                (region-end))
              (find-tag-default)))
      (setq word (replace-regexp-in-string " " "%20" word))
      (setq url (concat "http://www.answers.com/main/ntquery?s=" word))
      (w3m-browse-url url)))

)                                       ; chapter 16 ends here

;;* 17 (info "(emacs)Keyboard Macros")

(leuven--chapter leuven-chapter-17-keyboard-macros "17 Keyboard Macros"

;;** 17.1 (info "(emacs)Basic Keyboard Macro") Use

  (leuven--section "17.1 (emacs)Basic Keyboard Macro Use")

  (defun leuven-kmacro-turn-on-recording ()
    "Start recording a keyboard macro and toggle functionality of key binding."
    (interactive)
    (global-set-key (kbd "<S-f8>") 'leuven-kmacro-turn-off-recording)
    (kmacro-start-macro nil))

  (defun leuven-kmacro-turn-off-recording ()
    "Stop recording a keyboard macro and toggle functionality of key binding."
    (interactive)
    (global-set-key (kbd "<S-f8>") 'leuven-kmacro-turn-on-recording)
    (kmacro-end-macro nil))

  ;; start/stop recording a keyboard macro
  (global-set-key (kbd "<S-f8>") 'leuven-kmacro-turn-on-recording)

  ;; execute the most recent keyboard macro
  (global-set-key (kbd "<f8>") 'kmacro-call-macro)

;;** 17.5 Name and (info "(emacs)Save Keyboard Macro")s

  (leuven--section "17.5 (emacs)Name and Save Keyboard Macros")

  ;; assign a name to the last keyboard macro defined
  (global-set-key (kbd "<C-f8>") 'kmacro-name-last-macro)

)                                       ; chapter 17 ends here

;;* 18 (info "(emacs)Files") Handling

(leuven--chapter leuven-chapter-18-files "18 Files Handling"

;;** 18.1 (info "(emacs)File Names")

  (leuven--section "18.1 (emacs)File Names")

  ;; ;; name of default directory
  ;; (setq default-directory (concat (getenv "HOME") "/"))

;;** 18.2 (info "(emacs)Visiting") Files

  (leuven--section "18.2 (emacs)Visiting Files")

  (defadvice find-file (around leuven-find-file activate)
    "Open the file named FILENAME and report time spent."
    (let ((filename (ad-get-arg 0))
          (find-file-time-start (float-time)))
      (message "(Info) Finding file %s..." filename)
      ad-do-it
      (message "(Info) Found file %s in %.2f s" filename
               (- (float-time) find-file-time-start))))

  ;; visit a file
  (global-set-key (kbd "<f3>") 'find-file)

;;** 18.3 (info "(emacs)Saving") Files

  (leuven--section "18.3 (emacs)Saving Files")

  ;; make your changes permanent
  (global-set-key (kbd "<f2>") 'save-buffer)

  ;; make numbered backups
  (setq version-control t)

  ;; save backup files (i.e., `foo~' or `foo.~i~') in one central location
  ;; (instead of in the local directory)
  (GNUEmacs
    ;; filenames matching a regexp are backed up in the corresponding
    ;; directory
    (setq backup-directory-alist
          ;; Emacs will `make-directory' it, if necessary
          '((".*" . "~/.emacs.d/backups/")))) ; regexp => directory mappings

  ;; ;; number of oldest versions to keep when a new numbered backup is made
  ;; (setq kept-old-versions 0)            ; [default: 2]

  ;; number of newest versions to keep when a new numbered backup is made
  (setq kept-new-versions 5)            ; [default: 2]

  ;; don't ask me about deleting excess backup versions
  (setq delete-old-versions t)

  ;; always use copying to create backup files (don't clobber symlinks)
  (setq backup-by-copying t)

  ;; ensure a file ends in a newline when it is saved
  (setq require-final-newline t)
  ;; TODO Do this only for text and Fundamental modes, because I could
  ;; edit binary files (see `mode-require-final-newline')

  ;; update time stamps every time you save a buffer
  (add-hook 'before-save-hook 'time-stamp)

  ;; maintain last change time stamps (`Time-stamp: <>' occurring within
  ;; the first 8 lines) in files edited by Emacs
  (with-eval-after-load "time-stamp"

   ;; format of the string inserted by `M-x time-stamp':
   ;; `YYYY-MM-DD Day HH:MM' (see `system-time-locale' for non-numeric
   ;; formatted items of time)
   (setq-default time-stamp-format "%:y-%02m-%02d %3a %02H:%02M"))

  (GNUEmacs
    ;; update the copyright notice to indicate the current year
    (add-hook 'before-save-hook 'copyright-update))

;;** 18.4 (info "(emacs)Reverting") a Buffer

  (leuven--section "18.4 (emacs)Reverting a Buffer")

  ;; replace current buffer text with the text of the visited file on disk
  (defun leuven-revert-buffer-without-query ()
    "Unconditionally revert current buffer."
    (interactive)
    (revert-buffer t t)                 ; ignore-auto(-save), noconfirm
    (message "Buffer is up to date with file on disk"))

  ;; key binding
  (global-set-key (kbd "<C-f12>") 'leuven-revert-buffer-without-query)

  ;; ;; enable Global Auto-Revert mode
  ;; (global-auto-revert-mode 1)           ; can generate an awful lot of network
  ;;                                       ; traffic

;;** 18.6 (info "(emacs)Auto Save"): Protection Against Disasters

  (leuven--section "18.6 (emacs)Auto Save: Protection Against Disasters")

  ;; auto-save every 100 input events
  (setq auto-save-interval 100)         ; [default: 300]

  ;; auto-save after 10 seconds idle time
  (setq auto-save-timeout 10)           ; [default: 30]

  (define-minor-mode sensitive-mode
    "For sensitive files like password lists.
  It disables backup creation and auto saving in the current buffer.

  With no argument, this command toggles the mode.  Non-null prefix argument
  turns on the mode.  Null prefix argument turns off the mode."
    nil                                 ; initial value
    " Sensitive"                        ; indicator for the mode line
    nil                                 ; minor mode bindings
    (if (symbol-value sensitive-mode)
        (progn
          ;; disable backups
          (set (make-local-variable 'backup-inhibited) t)
          ;; disable auto-save
          (if auto-save-default
              (auto-save-mode -1)))
      ;; resort to default value of backup-inhibited
      (kill-local-variable 'backup-inhibited)
      ;; resort to default auto save setting
      (if auto-save-default
          (auto-save-mode 1))))

  (defface recover-this-file
    '((t (:weight bold :background "#FF3F3F")))
    "Face for buffers visiting files with auto save data."
    :group 'files)

  (defvar leuven--recover-this-file nil
    "If non-nil, an overlay indicating that the visited file has auto save data.")

  (defun leuven--recover-this-file ()
    (let ((warn (not buffer-read-only)))
      (when (and warn
                 ;; no need to warn if buffer is auto-saved under the name of
                 ;; the visited file
                 (not (and buffer-file-name
                           auto-save-visited-file-name))
                 (file-newer-than-file-p (or buffer-auto-save-file-name
                                             (make-auto-save-file-name))
                                         buffer-file-name))
        (set (make-local-variable 'leuven--recover-this-file)
             (make-overlay (point-min) (point-max)))
        (overlay-put leuven--recover-this-file
                     'face 'recover-this-file))))

  (add-hook 'find-file-hook 'leuven--recover-this-file)

;;** 18.9 (info "(emacs)Comparing Files")

  (leuven--section "18.9 (emacs)Comparing Files")

  ;; default to unified diffs
  (setq diff-switches "-u")

  ;; compare text in current window with text in next window
  (global-set-key (kbd "C-=") 'compare-windows)

;;** 18.10 (info "(emacs)Diff Mode")

  (leuven--section "18.10 (emacs)Diff Mode")

  ;; mode for viewing/editing context diffs
  (with-eval-after-load "diff-mode"

    ;; highlight the changes with better granularity
    (defun leuven-diff-make-fine-diffs ()
      "Enable Diff Auto-Refine mode."
      (interactive)
      (let (diff-auto-refine-mode)      ; avoid refining the hunks redundantly ...
        (condition-case nil
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (diff-hunk-next)
                (diff-refine-hunk)))    ; ... when this does it.
          (error nil))
        (run-at-time 0.0 nil
                     (lambda ()
                       (if (eq major-mode 'diff-mode)
                           ;; put back the cursor only if still in a Diff buffer
                           ;; after the delay
                           (goto-char (point-min)))))))

    (defun leuven--diff-make-fine-diffs-if-necessary ()
      "Auto-refine only the regions of 14,000 bytes or less."
      ;; check for auto-refine limit
      (unless (> (buffer-size) 14000)
        (leuven-diff-make-fine-diffs)))

    ;; (when (fboundp 'advice-add)
    ;;   (advice-add 'vc-diff :after
    ;;    (lambda (&rest _)
    ;;      (leuven--diff-make-fine-diffs-if-necessary))))

    ;; XXX I tried this simpler form, but does not work.
    (defadvice vc-diff (after leuven-vc-diff activate)
      "Push the auto-refine function after `vc-diff'."
      (leuven--diff-make-fine-diffs-if-necessary))

    )

  ;; ;; Ediff, a comprehensive visual interface to diff & patch
  ;; ;; setup for Ediff's menus and autoloads
  ;; (try-require 'ediff-hook)
  ;; already loaded (by Emacs?)

  (with-eval-after-load "ediff"

    ;; ignore space
    (setq ediff-diff-options (concat ediff-diff-options " -w"))
                                        ; add new options after the default ones

    ;; skip over difference regions that differ only in white space and line
    ;; breaks
    ;; (setq-default ediff-ignore-similar-regions  t)
    ;; XXX Make another key binding (than `E') with that value in a let-bind

    ;; sometimes grab the mouse and put it in the control frame
    (setq ediff-grab-mouse 'maybe)

    ;; do everything in one frame
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)

    ;; split the window (horizontally or vertically) depending on the frame
    ;; width
    (setq ediff-split-window-function
          (lambda (&optional arg)
            (if (> (frame-width) 160)
                (split-window-horizontally arg)
              (split-window-vertically
               arg)))))

;;** 18.11 (info "(emacs)Misc File Ops")

  (leuven--section "18.11 (emacs)Misc File Ops")

  ;; use the system's Trash (when it is available)
  (setq delete-by-moving-to-trash t)

  ;; the EasyPG Assistant, transparent file encryption
  (with-eval-after-load "epa-file"

    ;; stop EasyPG from asking for the recipient used for encrypting files
    (setq epa-file-encrypt-to "johndoe@example.com")
    ;; if no one is selected (""), symmetric encryption will always be
    ;; performed

    ;; cache passphrase for symmetric encryption (VERY important)
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)
    ;; Not to sound paranoid.  But if you want caching, it's recommended to
    ;; use *public-key encryption* instead of symmetric encryption.
    ;; `gpg-agent' is the preferred way to do this.

    ;; prompt for the password in the Emacs minibuffer (instead of using a
    ;; graphical password prompt for GPG)
    (setenv "GPG_AGENT_INFO" nil))

;;** 18.14 (info "(emacs)Remote Files")

  (leuven--section "18.14 (emacs)Remote Files")

;;*** Ange-FTP

  (leuven--section "Ange-FTP")

  ;; transparent FTP support
  (with-eval-after-load "ange-ftp"

    ;; try to use passive mode in ftp, if the client program supports it
    (setq ange-ftp-try-passive-mode t)) ; needed for Ubuntu

;;*** TRAMP - Transparent Remote Access, Multiple Protocols

  (leuven--section "TRAMP")

  (with-eval-after-load "tramp"         ; the autoloads are predefined

;;* 4 (info "(tramp)Configuration") of TRAMP for use

;;** 4.6 Selecting a (info "(tramp)Default Method")

    ;; default transfer method
    (setq tramp-default-method          ; [default: "scp"]
          (cond (win32p
                 ;; (issues with Cygwin `ssh' which does not cooperate
                 ;; with Emacs processes -> use `plink' from PuTTY, it
                 ;; definitely does work under Windows)
                 ;;
                 ;; `C-x C-f /plink:user@host:/some/directory/file'
                 "plink")
                (t
                 "ssh")))

    ;; You might try out the `rsync' method, which saves the remote
    ;; files quite a bit faster than SSH.  It's based on SSH, so it
    ;; works the same, just saves faster.

    ;; (nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods)))
    ;;        '(("bash" "-i")))
    ;; (setcdr (assq 'tramp-remote-sh (assoc "ssh" tramp-methods))
    ;;         '("bash -i"))

;;** 4.9 Connecting to a remote host using (info "(tramp)Multi-hops")

    ;; new proxy system (introduced with Tramp 2.1, instead of the old
    ;; "multi-hop" filename syntax) to edit files on a remote server by
    ;; going via another server
    (when (boundp 'tramp-default-proxies-alist)
      (add-to-list 'tramp-default-proxies-alist
                   ;; "final host" "user" "proxy in the middle"
                   '("10.10.13.123" "\\`root\\'" "/ssh:%h:")))
    ;; Opening `/sudo:10.10.13.123:' would connect first `10.10.13.123'
    ;; via `ssh' under your account name, and perform `sudo -u root' on
    ;; that host afterwards.  It is important to know that the given
    ;; method is applied on the host which has been reached so far.  The
    ;; trick is to think from the end.

    ;; /ssh:user1@host|sudo:user2@host:

;;** 4.12 (info "(tramp)Password handling") for several connections

    ;; how many seconds passwords are cached
    (setq password-cache-expiry 60)     ; [default: 16]

;;** 4.15 (info "(tramp)Remote shell setup") hints

    ;; string used for end of line in rsh connections
    (setq tramp-rsh-end-of-line         ; [default: "\n"]
          (cond (win32p "\n")
                (t "\r")))

;;** 4.16 (info "(tramp)Auto-save and Backup") configuration

    ;; faster auto saves
    (setq tramp-auto-save-directory temporary-file-directory)

;;* 9 How to Customize (info "(tramp)Traces and Profiles")

    ;; debugging Tramp
    (setq tramp-verbose 6)              ; [maximum: 10]

    ;; "turn off" the effect of `backup-directory-alist' for TRAMP
    ;; files
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))

    ;; make Emacs beep after reading from or writing to the remote host
    (defadvice tramp-handle-write-region ; XXX
      (after leuven-tramp-write-beep-advice activate)
      "Make Tramp beep after writing a file."
      (interactive)
      (beep))

    (defadvice tramp-handle-do-copy-or-rename-file ; XXX
      (after leuven-tramp-copy-beep-advice activate)
      "Make Tramp beep after copying a file."
      (interactive)
      (beep))

    (defadvice tramp-handle-insert-file-contents
      (after leuven-tramp-insert-beep-advice activate)
      "Make Tramp beep after inserting contents of a file."
      (interactive)
      (beep))

    (defun leuven-find-file-sudo-header-warning ()
      "*Display a warning in header line of the current buffer."
      (let* ((warning "WARNING: EDITING FILE WITH ROOT PRIVILEGES!")
             (space (+ 6 (- (frame-width) (length warning))))
             (bracket (make-string (/ space 2) ?-))
             (warning (concat bracket warning bracket)))
        (setq header-line-format
              (propertize warning 'face 'header-line))))

    (defun leuven-find-file-sudo (filename)
      "Open FILENAME with root privileges."
      (interactive "F")
      (set-buffer (find-file (concat "/sudo::" filename)))
      (leuven-find-file-sudo-header-warning))

    ;; ;; XXX already an existing defadvice around find-file!!
    ;; (defadvice find-file (around leuven-find-file activate)
    ;;   "Open FILENAME using tramp's sudo method if it's read-only."
    ;;   (if (and (file-exists-p (ad-get-arg 0))
    ;;            (not (file-writable-p (ad-get-arg 0)))
    ;;            (not (file-remote-p (ad-get-arg 0)))
    ;;            (y-or-n-p (concat "File "
    ;;                              (ad-get-arg 0)
    ;;                              " is read-only.  Open it as root? ")))
    ;;       (leuven-find-file-sudo (ad-get-arg 0))
    ;;     ad-do-it))

    )

;;** 18.17 (info "(emacs)File Conveniences")

  (leuven--section "18.17 (emacs)File Conveniences")

  ;; filenames excluded from the recent list
  (setq recentf-exclude                 ;! has to be set before your require
                                        ;! `recentf'
        '(
          "~$"                          ; Emacs (and others) backup
          "\\.aux$" "\\.log$" "\\.toc$" ; LaTeX
          "/tmp/"
          ))

  ;; setup a menu of recently opened files
  (GNUEmacs
    (idle-require 'recentf))

  (with-eval-after-load "recentf"

    ;; maximum number of items that will be saved
    (setq recentf-max-saved-items 100)  ; just 20 is too recent

    ;; file to save the recent list into
    (setq recentf-save-file "~/.emacs.d/.recentf")

    ;; (when using Tramp) turn off the cleanup feature of `recentf'
    (setq recentf-auto-cleanup 'never)  ; disable before we start recentf!

    ;; save file names relative to my current home directory
    (setq recentf-filename-handlers '(abbreviate-file-name))

    ;; enable `recentf' mode
    (recentf-mode 1))

  (leuven--section "FFAP")

  ;; visit a file
  (global-set-key (kbd "<f3>") 'find-file-at-point)

  ;; find file (or URL) at point
  (with-eval-after-load "ffap"

    ;; function called to fetch an URL
    (setq ffap-url-fetcher 'browse-url))
    ;; could be `browse-url-emacs' or `w3m-browse-url'

  (GNUEmacs

    (leuven--section "Helm")

    ;; open Helm (QuickSilver-like candidate-selection framework)
    (try-require 'helm-config)
    (with-eval-after-load "helm-config"

      ;; various functions for Helm (Shell history, etc.)
      (require 'helm-misc)

      (when (and (or win32p cygwinp)
                 (executable-find "es"))
                                        ; we could check for it in
                                        ; (concat (getenv "USERPROFILE") "/Downloads")

        ;; sort locate results by full path
        (setq helm-locate-command "es -s %s %s"))

      (global-set-key (kbd "<f3>") 'helm-for-files)
                                        ; better than `helm-find-files'

      (global-set-key (kbd "M-x") 'helm-M-x)

      ;; buffers only
      (global-set-key (kbd "C-x b") 'helm-buffers-list)

      (defun leuven-helm-org-prog-menu ()
        "Jump to a place in the buffer using an Index menu.
      For Org mode buffers, show Org headlines.
      For programming mode buffers, show functions, variables, etc."
        (interactive)
        (if (derived-mode-p 'org-mode)
            (helm-org-headlines)
          (helm-imenu)))

      (global-set-key (kbd "<f4>") 'leuven-helm-org-prog-menu) ; awesome
                                        ; and `C-c =' (like in RefTeX)?

      (defun leuven-helm-grep-org-files ()
        "Launch grep on Org files in `~/org'."
        (interactive)
        (let ((files (helm-walk-directory "~/org"
                                          :path 'full
                                          :directories nil
                                          :match ".*\\.\\(org\\|txt\\)$"
                                          :skip-subdirs t)))
          (helm-do-grep-1 files)))

      ;; better version of `occur'
      (global-set-key (kbd "C-o") 'helm-occur)

      (global-set-key (kbd "C-x r l") 'helm-bookmarks)

      ;; install from https://github.com/thierryvolpiatto/emacs-bmk-ext
      (global-set-key (kbd "C-x r b") 'helm-bookmark-ext)

      ;; use the *current window* (no popup) to show the candidates
      (setq helm-full-frame nil)

      ;; ;; open helm buffer in another window
      ;; (setq helm-split-window-default-side 'other)

      ;; always display `helm-buffer' in current window
      (setq helm-split-window-default-side 'same)

      ;; move to end or beginning of source when reaching top or bottom of
      ;; source
      (setq helm-move-to-line-cycle-in-source t)

      (defface leuven-separator
        '((t (:weight bold :foreground "slate gray")))
        "Face used to display state NEW.")

      ;; candidates separator of `multiline' source (such as
      ;; `helm-show-kill-ring')
      (setq helm-candidate-separator
            (propertize "--separator-------------------------------"
                        'face 'leuven-separator))

      ;; suppress displaying sources which are out of screen at first
      (setq helm-quick-update t)

      ;; time that the user has to be idle for, before candidates from
      ;; DELAYED sources are collected
      (setq helm-idle-delay 0.03)       ; useful for sources involving heavy
                                        ; operations, so that candidates from
                                        ; the source are not retrieved
                                        ; unnecessarily if the user keeps typing

      ;; time that the user has to be idle for, before ALL candidates
      ;; are collected (>= `helm-idle-delay')
      (setq helm-input-idle-delay 0.03) ; also effective for NON-DELAYED sources

      ;; ;; don't save history information to file
      ;; (remove-hook 'kill-emacs-hook 'helm-adaptive-save-history)

      ;; don't show only basename of candidates in `helm-find-files'
      (setq helm-ff-transformer-show-only-basename nil)

      ;; ;; don't truncate buffer names
      ;; (setq helm-buffer-max-length nil)

      ;; save command even when it fails
      (setq helm-M-x-always-save-history t)

      (defun helm-toggle-debug ()
        "Toggle Helm debug on/off."
        (interactive)
        (setq helm-debug (not helm-debug))
        (message "Helm debug %s" (if helm-debug
                                     "enabled"
                                   "disabled")))))

  (with-eval-after-load "helm"
    ;; ;; enable generic Helm completion (for all functions in Emacs that use
    ;; ;; `completing-read' or `read-file-name' and friends)
    ;; (helm-mode 1)

    ;; ;; enable adaptative sorting in all sources
    ;; (helm-adaptative-mode 1)
    )

  (leuven--section "Image mode")

  ;; show image files as images (not as semi-random bits)
  (GNUEmacs
    (add-hook 'find-file-hook 'auto-image-file-mode))

)                                       ; chapter 18 ends here

;;* 19 Using Multiple (info "(emacs)Buffers")

(leuven--chapter leuven-chapter-19-buffers "19 Using Multiple Buffers"

;;** 19.1 Creating and (info "(emacs)Select Buffer")

  (leuven--section "19.1 (emacs)Select Buffer")

  (defun switch-or-start (function buffer)
    "If the BUFFER is current, bury it.  If there is a buffer with that name,
  switch to it; otherwise, invoke the FUNCTION."
    (if (equal (buffer-name (current-buffer)) buffer)
        (bury-buffer)
      (if (get-buffer buffer)
          (switch-to-buffer buffer)
        (funcall function))))

  (defun switch-or-find-file (file)
    "If the FILE is current, bury it.  If there is a buffer with that name,
  switch to it; otherwise, open it."
    (when (file-exists-p file)
      (if (and (buffer-file-name)
               (string= (expand-file-name file)
                        (expand-file-name (buffer-file-name))))
          (bury-buffer)
        (find-file file))))

;;** 19.2 (info "(emacs)List Buffers")

  (leuven--section "19.2 (emacs)List Buffers")

  ;; rebind `C-x C-b'
  (global-set-key (kbd "C-x C-b") 'electric-buffer-list)
                                        ; `buffer-menu' moves point in the
                                        ; window which lists your buffers
                                        ; `electric-buffer-list' pops up
                                        ; a buffer describing the set of buffers

  ;; operate on buffers like Dired
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (with-eval-after-load "ibuffer"

    ;; completely replaces `list-buffer'
    (defalias 'ibuffer-list-buffers 'list-buffer)

    ;; don't show the names of filter groups which are empty
    (setq ibuffer-show-empty-filter-groups nil)

    ;; filtering groups
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("Chat"
              (mode . circe-mode))
             ("Org"
              (or
               (mode . diary-mode)
               (mode . org-mode)
               (mode . org-agenda-mode)))
             ("LaTeX"
              (or
               (mode . latex-mode)
               (mode . LaTeX-mode)
               (mode . bibtex-mode)
               (mode . reftex-mode)))
             ("Gnus & News"
              (or
               (mode . message-mode)
               (mode . bbdb-mode)
               (mode . mail-mode)
               (mode . gnus-group-mode)
               (mode . gnus-summary-mode)
               (mode . gnus-article-mode)
               (name . "^\\(\\.bbdb\\|dot-bbdb\\)$")
               (name . "^\\.newsrc-dribble$")
               (mode . newsticker-mode)))
             ("Files"
              (filename . ".*"))
             ("Dired"
              (mode . dired-mode))
             ("Shell"
              (mode . shell-mode))
             ("Version Control"
              (or
               (mode . svn-status-mode)
               (mode . svn-log-edit-mode)
               (name . "^\\*svn-")
               (name . "^\\*vc\\*$")
               (name . "^\\*Annotate")
               (name . "^\\*git-")
               (name . "^\\*vc-")))
             ("Emacs"
              (or
               (name . "^\\*scratch\\*$")
               (name . "^\\*Messages\\*$")
               (name . "^TAGS\\(<[0-9]+>\\)?$")
               (name . "^\\*Occur\\*$")
               (name . "^\\*grep\\*$")
               (name . "^\\*Compile-Log\\*$")
               (name . "^\\*Backtrace\\*$")
               (name . "^\\*Process List\\*$")
               (name . "^\\*gud\\*$")
               (name . "^\\*Kill Ring\\*$")
               (name . "^\\*Completions\\*$")
               (name . "^\\*tramp")
               (name . "^\\*compilation\\*$")))
             ("Emacs Source"
              (mode . emacs-lisp-mode))
             ("Documentation"
              (or
               (mode . Info-mode)
               (mode . apropos-mode)
               (mode . woman-mode)
               (mode . help-mode)
               (mode . Man-mode))))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

    ;; order the groups so the order is: [Default], [agenda], [emacs]
    (defadvice ibuffer-generate-filter-groups
      (after leuven-reverse-ibuffer-groups activate)
      (setq ad-return-value (nreverse ad-return-value))))

;;** 19.4 (info "(emacs)Kill Buffer")

  (leuven--section "19.4 (emacs)Kill Buffer")

  ;; kill buffer without confirmation (if not modified)
  (defun leuven-kill-this-buffer-without-query ()
    "Kill the current buffer without confirmation (if not modified)."
    (interactive)
    (kill-buffer nil))

  ;; key binding
  (global-set-key (kbd "<S-f12>") 'leuven-kill-this-buffer-without-query)

;;** 19.5 (info "(emacs)Several Buffers")

  (leuven--section "19.5 (emacs)Several Buffers")

  ;; put the current buffer at the end of the list of all buffers
  (global-set-key (kbd "<f12>") 'bury-buffer)
                                        ; conflict when GDB'ing Emacs under
                                        ; Win32

;;** 19.7 (info "(emacs)Buffer Convenience") and Customization of Buffer Handling

  (leuven--section "19.7 (emacs)Buffer Convenience and Customization of Buffer Handling")

  ;; unique buffer names dependent on file name
  (try-require 'uniquify)

  (with-eval-after-load "uniquify"

    ;; style used for uniquifying buffer names with parts of directory
    ;; name
    (setq uniquify-buffer-name-style 'forward)

    ;; distinguish directories by adding extra separator
    (setq uniquify-trailing-separator-p t))

)                                       ; chapter 19 ends here

;;* 20 Multiple (info "(emacs)Windows")

(leuven--chapter leuven-chapter-20-windows "20 Multiple Windows"

;;** 20.1 (info "(emacs)Basic Window")

  (leuven--section "20.1 (emacs)Basic Window")

  ;; turn off this horrible tab thingy in XEmacs
  (XEmacs
    (when (boundp 'default-gutter-visible-p)
      (set-specifier default-gutter-visible-p nil)))

;;** 20.3 (info "(emacs)Other Window")

  (leuven--section "20.3 (emacs)Other Window")

  ;; cycle through all windows on current frame
  (global-set-key (kbd "<f6>") 'other-window)

  ;; reverse operation of `C-x o' (or `f6')
  (global-set-key
    (kbd "<S-f6>")
    (lambda ()
      (interactive)
      (other-window -1)))

;;** 20.5 (info "(emacs)Change Window")

  (leuven--section "20.5 (emacs)Change Window")

  (defun leuven-delete-other-windows ()
    "Cycle between 1 window, 2 vertically and 2 horizontally split windows."
    (interactive)
    (let ((splitter
           (if (= (car (window-edges (selected-window)))
                  (car (window-edges (next-window))))
               'split-window-horizontally
             'split-window-vertically)))
      (cond ((= (count-windows) 1)
             (delete-other-windows)
             (split-window-vertically)
             (set-window-buffer (next-window) (nth 2 (buffer-list))))
            ((and (> (count-windows) 1)
                  (equal splitter 'split-window-horizontally))
             (delete-other-windows)
             (funcall splitter)
             (set-window-buffer (next-window) (nth 2 (buffer-list))))
            ((and (> (count-windows) 1)
                  (equal splitter 'split-window-vertically))
             (delete-other-windows)))))

  ;; delete all windows in the selected frame except the selected window
  (global-set-key (kbd "<f5>") 'leuven-delete-other-windows)

  ;; swap 2 windows
  (defun leuven-swap-windows ()
    "If you have 2 windows, swap them."
    (interactive)
    (cond ((not (= (count-windows) 2))
           (message "You need exactly 2 windows to swap them."))
          (t
           (let* ((wind-1 (first (window-list)))
                  (wind-2 (second (window-list)))
                  (buf-1 (window-buffer wind-1))
                  (buf-2 (window-buffer wind-2))
                  (start-1 (window-start wind-1))
                  (start-2 (window-start wind-2)))
             (set-window-buffer wind-1 buf-2)
             (set-window-buffer wind-2 buf-1)
             (set-window-start wind-1 start-2)
             (set-window-start wind-2 start-1)))))

  (global-set-key (kbd "C-c ~") 'leuven-swap-windows)

  (defun leuven-toggle-window-split ()
    "Toggle between vertical and horizontal split.
  Vertical split shows more of each line, horizontal split shows more lines.
  This code only works for frames with exactly two windows."
    (interactive)
    (cond ((not (= (count-windows) 2))
           (message "You need exactly 2 windows to toggle the window split."))
          (t
           (let* ((this-win-buffer (window-buffer))
                  (next-win-buffer (window-buffer (next-window)))
                  (this-win-edges (window-edges (selected-window)))
                  (next-win-edges (window-edges (next-window)))
                  (this-win-2nd (not (and (<= (car this-win-edges)
                                              (car next-win-edges))
                                          (<= (cadr this-win-edges)
                                              (cadr next-win-edges)))))
                  (splitter
                   (if (= (car this-win-edges)
                          (car (window-edges (next-window))))
                       'split-window-horizontally
                     'split-window-vertically)))
             (delete-other-windows)
             (let ((first-win (selected-window)))
               (funcall splitter)
               (if this-win-2nd (other-window 1))
               (set-window-buffer (selected-window) this-win-buffer)
               (set-window-buffer (next-window) next-win-buffer)
               (select-window first-win)
               (if this-win-2nd (other-window 1)))))))

  (global-set-key (kbd "C-c |") 'leuven-toggle-window-split)

;;** 20.6 (info "(emacs)Displaying Buffers")

  (leuven--section "20.6 (emacs)Pop Up Window")

  ;; don't allow splitting windows vertically
  (setq split-height-threshold nil)

  ;; minimum width for splitting windows horizontally
  (setq split-width-threshold 160)

)                                       ; chapter 20 ends here

;;* 21 (info "(emacs)Frames") and Graphical Displays

(leuven--chapter leuven-chapter-21-frames "21 Frames and Graphical Displays"

;;** 21.1 (info "(emacs)Mouse Commands")

  (leuven--section "21.1 (emacs)Mouse Commands")

  ;; scroll one line at a time
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 1)))

;;** 21.6 (info "(emacs)Creating Frames")

  (leuven--section "21.6 (emacs)Creating Frames")

  (when (display-graphic-p)

    ;; put Emacs exactly where you want it, every time it starts up
    (setq initial-frame-alist
          '((top . 0)
            (left . 0)))

    (GNUEmacs
      ;; auto-detect the screen dimensions and compute the height of Emacs
      (add-to-list 'default-frame-alist
                   (cons 'height
                         (/ (- (x-display-pixel-height) 106)
                            (frame-char-height)))))

    (XEmacs
      (set-frame-position (buffer-dedicated-frame) 0 0)
      (set-frame-width (buffer-dedicated-frame) 80)
      (set-frame-height (buffer-dedicated-frame) 42)))

  ;; title bar display of visible frames
  (setq frame-title-format
        (format "%s Emacs %s%s of %s - PID: %d"
                (capitalize (symbol-name system-type))
                emacs-version
                (if (and (boundp 'emacs-repository-version)
                         emacs-repository-version)
                    (concat " (r"
                            (replace-regexp-in-string " .*" ""
                                                      emacs-repository-version)
                            ")")
                  "")
                (format-time-string "%Y-%m-%d" emacs-build-time)
                (emacs-pid)))

  (defun detach-window ()
    "Close current window and re-open it in new frame."
    (interactive)
    (let ((current-buffer (window-buffer)))
      (delete-window)
      (select-frame (make-frame))
      (set-window-buffer (selected-window) current-buffer)))

;;** 21.7 (info "(emacs)Frame Commands")

  (leuven--section "21.7 (emacs)Frame Commands")

  (when x-window-p
    (defun toggle-fullscreen ()
      "Toggle between full screen and partial screen display on X11."
      (interactive)
      ;; WM must support EWMH
      ;; http://standards.freedesktop.org/wm-spec/wm-spec-latest.html
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

    (global-set-key (kbd "C-c z") 'toggle-fullscreen))

  (GNUEmacs
    (when win32p
      (defun w32-maximize-frame ()
        "Maximize the current frame."
        (interactive)
        (w32-send-sys-command 61488)
        (global-set-key (kbd "C-c z") 'w32-restore-frame))

      (global-set-key (kbd "C-c z") 'w32-maximize-frame)

      (defun w32-restore-frame ()
        "Restore a minimized frame."
        (interactive)
        (w32-send-sys-command 61728)
        (global-set-key (kbd "C-c z") 'w32-maximize-frame))))

;;** 21.9 (info "(emacs)Speedbar")

  (leuven--section "21.9 (emacs)Speedbar Frames")

  (unless (locate-library "helm-config") ; helm is better than speedbar!

    ;; jump to speedbar frame
    (global-set-key (kbd "<f4>") 'speedbar-get-focus))

  ;; everything browser (into individual source files), or Dired on
  ;; steroids
  (with-eval-after-load "speedbar"

    ;; number of spaces used for indentation
    (setq speedbar-indentation-width 2)

    ;; add new extensions for speedbar tagging (allow to expand/collapse
    ;; sections, etc.) -- do this BEFORE firing up speedbar?
    (speedbar-add-supported-extension
     '(".bib" ".css" ".jpg" ".js" ".nw" ".org" ".php" ".png" ".R" ".tex" ".txt"
       ".w" "README"))

    ;; bind the arrow keys in the speedbar tree
    (define-key speedbar-mode-map (kbd "<right>") 'speedbar-expand-line)
    (define-key speedbar-mode-map (kbd "<left>") 'speedbar-contract-line)

    ;; parameters to use when creating the speedbar frame in Emacs
    (setq speedbar-frame-parameters '((width . 30)
                                      (height . 45)
                                      (foreground-color . "blue")
                                      (background-color . "white")))

    ;; speedbar in the current frame (vs in a new frame)
    (when (and (not (locate-library "helm-config"))
                                        ; helm is better than speedbar!
               (locate-library "sr-speedbar"))

      (autoload 'sr-speedbar-toggle "sr-speedbar" nil t)
      (global-set-key (kbd "<f4>") 'sr-speedbar-toggle)))

;;** 21.12 (info "(emacs)Scroll Bars")

  (leuven--section "21.12 (emacs)Scroll Bars")

  (if (and (display-graphic-p)
           (or (featurep 'sml-modeline)
               (featurep 'smart-mode-line)
               (featurep 'powerline)))

      ;; turn scroll bar off
      (scroll-bar-mode -1)

    ;; position of the vertical scroll bar
    (setq-default vertical-scroll-bar 'right))

;;** 21.15 (info "(emacs)Tool Bars")

  (leuven--section "21.15 (emacs)Tool Bars")

  ;; turn tool bar off
  (when (display-graphic-p)
    (GNUEmacs
      (tool-bar-mode -1))
    (XEmacs
      (set-specifier default-toolbar-visible-p nil)))

;;** 21.16 Using (info "(emacs)Dialog Boxes")

  (leuven--section "21.16 (emacs)Using Dialog Boxes")

  ;; don't use dialog boxes to ask questions
  (setq use-dialog-box nil)

  ;; don't use a file dialog to ask for files
  (setq use-file-dialog nil)

;;** 21.17 (info "(emacs)Tooltips")

  (leuven--section "21.17 (emacs)Tooltips")

  ;; disable Tooltip mode (use the echo area for help and GUD tooltips)
  (unless consolep (tooltip-mode -1))

)                                       ; chapter 21 ends here

;;* 22 (info "(emacs)International") Character Set Support

(leuven--chapter leuven-chapter-22-international "22 International Character Set Support"

;;** 22.3 (info "(emacs)Language Environments")

  (leuven--section "22.3 (emacs)Language Environments")

  ;; specify your character-set locale
  (setenv "LANG" "en_US.utf8")          ; for svn not to report warnings

  ;; system locale to use for formatting time values
  (setq system-time-locale "C")         ; make sure that the weekdays in the
                                        ; time stamps of your Org mode files
                                        ; and in the agenda appear in English

  ;; (setq system-time-locale (getenv "LANG"))
  ;;                                       ; for weekdays in your locale settings

;;** 22.4 (info "(emacs)Input Methods")

  (leuven--section "22.4 (emacs)Input Methods")

  ;; get 8-bit characters in terminal mode (Cygwin Emacs)
  (set-input-mode (car (current-input-mode))
                  (nth 1 (current-input-mode))
                  0)

;;** 22.7 (info "(emacs)Recognize Coding") Systems

  (leuven--section "22.7 (emacs)Recognize Coding Systems")

  ;; default coding system (for new files),
  ;; also moved to the front of the priority list for automatic detection
  (GNUEmacs
   (prefer-coding-system 'utf-8-unix))  ; Unix flavor for code blocks executed
                                        ; via Org-babel

;;** 22.8 (info "(emacs)Specify Coding") System of a File

  (leuven--section "22.8 (emacs)Specify Coding System of a File")

  (GNUEmacs
    ;; to copy and paste to and from Emacs through the clipboard (with
    ;; coding system conversion)
    (cond (win32p
           (set-selection-coding-system 'compound-text-with-extensions))
          (t
           (set-selection-coding-system 'utf-8))))

)                                       ; chapter 22 ends here

;;* 23 (info "(emacs)Modes")

(leuven--chapter leuven-chapter-23-major-and-minor-modes "23 Major and Minor Modes"

;;** 23.3 (info "(emacs)Choosing Modes")

  (leuven--section "23.3 (emacs)Choosing File Modes")

  ;; list of filename patterns
  (setq auto-mode-alist
        (append '(("\\.log\\'"       . text-mode)
                  ;; ("\\.[tT]e[xX]\\'" . latex-mode)
                  ;; ("\\.cls\\'"       . LaTeX-mode)
                  ("\\.cgi\\'"       . perl-mode)
                  ;; ("[mM]akefile"     . makefile-mode)
                  (".ssh/config\\'"  . ssh-config-mode)
                  ("sshd?_config\\'" . ssh-config-mode)
                  ) auto-mode-alist))

  ;; major mode for fontifiying ssh config files
  (autoload 'ssh-config-mode "ssh-config-mode"
    "Major mode for fontifiying ssh config files." t)

  ;; ledger
  (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))

  ;; major mode for editing comma-separated value files
  (when (locate-library "csv-mode")

    (autoload 'csv-mode "csv-mode"
      "Major mode for editing comma-separated value files." t)

    (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

    ;; field separators: a list of *single-character* strings
    (setq csv-separators '("," ";")))

  ;; list of interpreters specified in the first line (starts with `#!')
  (push '("expect" . tcl-mode) interpreter-mode-alist)

  ;; ;; load generic modes which support e.g. batch files
  ;; (try-require 'generic-x)

)                                       ; chapter 23 ends here

;;* 24 (info "(emacs)Indentation")

(leuven--chapter leuven-chapter-24-indentation "24 Indentation"

;;** 24.1 (info "(emacs)Indentation Commands") and Techniques

  (leuven--section "24.1 (emacs)Indentation Commands and Techniques")

  (defun indent-whole-buffer ()
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (indent-for-tab-command)))

;;** 24.3 TABs vs. (info "(emacs)Just Spaces")

  (leuven--section "24.3 TABs vs. (emacs)Just Spaces")

  ;; indentation can't insert TABs
  (setq-default indent-tabs-mode nil)

)                                       ; chapter 24 ends here

;;* 25 Commands for (info "(emacs)Text") Human Languages

(leuven--chapter leuven-chapter-25-text "25 Commands for Human Languages"

;;** 25.1 (info "(emacs)Words")

  (leuven--section "25.1 (emacs)Words")

  ;; GNU Emacs default for killing back to the beginning of a word
  (XEmacs
    (global-set-key (kbd "<C-backspace>") 'backward-kill-word))

;;** 25.2 (info "(emacs)Sentences")

  (leuven--section "25.2 (emacs)Sentences")

  ;; ;; a single space does end a sentence
  ;; (setq-default sentence-end-double-space nil) ; see `ispell-dictionary'

  (defun leuven-nbsp-command ()
    "Insert the no-break space character 00A0."
    (interactive)
    (insert-char ?\u00A0))

  (global-set-key (kbd "S-SPC") 'leuven-nbsp-command)

;;** 25.5 (info "(emacs)Filling") Text

  (leuven--section "25.5 (emacs)Filling Text")

  ;; line-wrapping beyond that column (when pressing `M-q')
  (setq-default fill-column 80)

  ;; (un-)fill paragraph
  (defun leuven-fill-paragraph (&optional arg)
    "`M-q' runs the command `fill-paragraph'.
  `C-u M-q' runs \"unfill-paragraph\": it takes a multi-line paragraph and
  converts it into a single line of text."
    (interactive "P")
    (let ((fill-column (if arg
                           (point-max)
                         fill-column)))
      (fill-paragraph nil)))

  (global-set-key (kbd "M-q") 'leuven-fill-paragraph)

  ;; prevent breaking lines just before a punctuation mark such as `?' or `:'
  (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)

  ;; activate Auto Fill for all text mode buffers
  (add-hook 'text-mode-hook 'auto-fill-mode)

  ;; graphically indicate the fill column
  (try-require 'fill-column-indicator-XXX)
  (with-eval-after-load "fill-column-indicator-XXX"

    ;; color used to draw the fill-column rule
    (setq fci-rule-color "#FFE0E0")

    ;; show the fill-column rule as dashes
    (setq fci-rule-use-dashes t)

    ;; ratio of dash length to line height
    (setq fci-dash-pattern 0.5)

    ;; enable fci-mode as a global minor mode
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (global-fci-mode 1))

  (defun leuven-replace-nbsp-by-spc ()
    "Replace all nbsp by normal spaces."
    (interactive "*")
    (save-excursion
      (save-restriction
        (save-match-data
          (progn
            (goto-char (point-min))
            (while (re-search-forward "[  ]" nil t)
              (replace-match " " nil nil)))))))

  (defun leuven-good-old-fill-paragraph ()
    (interactive)
    (let ((fill-paragraph-function nil)
          (adaptive-fill-function nil))
      (fill-paragraph)))

  (defun leuven-smart-punctuation-apostrophe ()
    "Replace second apostrophe by backquote in front of symbol."
    (interactive)
    (cond
     ((or (bolp) (not (looking-back "'")))
      ;; insert just one '
      (self-insert-command 1))
     ((save-excursion
        (backward-char)
        ;; Skip symbol backwards.
        (and (not (zerop (skip-syntax-backward "w_")))
             (not (looking-back "`"))
             (or (insert-and-inherit "`") t))))
     (t
      ;; insert `' around following symbol
      (delete-char -1)
      (unless (looking-back "`") (insert-and-inherit "`"))
      (save-excursion
        (skip-syntax-forward "w_")
        (unless (looking-at "'") (insert-and-inherit "'"))))))

  (defun leuven-smart-punctuation-quotation-mark ()
    "Replace two following double quotes by French quotes."
    (interactive)
    (let ((dict (or ispell-local-dictionary
                    ispell-dictionary)))
      (if (and (string= dict "francais")
               (eq (char-before) ?\")
               (or (not (equal mode-name "Org"))
                   (not (member (org-element-type (org-element-at-point))
                                '(src-block keyword table dynamic-block)))))
          (progn
            (backward-delete-char 1)
            (insert "«  »")
            (backward-char 2))
        (insert "\""))))

  (defun leuven-smart-punctuation ()
    "Replace second apostrophe or quotation mark."
    (interactive)
    (local-set-key [39] 'leuven-smart-punctuation-apostrophe)
    (local-set-key "\"" 'leuven-smart-punctuation-quotation-mark))

  (add-hook 'text-mode-hook 'leuven-smart-punctuation)
  (add-hook 'message-mode-hook 'leuven-smart-punctuation)

;;** 25.6 (info "(emacs)Case") Conversion Commands

  (leuven--section "25.6 (emacs)Case Conversion Commands")

  ;; enable the use of the commands `downcase-region' and `upcase-region'
  ;; without confirmation
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

;;** 25.8 (info "(emacs)Outline Mode")

  (leuven--section "25.8 (emacs)Outline Mode")

  ;; outline mode commands for Emacs
  (with-eval-after-load "outline"

    ;; bind the outline minor mode functions to an easy to remember prefix
    ;; key (more accessible than the horrible prefix `C-c @')
    (setq outline-minor-mode-prefix (kbd "C-c C-o")) ; like in nXML mode

    ;; ;; make other `outline-minor-mode' files (LaTeX, etc.) feel the Org
    ;; ;; mode outline navigation (written by Carsten Dominik)
    ;; (try-require 'outline-magic)
    ;; (with-eval-after-load "outline-magic"
    ;;   (add-hook 'outline-minor-mode-hook
    ;;             (lambda ()
    ;;               (define-key outline-minor-mode-map
    ;;                 (kbd "<S-tab>") 'outline-cycle)
    ;;               (define-key outline-minor-mode-map
    ;;                 (kbd "<M-left>") 'outline-promote)
    ;;               (define-key outline-minor-mode-map
    ;;                 (kbd "<M-right>") 'outline-demote)
    ;;               (define-key outline-minor-mode-map
    ;;                 (kbd "<M-up>") 'outline-move-subtree-up)
    ;;               (define-key outline-minor-mode-map
    ;;                 (kbd "<M-down>") 'outline-move-subtree-down))))

    ;; ;; extra support for outline minor mode
    ;; (try-require 'out-xtra)


    ;; Org-style folding for a `.emacs' (and much more)

    ;; FIXME This should be in an `eval-after-load' of Org, so that
    ;; `org-level-N' are defined when used

    (defun leuven--outline-regexp ()
      "Calculate the outline regexp for the current mode."
      (let ((comment-starter (replace-regexp-in-string
                              "[[:space:]]+" "" comment-start)))
        (when (string= comment-start ";")
          (setq comment-starter ";;"))
        ;; (concat "^" comment-starter "\\*+")))
        (concat "^" comment-starter "[*]+ ")))

    ;; fontify the whole line for headings (with a background color)
    (setq org-fontify-whole-heading-line t)

    (defun leuven--outline-minor-mode-hook ()
      (setq outline-regexp (leuven--outline-regexp))
      (let* ((org-fontify-whole-headline-regexp "") ; "\n?")
             (heading-1-regexp
              (concat (substring outline-regexp 0 -1)
                      "\\{1\\} \\(.*" org-fontify-whole-headline-regexp "\\)"))
             (heading-2-regexp
              (concat (substring outline-regexp 0 -1)
                      "\\{2\\} \\(.*" org-fontify-whole-headline-regexp "\\)"))
             (heading-3-regexp
              (concat (substring outline-regexp 0 -1)
                      "\\{3\\} \\(.*" org-fontify-whole-headline-regexp "\\)"))
             (heading-4-regexp
              (concat (substring outline-regexp 0 -1)
                      "\\{4,\\} \\(.*" org-fontify-whole-headline-regexp "\\)")))
        (font-lock-add-keywords nil
         `((,heading-1-regexp 1 'org-level-1 t)
           (,heading-2-regexp 1 'org-level-2 t)
           (,heading-3-regexp 1 'org-level-3 t)
           (,heading-4-regexp 1 'org-level-4 t)))))

    (add-hook 'outline-minor-mode-hook
              'leuven--outline-minor-mode-hook)

    ;; Add the following as the top line of your `.emacs':
    ;;
    ;; ; -*- mode: emacs-lisp; eval: (outline-minor-mode 1); -*-
    ;;
    ;; Now you can add `;;' and `;;*', etc. as headings in your `.emacs'
    ;; and cycle using `<S-tab>', `<M-left>' and `<M-right>' will collapse
    ;; or expand all headings respectively.  I am guessing you mean to make
    ;; segments such as `;; SHORTCUTS' and `;; VARIABLES', this will do
    ;; that, but not too much more.
    )

    (add-hook 'outline-minor-mode-hook
              (lambda ()
                (when (and outline-minor-mode (derived-mode-p 'emacs-lisp-mode))
                  (hide-sublevels 1000))))

  ;; (add-hook 'outline-minor-mode-hook
  ;;   (lambda ()
  ;;     (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
  ;;     (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle))) ; backtab?

  (global-set-key (kbd "<S-tab>") 'org-cycle) ; that works (but on level 1+)
  ;; TODO Look at org-cycle-global and local below, they work better, but
  ;; still on level 1+
  ;; TODO Replace it by a function which alternatively does `hide-body' and
  ;; `show-all'

  ;; from Bastien

  ;; ;; XXX 2010-06-21 Conflicts with outline-minor-mode bindings
  ;; ;; add a hook to use `orgstruct-mode' in Emacs Lisp buffers
  ;; (add-hook 'emacs-lisp-mode-hook 'orgstruct-mode)

  (defun org-cycle-global ()
    (interactive)
    (org-cycle t))

  (global-set-key (kbd "C-M-]") 'org-cycle-global)
                                        ; XXX ok on Emacs Lisp, not on LaTeX
                                        ; <S-tab>?

  ;; (defun org-cycle-local ()
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (move-beginning-of-line nil)
  ;;     (org-cycle)))

  (defun org-cycle-local ()
    (interactive)
    (ignore-errors
      (end-of-defun)
      (beginning-of-defun))
    (org-cycle))

  (global-set-key (kbd "M-]") 'org-cycle-local)
                                        ; XXX ok on Emacs Lisp, not on LaTeX

;; C-M-] and M-] fold the whole buffer or the current defun.

  ;; ;; unified user interface for Emacs folding modes, bound to Org
  ;; ;; key-strokes
  ;; (GNUEmacs
  ;;   (try-require 'fold-dwim-org))

  ;; 25.8.2
  (global-set-key (kbd "<M-f6>") 'visible-mode)

;;** (info "(emacs-goodies-el)boxquote")

  (leuven--section "(emacs-goodies-el)boxquote")

  (when (locate-library "boxquote")

    ;; quote text with a semi-box
    (autoload 'boxquote-region "boxquote"
      "Draw a box around the left hand side of a region bounding START and END." t)

    (global-set-key (kbd "C-c q") 'boxquote-region))

  (with-eval-after-load "boxquote"
    (setq boxquote-top-and-tail  "────")
    (setq boxquote-title-format  " %s")
    (setq boxquote-top-corner    "  ╭")
    (setq boxquote-side          "  │ ")
    (setq boxquote-bottom-corner "  ╰"))

;;** (info "phonetic")

  (leuven--section "phonetic")

  ;; phonetic spelling
  (when (locate-library "phonetic")
    (autoload 'phonetize-region "phonetic"
      "Translate the region according to the phonetic alphabet." t))

)                                       ; chapter 25 ends here

;;* 25.9 Org Mode

;; (info "(org)Top") outline-based notes management and organizer

(leuven--chapter leuven-chapter-25.9-org-mode "25.9 Getting Things Done (with Org mode)"

;;* 1 (info "(org)Introduction")

;;** 1.2 (info "(org)Installation")

  ;; autoload functions
  (GNUEmacs
    (try-require 'org-loaddefs))

  ;; getting started
  (GNUEmacs
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode)))

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c b") 'org-switchb)

  ;; using links outside Org
  (global-set-key (kbd "C-c L") 'org-insert-link-global)
  (global-set-key (kbd "C-c O") 'org-open-at-point-global)

  (with-eval-after-load "org"
    ;; display the Org mode manual in Info mode
    (define-key global-map (kbd "C-h o") 'org-info))
                                        ; XXX not autoloaded

  ;; These variables need to be set before org.el is loaded...

  ;; face to be used by `font-lock' for highlighting in Org mode Emacs
  ;; buffers, and tags to be used to convert emphasis fontifiers for HTML
  ;; export
  (setq org-emphasis-alist              ; remove the strike-through emphasis
        '(("*" bold "<b>" "</b>")
          ("/" italic "<i>" "</i>")
          ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
          ("=" org-verbatim "<code>" "</code>" verbatim)
          ("~" org-code "<code>" "</code>" verbatim)))

  ;; (setq org-emphasis-alist
  ;;       '(("&" (:weight ultra-bold :foreground "#000000" :background "#FBFF00"))
  ;;         ;; ("?" (:box t))
  ;;         ("!" (:weight ultra-bold :foreground "#B40000")) ; = alert in some Wikis

  (with-eval-after-load "org"
    ;; allow both single and double quotes in the border
    (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
    (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)))

  ;; single character alphabetical bullets (a, b, c, ..., X, Y, Z) are allowed
  (setq org-list-allow-alphabetical t)

  ;; libraries that should (always) be loaded along with `org.el'
  ;; (loaded when opening the first Org file)
  (setq org-modules nil)

  ;; set the RESET_CHECK_BOXES and LIST_EXPORT_BASENAME properties in items as
  ;; needed
  (add-to-list 'org-modules 'org-checklist)

  ;; globally unique ID for Org mode entries (see `org-store-link')
  ;; (takes care of automatically creating unique targets for internal
  ;; links, see `C-h v org-id-link-to-org-use-id <RET>')
  (add-to-list 'org-modules 'org-id)

  ;; support for links to Gnus groups and messages from within Org mode
  (add-to-list 'org-modules 'org-gnus)

  ;; habit tracking code for Org mode
  (add-to-list 'org-modules 'org-habit)

  ;; make sure to turn `org-info' on in order to link to info nodes
  (add-to-list 'org-modules 'org-info)

  (add-hook 'org-mode-hook
            (lambda ()
              ;; (local-set-key
              ;;  (kbd "M-n") 'outline-next-visible-heading)
              ;; (local-set-key
              ;;  (kbd "M-p") 'outline-previous-visible-heading)

              ;; ;; create a binding for `org-show-subtree'
              ;; (org-defkey org-mode-map
              ;;             (kbd "C-c C-S-s") 'org-show-subtree)
              ;; (org-defkey org-mode-map
              ;;             (kbd "C-c s") 'org-show-subtree)

              (local-set-key
                (kbd "C-c h") 'hide-other)

              ;; table
              (local-set-key
                (kbd "C-M-w") 'org-table-copy-region)
              (local-set-key
                (kbd "C-M-y") 'org-table-paste-rectangle)
              (local-set-key
                (kbd "C-M-l") 'org-table-sort-lines)

              ;; remove the binding of `C-c SPC' (in Org tables), used by Ace Jump
              (local-set-key
                (kbd "C-c SPC") nil)))

  (with-eval-after-load "org"
    (message "... Org Introduction")

;;** 1.3 (info "(org)Activation")

    (leuven--section "1.3 (org)Activation")

    ;; insert the first line setting Org mode in empty files
    (setq org-insert-mode-line-in-empty-file t))

;;* 2 (info "(org)Document Structure")

  (with-eval-after-load "org"
    (message "... Org Document Structure")

    ;; improve display of the ellipsis
    (set-face-attribute 'org-ellipsis nil
                        :box '(:line-width 1 :color "#999999")
                        :foreground "#999999" :background "#FFF8C0"
                        :underline nil)

    ;; ellipsis to use in the Org mode outline
    (setq org-ellipsis
          (if (char-displayable-p ?\u25B7) ; white right-pointing triangle
              ;; this test takes ~ 0.40s; hence, wrapped in eval-after-load
              " \u25B7"                 ; string
            'org-ellipsis)))            ; face

  ;; <RET> follows links (except in tables, where you must use `C-c C-o')
  (setq org-return-follows-link t)

  ;; blank lines
  (setq org-blank-before-new-entry
        '(;; insert  a blank line before new heading
          (heading . t)

          ;; try to make an intelligent decision whether to insert a
          ;; blank line or not before a new item
          (plain-list-item . auto)))

;;** (info "(org)Headlines")

  (leuven--section "2.2 (org)Headlines")

  ;; ;; `C-a' and `C-e' behave specially in headlines and items
  (setq org-special-ctrl-a/e 'reversed)

  (with-eval-after-load "org"
    (message "... Org Headlines")

    ;; insert an inline task (independent of outline hierarchy)
    (try-require 'org-inlinetask))      ; needed

  (with-eval-after-load "org-inlinetask"

    ;; initial state (TODO keyword) of inline tasks
    (setq org-inlinetask-default-state "TODO")

    ;; template for inline tasks in HTML exporter
    (defun leuven--org-html-format-inlinetask (todo todo-type priority title
                                               tags contents)
      "Format an inline task element for HTML export."
      (let ((todo-kw
             (if todo
                 (format "<span class=\"%s %s\">%s</span> " todo-type todo todo)
               ""))
            (full-title-w/o-todo-kw
             (concat
              (when priority (format "[#%c] " priority))
              title
              (when tags
                (concat "&nbsp;&nbsp;&nbsp;"
                        "<span class=\"tag\">"
                        (mapconcat (lambda (tag)
                                     (concat "<span class= \"" tag "\">" tag
                                             "</span>"))
                                   tags
                                   "&nbsp;")
                        "</span>")))))
        (concat "<table class=\"inlinetask\" width=\"100%\">"
                  "<tr>"
                    "<td valign=\"top\"><b>" todo-kw "</b></td>"
                    "<td width=\"100%\"><b>" full-title-w/o-todo-kw "</b><br />"
                      (or contents "") "</td>"
                  "</tr>"
                "</table>")))

    ;; function called to format an inlinetask in HTML code
    (setq org-html-format-inlinetask-function
          'leuven--org-html-format-inlinetask)

    ;; template for inline tasks in LaTeX exporter
    (defun leuven--org-latex-format-inlinetask (todo todo-type priority title
                                                tags contents)
      "Format an inline task element for LaTeX export."
      (let* ((tags-string (format ":%s:" (mapconcat 'identity tags ":")))
             (opt-color
              (if tags
                  (cond ((string-match ":info:" tags-string)
                         "color=yellow!40")
                        ((string-match ":warning:" tags-string)
                         "color=orange!40")
                        ((string-match ":error:" tags-string)
                         "color=red!40")
                        (t ""))
                ""))
             (full-title
              (concat
               (when todo
                 (format "{\\color{red}\\textbf{\\textsf{\\textsc{%s}}}} "
                         todo))
               (when priority
                 (format "\\textsf{\\framebox{\\#%c}} " priority))
               title
               (when tags
                 (format "\\hfill{}:%s:"
                         (mapconcat 'identity tags ":")))))
             (opt-rule
              (if contents
                  "\\\\ \\rule[.3em]{\\textwidth}{0.2pt}\n"
                ""))
             (opt-contents
              (or contents "")))
        ;; this requires the `todonotes' package
        (format (concat "\\todo[inline,caption={},%s]{\n"
                        "  %s\n"
                        "  %s"
                        "  %s"
                        "}")
                opt-color
                full-title
                opt-rule
                opt-contents)))

    ;; function called to format an inlinetask in LaTeX code
    (setq org-latex-format-inlinetask-function
          'leuven--org-latex-format-inlinetask)

    )                                   ; with-eval-after-load "org-inlinetask" ends here

;;** (info "(org)Visibility cycling")

  (leuven--section "2.3 (org)Visibility cycling")

  ;; do not switch to OVERVIEW at startup
  (setq org-startup-folded nil)

  ;; inhibit startup when preparing agenda buffers -- agenda optimization
  (setq org-agenda-inhibit-startup t)

  ;; (add-hook 'org-mode-hook
  ;;           (lambda()
  ;;             (add-to-list 'mode-line-format
  ;;                          '(:eval (org-propertize
  ;;                                   (org-display-outline-path nil t " / " t)
  ;;                                   'face 'mode-line-emphasis
  ;;                                   'help-echo "Outline path")) t)))

;;** (info "(org)Motion")

  (leuven--section "2.4 (org)Motion")

  ;; outline-node based navigation similar to the behavior of paredit-mode in
  ;; Lisp files
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key
               (kbd "C-M-n") 'outline-next-visible-heading)
              (local-set-key
               (kbd "C-M-p") 'outline-previous-visible-heading)
              (local-set-key
               (kbd "C-M-u") 'outline-up-heading)))

  ;; headlines in the current buffer are offered via completion
  ;; (interface also used by the `refile' command)
  (setq org-goto-interface 'outline-path-completion)

  (with-eval-after-load "org"

    (defun leuven-org-reveal (&optional all-siblings)
      "Show all siblings of current level.
    `C-u C-c C-r' does the same as default Org mode: show all hidden siblings."
      (interactive "P")
      (if all-siblings
          (org-reveal t)
        (org-show-siblings)))

    (define-key org-mode-map (kbd "C-c C-r") 'leuven-org-reveal))

;;** (info "(org)Structure editing")

  (leuven--section "2.5 (org)Structure editing")

  ;; ;; FIXME Choose the right value!
  ;; (setq org-M-RET-may-split-line nil)

;;** (info "(org)Sparse trees")

  (leuven--section "2.6 (org)Sparse trees")

  ;; show full hierarchy when revealing a location
  (setq org-show-hierarchy-above t)

  ;; don't show following heading when revealing a location
  (setq org-show-following-heading nil)

  ;; don't show all sibling headings when revealing a location
  (setq org-show-siblings nil)

  ;; show the entry below a headline when revealing a location
  (setq org-show-entry-below t)
  (setq org-show-entry-below '((org-goto . t)))

;;** (info "(org)Plain lists")

  (leuven--section "2.7 (org)Plain lists")

  ;; maximum indentation for the second line of a description list
  (setq org-description-max-indent 3)

  ;; don't make tab cycle visibility on plain list items
  (setq org-cycle-include-plain-lists nil) ;; 'integrate?

  ;; an empty line does not end all plain list levels
  (setq org-list-empty-line-terminates-plain-lists nil)

;;** (info "(org)Footnotes")

  (leuven--section "2.10 (org)Footnotes")

  ;; use `C-c C-x f' to add a footnote, to go back to the message
  ;; *and* to go to a footnote
  (global-set-key (kbd "C-c C-x f") 'org-footnote-action)

;;* 3 (info "(org)Tables")

  (setq org-table-use-standard-references 'from)

;;** 3.1 The (info "(org)Built-in table editor")

  (leuven--section "3.1 The (org)Built-in table editor")

  ;; default export parameters for `org-table-export'
  (setq org-table-export-default-format "orgtbl-to-csv")

;;** 3.5 (info "(org)The spreadsheet")

  (leuven--section "3.5 (org)The spreadsheet")

  (with-eval-after-load "org-table"
    ;; some Calc mode settings for use in `calc-eval' for table formulas
    (setcar (cdr (memq 'calc-float-format org-calc-default-modes))
            '(float 12)))               ; [default: 8]

;;* 4 (info "(org)Hyperlinks")

  ;; don't hexify URL when creating a link
  (setq org-url-hexify-p nil)

  (with-eval-after-load "org"
    (message "... Hyperlinks")

    ;; open non-existing files
    (setq org-open-non-existing-files t)

    ;; function and arguments to call for following `mailto' links
    (setq org-link-mailto-program '(compose-mail "%a" "%s")))

  ;; support for links to Gnus groups and messages from within Org mode
  (with-eval-after-load "org-gnus"

    ;; create web links to Google groups or Gmane (instead of Gnus
    ;; messages)
    (setq org-gnus-prefer-web-links t))

  ;; global identifiers for Org-mode entries
  (with-eval-after-load "org-id"

    ;; storing a link to an Org file will use entry IDs
    (setq org-id-link-to-org-use-id
          'create-if-interactive-and-no-custom-id))

  (with-eval-after-load "org"
    (message "... Handling links")

    ;; 4.4 show inline images when loading a new Org file
    (setq org-startup-with-inline-images t) ; invokes org-display-inline-images

    ;; 4.4 try to get the width from an #+ATTR.* keyword and fall back on the
    ;; original width if none is found
    (setq org-image-actual-width nil)

    ;; shortcut links
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

  (leuven--section "5.1 (org)TODO basics functionality")

  ;; 5.1 select a TODO state and bypass any logging associated with that
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; some commands act upon headlines in the active region
  (setq org-loop-over-headlines-in-active-region 'start-level)

;;** 5.2 Use of (info "(org)TODO extensions")

  (leuven--section "5.2 Use of (org)TODO extensions")

  ;; list of TODO entry keyword sequences (+ fast access keys and specifiers
  ;; for state change logging)
  (setq org-todo-keywords
        '((sequence "NEW(n!)"           ; proposal, idea (under review)
                    "TODO(t!)"          ; open, not (yet) started
                    "STRT(s!)"          ; in progress, working on, doing
                    "WAIT(w!)"          ; on hold, assigned, feedback
                    "SDAY(y!)"          ; someday, maybe, perhaps, wish
                    "|"
                    "DONE(d!)"          ; completed, closed, resolved
                    "CANX(x!)")         ; wontfix, rejected

          (sequence "QTE(q!)"           ; planning
                    "QTD(Q!)"           ; awaiting approval
                    "|"
                    "APP(A!)"           ; approved
                    "REJ(R!)")          ; rejected

          (sequence "OPENPO(O!)"
                    "|"
                    "CLSDPO(C!)")))

  (with-eval-after-load "org-faces"

    ;; faces for specific TODO keywords
    (setq org-todo-keyword-faces
          '(("NEW"  . leuven-org-created-kwd)
            ("TODO" . org-todo)
            ("STRT" . leuven-org-in-progress-kwd)
            ("WAIT" . leuven-org-waiting-for-kwd)
            ("SDAY" . leuven-org-someday-kwd)
            ("DONE" . org-done)
            ("CANX" . org-done)

            ("QTE" . leuven-org-quote-kwd)
            ("QTD" . leuven-org-quoted-kwd)
            ("APP" . leuven-org-approved-kwd)
            ("REJ" . leuven-org-rejected-kwd)

            ("OPENPO" . leuven-org-openpo-kwd)
            ("CLSDPO" . leuven-org-closedpo-kwd)))

    ;; Org standard faces
    (set-face-attribute 'org-todo nil
                        :weight 'bold :box '(:line-width 1 :color "#D8ABA7")
                        :foreground "#D8ABA7" :background "#FFE6E4")

    (set-face-attribute 'org-done nil
                        :weight 'bold :box '(:line-width 1 :color "#BBBBBB")
                        :foreground "#BBBBBB" :background "#F0F0F0")

    ;; Org non-standard faces
    (defface leuven-org-created-kwd
      '((t (:weight normal :box (:line-width 1 :color "#EEE9C3")
            :foreground "#1A1A1A" :background "#FDFCD8")))
      "Face used to display state NEW.")
    (defface leuven-org-in-progress-kwd
      '((t (:weight bold :box (:line-width 1 :color "#D9D14A")
            :foreground "#D9D14A" :background "#FCFCDC")))
      "Face used to display state STRT.")
    (defface leuven-org-waiting-for-kwd
      '((t (:weight bold :box (:line-width 1 :color "#89C58F")
            :foreground "#89C58F" :background "#E2FEDE")))
      "Face used to display state WAIT.")
    (defface leuven-org-someday-kwd
      '((t (:weight bold :box (:line-width 1 :color "#9EB6D4")
            :foreground "#9EB6D4" :background "#E0EFFF")))
      "Face used to display state SDAY.")

    (defface leuven-org-quote-kwd
      '((t (:weight bold :box (:line-width 1 :color "#FC5158")
            :foreground "#FC5158" :background "#FED5D7")))
      "Face used to display .")
    (defface leuven-org-quoted-kwd
      '((t (:weight bold :box (:line-width 1 :color "#55BA80")
            :foreground "#55BA80" :background "#DFFFDF")))
      "Face used to display .")
    (defface leuven-org-approved-kwd
      '((t (:weight bold :box (:line-width 1 :color "#969696")
            :foreground "#969696" :background "#F2F2EE")))
      "Face used to display .")
    (defface leuven-org-rejected-kwd
      '((t (:weight bold :box (:line-width 1 :color "#42B5FF")
            :foreground "#42B5FF" :background "#D3EEFF")))
      "Face used to display state REJECTED.")

    (defface leuven-org-openpo-kwd
      '((t (:weight bold :box (:line-width 1 :color "#FC5158")
            :foreground "#FC5158" :background "#FED5D7")))
      "Face used to display OPEN purchase order.")
    (defface leuven-org-closedpo-kwd
      '((t (:weight bold :box (:line-width 1 :color "#969696")
            :foreground "#969696" :background "#F2F2EE")))
      "Face used to display CLOSED purchase order."))

  ;; block switching entries to DONE if
  ;; 1) there are undone child entries, or
  ;; 2) the parent has an `:ORDERED:' property and there are prior
  ;;    siblings not yet done
  (setq org-enforce-todo-dependencies t)

  ;; 5.2.7 don't dim blocked tasks in the agenda display -- agenda optimization
  (setq org-agenda-dim-blocked-tasks nil) ; XXX not sure about this one

  ;; block switching the parent to DONE if
  ;; there are unchecked checkboxes
  (setq org-enforce-todo-checkbox-dependencies t)

;;** 5.3 (info "(org)Progress logging")

  (leuven--section "5.3 (org)Progress logging")

  ;; ;; 5.3.1 don't insert a CLOSED time stamp each time a TODO entry is marked DONE
  ;; (setq org-log-done nil)

  ;; 5.3.2 the notes will be ordered according to time
  (setq org-log-states-order-reversed nil)

  ;; 5.3.2 insert state change notes and time stamps into a LOGBOOK drawer
  (setq org-log-into-drawer t)          ; should be the DEFAULT!

  ;; ~5.3.2 heading for state change added to entries
  (with-eval-after-load "org"
    (message "... Progress logging")

    (setcdr (assq 'state org-log-note-headings)
            "State %-12S  ->  %-12s %t")) ; "State old -> new + timestamp"

  (with-eval-after-load "org-habit"

    ;; show habits for future days
    (setq org-habit-show-habits-only-for-today nil)

    ;; use character "heavy check mark" to show completed days on which
    ;; a task was done
    (setq org-habit-completed-glyph ?\u2714)

    ;; use character "heavy quadruple dash vertical" to identify today
    (setq org-habit-today-glyph ?\u250B))

;;** 5.5 (info "(org)Breaking down tasks")

  (leuven--section "5.5 (org)Breaking down tasks")

  ;; automatically change a TODO entry to DONE when all children are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;* 6 (info "(org)Tags")

  ;; column to which tags should be indented in a headline
  (setq org-tags-column -80)

  ;; 6.2 list of tags ("contexts") allowed in Org mode files
  (setq org-tag-alist '((:startgroup . nil)
                         ("personal"  . ?p)
                         ("work"      . ?w)
                        (:endgroup . nil)
                        ("call"        . ?c)
                        ("errands"     . ?e)
                        ("finance"     . ?f)
                        ("mail"        . ?m)

                        ("notbillable" . ?B)
                        ("now"         . ?N)
                        ;; ("reading" . ?r)
                        ;; ("proj" . ?P)

                        ("ARCHIVE"     . ?a) ; speed command + action in task list
                        ("crypt"       . ?C)
                        ("FLAGGED"     . ??)))

  ;; faces for specific tags
  (setq org-tag-faces
        '(("refile"
           (:slant italic
            :foreground "#A9876E"))     ; :background "#FCEEB3"
          ("personal"
           (:slant italic
            :foreground "#5C88D3"))     ; :background "#BBDDFF"
          ("work"
           (:slant italic
            :foreground "#699761"))     ; :background "#C1D996"
          ("FLAGGED"
           (:slant italic
            :foreground "#C15F4E"))     ; :background "#EDC6C8"
          ("now"
           (:slant italic
            :foreground "#000000"))     ; :background "#FFEA80"
          ("notbillable"
           (:slant italic
            :foreground "#8774AF"))     ; :background "#DED0EA"
          ))

  ;; 6.2 exit fast tag selection after first change (toggle this with `C-c')
  (setq org-fast-tag-selection-single-key t)

  ;; remove redundant tags of headlines (from David Maus)
  (defun leuven-org-remove-redundant-tags ()
    "Remove redundant tags of headlines in current buffer.
  A tag is considered redundant if it is local to a headline and inherited by
  a parent headline."
    (interactive)
    (when (eq major-mode 'org-mode)
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

  ;; ;; always offer completion for all tags of all agenda files
  ;; (setq org-complete-tags-always-offer-all-agenda-tags t)

;;* 7 (info "(org)Properties and Columns")

;;** 7.1 (info "(org)Property syntax")

  (leuven--section "7.1 (org)Property syntax")

  ;; list of property/value pairs that can be inherited by any entry
  (setq org-global-properties
        '(("Effort_ALL" .
           "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"
           ;; "0d 1d 2d 3d 4d 5d 6d 7d 8d 10d"
           ;; "0 1:00 4:00 1d 2d 1w 2w"
           )))

;;* 8 (info "(org)Dates and Times")

  (leuven--section "8 (org)Dates and Times")

  ;; insinuate appt if Org mode is loaded
  (with-eval-after-load "org"
    (message "... Org Dates and Times")

    (try-require 'appt))

;;** 8.2 (info "(org)Creating timestamps")

  (leuven--section "8.2 (org)Creating time stamps")

  ;; prefer the future for incomplete dates
  (setq org-read-date-prefer-future 'time)

  ;; ;; advise `org-read-date' to bury the calendar buffer after selecting
  ;; ;; a date, so it is out of the way
  ;; (defadvice org-read-date
  ;;   (after leuven-bury-calendar-after-org-read-date
  ;;          (&optional with-time to-time from-string prompt
  ;;          default-time default-input) protect)
  ;;   "Bury the *Calendar* buffer after reading a date."
  ;;   (bury-buffer "*Calendar*"))
  ;; (ad-activate 'org-read-date)

  ;; number of minutes to round time stamps to
  (setq org-time-stamp-rounding-minutes '(1 1))

;;** 8.3 (info "(org)Deadlines and scheduling")

  (leuven--section "8.3 (org)Deadlines and scheduling")

  ;; information to record when the scheduling date is modified
  (setq org-log-reschedule nil)

  ;; information to record when the deadline date is modified
  (setq org-log-redeadline 'time)

  ;; number of days before expiration during which a deadline becomes active
  (setq org-deadline-warning-days 7)

  ;; skip deadline prewarning (up to 7 days before the actual deadline)
  ;; when entry is also scheduled
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 7)

  ;; don't show deadlines when the corresponding item is done
  (setq org-agenda-skip-deadline-if-done t)

  ;; skip scheduling line if same entry shows because of deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; don't show scheduled items in agenda when they are done
  (setq org-agenda-skip-scheduled-if-done t)

  ;; ~8.3 don't select item by time stamp or -range if it is DONE
  (setq org-agenda-skip-timestamp-if-done t)

  ;; ;; show all days between the first and the last date
  ;; (setq org-timeline-show-empty-dates t)

  ;; TODO state to which a repeater should return the repeating task
  (setq org-todo-repeat-to-state "TODO")

;;** 8.4 (info "(org)Clocking work time")

  (leuven--section "8.4 (org)Clocking work time")

  (global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
  (global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

  ;; the time clocking code for Org mode
  ;; (require 'org-clock)               ;! needed for trying to automatically
                                        ;! re-clock at Emacs startup

  ;; XXX Under test!
  (add-hook 'org-mode-hook
            (lambda ()
              (require 'org-clock)
              (setq org-clock-persist t)
              (org-clock-persistence-insinuate)))

  (with-eval-after-load "org-clock"

    ;; ;; 8.4 save both the running clock and the entire clock history when Emacs
    ;; ;; is closed, and resume it next time Emacs is started up
    ;; (setq org-clock-persist t)
    ;;
    ;; ;; 8.4 set up hooks for clock persistence
    ;; (org-clock-persistence-insinuate)

    ;; resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; number of clock tasks to remember in history
    (setq org-clock-history-length 35)  ; 1-9A-Z

    ;; 8.4.2 include the current clocking task time in clock reports
    (setq org-clock-report-include-clocking-task t)

    ;; 8.4.2 format string used when creating CLOCKSUM lines and when generating a
    ;; time duration (avoid showing days)
    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
                                        ; some clocktable functions cannot
                                        ; digest day formats (e.g.,
                                        ; org-clock-time%)

    ;; ;; 8.4.2 use fractional times
    ;; (setq org-time-clocksum-use-fractional t)

    ;; format string for the total time cells
    (setq org-clock-total-time-cell-format "%s")

    ;; format string for the file time cells
    (setq org-clock-file-time-cell-format "%s")

    (defun leuven-org-clock-in-interrupted-task ()
      "Clock back into the task that has been interrupted, if there is one."
      (interactive)
      (if (and (not org-clock-resolving-clocks-due-to-idleness)
               (marker-buffer org-clock-marker)
               (marker-buffer org-clock-interrupted-task))
          (org-with-point-at org-clock-interrupted-task
            (org-clock-in nil))
        (org-clock-out)))

    (global-set-key (kbd "C-c C-x C-q") 'leuven-org-clock-in-interrupted-task)

    ;; 8.4.3 resolve open clocks if the user is idle more than 120 minutes
    (setq org-clock-idle-time 120)

    (defun leuven--org-switch-to-started (kwd)
      "Switch task state to STRT.
    Skip normal headlines and capture tasks."
      (if (and kwd
               (not (string-equal kwd "STRT"))
               (not (and (boundp 'org-capture-mode) org-capture-mode)))
          "STRT"
        nil))

    ;; 8.4.3 set task to todo state STRT while clocking it
    (setq org-clock-in-switch-to-state 'leuven--org-switch-to-started)

    ;; clock won't be stopped when the clocked entry is marked DONE
    (setq org-clock-out-when-done nil)

    ;; time included for the mode line clock is all time clocked into this
    ;; task today
    (setq org-clock-mode-line-total 'today)
    (setq org-clock-mode-line-total 'all)

    ;; get an alert (notification) when your planned time is over
    (setq org-clock-sound "~/Public/Music/Sounds/alarm.wav")
    ;;! Use start-process to have an external program play the sound to
    ;;! avoid ignored keystrokes until after the sound plays (start-process
    ;;! "ding" nil "play" "~/Public/Music/Sounds/alarm.wav")

    ;; remove the clock line when the resulting time is 0:00
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; ;; when clocking into a task with a clock entry which has not been
    ;; ;; closed, resume the clock from that point
    ;; (setq org-clock-in-resume t)

    ;; ask the user if they wish to clock out before killing Emacs
    (defun leuven--org-query-clock-out ()
      "Ask the user before clocking out.
    This is a useful function for adding to `kill-emacs-query-functions'."
      (if (and (featurep 'org-clock)
               (funcall 'org-clocking-p)
               (y-or-n-p "You are currently clocking time, clock out? "))
          (org-clock-out)
        t))                             ; only fails on keyboard quit or error

    (add-hook 'kill-emacs-query-functions 'leuven--org-query-clock-out)

    )                                   ; with-eval-after-load "org-clock" ends here

;;** 8.5 (info "(org)Effort estimates")

  (leuven--section "8.5 (org)Effort estimates")

  ;; add an effort estimate on the fly when clocking in
  (defun leuven--org-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Estimated time (H:MM): "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))

  (add-hook 'org-clock-in-prepare-hook 'leuven--org-ask-effort)

;;* 9 (info "(org)Capture - Refile - Archive")

  (leuven--section "9.1 (org)Capture")

  ;; 9.1.2 directory with Org files
  (setq org-directory
        (directory-file-name            ; this function removes the final slash
         (cond ((file-directory-p "~/org/") "~/org/")
               (t "~/"))))

  ;; 9.1.2 default target for storing notes
  (setq org-default-notes-file          ; inbox for collecting [default: "~/.notes"]
        (concat org-directory "/refile.org"))

  ;; 9.1.2 templates for the creation of capture buffers

  ;; ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")

  ;; fast note taking in Org mode (the ultimate capture tool)
  (with-eval-after-load "org-capture"

    (add-to-list 'org-capture-templates
                 `("t" "Task" entry
                   (file+headline ,org-default-notes-file "Tasks")
                   "* NEW %^{Task}%?

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
                   (file+headline ,org-default-notes-file "Calendar")
                   "* %^{Appointment}%?
%^T

%i"
                   :empty-lines 1) t)
                   ;; TODO Prompt only for date, not time...

    (add-to-list 'org-capture-templates
                 `("Z" "Refile me!" entry
                   (function leuven-find-location)
                   "** TODO Put this in some other file\n\n"
                   :prepend t) t)

    (defun leuven-find-location ()
      "Find my CollectBox file and some headline in the current buffer."
      (find-file org-default-notes-file)
      (goto-char (point-min))
      (helm-org-headlines)
      (org-forward-heading-same-level 1))

    (add-to-list 'org-capture-templates
                 `("m" "Email processing...") t)

    (add-to-list 'org-capture-templates
                 `("mt" "Create a TODO Action + edit" entry
                   (file+headline ,org-default-notes-file "Email") ; #+FILETAGS: :mail:
                   "* TODO %^{Creating action}%? (from %:fromname)
   %:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From %a"
                   :empty-lines 1) t)

    (add-to-list 'org-capture-templates
                 `("mr" "Create a TODO Action Remind 3" entry
                   (file+headline ,org-default-notes-file "Email") ; #+FILETAGS: :mail:
                   "* TODO %:subject%? (from %:fromname)
   SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+3d\") nil nil nil nil \"\")
   %:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From %a"
                   :empty-lines 1 :immediate-finish t) t)

    (add-to-list 'org-capture-templates
                 `("p" "Phone call" entry
                   (file+headline ,org-default-notes-file "Phone calls")
                   "* %?"
                   :empty-lines 1 :clock-in t :clock-resume t) t)

    (add-to-list 'org-capture-templates
                 `("i" "interruption" entry
                   (file ,org-default-notes-file)
                   "A TEMPLATE HERE"
                   :clock-in t :clock-resume t) t)

    ;; thought
    (add-to-list 'org-capture-templates
                 `("n" "Note" entry
                   (file+headline ,(concat org-directory "/notes.org") "Notes")
                   "* %^{Thought}%?

%i"
                   :empty-lines 1) t)

    ;; shopping list (stuff to buy)
    (add-to-list 'org-capture-templates
                 `("b" "Buy" checkitem
                   (file+headline ,org-default-notes-file "Shopping")) t)

    ;; add a note to the currently clocked task
    (add-to-list 'org-capture-templates
                 `("c" "Clock sibling" entry
                   (clock)
                   "* %^{Title}
  %U
%a

%i") t)

    (add-to-list 'org-capture-templates
                 `("j" "Journal" entry
                   (file+datetree ,(concat org-directory "/journal.org"))
                   "* %T %?

  %U

%i

From %a"
                   ;; "* %^{Title}\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x"
                   :empty-lines 1) t)

    (add-to-list 'org-capture-templates
                 `("S" "secure" entry
                   (file+datetree+prompt "~/org/notes/secure.org.gpg")
                   "* %(format-time-string \"%H:%M\") %^{Entry} %^G
%i%?") t)

    ;;          ("w" "org-protocol" entry
    ;;           (file ,org-default-notes-file)
    ;;           "* TODO Review %c
    ;; %U"
    ;;           :immediate-finish t :clock-in t :clock-resume t)

    ;; ("web-clippings" ?w
    ;;  "* %^{Title} %^g \n  :PROPERTIES:\n  :date: %^t\n  :link: %^{link}\n  :END:\n\n %x %?"
    ;;  "~/org/data.org" "Web Clippings")

    (add-to-list 'org-capture-templates
                 `("w" "Default template" entry
                   ;; `org-protocol-default-template-key'
                   (file+headline ,(concat org-directory "/capture.org") "Notes")
                   "* %^{Title}%?
  %u

%i

From %c"
                   :empty-lines 1 :immediate-finish t) t)

    ;; default `org-capture-templates' key to use
    (setq org-protocol-default-template-key "w")

    (defun make-capture-frame ()
      "Create a new frame and run `org-capture'."
      (interactive)
      (make-frame '((name . "capture")
                    (width . 80)
                    (height . 10)))
      (select-frame-by-name "capture")
      ;; ;; setup buffer to wrap
      ;; (setq truncate-lines nil
      ;;       word-wrap t)
      (org-capture))

    ;; make the frame contain a single window (by default, `org-capture'
    ;; splits the window)
    (add-hook 'org-capture-mode-hook 'delete-other-windows)

    (defadvice org-capture-finalize
      (after leuven-delete-capture-finalize-frame activate)
      "Advise org-capture-finalize to close the frame (if it is the capture frame)."
      (if (equal "capture" (frame-parameter nil 'name))
          (delete-frame)))

    (defadvice org-capture-destroy      ; XXX
      (after leuven-delete-capture-destroy-frame activate)
      "Advise capture-destroy to close the frame (if it is the capture frame)."
      (if (equal "capture" (frame-parameter nil 'name))
          (delete-frame)))

    )                                   ; with-eval-after-load "org-capture" ends here

;; bug when C-c C-l
  ;; ;; 4.6 shortcut links
  ;; (add-to-list 'org-link-abbrev-alist '(("att" . org-attach-expand-link)))

  (leuven--section "9.4 (org)Protocols")

  ;; 9.4 capture from Firefox (to store links and text)
  (with-eval-after-load "org-protocol"

    ;; map online URL to an existing working file
    (add-to-list 'org-protocol-project-alist
                 '("Worg at http://orgmode.org/worg/"
                   :online-suffix ".html"
                   :working-suffix ".org"
                   :base-url "http://orgmode.org/worg/"
                   :working-directory "~/Public/Repositories/worg/") t))

  (with-eval-after-load "org"
    (message "... Org Refile")

    (defvar leuven-org-refile-extra-files
      (if (file-exists-p "~/org/notes/")
          (directory-files "~/org/notes/" t "^[^\\.#].*\\.\\(txt\\|org\\)$")
        nil)
      "List of extra files to be used as targets for refile commands.")

    ;; 9.5 any headline with level <= 3 is a target
    (setq org-refile-targets
          `((nil
             :maxlevel . 4)             ; current file
            (,(append org-agenda-files leuven-org-refile-extra-files)
             :maxlevel . 2)))

    ;; cache refile targets to speed up the process
    (setq org-refile-use-cache t)

    ;; 9.5 provide refile targets as paths, including the file name
    ;; (without directory) as level 1 of the path
    (setq org-refile-use-outline-path 'file)

    ;; 9.5 allow to create new nodes (must be confirmed by the user) as
    ;; refile targets
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; refile only within the current buffer
    (defun leuven-org-refile-within-current-buffer ()
      "Move the entry at point to another heading in the current buffer."
      (interactive)
      (let ((org-refile-targets '((nil :maxlevel . 4))))
        (org-refile)))
    ;; FIXME Add a smart key binding

    ;; exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude TODO keywords with a DONE state from refile targets."
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq org-refile-target-verify-function 'bh/verify-refile-target)

    (leuven--section "9.6 (org)Archiving")

    ;; 9.6.1 subtrees should be archived in the current file
    (setq org-archive-location "::* Archive")

    )

  (leuven--section "10 (org)Agenda Views")

;;* 10 (info "(org)Agenda Views")

  (with-eval-after-load "org-agenda"

    ;; multiple same-day time stamps in entry make multiple agenda lines
    (setq org-agenda-skip-additional-timestamps-same-entry nil)

    ;; show outline path in echo area after line motion (though, may bring
    ;; some slowness)
    (setq org-agenda-show-outline-path t)

    ;; 10.0 restore the window configuration when exiting the agenda
    (setq org-agenda-restore-windows-after-quit t)

    ;; ;; speed up agenda by avoiding to update some text properties
    ;; (setq org-agenda-ignore-drawer-properties '(effort category)) ; org.el

;;** 10.1 (info "(org)Agenda files")

    (leuven--section "10.1 (org)Agenda files")

    (when (boundp 'org-agenda-files)
      (message "(Info) Found %s entries in `org-agenda-files'"
               (length org-agenda-files))
      ;; (sit-for 0.5)
      )

;;** 10.2 (info "(org)Agenda dispatcher")

    (leuven--section "10.2 (org)Agenda dispatcher")

    ;; enable sticky agenda: `q' key will bury agenda buffers (instead of
    ;; killing)
    (setq org-agenda-sticky t)

;;** 10.3 The (info "(org)Built-in agenda views")

    (leuven--section "10.3 (org)Built-in agenda views")

    ;; default duration for appointments that only have a starting time
    (setq org-agenda-default-appointment-duration nil)

    ;; ;; duration of an appointment will add to day effort
    ;; (setq org-agenda-columns-add-appointments-to-effort-sum t)

    ;; show dated entries in the global `todo' list
    (setq org-agenda-todo-ignore-with-date nil)
                                        ;!! tricky setting

    ;; show entries with a time stamp in the global `todo' list
    (setq org-agenda-todo-ignore-timestamp nil)

    ;; 10.3.2 don't show scheduled entries in the global `todo' list
    (setq org-agenda-todo-ignore-scheduled 'future)
                                        ;!! tricky setting
    (setq org-agenda-todo-ignore-scheduled nil)

    ;; 10.3.2 don't show entries scheduled in the future in the global
    ;; `todo' list (until they are within the warning period)
    (setq org-agenda-todo-ignore-deadlines 'near)
                                        ;!! tricky setting
    (setq org-agenda-todo-ignore-deadlines nil)

    ;; 10.3.2 check also the sublevels of a TODO entry for TODO entries,
    ;; resulting in potentially much longer `todo' lists
    (setq org-agenda-todo-list-sublevels t)

    ;; 10.3.3 honor `todo' list `org-agenda-todo-ignore...' options also
    ;; in the `tags-todo' list
    (setq org-agenda-tags-todo-honor-ignore-options t)

    ;; ;; highlight current line (may bring some slowness)
    ;; (add-hook 'org-agenda-mode-hook 'hl-line-mode)

    ;; 10.3.5 list of extra files to be searched by text search commands
    ;; (C-c a s)
    (setq org-agenda-text-search-extra-files nil) ; org.el

    (defvar leuven-org-search-extra-files nil
      "List of extra files to be searched by custom search commands (`R s' and `R S').")

    ;; turn on individual word search (for Google addicts)
    (setq org-agenda-search-view-always-boolean t
          org-agenda-search-view-search-words-only t)

    ;; match part of a word
    (setq org-agenda-search-view-force-full-words nil)

    ;; don't search headline for a time-of-day
    (setq org-agenda-search-headline-for-time nil)

    ;; 10.3.6 how to identify stuck projects
    (setq org-stuck-projects
          '("+LEVEL=2/-DONE"            ; identify a project
            ("TODO" "STRT")             ; TODO keywords
            nil ""))                    ; tags, regexp

;;** 10.4 (info "(org)Presentation and sorting")

    (leuven--section "10.4 (org)Presentation and sorting")

    ;; 10.4 format specifications for the prefix of items in the agenda views
    (setq org-agenda-prefix-format
          '((agenda   . " %-11s%i %?-12t") ; agenda
            (timeline . " % s")         ; timeline
            (todo     . " %i %-12:c")   ; todo, alltodo
            (tags     . " %i %-12:c")   ; tags, tags-todo, stuck
            (search   . " %i %-12:c"))) ; search

    ;; text preceding scheduled items in the agenda view
    (setq org-agenda-scheduled-leaders
          '("Scheduled  "
            "           "))

    ;; text preceding item pulled into the agenda by inactive time stamps
    (setq org-agenda-inactive-leader "[")

    ;; text preceding deadline items in the agenda view
    (setq org-agenda-deadline-leaders
          '("Deadline   "
            "In %d d"                   ; or "%d d left"
            "%d d ago"))

    )                                   ; with-eval-after-load "org-agenda" ends here

  (with-eval-after-load "org-faces"

    ;; faces for showing deadlines in the agenda
    (setq org-agenda-deadline-faces
          '((1.0001 . leuven-org-deadline-overdue)
            (0.9999 . leuven-org-deadline-today)
            (0.8571 . leuven-org-deadline-tomorrow) ; = 6/7, see `org-deadline-warning-days'
            (0.0000 . leuven-org-deadline-future)))

    ;; see http://www.dgtale.ch/index.php?option=com_content&view=article&id=52&Itemid=61

    ;; Org non-standard faces
    (defface leuven-org-deadline-overdue
      '((t (:foreground "#F22659")))
      "Face used to highlight tasks whose due date is in the past.")

    (defface leuven-org-deadline-today
      '((t (:weight bold :foreground "#4F4A3D" :background "#FFFFCC")))
      "Face used to highlight tasks whose due date is today.")

    (defface leuven-org-deadline-tomorrow
      '((t (:foreground "#34AD00")))
      "Face used to highlight tasks whose due date is tomorrow.")

    (defface leuven-org-deadline-future
      '((t (:foreground "#34AD00")))
      "Face used to highlight tasks whose due date is for later."))

  (with-eval-after-load "org-agenda"

    ;; ;; 10.4 column to shift tags to (in agenda items)
    ;; (setq org-agenda-tags-column -132)

    ;; right-justify tags in the agenda buffer
    (defun leuven--org-agenda-right-justify-tags ()
      "Justify the tags to the right border of the agenda window."
      (let ((org-agenda-tags-column (- 2 (window-width))))
        (org-agenda-align-tags)))
    (add-hook 'org-agenda-finalize-hook 'leuven--org-agenda-right-justify-tags)

    ;; type "(" in agenda and todo buffers to show category name and task
    ;; length for each task
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
      (kbd "(") 'leuven-org-agenda-toggle-tasks-details))

  ;; 10.4.2 settings for time grid for agenda display
  (setq org-agenda-time-grid '((daily remove-match)
                               ""
                               (0800 1000 1200 1400 1600 1800 2000)))

  ;; string for the current time marker in the agenda
  (setq org-agenda-current-time-string "Right now")

  ;; 10.4.3 sorting structure for the agenda items of a single day
  (setq org-agenda-sorting-strategy   ; custom value
        '((agenda time-up category-up priority-down effort-down)
          (todo category-up priority-down effort-down)
          (tags category-up priority-down effort-down)
          (search category-up)))

  ;; (setq org-sort-agenda-notime-is-late nil)

  ;; show agenda in the current window, keeping all other windows
  (setq org-agenda-window-setup 'current-window)

;;** 10.5 (info "(org)Agenda commands")

  (leuven--section "10.5 (org)Agenda commands")

  ;; get a compact view during follow mode in the agenda
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
  ;; (add-hook 'org-agenda-after-show-hook 'leuven--compact-follow)

  ;; 10.5 number of days to include in overview display
  (setq org-agenda-span 'day)

  ;; always start the overview on the current day
  (setq org-agenda-start-on-weekday nil)

  ;; format string for displaying dates in the daily/weekly agenda
  ;; and in the timeline
  (setq org-agenda-format-date
        (concat                         ; "\n"
                "%Y-%m-%d" " %a "
                ;; (make-string (1- (window-width)) (string-to-char "_"))))
                (make-string 65 (string-to-char " "))
                "_"
                ;; (make-string 1 ?\u25AE)
                ))

  ;; 10.5 only show clocked entries in agenda log mode (no closed
  ;; entries, no state changes)
  (setq org-agenda-log-mode-items '(clock))

  ;; 10.5 parameters for the clocktable in clockreport mode
  (setq org-agenda-clockreport-parameter-plist
        '(:link nil :maxlevel 3 :fileskip0 t))
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 3 :fileskip0 t))

  ;; 10.5 definition of what constitutes a clocking problem (overlapping
  ;; clock entries, clocking gaps)
  (setq org-agenda-clock-consistency-checks
        '(:max-duration "10:00"
          :min-duration 0
          :max-gap "0:00"
          :gap-ok-around ("4:00")
          :default-face
          ((:weight bold
            :box (:line-width 1 :color "#AAEE77")
            :foreground "black" :background "#BFFA9E"))
          :gap-face
          ((:weight bold
            :box (:line-width 1 :color "#BBDDFF")
            :foreground "black" :background "#D0EDFF"))))

  ;; 10.5 text prepended to the entry text in agenda buffers
  (setq org-agenda-entry-text-leaders "               │ ")

  ;; 10.5 file to which to add new entries with the `i' key in agenda and
  ;; calendar (org.el)
  (setq org-agenda-diary-file "~/org/diary.org")

  ;; 10.5? keep filters from one agenda view to the next
  (setq org-agenda-persistent-filter t)

  ;; faces for specific Priorities (#A, #B and #C)
  (setq org-priority-faces
        '((?A . (:weight bold :foreground "#5F3731" :background "#EFC4C0"))
          (?B . (:foreground "#475443" :background "#D5E1D0"))
          (?C . (:foreground "#2D373F" :background "#C9DBE3"))))

  ;; 10.5 Commands in the agenda buffer
  (defun leuven--weekday-p ()
    "Return t if current day is between Monday and Friday."
    (let ((dow (nth 6 (decode-time))))
      (and (> dow 0)
           (< dow 6))))

  (defun leuven--working-p ()
    "Return t if current time is inside normal working hours.
  Currently: 08:30-12:30 and 13:30-17:30."
    (let* ((time (decode-time))
           (hour (nth 2 time))
           (mins (nth 1 time)))
      (and (leuven--weekday-p)
           (or (or (and (= hour 8) (>= mins 30))
                   (and (< 8 hour) (< hour 12))
                   (and (= hour 12) (<= mins 30)))
               (or (and (= hour 13) (>= mins 30))
                   (and (< 13 hour) (< hour 17))
                   (and (= hour 17) (<= mins 30)))))))

  (defun leuven--calling-hours-p ()
    "Return t if current time is inside normal calling hours.
  Currently: 08:00-21:59."
    (let* ((hour (nth 2 (decode-time))))
      (and (<= 8 hour) (<= hour 21))))

  (defun leuven--org-auto-exclude-function (tag)
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

  ;;! ensure that `:refile:' tags never will be excluded!
  (defun leuven--org-auto-exclude-function (tag)
    (and (cond
          ((string= tag "personal")
           (leuven--working-p))
          ((string= tag "work")
           (not (leuven--working-p)))
          ((or (string= tag "errands")
               (string= tag "call"))
           (not (leuven--calling-hours-p))))
         (concat "-" tag)))

  (setq org-agenda-auto-exclude-function 'leuven--org-auto-exclude-function)

  ;; make the block agenda more compact (no agenda span name, no week
  ;; number, no separator line)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-compact-blocks nil)

  (setq org-agenda-block-separator
        #("____________________________________________________________________________________________________________________________________"
          0 132 (face (:foreground "#59ACE2")))) ; lighter version with #C0E2F4
        ;; (make-string 132 (string-to-char "_"))

;;** 10.6 (info "(org)Custom agenda views")

  (leuven--section "10.6 (org)Custom agenda views")

  (with-eval-after-load "org-agenda"
    (let ((leuven-org-agenda-views
           "~/src/emacs-leuven/org-custom-agenda-views.el"))
      (when (file-exists-p leuven-org-agenda-views)
        (load-file leuven-org-agenda-views))))
                                        ; with-eval-after-load "org-agenda" ends here

  (defun leuven-org-todo-list-current-dir ()
    "Produce a view from all Org files in the current directory."
    (interactive)
    (let* ((fname (buffer-file-name))
           (dname (if fname
                      (if (file-directory-p fname)
                          fname
                        (file-name-directory fname))
                    default-directory))
           (org-agenda-files (directory-files dname t "\\.\\(org\\|txt\\)$"))
           (org-agenda-sorting-strategy '(todo-state-up priority-down))
           (org-agenda-overriding-header
            (format "List of TODO items restricted to directory\n%s" dname))
           (org-agenda-sticky nil))
      (message "%s..." org-agenda-overriding-header)
      (org-todo-list)))

  ;; "TODO list" without asking for a directory
  (global-set-key (kbd "<C-f3>") 'leuven-org-todo-list-current-dir)

;;** 10.7 (info "(org)Exporting Agenda Views")

  (leuven--section "10.7 (org)Exporting Agenda Views")

  ;; 10.7 alist of variable/value pairs that should be active during agenda
  ;; export
  (setq org-agenda-exporter-settings
        '((ps-number-of-columns 1)      ; 2?
          (ps-landscape-mode t)
          ;; (org-agenda-add-entry-text-maxlines 5)
          (htmlize-output-type 'css)))

;;** 10.8 (info "(org)Agenda column view")

  (leuven--section "10.8 (org)Agenda column view")

  ;; 10.8 default column format, if no other format has been defined
  (setq org-columns-default-format
        ;; "%65ITEM(Task) %DEADLINE(Due Date) %PRIORITY %6CLOCKSUM(Spent) %6Effort(Estim.){:}")
        ;; "%1BLOCKED %4TODO %CATEGORY %5Effort{:} %50ITEM %20TAGS %21ALLTAGS")
        ;; "%65ITEM(Task) %4TODO %PRIORITY %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
        ;; "%65ITEM(Task) %4TODO %PRIORITY %20TAGS %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
        ;; "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
        "%60ITEM(Details) %5PRIORITY(Prio) %14SCHEDULED(Scheduled) %15TAGS(Context) %7TODO(To Do) %6CLOCKSUM(Clock) %5Effort(Effort){:} ")

  ;; DUPLICATE Obey `eval' variables -- RISKY!
  (setq enable-local-eval t)

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (remove-text-properties (point-min) (point-max)
                                      '(mouse-face t))))

  (with-eval-after-load "org-agenda"

    (defun leuven-org-agenda-mark-done-and-add-followup ()
      "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
      (interactive)
      (org-agenda-todo "DONE")
      (org-agenda-switch-to)
      (org-capture 0 "t"))

    (define-key org-agenda-mode-map
      "Z" 'leuven-org-agenda-mark-done-and-add-followup)

    (defun leuven-org-agenda-new ()
      "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
      (interactive)
      (org-agenda-switch-to)
      (org-capture 0))

    ;; ;; new key assignment (overrides `org-agenda-next-item')
    ;; (define-key org-agenda-mode-map "N" 'leuven-org-agenda-new)
  )

;;* 11 (info "(org)Markup")

  (leuven--section "11 (org)Markup")

  (with-eval-after-load "org-faces"

    ;; add a face to #+begin_quote and #+begin_verse blocks
    (setq org-fontify-quote-and-verse-blocks t))

  (with-eval-after-load "org"
    (message "... Org Markup")

    ;;??? change the face of a headline (as an additional information) if it is
    ;; marked DONE (to face `org-headline-done')
    (setq org-fontify-done-headline t)

    ;; 11.1 hide the emphasis marker characters
    (setq org-hide-emphasis-markers t)  ; impact on table alignment!

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
              (message "Taking screenshot into %s" name)
              (call-process "import" nil nil nil name)
              (message "Taking screenshot...done"))
          (error "Cannot create image file")))
      (insert (concat "[[" name "]]"))
      (org-display-inline-images))

    ;; hide the brackets marking macro calls
    (setq org-hide-macro-markers t)

    (defun org-macro-insert ()
      (interactive)
      (let* ((macros (org-macro--collect-macros))
             (macro (completing-read "Insert macro: " (mapcar 'car macros)))
             (args (string-match "$[[:digit:]]" (cdr (assoc macro macros))))
             pos)
        (insert (format  "{{{%s" macro))
        (when args (insert "(") (setq pos (point)) (insert ")"))
        (insert "}}}")
        (when pos (goto-char pos)))))

  ;; 11.7.1 define user entities to produce special characters
  (with-eval-after-load "org-entities"

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

  ;; 11.7.2 interpret "_" and "^" for display when braces are used
  (setq org-use-sub-superscripts '{})

  ;; ;; 11.7.3 convert LaTeX fragments to images when exporting to HTML (using MathJax)
  ;; (setq org-export-with-latex t)

  ;; highlight LaTeX and related syntax
  (setq org-highlight-latex-and-related '(latex script entities))

  ;; show entities as UTF8 characters
  (setq org-pretty-entities t)

  ;; ;; pretty entity display doesn't include formatting sub/superscripts
  ;; (setq org-pretty-entities-include-sub-superscripts nil)

;;* 12 (info "(org)Exporting")

  ;; bind the exporter dispatcher to a key sequence
  (with-eval-after-load "org"
    (message "... Org Exporting")

    ;; libraries in this list will be loaded once the export framework is needed
    (setq org-export-backends '(ascii html icalendar latex odt))

    ;; (define-key org-mode-map (kbd "C-c C-e") 'org-export-dispatch)

    ;; XXX temporary (until Org 8 is bundled within Emacs)
    (define-key org-mode-map
      (kbd "C-c C-e")
      (lambda (&optional arg)
        (interactive "P")
        (if (fboundp 'org-export-dispatch)
            (funcall 'org-export-dispatch arg)
          (message (concat "This version of Org mode is no longer supported.  "
                           "Please upgrade to 8 or later"))
          (sit-for 1.5)))))

  (with-eval-after-load "org"

    (defun org-save-buffer-and-do-related ()
      "Save buffer, execute/tangle code blocks, and export to HTML/PDF."
      (interactive)
      (let* ((orgfile (buffer-file-name))
             (base-name (file-name-base orgfile))
             (htmlfile (concat base-name ".html"))
             (pdffile (concat base-name ".pdf")))
        (save-buffer)                     ; see other commands in
                                          ; `before-save-hook':
                                          ; `org-update-all-dblocks'
                                          ; `org-table-iterate-buffer-tables'
        (when (derived-mode-p 'org-mode)
          ;; (org-babel-execute-buffer)   ; XXX why should we execute all code blocks?
          (let ((before-save-hook nil))
            (save-buffer))
          (org-babel-tangle)
          (when (file-exists-p htmlfile)
            (if (file-newer-than-file-p orgfile htmlfile)
                (org-html-export-to-html)
              (message "HTML is up to date with Org file")))
          (when (file-exists-p pdffile)
            (if (file-newer-than-file-p orgfile pdffile)
                (if (string-match "^#\\+BEAMER_THEME: " (buffer-string))
                    (org-beamer-export-to-pdf)
                  (org-latex-export-to-pdf))
              (message "PDF is up to date with Org file")))
          (beep))))

    (define-key org-mode-map (kbd "<f9>") 'org-save-buffer-and-do-related))

;;** 12.2 (info "(org)Export options")

  (leuven--section "12.2 (org)Export options")

  ;; Org generic export engine
  (with-eval-after-load "ox"

    ;; 12.3 don't insert a time stamp into the exported file
    (setq org-export-time-stamp-file nil)

    ;; 13.1.5 export all drawers (including properties)
    ;; (setq org-export-with-drawers t)

    ;; default language of HTML export (see `org-export-language-setup' XXX)
    (setq org-export-default-language "en")

    ;; include priority cookies in export
    (setq org-export-with-priority t)

    ;; activate smart quotes during export (convert " to \og, \fg in French)
    (setq org-export-with-smart-quotes t) ; curly quotes in HTML

    ;; interpret "_" and "^" for export when braces are used
    (setq org-export-with-sub-superscripts '{})

    ;; allow #+BIND to define local variable values for export
    (setq org-export-allow-bind-keywords t)

    ;; ;; exported stuff will not be pushed onto the kill ring
    ;; (setq org-export-copy-to-kill-ring nil) ; new default since 2014-04-17

    ;; ;; export and publishing commands will run in background
    ;; (setq org-export-in-background t)

    ;; ;; use a non-intrusive export dispatcher
    ;; (setq org-export-dispatch-use-expert-ui t)

    ;; export snippet translations
    (add-to-list 'org-export-snippet-translation-alist
                 '("l" . "latex"))
    (add-to-list 'org-export-snippet-translation-alist
                 '("b" . "beamer"))

    )                                   ; with-eval-after-load "ox" ends here

  ;; execute buffer when exporting it (see some thread with Eric Schulte,
  ;; end of December 2010)
  ;;;;;;;;;; (add-hook 'org-export-first-hook 'org-babel-execute-buffer)

;;** 12.5 (info "(org)HTML export")

  ;; Org HTML export engine
  (with-eval-after-load "ox-html"

    (setq org-html-checkbox-type 'unicode)

    ;; output type to be used by htmlize when formatting code snippets
    (setq org-html-htmlize-output-type 'css)

    ;; ;; URL pointing to a CSS file defining text colors for htmlized Emacs
    ;; ;; buffers
    ;; (setq org-org-htmlized-css-url "style.css")

    ;; XML declaration
    (setq org-html-xml-declaration
          '(("html" . "<!-- <xml version=\"1.0\" encoding=\"%s\"> -->")
            ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
            ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

    ;; coding system for HTML export
    (setq org-html-coding-system 'utf-8)

    ;; format for the HTML postamble
    (setq org-html-postamble
          "  <div id=\"footer\"><div id=\"copyright\">\n    Copyright &copy; %d %a\n  </div></div>")

    ;; 13.1.5 don't include the JavaScript snippets in exported HTML files
    (setq org-html-head-include-scripts nil)

    ;; 12.5.9 turn inclusion of the default CSS style off
    (setq org-html-head-include-default-style nil)

    ;; check that `tidy' is in PATH, and that configuration file exists
    (when (and (executable-find "tidy")
               (file-exists-p "~/.tidyrc")) ; tidy-config

      (defun leuven--export-html-final-filter (contents backend info)
        (if (not (eq backend 'html)) contents
          (message "Tidy'fying...")
          (let* ((new-contents
                  (with-temp-buffer
                    (insert contents)
                    (shell-command-on-region (point-min) (point-max)
                                             "tidy -config ~/.tidyrc"
                                             t t "*Tidy errors*")
                    (buffer-string))))
            (message "Tidy'fying... Done")
            new-contents)))

      (add-to-list 'org-export-filter-final-output-functions
                   'leuven--export-html-final-filter))

      ;; HTML checkbox output
      (defun leuven--checkbox-filter (item backend info)
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

    )                                   ; with-eval-after-load "ox-html" ends here

;;** (info "(emacs-goodies-el)htmlize")

  (leuven--section "(emacs-goodies-el)htmlize")

  ;; HTML-ize font-lock buffers
  (autoload 'htmlize-buffer "htmlize"
    "Convert BUFFER to HTML, preserving colors and decorations." t)
  (autoload 'htmlize-region "htmlize"
    "Convert the region to HTML, preserving colors and decorations." t)
  (autoload 'htmlize-file "htmlize"
    "Load FILE, fontify it, convert it to HTML, and save the result." t)

  (with-eval-after-load "htmlize"

    ;; output type of generated HTML
    (setq htmlize-output-type 'css)

    ;; override output type `inline-css' used for htmlizing a region
    (defun htmlize-region-for-paste (beg end)
      "Htmlize the region and return just the HTML as a string.
    This forces the `css' style and only returns the HTML body, but without the
    BODY tag.  This should make it useful for inserting the text to another HTML
    buffer."
      (let* ((htmlize-output-type 'css)  ; was `inline-css'
             (htmlbuf (htmlize-region beg end)))
        (unwind-protect
            (with-current-buffer htmlbuf
              (buffer-substring
               (plist-get htmlize-buffer-places 'content-start)
               (plist-get htmlize-buffer-places 'content-end)))
          (kill-buffer htmlbuf))))

    ;; charset declared by the resulting HTML documents
    (setq htmlize-html-charset "utf-8")

    ;; non-ASCII characters (codes in the 128-255 range) are copied to
    ;; HTML without modification -- if your HTML is in Unicode
    (setq htmlize-convert-nonascii-to-entities nil)

    ;; key binding
    (global-set-key (kbd "M-P") 'htmlize-buffer)

    )                                   ; with-eval-after-load "htmlize" ends here

  ;; quick print preview (to Web browser) with `htmlize-view-buffer'
  (GNUEmacs
    (autoload 'htmlize-view-buffer "htmlize-view"
      "Convert buffer to html preserving faces and view in web browser." t)

    ;; same key binding as Org export to HTML (open in browser)
    (global-set-key (kbd "C-c C-e h o") 'htmlize-view-buffer)

    ;; view current buffer as html in web browser
    (with-eval-after-load "htmlize-view"

      ;; add "Quick Print" entry to file menu
      (htmlize-view-add-to-files-menu)))

;;** 12.6 (info "(org)LaTeX and PDF export")

  (leuven--section "12.6 (org)LaTeX and PDF export")

  ;; LaTeX back-end
  (with-eval-after-load "ox-latex"

    ;; markup for TODO keywords and for tags, as a printf format
    (defun org-latex-format-headline (todo todo-type priority text tags info)
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

    ;; function for formatting the headline's text
    (setq org-latex-format-headline-function
          'org-latex-format-headline)

    ;; default width for images
    (setq org-latex-image-default-width ".75\\linewidth")

    ;; format string for links with unknown path type
    (setq org-latex-link-with-unknown-path-format "\\colorbox{red}{%s}")

    (defun leuven--change-pdflatex-program (backend)
      "Automatically run XeLaTeX, if asked, when exporting to LaTeX."

      ;; default (in Windows binary)
      (setq org-latex-pdf-process
            (if (executable-find "latexmk")
                '("latexmk -CF -pdf %f && latexmk -c")
                                        ; must clean .fdb_latexmk, .fls, .ilg,
                                        ; .ind, etc.
              '("pdflatex -interaction=nonstopmode -output-directory=%o %f"
                "pdflatex -interaction=nonstopmode -output-directory=%o %f"
                "pdflatex -interaction=nonstopmode -output-directory=%o %f")))

      (when (string-match "^#\\+LATEX_CMD: xelatex" (buffer-string))
        (setq org-latex-pdf-process
              (if (executable-find "latexmk")
                  '("latexmk -CF -pdf -pdflatex=xelatex %f && latexmk -c")
                '("xelatex -interaction=nonstopmode -output-directory=%o %f"
                  "xelatex -interaction=nonstopmode -output-directory=%o %f"
                  "xelatex -interaction=nonstopmode -output-directory=%o %f")))))

    ;; hook run before parsing an export buffer
    (add-hook 'org-export-before-parsing-hook 'leuven--change-pdflatex-program)

    ;; export source code using `listings' (instead of `verbatim')
    (setq org-latex-listings t)

    ;; 12.6.2 default packages to be inserted in the header
    ;; include the `listings' package for fontified source code
    (add-to-list 'org-latex-packages-alist '("" "listings") t)

    ;; include the `xcolor' package for colored source code
    (add-to-list 'org-latex-packages-alist '("" "xcolor") t)

    ;; filter for non-breaking spaces
    (defun leuven--latex-filter-nbsp (text backend info)
      "Convert non-breaking spaces when exporting to LaTeX/Beamer."
      (when (memq backend '(latex beamer))
        (replace-regexp-in-string " " "~" text)))

    (add-to-list 'org-export-filter-plain-text-functions
                 'leuven--latex-filter-nbsp)

    ;; include the `babel' package for language-specific hyphenation and
    ;; typography
    (add-to-list 'org-latex-packages-alist '("french" "babel") t)

    (defun leuven--change-pdflatex-packages (backend)
      "Automatically select the LaTeX packages to include (depending on PDFLaTeX
    vs. XeLaTeX) when exporting When exporting to LaTeX."

      ;; unconditionally remove `inputenc' from all the default packages
      (setq org-latex-packages-alist
            (delete '("AUTO" "inputenc" t)
                    org-latex-packages-alist))

      ;; unconditionally remove `fontenc' from all the default packages
      (setq org-latex-packages-alist
            (delete '("T1" "fontenc" t)
                    org-latex-packages-alist))

      ;; unconditionally remove `textcomp' from all the default packages
      (setq org-latex-packages-alist
            (delete '("" "textcomp" t)
                    org-latex-packages-alist))

      (if (string-match "^#\\+LATEX_CMD: xelatex" (buffer-string))
          ;; packages to include when XeLaTeX is used
          (setq org-export-latex-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ;; add here things like `\setmainfont{Georgia}'
                  ))

        ;; packages to include when PDFLaTeX is used
        (setq org-export-latex-packages-alist
              '(("AUTO" "inputenc" t)
                ("T1" "fontenc" t)
                ("" "textcomp" t))))

      ;; packages to always include
      (add-to-list 'org-export-latex-packages-alist
                   '("frenchb" "babel") t))

    ;; hook run before parsing an export buffer
    (add-hook 'org-export-before-parsing-hook 'leuven--change-pdflatex-packages)

    ;; 12.6.5 default position for LaTeX figures
    (setq org-latex-default-figure-position "!htbp")

    (defun org-export-ignore-headlines (data backend info)
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
                      ;; recursively promote all nested headlines
                      (org-element-map el 'headline
                        (lambda (el)
                          (when (equal 'headline (org-element-type el))
                            (unless level-diff
                              (setq level-diff (- (org-element-property :level el)
                                                  level-top)))
                            (org-element-put-property el
                              :level (- (org-element-property :level el)
                                        level-diff)))))
                      ;; insert back into parse tree
                      (org-element-insert-before el object))
                    (org-element-contents object)))
            (org-element-extract-element object)))
        info nil)
      data)

    (add-hook 'org-export-filter-parse-tree-functions
              'org-export-ignore-headlines)

    )                                   ; with-eval-after-load "ox-latex" ends here

  ;; 12.6.6 Beamer class export
  ;; (require 'ox-beamer)
  (with-eval-after-load "ox-beamer"

    ;; default title of a frame containing an outline
    (setq org-beamer-outline-frame-title "Plan")) ; [default: "Outline"]

  (with-eval-after-load "ox-odt"

    ;; convert "odt" format to "doc" format
    (setq org-odt-preferred-output-format "doc"))

;;* 13 (info "(org)Publishing")

  (leuven--section "13 (org)Publishing")

  (with-eval-after-load "ox-publish"

    ;; show message about files *not* published
    (setq org-publish-list-skipped-files nil)

    ;; ;; 13.2 always publish all files
    ;; ;; (do not use time stamp checking for skipping unmodified files)
    ;; (setq org-publish-use-timestamps-flag nil)

    ;; 13.4 force publishing all files
    (defun org-publish-all-force ()
      (interactive)
      (org-publish-all t)))

;;* 14 (info "(org)Working With Source Code")

  (with-eval-after-load "ob-core"

    ;; make the images in the Emacs buffer automatically refresh after
    ;; execution
    (add-hook 'org-babel-after-execute-hook
              (lambda ()
                (org-display-inline-images nil t)))
                                        ; more efficient with refresh == t

    (defadvice org-babel-next-src-block
      (after leuven-org-babel-next-src-block activate)
      "Recenter after jumping to the next source block."
      (recenter))

    (defadvice org-babel-previous-src-block
      (after leuven-org-babel-previous-src-block activate)
      "Recenter after jumping to the previous source block."
      (recenter))
    )

    (defadvice org-agenda-switch-to
      (after leuven-org-agenda-switch-to activate)
      "Recenter after jumping to the file which contains the item at point."
      (recenter))

  ;; (with-eval-after-load "ob-shell"
  ;;
  ;;   ;; command used to invoke a shell
  ;;   (setq org-babel-sh-command "bash")  ; now uses `shell-file-name' throughout (2013-12-14)
  ;;
  ;;   ;; use plain old syntax (instead of `$(...)') for Cygwin
  ;;   (setq org-babel-sh-var-quote-fmt
  ;;         "`cat <<'BABEL_TABLE'\n%s\nBABEL_TABLE\n`"))

;;** 14.2 (info "(org)Editing source code")

  (leuven--section "14.2 (org)Editing source code")

  (with-eval-after-load "org-src"

    ;; mapping languages to their major mode (for editing the source code block
    ;; with `C-c '')
    (add-to-list 'org-src-lang-modes
                 '("dot" . graphviz-dot)))

  ;; display the source code edit buffer in the current window, keeping
  ;; all other windows
  (setq org-src-window-setup 'current-window)

  ;; FIXME Bind this to the correct keys
  (defun leuven-org-babel-expand-src-block ()
    (interactive)
    (let ((org-src-window-setup 'reorganize-frame))
      (org-babel-expand-src-block)))

  ;; indent the content of a source code block
  (setq org-edit-src-content-indentation 2)

  ;; fontify code in code blocks (highlight syntax in the Org buffer)
  (setq org-src-fontify-natively t)     ;! create overlay
                                        ;! `org-block-background' and remove
                                        ;! text property `org-block'

  ;; preserve spaces and `tab' characters in source code blocks
  (setq org-src-preserve-indentation t) ; or add a `-i' flag to you source block

  ;; same effect for `tab' as in the language major mode buffer
  (setq org-src-tab-acts-natively t)


  ;; (with-eval-after-load "org"
  ;;   (message "... Org Editing source code")
  ;;
  ;;   ;; allow indent region in the code edit buffer (according to language)
  ;;   (defun leuven-org-indent-region (&optional arg)
  ;;     (interactive "P")
  ;;     (or (org-babel-do-key-sequence-in-edit-buffer (kbd "C-M-\\"))
  ;;         (indent-region arg)))
  ;;
  ;;   ;; make `C-c C-v C-x C-M-\' more convenient
  ;;   (define-key org-mode-map (kbd "C-M-\\") 'leuven-org-indent-region))

  ;; prevent auto-filling in src blocks
  (setq org-src-prevent-auto-filling t)

  (global-set-key (kbd "C-c C-v C-d") 'org-babel-demarcate-block)

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

;;** 14.5 (info "(org)Evaluating code blocks")

  (leuven--section "14.5 (org)Evaluating code blocks")

  ;; I don't want to execute code blocks with `C-c C-c' (evaluate code
  ;; block only with `C-c C-v e')
  (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

  ;; languages for which Org-babel will raise literate programming errors when
  ;; noweb references can not be resolved.

  (with-eval-after-load "ob-core"
    (add-to-list 'org-babel-noweb-error-langs "emacs-lisp"))

;;** 14.6 (info "(org)Library of Babel")

  (leuven--section "14.6 (org)Library of Babel")

  (with-eval-after-load "org"

    ;; load the NAMED code blocks defined in Org mode files into the
    ;; library of Babel (global `org-babel-library-of-babel' variable)
    (let ((lob-file (concat (file-name-directory (locate-library "org"))
                            "../doc/library-of-babel.org")))
      (when (file-exists-p lob-file)
        (org-babel-lob-ingest lob-file))))

  (with-eval-after-load "ob-exp"
    ;; template used to export the body of code blocks
    (setq org-babel-exp-code-template
          ;; (concat "\n=%name=:\n"
                  org-babel-exp-code-template)
          ;; )
    )

  ;; keep lower-case
  (setq org-babel-results-keyword "results")

;;** 14.7 (info "(org)Languages")

  (leuven--section "14.7 (org)Languages")

  ;; FIXME Test executable-find (of Rterm, gnuplot, ruby, etc.) before
  ;; setting language to yes...

  (with-eval-after-load "org"
    (message "... Org Languages")

    ;; configure Babel to support most languages
    (if (locate-library "ob-shell")     ; ob-sh renamed on Dec 13th, 2013
        (org-babel-do-load-languages    ; loads org, gnus-sum, etc...
         'org-babel-load-languages
         '((R          . t)             ; requires R and ess-mode
           (awk        . t)
           (ditaa      . t)             ; sudo aptitude install openjdk-6-jre
           (dot        . t)
           (emacs-lisp . t)
           ;; (latex   . t)             ; shouldn't you use #+begin/end_latex blocks instead?
           (ledger     . t)             ; requires ledger
           (org        . t)
           (shell      . t)
           (sql        . t)))
      ;; XXX (in the future) message saying "Upgrade to Org 8.3"
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((R          . t)
         (awk        . t)
         (ditaa      . t)
         (dot        . t)
         (emacs-lisp . t)
         ;; (latex   . t)
         (ledger     . t)
         (org        . t)
         (sh         . t)
         (sql        . t))))

    ;; accented characters on graphics
    (setq org-babel-R-command
          (concat org-babel-R-command " --encoding=UTF-8")))

;;* 15 (info "(org)Miscellaneous")

  ;; from Dan Davison
  (defun leuven-switch-to-org-scratch ()
    "Switch to a temp Org buffer.  If the region is active, insert it."
    (interactive)
    (let ((contents (and (region-active-p)
                         (buffer-substring (region-beginning)
                                           (region-end)))))
      (find-file "/tmp/org-scratch.org")
      (if contents (insert contents))))

  (defun leuven--org-switch-dictionary ()
    "Set language if Flyspell is enabled and `#+LANGUAGE:' is on top 8 lines."
    (when (and (boundp 'ispell-dictionary-alist)
               ispell-dictionary-alist)
      (save-excursion
        (goto-char (point-min))
        (forward-line 8)
        (let (lang dict
              (dict-alist '(("en" . "american")
                            ("fr" . "francais"))))
          (when (re-search-backward "#\\+LANGUAGE: +\\([[:alpha:]_]*\\)" 1 t)
            (setq lang (match-string 1))
            (setq dict (cdr (assoc lang dict-alist)))
            (if dict
                (progn
                  (ispell-change-dictionary dict)
                  (force-mode-line-update))
              (message "No Ispell dictionary for language `%s' (see file `%s')"
                       lang (file-name-base))
              (sit-for 1.5)))))))

  ;; guess dictionary
  (add-hook 'org-mode-hook 'leuven--org-switch-dictionary)

;;** 15.2 (info "(org)Easy Templates")

  (leuven--section "15.2 (org)Easy Templates")

  (with-eval-after-load "org"
    (message "... Org Easy Templates")

    ;; modify `org-structure-template-alist' to keep lower-case easy templates
    (mapc (lambda (asc)
            (let ((org-sce-dc (downcase (nth 1 asc))))
              (setf (nth 1 asc) org-sce-dc)))
          org-structure-template-alist)

    (add-to-list 'org-structure-template-alist
                 '("E" "\\begin\{equation\}\n?\n\\end\{equation\}" ""))

    (add-to-list 'org-structure-template-alist
                 '("C" "#+begin_comment\n?\n#+end_comment")))

;;** 15.3 (info "(org)Speed keys")

  (leuven--section "15.3 (org)Speed keys")

  (with-eval-after-load "org"
    (message "... Org Speek keys")

    ;; activate single letter commands at beginning of a headline
    (setq org-use-speed-commands t)

    (add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))
    (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))

    (setq w32-pass-apps-to-system nil)
    (setq w32-apps-modifier 'hyper) ; Apps key

    (define-key org-mode-map (kbd "H-e") 'org-babel-execute-maybe)
    (define-key org-mode-map (kbd "H-t") 'org-babel-tangle)

    )

;;** 15.4 (info "(org)Code evaluation security") issues

  (leuven--section "15.4 (org)Code evaluation security issues")

  (with-eval-after-load "ob-core"

    ;;!! don't be prompted on every code block evaluation
    (setq org-confirm-babel-evaluate nil))

  ;; change the color of code blocks while they are being executed
  (defadvice org-babel-execute-src-block (around progress nil activate)
    "Create an overlay indicating when code block is running."
    (let ((ol (make-overlay (org-element-property :begin (org-element-at-point))
                            (org-element-property :end (org-element-at-point)))))
      (overlay-put ol 'face '(background-color . "thistle1"))
      ad-do-it
      (delete-overlay ol)))

;;** 15.8 A (info "(org)Clean view")

  (with-eval-after-load "org"
    (message "... Org Clean view")

    ;; 15.8 don't skip even levels for the outline
    (setq org-odd-levels-only nil))

;;** 15.10 (info "(org)Interaction")

  (leuven--section "15.10 (org)Interaction")

  (with-eval-after-load "org"

    ;; support shift-selection for making and enlarging regions when the cursor
    ;; is not in a special context
    (setq org-support-shift-select t)

    ;; maximum level for Imenu access to Org-mode headlines
    (setq org-imenu-depth 3)

    ;; extension of Imenu
    (when (and (fboundp 'org-babel-execute-src-block) ; `org-babel' has been
                                                      ; loaded
               (fboundp 'try-to-add-imenu)) ; `imenu' has been loaded

      (setq org-src-blocks-imenu-generic-expression
            `(("Snippets" ,org-babel-src-name-w-name-regexp 2)))

      (add-hook 'org-mode-hook
                (lambda ()
                  (setq imenu-generic-expression
                        org-src-blocks-imenu-generic-expression))))

    ;; alternative to imenu
    (defun dan/find-in-buffer ()
      (interactive)
      (let ((targets
             `(("<named src blocks>" . ,org-babel-src-name-regexp)
               ("<src block results>" . ,org-babel-result-regexp))))
        (occur
         (cdr
          (assoc
           (completing-read "Find: " (mapcar #'car targets)) targets)))
        (other-window 1))))

  ;; keep my encrypted data (like account passwords) in my Org mode
  ;; files with a special tag instead
  (with-eval-after-load "org"
    (message "... Org Crypt")

    (try-require 'org-crypt))           ; loads org, gnus-sum, etc...

  (with-eval-after-load "org-crypt"

    ;; encrypt all entries before saving
    (org-crypt-use-before-save-magic)

    ;; which tag is used to mark headings to be encrypted
    (setq org-tags-exclude-from-inheritance '("crypt")))

  (defun leuven-scramble-contents ()
    (interactive)
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree '(code comment comment-block example-block fixed-width
                                   keyword link node-property plain-text verbatim)
        (lambda (obj)
          (case (org-element-type obj)
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

  ;; don't pad tangled code with newlines
  (setq org-babel-tangle-pad-newline nil)

  ;; how to combine blocks of the same name during tangling
  (setq org-babel-tangle-named-block-combination 'append)

  ;; speed up tangling dramatically (a couple of orders of magnitude)
  (setq org-babel-use-quick-and-dirty-noweb-expansion t)
                                        ; :noweb-ref feature must NOT be used!

  ;; minimum number of lines for output *block* (placed in a
  ;; #+begin_example...#+end_example) vs. output marked as literal by
  ;; inserting a *colon* at the beginning of the lines
  (setq org-babel-min-lines-for-block-output 2)

  ;; ;; FIXME Make this the default behavior
  ;; ;; grab the last line too, when selecting a subtree
  ;; (org-end-of-subtree nil t)

  ;; backend aware export preprocess hook
  (defun leuven--org-export-preprocess-hook ()
    "Backend-aware export preprocess hook."
    (save-excursion
      (when (eq org-export-current-backend 'latex)
        ;; ignoreheading tag for bibliographies and appendices
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

  (add-hook 'org-export-preprocess-hook 'leuven--org-export-preprocess-hook)

  (defun insert-one-equal-or-two ()
    (interactive)
    (cond
     ((or (bolp) (not (looking-back "=")))
      ;; insert just one =
      (self-insert-command 1))
     ((save-excursion
        (backward-char)
        ;; Skip symbol backwards.
        (and (not (zerop (skip-syntax-backward "w_")))
             (not (looking-back "="))
             (or (insert-and-inherit "=") t))))
     (t
      ;; insert == around following symbol
      (delete-char -1)
      (unless (looking-back "=") (insert-and-inherit "="))
      (save-excursion
        (skip-syntax-forward "w_")
        (unless (looking-at "=") (insert-and-inherit "="))))))

  ;; must be in eval-after-load "org"?
  ;; (define-key org-mode-map (kbd "=") 'insert-one-equal-or-two)

  (with-eval-after-load "org"
    (message "... Org Mime")

    ;; using Org mode to send buffer/subtree per mail
    (try-require 'org-mime))

  (with-eval-after-load "org-mime"

    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key
                  (kbd "C-c m") 'org-mime-subtree)))

    (defun leuven-mail-subtree ()
      (interactive)
      (org-agenda-goto)
      (org-mime-subtree))

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (local-set-key
                  (kbd "C-c m") 'leuven-mail-subtree)))

    ;; add a `mail_composed' property with the current time when
    ;; `org-mime-subtree' is called
    (add-hook 'org-mime-send-subtree-hook
              (lambda ()
                (org-entry-put (point) "mail_composed"
                               (current-time-string)))))

;;** A.3 (info "(org)Adding hyperlink types")

  (with-eval-after-load "org"
    (message "... Org Adding hyperlink types")

    ;; define a new link type (`latex') whose path argument can hold the name of
    ;; any LaTeX command
    (org-add-link-type
     "latex" nil
     (lambda (path desc format)
       (cond
        ((eq format 'html)
         (format "<span class=\"%s\">%s</span>" path desc))
        ((eq format 'latex)
         (format "\\%s{%s}" path desc)))))

    ;; add background color by using custom links like [[bgcolor:red][Warning!]]
    (org-add-link-type
      "bgcolor" nil
      (lambda (path desc format)
       (cond
        ((eq format 'html)
         (format "<span style=\"background-color:%s;\">%s</span>" path desc))
        ((eq format 'latex)
         (format "\\colorbox{%s}{%s}" path desc))
        (t
         (format "BGCOLOR LINK (%s): {%s}{%s}" format path desc))))))

  (defun leuven-org-send-all-buffer-tables ()
    "Export all Org tables of the LaTeX document to their corresponding LaTeX tables."
     (interactive)
     (org-table-map-tables
        (lambda () (orgtbl-send-table 'maybe))))

;;** A.6 (info "(org)Dynamic blocks")

  (with-eval-after-load "org"
    (message "... Org Update dynamic blocks and tables")

    (defun leuven-org-update-buffer ()
      "Update all dynamic blocks and all tables in the buffer."
      (interactive)
      (when (eq major-mode 'org-mode)
        (message "(Info) Update Org buffer %s"
                 (file-name-nondirectory (buffer-file-name)))
        (sit-for 1.5)
        (let ((flyspell-mode-before-save flyspell-mode))
          (flyspell-mode -1)              ; temporarily disable Flyspell to
                                          ; avoid checking the following
                                          ; modifications of the buffer
          (org-align-all-tags)
          (org-update-all-dblocks)
          (org-table-iterate-buffer-tables)
          (when (file-exists-p (buffer-file-name (current-buffer)))
            (leuven-org-remove-redundant-tags))
          (when flyspell-mode-before-save (flyspell-mode 1)))))

    ;; make sure that all dynamic blocks and all tables are always
    ;; up-to-date
    (add-hook 'before-save-hook 'leuven-org-update-buffer)
    (message "Add leuven-org-update-buffer to before-save-hook  <<<<<<<<<<<<<<"))

  ;; (with-eval-after-load "org"
  ;;   (message "... Org Effectiveness")
  ;;
  ;;   (try-require 'org-effectiveness)
  ;;   (with-eval-after-load "org-effectiveness"
  ;;
  ;;     (add-hook 'org-mode-hook
  ;;               (lambda ()
  ;;                 (org-effectiveness-count-todo)
  ;;                 (sit-for 0.2)))))

  (GNUEmacs
    ;; add weather forecast in your Org agenda
    (autoload 'org-google-weather "org-google-weather"
      "Return Org entry with the weather for LOCATION in LANGUAGE." t)

    (with-eval-after-load "org-google-weather"
      ;; (try-require 'url)

      ;; add the city
      (setq org-google-weather-format "%C %i %c, %l°-%h°")))

)                                       ; chapter 25.9-org-mode ends here

;;** 25.10 (info "(emacs)TeX Mode")

(leuven--chapter leuven-chapter-25.10-tex-mode "25.10 TeX Mode"

  (leuven--section "25.10 (emacs)TeX Mode")

  ;; get colored PDFLaTeX output
  (define-derived-mode latex-output-mode fundamental-mode "LaTeX-Output"
    "Simple mode for colorizing LaTeX output."
    (set (make-local-variable 'font-lock-defaults)
         '((("^!.*" .
             compilation-error-face)    ; LaTeX error
            ("^-+$" .
             compilation-info-face)     ; Latexmk separator
            ("^Package .* Warning: .*" .
             compilation-warning-face)
            ("Reference .* undefined" .
             compilation-warning-face)
            ("^\\(?:Overfull\\|Underfull\\|Tight\\|Loose\\).*" .
             font-lock-string-face)
            ("^LaTeX Font Warning:" .
             font-lock-string-face)
            ;; ...
            ))))

  (defadvice TeX-recenter-output-buffer
    (after leuven-colorize-latex-output activate)
    (with-selected-window (get-buffer-window (TeX-active-buffer))
      (latex-output-mode)))

  (leuven--section "25.10 (emacs)AUCTeX Mode")

;;** 1.2 (info "(auctex)Installation") of AUCTeX

  (try-require 'tex-site)

  ;; support for LaTeX documents
  (GNUEmacs
    (with-eval-after-load "latex"

      ;; ;; LaTeX-sensitive spell checking
      ;; (add-hook 'tex-mode-hook
      ;;           (lambda ()
      ;;             (make-local-variable 'ispell-parser)
      ;;             (setq ispell-parser 'tex)))

;;** 2.6 (info "(auctex)Completion")

      (leuven--section "2.6 (auctex)Completion")

      ;; if this is non-nil when AUCTeX is loaded, the TeX escape
      ;; character `\' will be bound to `TeX-electric-macro'
      (setq TeX-electric-escape t)

;;** 2.8 (info "(auctex)Indenting")

      (leuven--section "2.8 (auctex)Indenting")

      ;; leave the `tikzpicture' code unfilled when doing `M-q'
      (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))

      ;; auto-indentation (suggested by the AUCTeX manual -- instead of
      ;; adding a local key binding to `<RET>' in the `LaTeX-mode-hook')
      (setq TeX-newline-function 'newline-and-indent)

;;* 3 Controlling Screen (info "(auctex)Display")

;;** 3.1 (info "(auctex)Font Locking")

      (leuven--section "3.1 (auctex)Font Locking")

      ;; (for Org mode) add the `comment' environment to the variable
      ;; `LaTeX-verbatim-environments' so that, if the `#+TBLFM' line
      ;; contains an odd number of dollar characters, this does not
      ;; cause problems with font-lock in LaTeX-mode
      (add-to-list 'LaTeX-verbatim-environments "comment")

;;** 4.1 Executing (info "(auctex)Commands")

      (leuven--section "4.1 Executing (auctex)Commands")

      ;; add a command to execute on the LaTeX document
      (add-to-list 'TeX-command-list
                   '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))

      (defun leuven--LaTeX-mode-hook ()
        ;; default command to run in the LaTeX buffer
        (setq TeX-command-default
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (let ((re (concat
                             "^\\s-*\\\\usepackage\\(?:\\[.*\\]\\)?"
                             "{.*\\<\\(?:font\\|math\\)spec\\>.*}")))
                    (save-match-data
                      (if (re-search-forward re 3000 t)
                          "XeLaTeX"
                        "LaTeX")))))))

      (add-hook 'LaTeX-mode-hook 'leuven--LaTeX-mode-hook)

      ;; don't ask user for permission to save files before starting TeX
      (setq TeX-save-query nil)

      (defun TeX-default ()
        "Choose the default command from `C-c C-c'."
        (interactive)
        (TeX-save-document "")          ; or just use `TeX-save-query'
        (execute-kbd-macro (kbd "C-c C-c RET")))

      ;; rebind the "compile command" to default command from `C-c C-c'
      ;; (in LaTeX mode only)
      (define-key LaTeX-mode-map (kbd "<f9>") 'TeX-default)

      ;; use PDF mode by default (instead of DVI)
      (setq-default TeX-PDF-mode t)

;;** 4.2 (info "(auctex)Viewing") the formatted output

      (leuven--section "4.2 (auctex)Viewing the formatted output")

      (defvar sumatrapdf-command
        (concat windows-program-files-dir "SumatraPDF/SumatraPDF.exe")
        "Path to the SumatraPDF executable.")

      ;; use a saner PDF viewer (evince, SumatraPDF)
      (setcdr (assoc "^pdf$" TeX-output-view-style)
              (cond (win32p
                     `("." (concat "\"" ,sumatrapdf-command "\" %o")))
                    ;; under Windows, we could open the PDF file with
                    ;; `start "" xxx.pdf' (in a command prompt)
                    (t
                     '("." "evince %o"))))

      ;; for AUCTeX 11.86+
      (when (boundp 'TeX-view-program-list)
        (add-to-list 'TeX-view-program-list
                     `("SumatraPDF"
                       (concat "\"" ,sumatrapdf-command "\" %o"))))

      (when win32p
        (setcdr (assoc 'output-pdf TeX-view-program-selection)
                '("SumatraPDF")))

;;** 4.7 (info "(auctex)Documentation")

;;** 5.2 (info "(auctex)Multifile") Documents

      ;; ;; assume that the file is a master file itself
      ;; (setq-default TeX-master t)

;;** 5.3 Automatic (info "(auctex)Parsing Files")

      ;; enable parse on load (if no style hook is found for the file)
      (setq TeX-parse-self t)

      ;; enable automatic save of parsed style information when saving
      ;; the buffer
      (setq TeX-auto-save t)

;;** 5.5 (info "(auctex)Automatic") Customization

      ;; TODO Add beamer.el to TeX-style-path

;;*** 5.5.1 (info "(auctex)Automatic Global") Customization for the Site

      (leuven--section "5.5.1 (auctex)Automatic Global Customization for the Site")

      ;; directory containing automatically generated TeX information
      (setq TeX-auto-global
            ;; must end with a slash
            "~/.emacs.d/auctex-auto-generated-info/")

;;*** 5.5.3 (info "(auctex)Automatic Local") Customization for a Directory

      (leuven--section "5.5.3 (auctex)Automatic Local Customization for a Directory")

      ;; directory containing automatically generated TeX information
      (setq TeX-auto-local "~/.emacs.d/auctex-auto-generated-info/")
                                        ; must end with a slash

;;** (info "(preview-latex)Top")

      (leuven--section "(preview-latex)Top")

      (with-eval-after-load "preview"

        ;; path to `gs' command (for format conversions)
        (setq preview-gs-command
          (cond (win32p
                 (or (executable-find "gswin32c.exe")
                     "C:/texlive/2014/tlpkg/tlgs/bin/gswin32c.exe"))
                                        ; default value
                (t
                 "/usr/bin/gs")))
        (leuven--file-exists-and-executable-p preview-gs-command)

        ;; scale factor for included previews
        (setq preview-scale-function 1.2))

      (add-hook 'LaTeX-mode-hook 'reftex-mode) ; with AUCTeX LaTeX mode

      ;; minor mode with distinct support for `\label', `\ref', `\cite'
      ;; and `\index' in LaTeX
      (with-eval-after-load "reftex"

        ;; turn all plug-ins on
        (setq reftex-plug-into-AUCTeX t)

        ;; use a separate selection buffer for each label type -- so the
        ;; menu generally comes up faster
        (setq reftex-use-multiple-selection-buffers t))

      ;; BibTeX mode
      (with-eval-after-load "bibtex"

        ;; current BibTeX dialect
        (setq bibtex-dialect 'biblatex))

      ))                                ; with-eval-after-load "latex" ends here

)                                       ; chapter 25.10-tex-mode ends here

(leuven--chapter leuven-chapter-25-text "25 Commands for Human Languages"

;;** 25.11 (info "(emacs)HTML Mode")

  (leuven--section "25.11 (emacs)HTML Mode")

  (when (and (locate-library "tidy")
             (executable-find "tidy"))

    (autoload 'tidy-buffer "tidy"
      "Run Tidy HTML parser on current buffer." t)
    (autoload 'tidy-parse-config-file "tidy"
      "Parse the `tidy-config-file'." t)
    (autoload 'tidy-save-settings "tidy"
      "Save settings to `tidy-config-file'." t)
    (autoload 'tidy-build-menu  "tidy"
      "Install an options menu for HTML Tidy." t)

    (defun leuven--html-mode-hook ()
      "Customize html(-helper)-mode."

      ;; set up a "tidy" menu in the menu bar
      (when (boundp 'html-mode-map)
        (tidy-build-menu html-mode-map))
      (when (boundp 'html-helper-mode-map)
        (tidy-build-menu html-helper-mode-map))

      ;; bind the key sequence `C-c C-c' to `tidy-buffer'
      (local-set-key
        (kbd "C-c C-c") 'tidy-buffer)

      (setq sgml-validate-command "tidy"))

    ;; also run from `html-helper-mode'
    (add-hook 'html-mode-hook 'leuven--html-mode-hook))

  (when (locate-library "html-helper-mode")

    (autoload 'html-helper-mode "html-helper-mode"
      "Mode for editing HTML documents." t)

    ;; invoke html-helper-mode automatically on .html files
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-helper-mode))

    ;; invoke html-helper-mode automatically on .asp files
    (add-to-list 'auto-mode-alist '("\\.asp\\'" . html-helper-mode))

    ;; invoke html-helper-mode automatically on .jsp files
    (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-helper-mode)))

  (add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . xml-mode))
                                        ; alias for `nxml-mode'

  (with-eval-after-load "nxml-mode"

    ;; remove the binding of `C-c C-x' (`nxml-insert-xml-declaration'), used
    ;; by Org timeclocking commands
    (define-key nxml-mode-map (kbd "C-c C-x") nil)

    ;; view the buffer contents in a browser
    (define-key nxml-mode-map (kbd "C-c C-v") 'browse-url-of-buffer))
                                        ; XXX normally bound to
                                        ; `rng-validate-mode'

  ;; highlight the current SGML tag context
  (try-require 'hl-tags-mode)
  (with-eval-after-load "hl-tags-mode"

    (add-hook 'html-mode-hook
              (lambda ()
                (require 'sgml-mode)
                ;; when `html-mode-hook' is called from `html-helper-mode'
                (hl-tags-mode 1)))

    (add-hook 'nxml-mode-hook 'hl-tags-mode))

)                                       ; chapter 25 ends here

;;* 26 Editing (info "(emacs)Programs")

(leuven--chapter leuven-chapter-26-programs "26 Editing Programs"

;;** 26.1 Major Modes for (info "(emacs)Program Modes")

  (leuven--section "26.1 Major Modes for (emacs)Program Modes")

  (autoload 'graphviz-dot-mode "graphviz-dot-mode"
    "Major mode for the dot language." t)
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;** 26.2 Top-Level Definitions, or (info "(emacs)Defuns")

  (leuven--section "26.2 Top-Level Definitions, or (emacs)Defuns")

  (GNUEmacs
    ;; making buffer indexes as menus
    (try-require 'imenu)                ; awesome!
    (with-eval-after-load "imenu"

      ;; automatically add Imenu to the menu bar in /any/ mode that supports it
      (defun try-to-add-imenu ()
        (condition-case nil
            (imenu-add-to-menubar "Imenu")
          (error nil)))
      (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

      ;; show current function in mode line (based on Imenu)
      (which-function-mode 1)))         ; ~ Stickyfunc mode (in header line)

    ;; (try-require 'imenu+)

;;** 26.3 (info "(emacs)Program Indent")ation

    (leuven--section "26.3 (emacs)Program Indentation")

    ;; turn on auto-fill mode in Lisp modes
    (add-hook 'lisp-mode-hook 'auto-fill-mode)
    (add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)

    ;; auto-indentation: automatically jump to the "correct" column when
    ;; the <RET> key is pressed while editing a program (act as if you
    ;; pressed `C-j')
    (GNUEmacs24
      (add-hook 'prog-mode-hook
                (lambda ()
                  (local-set-key
                    (kbd "<return>") 'newline-and-indent))))

    ;; (defun back-to-indentation-or-beginning ()
    ;;   (interactive)
    ;;   (if (/= (point) (line-beginning-position))
    ;;       (beginning-of-line)
    ;;     (back-to-indentation)))
    ;;
    ;; (defun align-with-spaces (beg end)
    ;;   "Align selected using only spaces for whitespace."
    ;;   (interactive "r")
    ;;   (let ((indent-tabs-mode nil))
    ;;     (align beg end)))

    (with-eval-after-load "sh-script"

      ;; use the SMIE code for navigation and indentation
      (setq sh-use-smie t))

;;** 26.4 Commands for Editing with (info "(emacs)Parentheses")

  (leuven--section "26.4 Commands for Editing with (emacs)Parentheses")

  ;; highlight matching paren
  (GNUEmacs
    (show-paren-mode 1)
    (setq show-paren-style 'mixed)
    (setq show-paren-ring-bell-on-mismatch t))
  (XEmacs
    (paren-set-mode 'paren))

  ;; highlight surrounding parentheses
  (GNUEmacs
    (autoload 'highlight-parentheses-mode "highlight-parentheses"
      "Minor mode to highlight the surrounding parentheses." t)

    ;; (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)

    (with-eval-after-load "highlight-parentheses"

      (define-globalized-minor-mode global-highlight-parentheses-mode
        highlight-parentheses-mode
        (lambda ()
          (highlight-parentheses-mode t)))
      (global-highlight-parentheses-mode t)

      (setq hl-paren-background-colors
            '("#FF993F" "#B0FF3F" "#3FFFB0" "#3F99FF"))

      (setq hl-paren-colors
            '("black" "black" "black" "black"))))

  ;; jump to matching parenthesis
  (defun match-paren (arg)
    "Go to the matching parenthesis, if on a parenthesis."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

  (global-set-key (kbd "C-)") 'match-paren)

  (electric-pair-mode 1)

;;** 26.5 (info "(emacs)Comments")

  (leuven--section "26.5 (emacs)Comments")

  ;; always comments out empty lines
  (setq comment-empty-lines t)

  (GNUEmacs
    (defadvice comment-dwim (around leuven-comment activate)
      "When called interactively with no active region, comment a single line instead."
      (if (or (use-region-p) (not (called-interactively-p 'any)))
          ad-do-it
        (comment-or-uncomment-region (line-beginning-position)
                                     (line-end-position))
        (message "Commented line"))))

;;** 26.6 (info "(emacs)Documentation") Lookup

  (leuven--section "26.6 (emacs)Documentation Lookup")

  ;; idle time to wait before printing documentation
  (setq eldoc-idle-delay 0.2)

  ;; resize echo area to fit documentation
  (setq eldoc-echo-area-use-multiline-p t)

  ;; show the function arglist or the variable docstring in the echo area
  (GNUEmacs
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))

  ;; highlight the arguments in `font-lock-variable-name-face'
  (defun leuven--frob-eldoc-argument-list (string)
    "Upcase and fontify STRING for use with `eldoc-mode'."
    (propertize (upcase string)
                'face 'font-lock-variable-name-face))
  (setq eldoc-argument-case 'leuven--frob-eldoc-argument-list)

;;** 26.7 (info "(emacs)Hideshow") minor mode

  (leuven--section "26.7 (emacs)Hideshow minor mode")

  ;; enable Hideshow (code folding) for programming modes
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  (with-eval-after-load "hideshow"

    ;; change those really awkward key bindings with `@' in the middle
    (define-key hs-minor-mode-map (kbd "<C-M-S-left>") 'hs-hide-block)
                                        ; `C-c @ C-h'
    (define-key hs-minor-mode-map (kbd "<C-M-S-right>") 'hs-show-block)
                                        ; `C-c @ C-s'
    (define-key hs-minor-mode-map (kbd "<C-M-S-up>") 'hs-hide-all)
                                        ; `C-c @ C-M-h'
    (define-key hs-minor-mode-map (kbd "<C-M-S-down>") 'hs-show-all)
                                        ; `C-c @ C-M-s'

    (defcustom hs-face 'hs-face
      "*Specify the face to to use for the hidden region indicator"
      :type 'face
      :group 'hideshow)

    (defface hs-face
      '((t (:box (:line-width 1 :color "#999999")
            :foreground "#999999" :background "#FFF8C0")))
      "Face to hightlight the ... area of hidden regions"
      :group 'hideshow)

    (defun display-code-line-counts (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (overlay-put ov 'display
                     (propertize "..." 'face 'hs-face))))

    (setq hs-set-up-overlay 'display-code-line-counts))

;;** 26.8 (info "(emacs)Symbol Completion")

  (leuven--section "26.8 (emacs)Symbol Completion")

  ;; when you hit `<C-tab>', call the command normally bound to `<M-tab>'
  (global-set-key
    (kbd "<C-tab>")
    (lambda ()
      (interactive)
      (call-interactively (key-binding (kbd "<M-tab>")))))

;;** 26.9 (info "(emacs)Glasses") minor mode

  (leuven--section "26.9 (emacs)Glasses minor mode")

  (with-eval-after-load "glasses"

    ;; face to be put on capitals of an identifier looked through glasses
    (setq glasses-face 'bold)

    ;; string to be displayed as a visual separator in unreadable
    ;; identifiers
    (setq glasses-separator ""))

)                                       ; chapter 26 ends here

;;* 27 (info "(emacs)Building") Compiling and Testing Programs

(leuven--chapter leuven-chapter-27-building "27 Compiling and Testing Programs"

  (autoload 'flymake-mode "flymake"
    "Toggle on-the-fly syntax checking." t)

  (with-eval-after-load "flymake"

    ;; set up `flymake'
    (defun activate-flymake ()
      "Activate Flymake when real buffer and you have write access."
      (when (and (buffer-file-name)
                 (file-writable-p buffer-file-name))
        (flymake-mode t)))

    ;; XXX add errors to mode line
    (defun leuven--flymake-show-help ()
      "Display the error output of the current line in the mode line."
      (when (get-char-property (point) 'flymake-overlay)
        (let ((help (get-char-property (point) 'help-echo)))
          (if help (message "%s" help)))))

    (add-hook 'post-command-hook 'leuven--flymake-show-help))

;;** 27.1 Running (info "(emacs)Compilation")s under Emacs

  (leuven--section "27.1 Running (emacs)Compilations under Emacs")

  ;; invoke a compiler with the same command as in the last invocation of
  ;; `compile'
  (autoload 'recompile "compile"
    "Re-compile the program including the current buffer." t)

  (global-set-key (kbd "<f9>") 'recompile)

  ;; scroll the `*compilation*' buffer window to follow output as it appears
  (setq compilation-scroll-output t)

  ;; number of lines in a compilation window
  (setq compilation-window-height 8)

  ;; ;; I also don't like that the compilation window sticks around after
  ;; ;; a successful compile.  After all, most of the time, all I care
  ;; ;; about is that the compile completed cleanly.  Here's how I make the
  ;; ;; compilation window go away, only if there was no compilation
  ;; ;; errors:
  ;; (setq compilation-finish-function
  ;;       (lambda (buf str)
  ;;         (if (string-match "exited abnormally" str)
  ;;             ;; there were errors
  ;;             (message "Compilation errors, press C-x ` to visit")
  ;;           ;; no errors, make compilation window go away in 0.5 sec
  ;;           (run-at-time 0.5 nil 'delete-windows-on buf)
  ;;           (message "NO COMPILATION ERRORS!"))))

  (GNUEmacs
    (defun cc-goto-first-error( buffer exit-condition )
      (with-current-buffer buffer
        (goto-char (point-min))
        (compilation-next-error 1)
        (beep)))

    (add-to-list 'compilation-finish-functions 'cc-goto-first-error))

  (defvar make-clean-command "make clean all"
    "*Command used by the `make-clean' function.")

  (defun make-clean (&optional arg)
    "Run a make clean."
    (interactive "P")
    (require 'compile)                  ; needed for compile-internal
    (if arg
        (setq make-clean-command
              (read-string "Command: " make-clean-command)))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-internal make-clean-command "No more errors"))

  (global-set-key (kbd "<S-f9>") 'make-clean)

;;** 27.2 (info "(emacs)Compilation Mode")

  (leuven--section "27.2 (emacs)Compilation Mode")

  ;; automatically jump to the first error during compilation
  (setq compilation-auto-jump-to-first-error t)

  ;; display the next compiler error message
  (global-set-key (kbd "<f10>") 'next-error)
                                        ; also on `C-x `' and `M-g n'

  ;; display the previous compiler error message
  (global-set-key (kbd "<S-f10>") 'previous-error)
                                        ; also on `M-g p'

  ;; display the first compiler error message
  (global-set-key (kbd "<C-f10>") 'first-error)

  ;; highlight and parse the whole compilation output as soon as it
  ;; arrives
  (setq compile-auto-highlight t)

;;** 27.4 (info "(emacs)Grep Searching") under Emacs

  (leuven--section "27.4 (emacs)Grep Searching under Emacs")

  ;; ignore case distinctions in the default `grep' command
  (setq grep-command "grep -i -H -n -e ")

  ;; do not append `null-device' (`/dev/null' or `NUL') to `grep' commands
  (setq grep-use-null-device nil)       ; not necessary if the `grep' program
                                        ; used supports the `-H' option

  ;; ;; for Windows
  ;; (setq grep-find-command '("findstr /sn *" . 13))

  ;; use `find -print0' and `xargs -0'
  (setq grep-find-use-xargs 'gnu)

  ;; run `grep' via `find', with user-friendly interface
  (global-set-key (kbd "C-c 3") 'rgrep)

  ;; 10.3.5 Org keyword search
  (defun leuven-grep-org-files (regexp &optional context)
    "Recursively search for REGEXP in Org files in directory tree rooted at `org-directory'.
  Prefix argument determines number of lines of output context."
    (interactive "sSearch regexp: \nP")
    (let ((grep-find-ignored-files '("#*" ".#*"))
          (grep-template (concat "grep <X> -i -nH "
                                 (when context
                                   (concat "-C" (number-to-string context)))
                                 " -e <R> <F>")))
      (rgrep regexp "*.org" org-directory)))

;;** 27.5 (info "(emacs)Flymake")

  (leuven--section "27.5 (emacs)Flymake")

  ;; modern on-the-fly syntax checking
  (try-require 'flycheck)
  (with-eval-after-load "flycheck"

    ;; ;; indicate errors and warnings via icons in the right fringe
    ;; (setq flycheck-indication-mode 'right-fringe)

    ;; enable Flycheck mode in all buffers
    (add-hook 'after-init-hook 'global-flycheck-mode)

    (defun leuven--adjust-flycheck-automatic-syntax-eagerness ()
      "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in
a clean buffer we're an order of magnitude laxer about checking."
      (setq flycheck-idle-change-delay
            (if (assq 'error (flycheck-count-errors flycheck-current-errors))
                ; only check for REAL errors (original source: Magnars)
                1
              20)))

    ;; Each buffer get its local `flycheck-idle-change-delay' because of the
    ;; buffer-sensitive adjustment above.
    (make-variable-buffer-local 'flycheck-idle-change-delay)

    (add-hook 'flycheck-after-syntax-check-hook
              'leuven--adjust-flycheck-automatic-syntax-eagerness)

    ;; Remove newline checks, since they would trigger an immediate check when
    ;; we want the `flycheck-idle-change-delay' to be in effect while editing.
    (setq flycheck-check-syntax-automatically
          '(save
            idle-change
            ;; new-line
            mode-enabled))

    (defun flycheck-handle-idle-change ()
      "Handle an expired idle time since the last change.

This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
      (flycheck-clear-idle-change-timer)
      (flycheck-buffer-automatically 'idle-change)))

;;** 27.6 Running (info "(emacs)Debuggers") Under Emacs

  (leuven--section "27.6 Running (emacs)Debuggers Under Emacs")

  (with-eval-after-load "gdb-mi"

    ;; enable Gdb-Many-Windows mode
    (setq gdb-many-windows t))
    ;; the only important parameter for GDB

;;** Debugging Lisp programs

  ;; source-level debugger for Emacs Lisp
  (with-eval-after-load "edebug"

    ;; display a trace of function entry and exit
    (setq edebug-trace t)

    (defadvice edebug-overlay-arrow (around leuven-highlight-line activate)
      "Highlight line currently being Edebug'ged."
      (require 'hl-line)
      (hl-line-mode)
      ad-do-it)

    (defun leuven-edebug-quit ()
      "Stop Edebug'ging and remove highlighting."
      (interactive)
      (hl-line-mode -1)
      (top-level))

    (define-key edebug-mode-map [remap top-level] 'leuven-edebug-quit))

;;** 27.8 (info "(emacs)Lisp Libraries") for Emacs

  (leuven--section "27.8 (emacs)Lisp Libraries")

  ;; force load of `.el' files when they are newer than the `.elc' files
  (setq load-prefer-newer t)            ; from Emacs 24.4

;;** 27.9 (info "(emacs)Lisp Eval") Expressions

  (leuven--section "27.9 (emacs)Lisp Eval Expressions")

  ;; enable the use of the command `eval-expression' without confirmation
  (put 'eval-expression 'disabled nil)

  ;; maximum depth of lists to print in the result of the evaluation
  ;; commands before abbreviating them
  (setq eval-expression-print-level nil) ; no limit

  ;; maximum length of lists to print in the result of the evaluation
  ;; commands before abbreviating them
  (setq eval-expression-print-length nil) ; no limit

;;** 27.10 Lisp Interaction Buffers

  (leuven--section "27.10 (emacs)Lisp Interaction Buffers")

  ;; don't display the "Welcome to GNU Emacs" buffer on startup
  (setq inhibit-startup-screen t)

  ;; don't insert instructions in the `*scratch*' buffer at startup
  (setq initial-scratch-message nil)

  ;; major mode command symbol to use for the initial `*scratch*' buffer
  (setq initial-major-mode 'fundamental-mode)

  (defun elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))

  (with-eval-after-load "elint"
    (add-to-list 'elint-standard-variables 'current-prefix-arg)
    (add-to-list 'elint-standard-variables 'command-line-args-left)
    (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
    (add-to-list 'elint-standard-variables 'emacs-major-version)
    (add-to-list 'elint-standard-variables 'window-system))

  ;; (defun elisp-indent-or-complete (&optional arg)
  ;;   (interactive "p")
  ;;   (call-interactively 'lisp-indent-line)
  ;;   (unless (or (looking-back "^\\s-*")
  ;;          (bolp)
  ;;          (not (looking-back "[-[:alnum:]_*+/=<>!?]+")))
  ;;     (call-interactively 'lisp-complete-symbol)))
  ;;
  ;; (with-eval-after-load "lisp-mode"
  ;;   (define-key emacs-lisp-mode-map (kbd "<tab>") 'elisp-indent-or-complete))

)                                       ; chapter 27 ends here

;;* 28 (info "(emacs)Maintaining") Programs

(leuven--chapter leuven-chapter-28-maintaining "28 Maintaining Programs"

;;** 28.1 (info "(emacs)Version Control")

  (leuven--section "28.1 (emacs)Version Control")

;;*** 28.1.2 (info "(emacs)VC Mode Line")

  (leuven--section "28.1.2 Version Control and the Mode Line")

  ;; (defpowerline powerline-vc (when (and (buffer-file-name (current-buffer)) vc-mode) (format-mode-line '(vc-mode vc-mode))))

  (with-eval-after-load "vc"

    (GNUEmacs
      (when (image-type-available-p 'png)
        ;; http://www.emacswiki.org/emacs/VcIcon
        (defun vc-icon ()
          "Display a colored icon indicating the vc status of the current file."
          (let ((icon (if (vc-workfile-unchanged-p (buffer-file-name))
                          (concat leuven--directory
                                  "Pictures/NormalIcon.png")
                        (concat leuven--directory
                                "Pictures/ModifiedIcon.png")))
                (bg-colour (face-attribute 'mode-line :background)))
            (propertize
             "  "
             'display (find-image `((:type png
                                     :file ,icon
                                     :ascent center
                                     :background ,bg-colour))))))

        (setq-default mode-line-format
                      (push '(vc-mode (:eval (vc-icon))) mode-line-format)))))

;;*** 28.1.4 (info "(emacs)Log Buffer")

  (defun leuven--vc-log-mode-setup ()
    ;; check if `ispell-program-name' seems correct
    (when (ispell-check-program-name)
      (setq ispell-local-dictionary "american")
      (flyspell-mode)))

  (add-hook 'vc-log-mode-hook 'leuven--vc-log-mode-setup)

  (with-eval-after-load "vc-git"

    ;; major mode for editing git commit messages
    (try-require 'git-commit-mode))

  (with-eval-after-load "git-commit-mode"

    ;; turn on on-the-fly spell-checking
    (add-hook 'git-commit-mode-hook 'flyspell-mode)

    ;; turn off save-place
    (add-hook 'git-commit-mode-hook
              (lambda ()
                (toggle-save-place 0))))

;;*** 28.1.6 (info "(emacs)Old Revisions")

  (leuven--section "28.1.6 Examining And Comparing Old Revisions")

  ;; switches for diff under VC
  (setq vc-diff-switches diff-switches)

;;*** 28.1.9 (info "(emacs)VC Directory Mode")

  (leuven--section "28.1.9 VC Directory Mode")

  (defun leuven-vc-jump ()
    "Jump to VC status buffer."
    (interactive)
    (let* ((fname (buffer-file-name))
           (dname (if fname
                      (if (file-directory-p fname)
                          fname
                        (file-name-directory fname))
                    default-directory)))
      (message "VC status for directory: %s" dname)
      (vc-dir dname)))

  ;; vc status without asking for a directory
  (global-set-key (kbd "<C-f9>") 'leuven-vc-jump)

  ;; hide up-to-date and unregistered files
  (add-hook  'vc-dir-mode-hook
             (lambda ()
               (define-key vc-dir-mode-map
                 (kbd "x") 'leuven-vc-dir-hide-up-to-date-and-unregistered)
               (define-key vc-dir-mode-map
                 (kbd "E") 'vc-ediff)
               (define-key vc-dir-mode-map
                 (kbd "#") 'vc-ediff-ignore-whitespace)
                                         ; ediff-windows-wordwise?
               ))

  (defun leuven-vc-dir-hide-up-to-date-and-unregistered ()
    (interactive)
    (vc-dir-hide-up-to-date)
    (vc-dir-hide-unregistered))

  (defun vc-dir-hide-unregistered ()
    "Hide unregistered items from display."
    (interactive)
    (let ((crt (ewoc-nth vc-ewoc -1))
          (first (ewoc-nth vc-ewoc 0)))
      ;; go over from the last item to the first and remove the
      ;; unregistered files and directories with no child files
      (while (not (eq crt first))
        (let* ((data (ewoc-data crt))
               (dir (vc-dir-fileinfo->directory data))
               (next (ewoc-next vc-ewoc crt))
               (prev (ewoc-prev vc-ewoc crt))
               ;; ewoc-delete does not work without this...
               (inhibit-read-only t))
          (when (or
                 ;; remove directories with no child files
                 (and dir
                      (or
                       ;; nothing follows this directory
                       (not next)
                       ;; next item is a directory
                       (vc-dir-fileinfo->directory (ewoc-data next))))
                 ;; remove files in the unregistered state
                 (eq (vc-dir-fileinfo->state data) 'unregistered))
            (ewoc-delete vc-ewoc crt))
          (setq crt prev)))))

  (defun vc-ediff-ignore-whitespace (historic &optional not-urgent)
    "Ignore regions that differ in white space & line breaks only."
    (interactive (list current-prefix-arg t))
    (require 'ediff)
    (let ((ediff-ignore-similar-regions t))
      (call-interactively 'vc-ediff)))  ; XXX does not work yet

;;*** 28.1.13 (info "(emacs)Customizing VC")

  (leuven--section "28.1.13 Customizing VC")

  ;; files covered by VC get backups (as with other files)
  (setq vc-make-backup-files t)

  ;; http://www.emacswiki.org/emacs/VcTopDirectory
  ;; For git
  (defadvice vc-dir-prepare-status-buffer
             (before leuven-vcs-goto-top-directory activate compile)
    (let* ((backend (ad-get-arg 2))
           (vcs-dir (ad-get-arg 1))
           (vcs-top-dir (vc-call-backend backend 'responsible-p vcs-dir)))
      (when (stringp vcs-top-dir)
        (ad-set-arg 1 vcs-top-dir))))

  (GNUEmacs
    (defun leuven--ediff-revision (file rev1 &optional rev2)
      "Run Ediff by comparing 'master' against the 'current'."
      (require 'ediff)
      (find-file file)
      (if (and (buffer-modified-p)
               (y-or-n-p (format "Buffer %s is modified.  Save buffer? "
                                 (buffer-name))))
          (save-buffer (current-buffer)))
      (ediff-load-version-control)
      (funcall
       (intern (format "ediff-%S-internal" ediff-version-control-package))
       rev1 rev2 nil))

    (defun leuven-vc-diff (&optional arg)
      (interactive "P")
      (call-interactively
       (cond (arg
              (lambda ()
                (interactive)
                (vc-diff nil)))
             (t
              (lambda ()
                (interactive)
                (leuven--ediff-revision (buffer-file-name)
                                        (read-string "revision? "
                                                     "HEAD" nil "HEAD")
                                        ""))))))

    (define-key vc-prefix-map (kbd "=") 'leuven-vc-diff))

;;** 28.2 (info "(emacs)Change Log")

  (leuven--section "28.2 (emacs)Change Logs")

  (with-eval-after-load "add-log"

    ;; don't make a new entry, when the last entry was made by you and on
    ;; the same date
    (setq add-log-always-start-new-record nil)

    ;; adds the file's version number to the change log entry
    (setq change-log-version-info-enabled t)

    (add-hook 'change-log-mode-hook
              (add-to-list
               'change-log-font-lock-keywords
               '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                 (0 'change-log-date-face)
                 ("\\([^<(]+?\\)[   ]*[(<]\\([[:alnum:]_.+-]+@[[:alnum:]_.-]+\\)[>)]" nil nil
                  (1 'change-log-name)
                  (2 'change-log-email))))))

;;** 28.3 (info "(emacs)Tags")

  (leuven--section "28.3 (emacs)Tags Tables")

  ;; list of file names of tags tables to search
  (setq tags-table-list
        '(
          "~/TAGS"
          ;; "/usr/local/lib/emacs/src/TAGS"
          ;; "/usr/share/texmf-texlive/tex/latex/TAGS"
          ))

  (defun find-next-tag ()
    (interactive)
    (find-tag nil t))

  (with-eval-after-load "etags"

    ;; select from multiple tags
    (try-require 'etags-select))

  (with-eval-after-load "etags-select"

    ;; do a `find-tag-at-point', and display all exact matches
    (global-set-key (kbd "M-?") 'etags-select-find-tag-at-point))

  ;; find the definition of the Emacs Lisp function or variable near point
  (GNUEmacs
    (find-function-setup-keys))

(progn
  (require 'thingatpt)

  (defun leuven-find-symbol-at-point ()
    "Find the definition of the symbol at point."
    (interactive)
    (let ((sym (symbol-at-point)))
      (funcall (pcase sym
                 ((pred facep)           'find-face)
                 ((pred symbol-function) 'find-function)
                 (_                      'find-variable))
               sym)))

  (global-set-key (kbd "<S-return>") 'leuven-find-symbol-at-point))

;;** 28.4 (info "(emacs)EDE")

  (leuven--section "28.4 Emacs Development Environment")

  (unless (string< emacs-version "23.2")
    ;; ;; enable global EDE (project management) features
    ;; (global-ede-mode 1)

    (setq semantic-default-submodes
          '(
            ;; turn Semantic DB mode on (Semantic parsers store the results of
            ;; parsing source code in a database file, which can be saved for
            ;; future Emacs sessions)
            global-semanticdb-minor-mode

            ;; the idle scheduler will automatically reparse buffers in idle
            ;; time
            global-semantic-idle-scheduler-mode ; [minimum-features]

            ;; display a summary of the symbol at point in the echo area
            ;; (~ ElDoc)
            global-semantic-idle-summary-mode ; [code-helpers]

            ;; display a tooltip with a list of possible completions near the
            ;; cursor
            global-semantic-idle-completions-mode ; [gaudy-code-helpers]

            ;; turn Semantic MRU Bookmarks on (keep track of the Most
            ;; Recently Used tags)
            global-semantic-mru-bookmark-mode

            ;; enable Semantic-Stickyfunc mode (display a header line that shows
            ;; the declaration line of the function or tag)
            global-semantic-stickyfunc-mode ; [gaudy-code-helpers]

            ;; enable Semantic-Highlight-Func mode
            global-semantic-highlight-func-mode ; [excessive-code-helpers]

            ;; turn on all active decorations
            global-semantic-decoration-mode ; [gaudy-code-helpers]
            ))

    ;; XXX if prog-mode, then Semantic will be launched after Emacs init, as
    ;; the scratch buffer is in Emacs Lisp...
    (add-hook 'java-mode-hook 'semantic-mode) ; enable parser features (Semantic
                                        ; mode) and install a `Development' menu
                                        ; on the menu-bar

    ;; ;; smart completion, and display of information for tags & classes
    ;; (require 'semantic/ia)
    ;;
    ;; (require 'semantic/db)

    (with-eval-after-load "semantic"

      (defun leuven--semantic ()
        ;; automatically complete whatever symbol you are typing
        (local-set-key
          (kbd "C-c ?") 'semantic-ia-complete-symbol) ; better binding: `M-/'?

        ;; jump to the definition of the symbol under cursor
        (local-set-key
          (kbd "C-c j") 'semantic-ia-fast-jump) ; where a symbol is declared

        ;; show the documentation of the symbol under cursor
        (local-set-key
          (kbd "C-c q") 'semantic-ia-show-doc) ; show javadoc of the right method

        ;; show a summary about the symbol under cursor
        (local-set-key
          (kbd "C-c s") 'semantic-ia-show-summary)


        ;; show possible public member functions or data members (when at `.'
        ;; or `->' after an object name)
        (local-set-key
          (kbd "C-c >") 'semantic-complete-analyze-inline)

        ;; toggle between the implementation and a prototype of symbol under
        ;; cursor
        (local-set-key
          (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)

        ;; visit the header file under cursor
        (local-set-key
          (kbd "C-c =") 'semantic-decoration-include-visit)


        ;; unfold the block under cursor
        (local-set-key
          (kbd "C-c +") 'semantic-tag-folding-show-block)

        ;; fold the block under cursor
        (local-set-key
          (kbd "C-c -") 'semantic-tag-folding-fold-block)

        ;; C-c C-c is not a prefix key!
        ;; ;; unfold all
        ;; (local-set-key
        ;;   (kbd "C-c C-c +") 'semantic-tag-folding-show-all)
        ;;
        ;; ;; fold all
        ;; (local-set-key
        ;;   (kbd "C-c C-c -") 'semantic-tag-folding-fold-all)
        )

      (add-hook 'prog-mode-hook 'leuven--semantic)

      (defun leuven--c-mode-semantic ()
        "Completion on `.' or `->'."
        (local-set-key (kbd ".") 'semantic-complete-self-insert)
        (local-set-key (kbd ">") 'semantic-complete-self-insert)
        (local-set-key (kbd "C-c C-r") 'semantic-symref))

      (add-hook 'c-mode-common-hook 'leuven--c-mode-semantic))

    ;; hooks, specific for Semantic
    (defun leuven--semantic-imenu ()
      (imenu-add-to-menubar "TAGS"))

    (add-hook 'semantic-init-hooks 'leuven--semantic-imenu)

    )

)                                       ; chapter 28 ends here

;;* 29 (info "(emacs)Abbrevs")

(leuven--chapter leuven-chapter-29-abbrevs "29 Abbrevs"

  ;; See (info "(autotype)") as well

;;** 29.3 Controlling (info "(emacs)Expanding Abbrevs")

  (leuven--section "29.3 Controlling Expanding Abbrevs")

  ;; Yet Another Snippet extension for Emacs
  (GNUEmacs
    ;; use the "standard" package (NOT `yasnippet-bundle'!)
    (try-require 'yasnippet)
    (with-eval-after-load "yasnippet"

      ;; enable YASnippet in all buffers
      (yas-global-mode 1)

      ;; root directories that store the snippets
      (let ((my-snippets                ; my personal additional YASnippets
             (concat leuven--directory "snippets"))
            (org-snippets
             (concat leuven-local-repos-directory "yasnippet-org-mode")))

        (when (file-directory-p org-snippets)
          (add-to-list 'yas-snippet-dirs org-snippets))

        (when (file-directory-p my-snippets)
          (add-to-list 'yas-snippet-dirs my-snippets)))
                                        ; the first element (inserted last) is
                                        ; always the user-created snippets
                                        ; directory

      ;; use Snippet mode for files with a `yasnippet' extension
      (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

      ;; insert snippet at point
      (global-set-key (kbd "C-c s") 'yas-insert-snippet) ; also on `C-c & C-s'

      ;; bind `yas-expand' to SPC
      (define-key yas-minor-mode-map (kbd "<tab>") nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      (define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)

      ;; don't expand when you are typing in a string or comment
      (add-hook 'prog-mode-hook
                '(lambda ()
                   (setq yas-buffer-local-condition
                         '(if (nth 8 (syntax-ppss)) ; non-nil if in a string or comment
                              '(require-snippet-condition . force-in-comment)
                            t))))

      ;; UI for selecting snippet when there are multiple candidates
      (setq yas-prompt-functions '(yas-dropdown-prompt))

      (global-set-key (kbd "C-c & C-l") 'yas-describe-tables)

      (defvar lawlist-context-menu-map
        (let ((map (make-sparse-keymap "Context Menu")))
          (define-key map [help-for-help] (cons "Help" 'help-for-help))
          (define-key map [seperator-two] '(menu-item "--"))
          (define-key map [my-menu] (cons "LAWLIST" (make-sparse-keymap "My Menu")))
          (define-key map [my-menu 01] (cons "Next Line" 'next-line))
          (define-key map [my-menu 02] (cons "Previous Line" 'previous-line))
          (define-key map [seperator-one] '(menu-item "--"))
        map) "Keymap for the LAWLIST context menu.")

      (defun lawlist-popup-context-menu  (event &optional prefix)
        "Popup a context menu."
        (interactive "@e \nP")
          (define-key lawlist-context-menu-map [lawlist-major-mode-menu]
            `(menu-item ,(symbol-name major-mode)
              ,(mouse-menu-major-mode-map) :visible t))
          (define-key lawlist-context-menu-map (vector major-mode)
            `(menu-item ,(concat "YAS " (symbol-name major-mode))
              ,(gethash major-mode yas--menu-table)
                :visible (yas--show-menu-p ',major-mode)))
          (popup-menu lawlist-context-menu-map event prefix))

      (global-set-key [mouse-3] 'lawlist-popup-context-menu)

      ;; automatically reload snippets after saving
      (defun recompile-and-reload-all-snippets ()
        (interactive)
        (when (eq major-mode 'snippet-mode)
          (yas-recompile-all)
          (yas-reload-all)
          (message "Reloaded all snippets")))

      (add-hook 'after-save-hook 'recompile-and-reload-all-snippets)

      (add-hook 'snippet-mode-hook
                (lambda ()
                  (setq require-final-newline nil)))))

;;** 29.7 (info "(emacs)Dabbrev Customization")

  (leuven--section "29.7 Dabbrev Customization")

  ;; (with-eval-after-load "dabbrev"
  ;;
  ;;   ;; preserve case when expanding the abbreviation
  ;;   (setq dabbrev-case-replace nil))

  ;; expand text trying various ways to find its expansion
  (global-set-key (kbd "M-/") 'hippie-expand)

  (with-eval-after-load "hippie-exp"

    ;; list of expansion functions tried (in order) by `hippie-expand'
    ;; (completion strategy)
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev          ; current buffer
            try-expand-dabbrev-visible  ; visible (parts of all) buffers
            try-expand-dabbrev-from-kill ; kill ring
            try-complete-file-name-partially ; file names
            try-complete-file-name
            try-expand-all-abbrevs      ; abbreviations
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-whole-kill))

    (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially ; file names
            try-complete-file-name
            try-expand-all-abbrevs      ; abbreviations
            try-expand-dabbrev          ; current buffer
            try-expand-dabbrev-from-kill)) ; kill ring

    ;; integrate YASnippet with `hippie-expand'
    (with-eval-after-load "yasnippet"

      (add-to-list 'hippie-expand-try-functions-list
                    'yas-hippie-try-expand)))
                                        ; makes more sense when placed at the
                                        ; top of the list

  (GNUEmacs

    ;; Auto Completion
    (when (locate-library "auto-complete-config")
      (idle-require 'auto-complete-config))

    (with-eval-after-load "auto-complete-config"

      ;; ;; 5.4 completion will be started automatically by inserting 2 characters
      ;; (setq ac-auto-start 2)

      ;; 6.1 set a list of sources to use (by default + for some major modes)
      (ac-config-default)             ; ... and enable Auto-Complete mode in all
                                      ; buffers

      ;; 7.5 use `C-n/C-p' to select candidates (only when completion menu is
      ;; displayed)
      (setq ac-use-menu-map t)
      (define-key ac-menu-map (kbd "C-n") 'ac-next)
      (define-key ac-menu-map (kbd "C-p") 'ac-previous)

      ;; unbind some keys (inconvenient in Comint buffers)
      (define-key ac-completing-map (kbd "M-n") nil)
      (define-key ac-completing-map (kbd "M-p") nil)

      ;; add other modes into `ac-modes'
      (setq ac-modes
            (append ac-modes
                    '(change-log-mode
                      org-mode
                      prog-mode       ; programming modes
                      snippet-mode
                      sql-mode
                      text-mode)))

      ;; 7.8 enable auto-complete-mode automatically for Sword mode
      (add-to-list 'ac-modes 'sword-mode) ; brand new mode

      ;; 8.1 delay to completions will be available
      (setq ac-delay 0)               ; faster than default 0.1

      ;; 8.2 completion menu will be automatically shown
      (setq ac-auto-show-menu 0.2)    ; [default: 0.8]

      ;; 8.13 delay to show quick help
      (setq ac-quick-help-delay 0.5)

      ;; 8.15 max height of quick help
      (setq ac-quick-help-height 10)  ; same as `ac-menu-height'

      ;; 8.16 limit on number of candidates
      (setq ac-candidate-limit 100)

      ;; (setq ac-disable-inline t)
      ;; (setq ac-candidate-menu-min 0)

      ;; completion by TAB
      (define-key ac-completing-map (kbd "<tab>") 'ac-complete)

      ;; completion by right arrow
      (define-key ac-completing-map (kbd "<right>") 'ac-complete)

      ;; abort
      (define-key ac-completing-map (kbd "C-g") 'ac-stop)
      (define-key ac-completing-map (kbd "<left>") 'ac-stop)

      ;; 11.1 avoid Flyspell processes when auto completion is being started
      (ac-flyspell-workaround)))

  ;; modular text completion framework
  (try-require 'company-XXX)
  (with-eval-after-load "company-XXX"

    ;; minimum prefix length for idle completion
    (setq company-minimum-prefix-length 2)

    ;; start completion immediately
    (setq company-idle-delay 0)

    ;; show quick-access numbers for the first ten candidates
    (setq company-show-numbers t)

    ;; enable Company mode in all buffers ...
    (add-hook 'after-init-hook 'global-company-mode)

    ;; ... except in some modes
    (setq company-global-modes
          '(not ess-mode                ; in (i)ESS buffers, Auto-Complete is
                inferior-ess-mode       ; enabled by default
                magit-status-mode
                help-mode))

    ;; use `C-n/C-p' to select candidates (only when completion menu is
    ;; displayed)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)

    ;; unbind some keys (inconvenient in Comint buffers)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)

    ;; completion by TAB
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)

    ;; completion by right arrow
    (define-key company-active-map (kbd "<right>") 'company-complete-selection)

    ;; abort
    (define-key company-active-map (kbd "C-g") 'company-abort)
    (define-key company-active-map (kbd "<left>") 'company-abort))

)                                       ; chapter 29 ends here

;;* 30 (info "(emacs)Dired"), the Directory Editor

(leuven--chapter leuven-chapter-30-dired "30 Dired, the Directory Editor"

;;** (info "(emacs)Dired Enter")

  ;; directory-browsing commands
  (with-eval-after-load "dired"

    (leuven--section "30.1 (emacs)Dired Enter")

    ;; switches passed to `ls' for Dired
    (setq dired-listing-switches
          (cond (win32p
                 "-a -F -l -p")
                (t
                 "-a -F --group-directories-first -l -p --time-style=long-iso")))

;;** (info "(emacs)Dired Deletion")

    (leuven--section "30.3 (emacs)Dired Deletion")

    ;; recursive deletes allowed, after asking for each directory at top level
    (setq dired-recursive-deletes 'top)

;;** (info "(emacs)Dired Visiting")

    (leuven--section "30.5 (emacs)Dired Visiting")

    ;; reuse Dired buffers, by running the command
    ;; `dired-find-alternate-file' (bound to `a') on a directory
    (put 'dired-find-alternate-file 'disabled nil)

    ;; reuse the current Dired directory buffer to visit another directory
    ;; (limit Dired to 1 single buffer)
    (try-require 'dired-single)
    (with-eval-after-load "dired-single"

      (define-key dired-mode-map (kbd "<return>") 'dired-single-buffer)

      (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)

      (define-key dired-mode-map
        (kbd "^")
        (lambda ()
          (interactive)
          (dired-single-buffer "..")))

      (define-key dired-mode-map
        (kbd "C-x C-j")
        (lambda ()
          (interactive)
          (dired-single-buffer ".."))))

    (define-key dired-mode-map (kbd "e") 'browse-url-of-dired-file) ; <C-RET>?

    ;; open files using Windows associations
    (GNUEmacs
      (when win32p
        (defun w32-dired-open-files-externally (&optional arg)
          "In Dired, open the marked files (or directories) with the default
        Windows tool."
          (interactive "P")
          (mapcar
           (lambda (file)
             (w32-shell-execute "open" (convert-standard-filename file)))
           (dired-get-marked-files nil arg)))

        ;; bind it to `E' in Dired mode
        (define-key dired-mode-map (kbd "E") 'w32-dired-open-files-externally)))

    ;; open current file with w3m
    (when (executable-find "w3m")
      (defun dired-find-w3m ()
        "In Dired, visit (with find-w3m) the file named on this line."
        (interactive)
        (w3m-find-file (file-name-sans-versions (dired-get-filename) t)))

      ;; add a binding "W" -> `dired-find-w3m' to Dired
      (define-key dired-mode-map "W" 'dired-find-w3m))

;;** (info "(emacs)Operating on Files")

    (leuven--section "30.7 (emacs)Operating on Files")

    ;; try to guess a default target directory
    (setq dired-dwim-target t)

    ;; copy recursively without asking
    (setq dired-recursive-copies 'always)

;;** (info "(emacs)Dired Updating")

    (leuven--section "30.15 (emacs)Dired Updating")

    ;; Dired sort
    (try-require 'dired-sort-map)
    ;; press `s' then `s', `x', `t', `n' or `d' to sort by
    ;; Size, eXtension, Time, Name or name grouping Dirs first

;;** (info "(emacs)Dired and Find")

    (leuven--section "30.16 (emacs)Dired and Find")

    ;; ;; what to use in place of `-ls' as the final argument
    ;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
    ;; ;; quicker to collate the matches and then use `xargs' to run the
    ;; ;; command (variable defined in `find-dired.el')

;; (when Cygwin... XXX
    ;; search for files with names matching a wild card pattern and Dired
    ;; the output
    (global-set-key (kbd "C-c 1") 'find-name-dired)
      ;; case insensitive if `read-file-name-completion-ignore-case' is non-nil

    ;; `find-grep-dired' case insensitivity
    (setq find-grep-options "-i -q")

    ;; search for files with contents matching a wild card pattern and Dired
    ;; the output
    (global-set-key (kbd "C-c 2") 'find-grep-dired)

;;** (info "(emacs)Wdired")

    (leuven--section "30.17 Editing the (emacs)Wdired Buffer")

    ;; put a Dired buffer in a mode in which filenames are editable
    (with-eval-after-load "wdired"

      ;; permissions bits of the files are editable
      (setq wdired-allow-to-change-permissions t))

;;** (info "(emacs)Image-Dired")

    (leuven--section "30.18 Viewing Image Thumbnails in Dired")

    ;; use Dired to browse and manipulate your images
    (with-eval-after-load "image-dired"

      ;; maximum number of files to show before warning the user
      (setq image-dired-show-all-from-dir-max-files 100)

      ;; size of button-like border around thumbnails
      (setq image-dired-thumb-relief 0)

      ;; size of the margin around thumbnails
      (setq image-dired-thumb-margin 4))

;;** Dired Extra

    (leuven--section "30.XX (dired-x)Top")

    ;; load `dired-x.el' when Dired is first invoked (for example, when
    ;; you first type `C-x d')
    (add-hook 'dired-load-hook
              (lambda ()
                (load "dired-x")))

    )                                   ; with-eval-after-load "dired" ends here

;;** Dired+

  (leuven--section "30.XX Dired+")

  ;; extensions to Dired (provides fancy highlighting, etc.)
  (add-hook 'dired-load-hook
            (lambda ()
              ;; don't hide details in Dired
              (setq diredp-hide-details-initially-flag nil)

              ;; don't wrap "next" command around to buffer beginning
              (setq diredp-wrap-around-flag nil)

              (try-require 'dired+)))

;;** (info "(emacs)ls in Lisp")

  (leuven--section "G.4 (emacs)ls in Lisp")

  ;; emulate insert-directory completely in Emacs Lisp
  (with-eval-after-load "ls-lisp"

    ;; disable the case sensitive sort of file names
    (setq ls-lisp-ignore-case t)

    ;; sort directories first
    (setq ls-lisp-dirs-first t)

    ;; use ISO 8601 dates (on MS-Windows)
    (setq ls-lisp-format-time-list
          '("%Y-%m-%d %H:%M"
            "%Y-%m-%d %H:%M"))

    ;; use localized date/time format
    (setq ls-lisp-use-localized-time-format t))

)                                       ; chapter 30 ends here

;;* 31 The (info "(emacs)Calendar/Diary")

(leuven--chapter leuven-chapter-31-calendar-diary "31 The Calendar and the Diary"

;;** 31.1 (info "(emacs)Calendar Motion")

  (leuven--section "31.1 (emacs)Calendar Motion")

  ;; years must be written in full
  (setq diary-abbreviated-year-flag nil)

  ;; set the style of calendar and diary dates to ISO (how to interpret the
  ;; dates)
  (setq calendar-date-style 'iso)

  ;; week in the calendar begins on Monday
  (setq calendar-week-start-day 1)

  ;; mark all visible dates that have diary entries
  (when (file-exists-p "~/diary")
    (setq calendar-mark-diary-entries-flag t))

  ;; mark the current date (by changing its face) after generating a calendar,
  ;; if today's date is visible
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;;** 31.2 (info "(emacs)Scroll Calendar")

  (leuven--section "31.2 (emacs)Scroll Calendar")

  ;; fix foolish calendar-mode scrolling after loading `calendar.el'
  (add-hook 'calendar-load-hook
            (lambda ()
              (define-key calendar-mode-map (kbd ">") 'scroll-calendar-left)
              (define-key calendar-mode-map (kbd "<") 'scroll-calendar-right)))

;;** 31.7 Times of (info "(emacs)Sunrise/Sunset")

  (leuven--section "31.7 Times of (emacs)Sunrise/Sunset")

  ;; calendar functions for solar events
  (with-eval-after-load "solar"

    ;; name of the calendar location
    (setq calendar-location-name "Leuven, BE")

    ;; latitude of `calendar-location-name'
    (setq calendar-latitude 50.88)

    ;; longitude of `calendar-location-name'
    (setq calendar-longitude 4.70))

;;** 31.11 (info "(emacs)Appointments")

  (leuven--section "31.11 (emacs)Appointments")

  ;; insinuate appt if `diary-file' exists
  (if (file-readable-p "~/diary")
      (try-require 'appt)               ; requires `diary-lib', which requires
                                        ; `diary-loaddefs'
    (message "Appointment reminders lib `appt' not loaded (no diary file found)"))

  (with-eval-after-load "appt"

    ;; send the first warning 60 minutes before an appointment
    (setq appt-message-warning-time 60) ; [default: 12]

    ;; warn every 15 minutes
    (setq appt-display-interval 15)     ; [default: 3]

    ;; use a separate window to display appointment reminders
    (setq appt-display-format 'window)

    ;; function called to display appointment reminders *in a window*
    (setq appt-disp-window-function (function leuven--appt-display))

    (defun leuven--appt-display (mins-to-appt current-time notification-string)
      "Display a reminder for appointments.
    Use `libnotify' if available and if display is graphical, or fall back on a
    message in the echo area."
      (or (listp mins-to-appt)
          (setq notification-string (list notification-string)))
      (dotimes (i (length notification-string))
        (cond ((and (display-graphic-p)
                    (executable-find "notify-send"))
               (shell-command
                (concat "notify-send "
                        "-i /usr/share/icons/gnome/32x32/status/appointment-soon.png "
                        "-t 1000 "
                        "'Appointment' "
                        "'" (nth i notification-string) "'")))
              (t
               (message "%s" (nth i notification-string))
               (sit-for 1)))))

    ;; turn appointment checking on (enable reminders)
    (when leuven-load-verbose
      (message "(Info) Enable appointment reminders..."))
    (GNUEmacs
      (appt-activate 1))
    (XEmacs
      (appt-initialize))
    (when leuven-load-verbose
      (message "(Info) Enable appointment reminders... Done"))

    ;; enable appointment notification, several minutes beforehand
    (add-hook 'diary-hook 'appt-make-list)

    ;; keep your appointment list clean: if you delete an appointment from your
    ;; Org agenda file, delete the corresponding alert
    (defadvice org-agenda-to-appt (before leuven-org-agenda-to-appt activate)
      "Clear the existing `appt-time-msg-list'."
      (setq appt-time-msg-list nil))

    ;; add today's appointments (found in `org-agenda-files') each time the
    ;; agenda buffer is (re)built
    (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)
                                        ;! don't use the `org-agenda-mode-hook'
                                        ;! because the Org agenda files would be
                                        ;! opened once by `org-agenda-to-appt',
                                        ;! and then killed by
                                        ;! `org-release-buffers' (because
                                        ;! `org-agenda-to-appt' closes all the
                                        ;! files it opened itself -- as they
                                        ;! weren't already opened), to be
                                        ;! finally re-opened!

    ;; add today's appointments (found in `org-agenda-files') each time such a
    ;; file is saved
    (add-hook 'after-save-hook
              (lambda ()
                (when (and (eq major-mode 'org-mode)
                           (org-agenda-file-p))
                  (org-agenda-to-appt))))

    )                                   ; with-eval-after-load "appt" ends here

;;** 31.15 (info "(emacs)Advanced Calendar/Diary Usage")

  (leuven--section "31.15 (emacs)Advanced Calendar/Diary Usage")

  ;; get rid of some holidays
  (setq holiday-general-holidays nil)   ; too U.S.-centric holidays
  (setq holiday-oriental-holidays nil)  ; Oriental holidays
  (setq holiday-hebrew-holidays nil)    ; religious holidays
  (setq holiday-islamic-holidays nil)   ; religious holidays
  (setq holiday-bahai-holidays nil)     ; Baha'i holidays
  (setq holiday-solar-holidays nil)     ; sun-related holidays

  ;; mark dates of holidays in the calendar window
  (setq calendar-mark-holidays-flag t)

  (defun leuven-insert-current-date (prefix)
    "Insert the current date in ISO format.
  With one PREFIX argument, add day of week.  With two PREFIX arguments, add day
  of week and time."
    (interactive "P")
    (let ((format (cond ((not prefix) "%Y-%m-%d")
                        ((equal prefix '(4)) "%Y-%m-%d %a")
                        ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
      (insert (format-time-string format))))

  (global-set-key (kbd "C-c .") 'leuven-insert-current-date)

;;* Calendar view framework on Emacs

  ;; calendar view framework on Emacs
  (with-eval-after-load "calfw"

    ;; Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓))

  ;; calendar view for org-agenda
  (with-eval-after-load "calfw-org"

    ;; remove some strings (tags and filenames) from item summary
    (defun cfw:org-summary-format (item)
      "Format an item (How should it be displayed?)."
      (let* ((time (cfw:org-tp item 'time))
             (time-of-day (cfw:org-tp item 'time-of-day))
             (time-str (and time-of-day
                            (format "%02i:%02i "
                                    (/ time-of-day 100)
                                    (% time-of-day 100))))
             (category (cfw:org-tp item 'org-category))
             (tags (cfw:org-tp item 'tags))
             (marker (cfw:org-tp item 'org-marker))
             (buffer (and marker (marker-buffer marker)))
             (text (cfw:org-extract-summary item))
             (props (cfw:extract-text-props item 'face 'keymap)))
        (propertize
         (concat
          (if time-str (apply 'propertize time-str props)) text " "
          ;; (and buffer (buffer-name buffer))
          )
         'keymap cfw:org-text-keymap
         ;; Delete the display property, since displaying images will break our
         ;; table layout
         'display nil))))

)                                       ; chapter 31 ends here

;;* 32 (info "(emacs)Sending Mail")

(leuven--chapter leuven-chapter-32-sending-mail "32 Sending Mail"

  ;; full name of this user
  (setq user-full-name "John Doe")

  ;; full mailing address of this user
  ;; (used in MAIL envelope FROM, and to select the default personality ID)
  (setq user-mail-address "johndoe@example.com")

  ;; sending mail
  (setq send-mail-function 'smtpmail-send-it)

  ;; default SMTP server (overriden by `smtpmail-smtp-server')
  (setq smtpmail-default-smtp-server "smtp")
                                        ; SMTP process must be running there

)                                       ; chapter 32 ends here

;;* 34 (info "(emacs)Gnus")

(leuven--chapter leuven-chapter-34-gnus "34 Gnus"

  (global-set-key
    (kbd "C-c n")
    (lambda ()
      (interactive)
      (switch-or-start 'gnus "*Group*")))

  ;; directory beneath which additional per-user Gnus-specific files are placed
  (setq gnus-directory "~/.gnus.d/")    ; this should end with a directory
                                        ; separator

  ;; a newsreader for GNU Emacs
  (with-eval-after-load "gnus"

    ;; package to compose an outgoing mail (Message, with Gnus paraphernalia)
    (setq mail-user-agent 'gnus-user-agent)
    (XEmacs
      (setq toolbar-mail-reader 'gnus))

    ;; reading mail with Gnus
    (setq read-mail-command 'gnus))

;;** Insidious bbdb

  (leuven--section "Insidious bbdb")

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

  ;; search the BBDB
  (global-set-key (kbd "<C-f11>") 'bbdb)

  (with-eval-after-load "bbdb"

    ;; coding system used for reading and writing `bbdb-file'
    (setq bbdb-file-coding-system 'utf-8)

    ;; ensure `~/.bbdb' never becomes non utf-8 again (it is defined with
    ;; `defconst', so it is reset whenever `bbdb.el' is loaded)
    (add-hook 'bbdb-load-hook
              (lambda ()
                (setq bbdb-file-coding-system 'utf-8)))

    ;; enable the various package-specific BBDB functions
    (bbdb-initialize 'gnus 'message)
    ;; - add bindings for the default keys to Gnus and configure Gnus to notify
    ;;   the BBDB when new messages are loaded (required if the BBDB is to be
    ;;   able to display BBDB entries for messages displayed in Gnus)
    ;;
    ;; - add a binding for `M-TAB' to Message mode.  This will enable completion
    ;;   of addresses based on BBDB records

    ;; what do we do when invoking bbdb interactively (`:' to display sender)
    (setq bbdb-mua-update-interactive-p '(query . create))

    ;; update BBDB silently (don't display an auto-updated BBDB window)
    (setq bbdb-mua-pop-up nil)

;;* (info "(bbdb)Interfaces")

    ;; mail aliases (local mailing lists)
    ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases) ; BBDB 2.35
    (add-hook 'message-setup-hook 'bbdb-mail-aliases) ; BBDB 3

    ;; always use full name when sending mail
    ;; (even if User Name has an address of the form <user.name@domain>)
    (setq bbdb-dwim-net-address-allow-redundancy t) ; BBDB 2.35
    (setq bbdb-mail-avoid-redundancy nil) ; BBDB 3

    ;; no popup on auto-complete
    (setq bbdb-completion-display-record nil)

    ;; completion is done across the set of all full-names and user-ids
    (setq bbdb-completion-type nil)

;;* (info "(bbdb)Reader-specific Features")

    ;; marking posters with records in the BBDB
    (setq bbdb/gnus-summary-mark-known-posters t)

    ;; mark authors in the Summary Buffer who have records in the BBDB
    (setq bbdb/gnus-summary-known-poster-mark "B")

    ;; display the poster's name from the BBDB if we have one
    (setq bbdb/gnus-summary-prefer-real-names t)

    ;; replace the information provided in the From header with data from the
    ;; BBDB if we have one
    (setq bbdb/gnus-summary-prefer-bbdb-data t)

    (setq bbdb/gnus-summary-show-bbdb-names t)

;;* (info "(bbdb)Options")

    ;; You can add the author of a mail or posting to the BBDB
    ;; by hitting `:'

    ;; name of the file which contains your personal database
    (setq bbdb-file "~/.bbdb")

    ;; no default area code to use when prompting for a new phone number
    (setq bbdb-default-area-code nil)

    ;; default country to use if none is specified
    (setq bbdb-default-country "")

    ;; disable syntax-checking of telephone numbers
    (setq bbdb-north-american-phone-numbers-p nil) ; BBDB 2.35
    (setq bbdb-phone-style nil)         ; BBDB 3

    ;; restoration of the window configuration
    (setq bbdb-electric-p t)            ; BBDB 2.35
    (setq bbdb-electric t)              ; BBDB 3

    ;; don't display a continuously-updating BBDB window while in GNUS
    ;; (setq bbdb-use-pop-up nil)       ; BBDB 2.35
    ;; (setq bbdb-pop-up-layout nil)    ; BBDB 3

    ;; desired number of lines in a GNUS pop-up BBDB window
    (setq bbdb-pop-up-target-lines 1)   ; BBDB 2.35
    (setq bbdb-pop-up-window-size 1)    ; BBDB 3

    ;; default display layout
    (setq bbdb-display-layout 'multi-line)

    ;; default display layout pop-up BBDB buffers
    (setq bbdb-pop-up-display-layout 'one-line)

    ;; omit creation-date and time stamp from BBDB display
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

    ;; allow cycling of email addresses while completing them
    (setq bbdb-complete-name-allow-cycling t) ; BBDB 2.35
    (setq bbdb-complete-mail-allow-cycling t) ; BBDB 3

    ;; save the database without asking (any time it would ask)
    (setq bbdb-offer-save 'auto)

    ;; automatically add some text to the notes field of the BBDB record
    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

    ;; capture auto-notes
    (setq bbdb-auto-notes-alist
          ;; organization
          `(("Organization" (".*" Organization 0))

            ;; X-Face bitmaps of the people
            ("x-face" ,(list (concat "[ \t\n]*\\([^ \t\n]*\\)"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?")
                             'face
                             "\\1\\3\\5\\7")))))

)                                       ; chapter 34 ends here

;;* 35 (info "(emacs)Document View")

(leuven--chapter leuven-chapter-35-document-view "35 Document Viewing"

  ;; view PDF/PostScript/DVI files in Emacs

;;** 35.1 (info "(emacs)Navigation")

  (leuven--section "35.1 (emacs)Navigation")

  ;; `doc-view' integrates with the usual bookmark facility.  So simply use
  ;; `C-x r m' (`bookmark-set') to jump back to the last page you've read in
  ;; a PDF document.

  ;; antiword will be run on every `.doc' file you open
  ;; TODO sudo aptitude install antiword (or via Cygwin setup)
  (autoload 'no-word "no-word"
    "Word to txt.")
  (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))



  (add-to-list 'auto-mode-alist '("\\.docx\\'" . docx2txt))

  (defun docx2txt ()
    "Run `docx2txt' on the entire buffer."
    (shell-command-on-region (point-min) (point-max) "docx2txt.pl" t t))



  ;; un-xls files
  ;; TODO sudo aptitude install xlhtml
  (add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
  (defun no-xls (&optional filename)
    "Run `xlhtml' and `w3m -dump' on the entire buffer.
  Optional FILENAME says what filename to use.  This is only necessary for
  buffers without proper `buffer-file-name'.  FILENAME should be a real
  filename, not a path."
    (interactive "fExcel File: ")
    (when (and filename
               (not (buffer-file-name)))
      (write-file (make-temp-file filename)))
    (erase-buffer)
    (shell-command
     (format "xlhtml -nc -te %s | w3m -dump -T text/html"
             (buffer-file-name))
     (current-buffer))
    (setq buffer-file-name nil)
    (set-buffer-modified-p nil))

  ;; no-ppt
  ;; TODO sudo aptitude install ppthtml
  ;; FIXME Not that good! (some text repeated multiple times)
  (defun no-ppt (&optional filename)
    "Run `ppthtml' and `w3m -dump' on the entire buffer.
  Optional FILENAME says what filename to use.  This is only necessary for
  buffers without proper `buffer-file-name'.  FILENAME should be a real
  filename, not a path."
    (interactive "fPowerPoint File: ")
    (when (and filename
               (not (buffer-file-name)))
      (write-file (make-temp-file filename)))
    (erase-buffer)
    (shell-command
     (format "ppthtml %s | w3m -dump -T text/html" (buffer-file-name))
     (current-buffer))
    (setq buffer-file-name nil)
    (set-buffer-modified-p nil))
  (add-to-list 'auto-mode-alist '("\\.ppt\\'" . no-ppt))

)                                       ; chapter 35 ends here

;;* 36 Running (info "(emacs)Shell") Commands from Emacs

(leuven--chapter leuven-chapter-36-shell "36 Running Shell Commands from Emacs"

  ;; transform shell names to what they really are
  (with-eval-after-load "sh-script"

    (add-to-list 'sh-alias-alist '(sh . bash)))

  ;; XXX Test the following (added on 2011-08-03)
  ;; (when (eq system-type 'windows-nt)
  ;;   ;; Workaround for Cygwin shell, when set 'CYGWIN=noglob'.  By default
  ;;   ;; shell-quote-argument' quoted by double '\' chars, this cause failure.
  ;;   (defun shell-quote-argument (argument)
  ;;     (concat "'" argument "'"))
  ;;   ;; Workaround for Cygwin when 'shell-file-name' is 'bash'.
  ;;   (setq null-device "/dev/null"))
  ;;
  ;; ;; Use shell from Cygwin/MinGW.
  ;; (setq shell-file-name "bash")
  ;; (setenv "SHELL" "/bin/bash")
  ;; (setq explicit-bash-args '("-i"))
  ;; (setq explicit-sh-args '("-i"))

;;** 36.1 Single Shell

  (leuven--section "36.1 Single Shell")

  ;; force interactive behavior (to get my handy shell aliases)
  ;; FIXME Fix for Zsh (zsh:1: command not found: shopt)
  ;; (defadvice shell-command (before leuven-shell-command activate)
  ;;   (ad-set-arg 0
  ;;               (concat "source ~/.bashrc; shopt -s -q expand_aliases;\n "
  ;;                       (ad-get-arg 0))))

  ;; for single shell commands (= "the" reference)
  (setq shell-file-name                 ; must be in the `PATH'
        (or (ignore-errors
              (file-name-nondirectory (or (executable-find "zsh")
                                          (executable-find "bash")
                                          (executable-find "sh"))))
            (when win32p "cmdproxy.exe")))

  ;; use `shell-file-name' as the default shell
  (setenv "SHELL" shell-file-name)

  ;; switch used to have the shell execute its command line argument
  (setq shell-command-switch "-c")      ; `/c' does not work with XEmacs

  ;; quote process arguments to ensure correct parsing on Windows
  (setq w32-quote-process-args t)

;;** 36.2 Interactive Shell

  (leuven--section "36.2 Interactive Shell")

  ;; for the interactive (sub)shell (and AUCTeX compilation?)
  (setq explicit-shell-file-name shell-file-name)

;;** 36.3 Shell Mode

  (leuven--section "36.3 Shell Mode")

  ;; general command-interpreter-in-a-buffer stuff (Shell, SQLi, Lisp, R,
  ;; Python, ...)
  ;; (try-require 'comint)
  ;; (with-eval-after-load "comint"

    ;; comint prompt is read only
    (setq comint-prompt-read-only t)

    ;; no duplicates in command history
    (setq-default comint-input-ignoredups t)

    ;; input to interpreter causes windows showing the buffer to scroll
    ;; (insert at the bottom)
    (setq-default comint-scroll-to-bottom-on-input t)

    ;; output to interpreter causes windows showing the buffer to scroll
    ;; (add output at the bottom)
    (setq-default comint-move-point-for-output t)

    ;; maximum size in lines for Comint buffers
    (setq comint-buffer-maximum-size (* 5 1024)) ; if the function
                                        ; `comint-truncate-buffer' is added to
                                        ; `comint-output-filter-functions'

    ;; strip `^M' characters
    (add-to-list 'process-coding-system-alist
                 '("bash" . (undecided-dos . undecided-unix)))
    (add-to-list 'process-coding-system-alist
                 '("zsh" . (undecided-dos . undecided-unix)))

    ;; show completion list when ambiguous
    (setq comint-completion-autolist t)

    (defun leuven-comint-clear-buffer ()
      "Clear the Comint buffer."
      (interactive)
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))

    (with-eval-after-load "comint"
      (define-key comint-mode-map (kbd "C-c C-k") 'leuven-comint-clear-buffer))

;; )

;;** 36.4 Shell Prompts

  (leuven--section "36.4 Shell Prompts")

  ;; regexp to match prompts in the inferior shell
  (setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

  ;; regexp to recognize prompts in the inferior process
  (setq comint-prompt-regexp shell-prompt-pattern)
                                        ;! only used if the variable
                                        ;! `comint-use-prompt-regexp' is non-nil

;;** 36.5 Shell Command History

  (leuven--section "36.5 Shell Command History")

  ;; rejects short commands
  (setq comint-input-filter
    #'(lambda (str)
        (and (not (string-match "\\`\\s *\\'" str))
             (> (length str) 2))))      ; ignore '!!' and kin

  (with-eval-after-load "comint"

    ;; cycle backwards/forwards through input history
    (define-key comint-mode-map
      (kbd "C-p") 'comint-previous-input) ; Shell
    (define-key comint-mode-map
      (kbd "<up>") 'comint-previous-input) ; Shell + RStudio
    (define-key comint-mode-map
      (kbd "C-n") 'comint-next-input)   ; Shell
    (define-key comint-mode-map
      (kbd "<down>") 'comint-next-input) ; Shell + RStudio

    ;; search backwards/forwards through input history for match for current
    ;; input
    (define-key comint-mode-map
      (kbd "M-p") 'comint-previous-matching-input-from-input) ; Shell
    (define-key comint-mode-map
      (kbd "<C-up>") 'comint-previous-matching-input-from-input) ; RStudio
    (define-key comint-mode-map
      (kbd "M-n") 'comint-next-matching-input-from-input) ; Shell
    (define-key comint-mode-map
      (kbd "<C-down>") 'comint-next-matching-input-from-input) ; RStudio

    (when (featurep 'helm-misc)
      ;; provide completion of `comint' history
      (define-key comint-mode-map
        (kbd "C-c C-l") 'helm-comint-input-ring)))

;;** 36.6 Directory Tracking

  (leuven--section "36.6 Directory Tracking")

  (defun leuven--rename-buffer-to-curdir (&optional _string)
    "Change Shell buffer's name to current directory."
    (rename-buffer (concat "*shell " default-directory "*")))

  (add-hook 'shell-mode-hook
            (lambda ()
              (leuven--rename-buffer-to-curdir)
              (add-hook 'comint-output-filter-functions
                        'leuven--rename-buffer-to-curdir nil t)))
                                        ; local to Shell comint

;;** 36.7 Options

  (leuven--section "36.7 Options")

  ;; disable command echoing
  (setq-default comint-process-echoes t) ; for Linux (not needed for Cygwin)

  (setenv "PAGER" "/usr/bin/cat")

;;** 36.8 Terminal emulator

  (leuven--section "36.8 Terminal emulator")

;;** 36.9 Term Mode

  (leuven--section "36.9 Term Mode")

  ;; managing multiple terminal buffers in Emacs
  ;; (and fixing some troubles of `term-mode': key bindings, etc.)

  ;; "multi-term on POSIX hosts has let me switch from using screen, with one
  ;; Emacs screen and lots of shell screens; to just using Emacs, with lots of
  ;; terminals inside it."

  (when (locate-library "multi-term")

    (autoload 'multi-term "multi-term"
      "Create new term buffer." t)
    (autoload 'multi-term-next "multi-term"
      "Go to the next term buffer." t)

    (setq multi-term-program shell-file-name)

    ;; (global-set-key (kbd "C-c t") 'multi-term-next)
    (global-set-key (kbd "C-c T") 'multi-term)) ; create a new one

  ;; ;; run an inferior shell, with I/O through buffer `*shell*'
  ;; (global-set-key
  ;;   (kbd "C-c !")
  ;;   (cond (win32p 'shell)
  ;;         (t 'term)))

  ;; toggle to and from the `*shell*' buffer
  (global-set-key
    (kbd "C-!")
    (lambda ()
      (interactive)
      (switch-or-start 'shell "*shell*")))

;;** 36.10 Remote Host Shell

  (leuven--section "36.10 Remote Host Shell")

  ;; load ssh.el file
  (add-to-list 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")
  (autoload 'ssh "ssh"
    "Open a network login connection via `ssh'." t)
    ;; this is to run ESS remotely on another computer in my own Emacs, or just
    ;; plain old reading remote files

  ;; See http://emacs.1067599.n5.nabble.com/SSH-inside-Emacs-td225528.html
  ;; - plink (with `dumb' terminal option?) as interactive shell
  ;; - ssh -t -t user@host
  ;; - Cygwin'ized Emacs
  ;; - MSYS (MinGW)

    ;; ;; let Emacs recognize Cygwin paths (e.g. /usr/local/lib)
    ;; (when (and win32p
    ;;            (executable-find "mount")) ; Cygwin bin directory found
    ;;   (try-require 'cygwin-mount)
    ;;   (with-eval-after-load "cygwin-mount"
    ;;     (cygwin-mount-activate)))

  (leuven--section "Utilities -- ESS")

  ;; ESS: Emacs Speaks Statistics
  (autoload 'R "ess-site"
    "Call 'R', the 'GNU S' system from the R Foundation." t)

  (autoload 'R-mode "ess-site"
    "Major mode for editing R source." t)

  (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

  ;; start R in current working directory, don't ask user
  (setq ess-ask-for-ess-directory nil)

  ;; new inferior ESS process appears in another window in the current frame
  (setq inferior-ess-same-window nil)

  (when cygwinp                         ; using R from Cygwin

    ;; safe 8.3 name for 32-bit programs
    (setq ess-program-files "c:/PROGRA~2")

    ;; safe 8.3 name for 64-bit programs
    (setq ess-program-files-64 "c:/PROGRA~1")

    ;; program name for invoking an inferior ESS with `M-x R'
    (setq inferior-R-program-name "R")) ; [default: Rterm]

  ;; accented characters on graphics
  (add-to-list 'process-coding-system-alist
               '("R.*" . iso-latin-1))

  ;; ;; display input commands in the process buffer
  ;; (setq ess-eval-visibly 'nowait)       ; but avoid Emacs hanging on large
  ;;                                       ; evaluations

  ;; default ESS indentation style
  (setq ess-default-style 'DEFAULT)

  (with-eval-after-load "ess-site"

    ;; code folding in ESS mode
    (add-hook 'ess-mode-hook 'hs-minor-mode)

    ;; prototype object browser for R, looks like dired mode
    (autoload 'ess-rdired "ess-rdired"
      "View *R* objects in a dired-like buffer." t)

)

;;* Proced

  ;; start Proced in a similar manner to Dired
  (global-set-key (kbd "C-x p") 'proced)

  (with-eval-after-load "proced"

    ;; current sort scheme for proced listing
    (setq-default proced-sort 'start)

    ;; display of Proced buffer as process tree
    (setq-default proced-tree-flag t))

)

;;* 37 (info "(emacs)Emacs Server")

(leuven--chapter leuven-chapter-37-emacs-server "37 Using Emacs as a Server"

  ;; use Emacs as a server (with the `emacsclient' program)
  (GNUEmacs
    (idle-require 'server)              ; after init
    (with-eval-after-load "server"

      ;; test whether server is (definitely) running, avoiding the message of
      ;; "server-start" while opening another Emacs session
      (or (equal (server-running-p) t)

          ;; start the Emacs server
          (server-start))               ; ~ 0.20 s

      ;; save file without confirmation before returning to the client
      (defadvice server-edit (before save-buffer-if-needed activate)
        "Save current buffer before marking it as done."
        (when server-buffer-clients (save-buffer)))))

)                                       ; chapter 37 ends here

;;* 38 (info "(emacs)Printing")

(leuven--chapter leuven-chapter-38-printing "38 Printing Hard Copies"

  ;; print Emacs buffer on line printer
  ;; for {lpr,print}-{buffer,region}
  (with-eval-after-load "lpr"

    ;; name of program for printing a file
    (setq lpr-command (executable-find "enscript"))
                                    ; TODO Install `enscript'

    ;; list of strings to pass as extra options for the printer program
    (setq lpr-switches (list "--font=Courier8"
                             "--header-font=Courier10"
                             (format "--header=%s" (buffer-name))))

    ;; name of a printer to which data is sent for printing
    (setq printer-name
          (cond (win32p "//PRINT-SERVER/Brother HL-4150CDN") ; XXX
                (t t))))

  (defun leuven-ps-print-buffer-with-faces-query ()
    "Query user before printing the buffer."
    (interactive)
    (when (y-or-n-p "Are you sure you want to print this buffer? ")
      (ps-print-buffer-with-faces)))

  ;; generate and print a PostScript image of the buffer
  (GNUEmacs
    (when win32p
      ;; override `Print Screen' globally used as a hotkey by Windows
      (w32-register-hot-key (kbd "<snapshot>"))
      (global-set-key
        (kbd "<snapshot>") 'leuven-ps-print-buffer-with-faces-query)))

  (global-set-key (kbd "M-p") 'leuven-ps-print-buffer-with-faces-query)

  (XEmacs
    (setq toolbar-print-function 'ps-print-buffer-with-faces))

  ;; print text from the buffer as PostScript
  (with-eval-after-load "ps-print"

    (defvar gsprint-program
      (concat windows-program-files-dir "Ghostgum/gsview/gsprint.exe")
      "Defines the Windows path to the gsview executable.")

    (leuven--file-exists-and-executable-p gsprint-program)

    (if (and gsprint-program
             (executable-find gsprint-program))

        (progn
          ;; name of a local printer for printing PostScript files
          ;; adjusted to run Ghostscript
          (setq ps-printer-name t)

          ;; name of program for printing a PostScript file
          ;; tell Emacs where Ghostscript print utility is located
          (setq ps-lpr-command gsprint-program)

          ;; list of extra switches to pass to `ps-lpr-command'
          ;; tell Ghostscript to query which printer to use
          (setq ps-lpr-switches '("-query")))
                                        ; '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")

      (setq ps-printer-name "//PRINT-SERVER/Brother HL-4150CDN") ; XXX
      (setq ps-lpr-command "")
      (setq ps-lpr-switches '("raw")))

    ;; (setq ps-error-handler-message 'system)

    ;; size of paper to format for
    (setq ps-paper-type 'a4)
    (setq ps-warn-paper-type nil)

    ;; print in portrait mode
    (setq ps-landscape-mode nil)

    ;; (setq ps-print-control-characters nil)

    ;; number of columns
    (setq ps-number-of-columns 1)

    (setq ps-left-margin 40)
    (setq ps-right-margin 56)
    (setq ps-bottom-margin 22)
    (setq ps-top-margin 32)

    ;; Page layout: Header [file-name     2001-06-18 Mon]
    (setq ps-print-header-frame nil)    ; no box around the header
    ;; see http://www.emacswiki.org/emacs/PsPrintPackage-23
    (setq ps-header-frame-alist '((fore-color . "#CCCCCC")))
    (setq ps-header-lines 1)
    (setq ps-header-font-family 'Helvetica)
    ;; (setq ps-header-font-size 11)
    (setq ps-header-title-font-size 11)
    (defun ps-time-stamp-yyyy-mm-dd-aaa ()
      "Return date as \"2001-06-18 Mon\" (ISO date + day of week)."
      (format-time-string "%Y-%m-%d %a"))
    (setq ps-right-header '(ps-time-stamp-yyyy-mm-dd-aaa))

    ;; Page layout: Footer [                         n/m]
    (setq ps-footer-offset 14)
    (setq ps-footer-line-pad .50)
    (setq ps-print-footer t)
    (setq ps-print-footer-frame nil)    ; no box around the footer
    (setq ps-footer-frame-alist '((fore-color . "#666666")))
    (setq ps-footer-lines 1)
    (setq ps-footer-font-family 'Helvetica)
    (setq ps-footer-font-size 8)
    (setq ps-left-footer nil)
    (setq ps-right-footer (list "/pagenumberstring load")) ; Page n of m

    (setq ps-font-family 'Courier)      ; see `ps-font-info-database'
    ;; legitimate values include Courier, Helvetica, NewCenturySchlbk,
    ;; Palatino and Times
    (setq ps-font-size 9.1)

    (setq ps-use-face-background t)

    (setq ps-line-spacing 3))

)                                       ; chapter 38 ends here

;;* 39 (info "(emacs)Sorting") Text

(leuven--chapter leuven-chapter-39-sorting "39 Sorting Text"

  ;; key binding
  (global-set-key (kbd "C-c ^") 'sort-lines)

)                                       ; chapter 39 ends here

;;* 42 (info "(emacs)Saving Emacs Sessions")

(leuven--chapter leuven-chapter-42-saving-emacs-sessions "42 Saving Emacs Sessions"

  (try-require 'saveplace)
  (with-eval-after-load "saveplace"

    ;; automatically save place in each file
    (setq-default save-place t)         ; default value for all buffers

    ;; name of the file that records `save-place-alist' value
    (setq save-place-file
          (convert-standard-filename "~/.emacs.d/.places"))
                                        ;! a .txt extension would load `org' at
                                        ;! the time Emacs is killed (if not
                                        ;! already loaded)!

    ;; do not make backups of master save-place file
    (setq save-place-version-control "never"))

)                                       ; chapter 42 ends here

;;* 45 (info "(emacs)Hyperlinking")

(leuven--chapter leuven-chapter-45-hyperlinking "45 Hyperlinking and Navigation Features"

  ;; use proxy
  (setq url-proxy-services              ;! Emacs expects just hostname and port
                                        ;! in `url-proxy-services', NOT prefixed
                                        ;! with "http://"
        `(("http"     . ,(getenv "http_proxy"))
          ("ftp"      . ,(getenv "http_proxy"))
          ("no_proxy" . "^.*example.com")))
          ;; disable proxy for some hosts

;;** pass a URL to a WWW browser

  (leuven--section "pass a URL to a WWW browser")

  ;; default browser started when you click on some URL in the buffer
  (setq browse-url-browser-function
        (cond ((or win32p cygwinp)
               'browse-url-default-windows-browser)
              (macp
               'browse-url-default-macosx-browser)
              ((not (display-graphic-p)) ; console
               'w3m-browse-url)
              (t
               'browse-url-generic)))

  ;; name of the browser program used by `browse-url-generic'
  (setq browse-url-generic-program (executable-find "firefox"))
                                        ; could be `google-chrome'

  (defun leuven--browse (url)
    "If prefix is specified, use the system default browser, else use the
  configured Emacs one."
    (require 'browse-url)
    (if current-prefix-arg
        ;; open in your desktop browser (firefox here)
        (when url (browse-url-default-browser url))
      ;; open using your Emacs browser (whatever that is configured to)
      (if url (browse-url url) (call-interactively 'browse-url))))

  (defun leuven-browse-url (&optional url)
    "Browse the URL passed in."
    (interactive)
    (require 'w3m)
    (require 'browse-url)
    (setq url (or url
                  (w3m-url-valid (w3m-anchor))
                  (browse-url-url-at-point)
                  (find-tag-default)))
    (setq url (read-string (format "URL \"%s\" :" url) url nil url))
    (leuven--browse url))

;;** Web search

  (leuven--section "Web search")

  (defconst leuven--google-maxlen (* 32 7)
    "Maximum length of search string to send.
  This prevents you from accidentally sending a 5 MB query string.")

  (defun leuven-google-search ()
    "Prompt for a query in the minibuffer, launch the web browser and
  query Google."
    (interactive)
    (let ((query (read-from-minibuffer "Google Search: ")))
      (browse-url (concat "http://www.google.com/search?q="
                          (url-hexify-string query)))))

  ;; (defun google-it (search-string)
  ;;   "Search for SEARCH-STRING on Google."
  ;;   (interactive "sSearch for: ")
  ;;   (browse-url (concat "http://www.google.com/search?q="
  ;;                   (url-hexify-string
  ;;                     (encode-coding-string search-string 'utf-8)))))

  (defun leuven-google-search-word-at-point ()
    "Google the word at point."
    (interactive)
    (browse-url
     (concat "http://www.google.com/search?q=" (find-tag-default))))

  (defun leuven-google-search-region (prefix start end)
    "Create a search URL and send it to the web browser."
    (interactive "P\nr")
    (if (> (- end start) leuven--google-maxlen)
        (message "Search string too long!")
      (let ((query (buffer-substring-no-properties start end)))
        (browse-url
         (concat "http://www.google.com/search?q="
                 (url-hexify-string query))))))

  ;; (defun google-search-selection ()
  ;;   "Create a Google search URL and send it to your web browser."
  ;;   (interactive)
  ;;   (let (start end term url)
  ;;     (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
  ;;         (progn
  ;;           (setq start (region-beginning)
  ;;                 end   (region-end))
  ;;           (if (> (- start end) leuven--google-maxlen)
  ;;               (setq term (buffer-substring
  ;;                           start (+ start leuven--google-maxlen)))
  ;;             (setq term (buffer-substring start end)))
  ;;           (google-it term))
  ;;       (beep)
  ;;       (message "Region not active"))))


       (defun google (what)
         "Use Google to search for WHAT."
         (interactive "sSearch: ")
         (save-window-excursion
           (delete-other-windows)
           (let ((dir default-directory))
             (w3m-browse-url (concat "http://www.google.com/search?q="
                                     (w3m-url-encode-string what)))
             (cd dir)
             (recursive-edit))))
       (global-set-key (kbd "C-c g s") 'google)

  (defun pm/region-or-word (prompt)
    "Read a string from the minibuffer, prompting with PROMPT.
  If `transient-mark-mode' is non-nil and the mark is active, it defaults to the
  current region, else to the word at or before point.  This function returns a
  list (string) for use in `interactive'."
    (list (read-string prompt (or (and (use-region-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                                  (current-word)))))

  (defun pm/google (string)
    "Ask a WWW browser to Google STRING.
  Prompt for a string, defaulting to the active region or the current word at or
  before point."
    (interactive (pm/region-or-word "Google: "))
    (browse-url (concat "http://google.com/search?num=100&q=" string)))

  (defvar leuven--google-prefix-map (make-sparse-keymap)
    "Keymap for my Google commands.")

  ;;     (global-set-key (kbd "M-s") 'leuven-google-search-region)

  (global-set-key (kbd "C-c g") leuven--google-prefix-map)

  (define-key leuven--google-prefix-map
    (kbd "g") 'leuven-google-search)

  (define-key leuven--google-prefix-map
    (kbd "w") 'leuven-google-search-word-at-point)

  (define-key leuven--google-prefix-map
    (kbd "r") 'leuven-google-search-region)

;;** Emacs-w3m

  (leuven--section "Emacs-w3m")

  ;; only use if `w3m' command is available on system
  (when (executable-find "w3m")

    ;; name of the executable file of the `w3m' command
    (setq w3m-command "w3m")
    ;; I don't want `/usr/bin/w3m' (which requires `cygwin-mount')

    ;; `w3m' slows down the startup process dramatically
    (try-require 'w3m-autoloads)
    (if (not (featurep 'w3m-autoloads))
      (autoload 'w3m "w3m"
        "Visit the WWW page using w3m." t)
      (autoload 'w3m-find-file "w3m"
        "Find a local file using emacs-w3m." t)
      (autoload 'w3m-browse-url "w3m"
        "Ask emacs-w3m to show a URL." t))

    (with-eval-after-load "w3m"

;;*** 3.1 Browsing Web Pages

      ;; go ahead, just try it
      (defun leuven-w3m-goto-url ()
        "Type in directly the URL to visit (avoiding to hit `C-k')."
        (interactive)
        (let ((w3m-current-url ""))
          (call-interactively 'w3m-goto-url)))

      ;; make w3m stop "stealing" my arrow keys, allowing to move the
      ;; cursor down the lines of an HTML email (in Gnus)
      (setq w3m-minor-mode-map nil)

      (define-key w3m-mode-map (kbd "U") 'leuven-w3m-goto-url)

      ;; fix inappropriate key bindings for moving from place to place in a page
      ;; (let the cursor keys behave normally, don't jump from link to link)
      (define-key w3m-mode-map (kbd "<up>") 'previous-line)
      (define-key w3m-mode-map (kbd "<down>") 'next-line)
      (define-key w3m-mode-map (kbd "<left>") 'backward-char)
      (define-key w3m-mode-map (kbd "<right>") 'forward-char)

      (define-key w3m-mode-map (kbd "<tab>") 'w3m-next-anchor)

      ;; moving from page to page
      (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)

;;*** 3.5 Using Tabs

      (define-key w3m-mode-map (kbd "<C-tab>") 'w3m-next-buffer)
      (define-key w3m-mode-map (kbd "<C-S-tab>") 'w3m-previous-buffer)

      (defun w3m-new-tab ()
        (interactive)
        (w3m-copy-buffer nil nil nil t))

      (define-key w3m-mode-map (kbd "C-t") 'w3m-new-tab)

      (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)

;;*** 5.1 General Variables

      ;; send referers only when both the current page and the target page are
      ;; provided by the same server
      (setq w3m-add-referer 'lambda)

      ;; home page
      (setq w3m-home-page "http://www.emacswiki.org/")

      ;; number of steps in columns used when scrolling a window horizontally
      (setq w3m-horizontal-shift-columns 1)  ; 2

      ;; proxy settings
      (when (string= (upcase (system-name)) "PC3701")
        (setq w3m-command-arguments
                 (nconc w3m-command-arguments
                        '("-o" "http_proxy=proxy:8080"))))
                                 ; FIXME https_proxy for HTTPS support

      (setq w3m-no-proxy-domains '("localhost" "127.0.0.1"))

;;*** 5.2 Image Variables

      ;; always display images
      (setq w3m-default-display-inline-images t)

      ;; show favicon images if they are available
      (setq w3m-use-favicon t)

;;*** 5.4 Cookie Variables

      ;; functions for cookie processing
      (with-eval-after-load "w3m-cookie"

        ;; ask user whether accept bad cookies or not
        (setq w3m-cookie-accept-bad-cookies 'ask)

        ;; list of trusted domains
        (setq w3m-cookie-accept-domains
              '("google.com" "google.be"
                "yahoo.com" ".yahoo.com" "groups.yahoo.com"
                "www.dyndns.org")))

      ;; enable cookies (mostly required to use sites such as Gmail)
      (setq w3m-use-cookies t)

;;*** 5.14 Other Variables

      ;; list of content types, regexps (matching a url or a file
      ;; name), commands to view contents, and filters to override the
      ;; content type specified at first
      (setq w3m-content-type-alist
            (append '(("text/html" "\\.xhtml\\'" nil nil))
                    w3m-content-type-alist))

      ;; toggle a minor mode showing link numbers
      (try-require 'w3m-lnum)
      (with-eval-after-load "w3m-lnum"

        (defun leuven-w3m-go-to-link-number ()
          "Turn on link numbers and ask for one to go to."
          (interactive)
          (let ((active w3m-lnum-mode))
            (when (not active) (w3m-lnum-mode))
            (unwind-protect
                (w3m-move-numbered-anchor (read-number
                                           "Anchor number: "))
              (when (not active) (w3m-lnum-mode))
              (w3m-view-this-url))))

        (define-key w3m-mode-map (kbd "f") 'leuven-w3m-go-to-link-number)

        ;; enable link numbering mode by default
        (add-hook 'w3m-mode-hook 'w3m-lnum-mode))

      ))

;;** Babel

  (leuven--section "Babel")

  ;; interface to web translation services such as Babelfish
  (when (locate-library "babel")

    (autoload 'babel "babel"
      "Use a web translation service to translate the message MSG." t)
    (autoload 'babel-region "babel"
      "Use a web translation service to translate the current region." t)
    (autoload 'babel-as-string "babel"
      "Use a web translation service to translate MSG, returning a string." t)
    (autoload 'babel-buffer "babel"
      "Use a web translation service to translate the current buffer." t)

    (defun leuven-babel-translate ()
      "Translate using many online translators."
      (interactive)
      (require 'babel)
      (let (source)
        (switch-to-buffer "*leuven--translate*")
        (erase-buffer)
        (yank)
        (setq source (buffer-substring-no-properties (point-min) (point-max)))
        (erase-buffer)
        (insert "--- Source ---\n\n")
        (insert source)
        (insert "\n\n\n--- Translation FR -> EN done by FreeTranslation ---\n\n")
        (insert (babel-work source "fr" "en" 'babel-free-fetch 'babel-free-wash))
        (insert "\n\n\n--- Translation EN -> FR done by FreeTranslation ---\n\n")
        (insert (babel-work source "en" "fr" 'babel-free-fetch 'babel-free-wash)))))

)

;;* 46 Other (info "(emacs)Amusements")

(leuven--chapter leuven-chapter-46-amusements "46 Other Amusements"

  ;; define a default menu bar
  (with-eval-after-load "menu-bar"

    ;; get rid of the Games in the Tools menu
    (define-key menu-bar-tools-menu [games] nil))

)                                       ; chapter 46 ends here

;;* 48 (info "(emacs)Customization")

(leuven--chapter leuven-chapter-48-customization "48 Customization"

  (GNUEmacs24
    (ignore-errors
      (load-theme 'leuven t)))

  ;; color sort order for `list-colors-display'
  (setq list-colors-sort '(rgb-dist . "#FFFFFF"))

  (XEmacs
    ;; the real color theme functions
    (try-require 'color-theme-autoloads)
    (with-eval-after-load "color-theme-autoloads"

      ;; `color-theme-print' allows to keep what you see

      ;; initialize the color theme package
      (if (fboundp 'color-theme-initialize)
          (color-theme-initialize))

      ;; color themes will be installed for all frames
      (setq color-theme-is-global t)

      ;; set my default color theme
      (try-require 'color-theme-leuven)
      (with-eval-after-load "color-theme-leuven"
        (color-theme-leuven)))

    ;; save whatever changes you make to the faces (colors and other font
    ;; properties)
    (setq options-save-faces t))

  ;; ;; limit serving to catch infinite recursions for you before they
  ;; ;; cause actual stack overflow in C, which would be fatal for Emacs
  ;; (setq max-lisp-eval-depth 600)        ; 1000?

  ;; limit on number of Lisp variable bindings & unwind-protects
  (setq max-specpdl-size 3000)          ; XEmacs 21.5.29

  ;; speed up things by preventing garbage collections
  (setq gc-cons-threshold 3500000)      ; make Gnus fast
                                        ; from (info "(gnus)FAQ 9-2")

  ;; don't display messages at start and end of garbage collection (as it hides
  ;; too many interesting messages)
  (setq garbage-collection-messages nil)

;;** 48.3 (info "(emacs)Variables")

  (leuven--section "48.3 (emacs)Variables")

  ;; file local variables specifications are obeyed, without query --
  ;; RISKY!
  (setq enable-local-variables t)

  ;; obey `eval' variables -- RISKY!
  (setq enable-local-eval t)

  ;; record safe values for some local variables
  (setq safe-local-variable-values
        '((TeX-master . t)
          (ac-sources . (ac-source-words-in-buffer ac-source-dictionary))
          (flycheck-emacs-lisp-initialize-packages . t)
          (flycheck-mode . nil)
          (flyspell-mode . -1)
          (flyspell-mode . 1)
          (ispell-local-dictionary . "american")
          (ispell-local-dictionary . "francais")
          (org-tags-column . -80)       ; org-issues.org
          (outline-minor-mode)
          (whitespace-style face tabs spaces trailing lines
                            space-before-tab::space newline indentation::space
                            empty space-after-tab::space space-mark tab-mark
                            newline-mark)))

;;** 48.4 Customizing (info "(emacs)Key Bindings")

  (leuven--section "48.4 Customizing (emacs)Key Bindings")

  ;; print the key bindings in a tabular form
  (GNUEmacs
   (defun leuven-keytable (arg)
     "Print the key bindings in a tabular form."
     (interactive "sEnter a modifier string:")
     (with-output-to-temp-buffer "*Key table*"
       (let* ((i 0)
              (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
                          "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x"
                          "y" "z" "<return>" "<down>" "<up>" "<right>"
                          "<left>" "<home>" "<end>" "<f1>" "<f2>" "<f3>"
                          "<f4>" "<f5>" "<f6>" "<f7>" "<f8>" "<f9>"
                          "<f10>" "<f11>" "<f12>" "1" "2" "3" "4" "5" "6"
                          "7" "8" "9" "0" "`" "~" "!" "@" "#" "$" "%" "^"
                          "&" "*" "(" ")" "-" "_" "=" "+" "\\" "|" "{" "["
                          "]" "}" ";" "'" ":" "\"" "<" ">" "," "." "/" "?"
                          ))
              (n (length keys))
              (modifiers (list "" "S-" "C-" "M-" "M-C-"))
              (k))
         (or (string= arg "") (setq modifiers (list arg)))
         (setq k (length modifiers))
         (princ (format " %-10.10s |" "Key"))
         (let ((j 0))
           (while (< j k)
             (princ (format " %-28.28s |" (nth j modifiers)))
             (setq j (1+ j))))
         (princ "\n")
         (princ (format "_%-10.10s_|" "__________"))
         (let ((j 0))
           (while (< j k)
             (princ (format "_%-28.28s_|"
                            "_______________________________"))
             (setq j (1+ j))))
         (princ "\n")
         (while (< i n)
           (princ (format " %-10.10s |" (nth i keys)))
           (let ((j 0))
             (while (< j k)
               (let* ((binding
                       (key-binding (read-kbd-macro
                                     (concat (nth j modifiers)
                                             (nth i keys)))))
                      (binding-string "_"))
                 (when binding
                   (if (eq binding 'self-insert-command)
                       (setq binding-string (concat "'" (nth i keys) "'"))
                     (setq binding-string (format "%s" binding))))
                 (setq binding-string
                       (substring binding-string 0
                                  (min (length binding-string) 28)))
                 (princ (format " %-28.28s |" binding-string))
                 (setq j (1+ j)))))
           (princ "\n")
           (setq i (1+ i)))
         (princ (format "_%-10.10s_|" "__________"))
         (let ((j 0))
           (while (< j k)
             (princ (format "_%-28.28s_|"
                            "_______________________________"))
             (setq j (1+ j))))))
     (delete-window)
     (setq truncate-lines t)))

;;** 48.5 The (info "(emacs)Syntax") Table

  (leuven--section "48.5 The (emacs)Syntax Table")

  ;; now '-' is not considered a word-delimiter
  ;; (add-hook 'emacs-lisp-mode-hook
  ;;           (lambda ()
  ;;             (modify-syntax-entry ?- "w")))

)                                       ; chapter 48 ends here

;;* Emacs Display

(leuven--chapter leuven-chapter-XX-emacs-display "XX Emacs Display"

;;** (info "(elisp)Faces")

  (leuven--section "Faces")

  (defun merge-x-resources ()
    (let ((file (file-name-nondirectory (buffer-file-name))))
      (when (or (string= file ".Xdefaults")
                (string= file ".Xresources"))
        (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
        (message (format "Merged %s into X resource database" file)))))

  (add-hook 'after-save-hook 'merge-x-resources)

  ;; allow any scalable font
  (when win32p
    (setq scalable-fonts-allowed t))

)

;;* App G Emacs and (info "(emacs)Microsoft Windows/MS-DOS")

(leuven--chapter leuven-chapter-AppG-ms-dos "Appendix G Emacs and MS-DOS"

  ;; divide key (needed in GNU Emacs for Windows)
  (GNUEmacs
    (global-set-key (kbd "<kp-divide>") (kbd "/")))

  ;; numeric keypad (needed in XEmacs for Windows)
  (XEmacs
    ;; keys to the right of the regular keyboard
    (define-key key-translation-map [kp-divide]     [?/])
    (define-key key-translation-map [kp-multiply]   [?*])
    (define-key key-translation-map [kp-subtract]   [?-])
    (define-key key-translation-map [kp-add]        [?+])
    (define-key key-translation-map [kp-enter]     [?\r])
    (define-key key-translation-map [kp-decimal]    [?.])

    ;; keys with digits
    (define-key key-translation-map [kp-0]          [?0])
    (define-key key-translation-map [kp-1]          [?1])
    (define-key key-translation-map [kp-2]          [?2])
    (define-key key-translation-map [kp-3]          [?3])
    (define-key key-translation-map [kp-4]          [?4])
    (define-key key-translation-map [kp-5]          [?5])
    (define-key key-translation-map [kp-6]          [?6])
    (define-key key-translation-map [kp-7]          [?7])
    (define-key key-translation-map [kp-8]          [?8])
    (define-key key-translation-map [kp-9]          [?9])

    ;; additional keypad duplicates of keys ordinarily found elsewhere
    (define-key key-translation-map [kp-left]     [left])
    (define-key key-translation-map [kp-right]   [right])
    (define-key key-translation-map [kp-up]         [up])
    (define-key key-translation-map [kp-down]     [down])
    (define-key key-translation-map [kp-begin]   [begin])
    (define-key key-translation-map [kp-home]     [home])
    (define-key key-translation-map [kp-end]       [end])
    (define-key key-translation-map [kp-next]     [next])
    (define-key key-translation-map [kp-prior]   [prior])
    (define-key key-translation-map [kp-insert] [insert])
    (define-key key-translation-map [kp-delete] [delete]))

)                                       ; chapter G ends here

;;* Profiler

  (setq profiler-report-cpu-line-format
    '((100 left)
                                        ; The 100 above is increased from the
                                        ; default of 50 to allow the deeply
                                        ; nested call tree to be seen.
      (24 right ((19 right)
                 (5 right)))))

;; Recovery from Problems

;;* Reporting Bugs

(leuven--chapter leuven-chapter-99-debugging "99 Debugging"

  ;; get the backtrace when uncaught errors occur
  (setq debug-on-error nil))            ; was set to `t' at beginning of buffer

(when (and (string-match "GNU Emacs" (version))
           leuven-load-verbose)
  (ad-disable-advice 'message 'before 'leuven-when-was-that)
  (ad-update 'message))

(when leuven-load-verbose
  (message "| Chapter | Time |")
  (message "|---------+------|")
  (mapcar (lambda (el)                  ; FIXME use mapc or dolist
            (message el))
          (nreverse leuven--load-times-list))
  (message "|---------+------|")
  (message "|         | =vsum(@-I..@-II) |"))

(message "Loading `%s'...done (in %.3f s)"
         load-file-name
         (- (float-time) leuven-before-time))
(sit-for 0.3)

(message "* --[ Loaded Emacs Leuven 20140904.1437]--")

(provide 'emacs-leuven)


;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "american"
;; eval: (when (locate-library "rainbow-mode") (require 'rainbow-mode) (rainbow-mode))
;; flycheck-mode: nil
;; flycheck-emacs-lisp-initialize-packages: t
;; End:

;;; emacs-leuven.el ends here
