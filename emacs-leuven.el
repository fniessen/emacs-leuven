;;; emacs-leuven.el --- Emacs configuration file with more pleasant defaults

;; Copyright (C) 1999-2013 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven
;; Version: 20130809.1045
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
;;     (setq leuven-packages nil)
;;
;; For help on the Emacs Editor, see (info "(emacs)")  <== `C-x C-e' here!

;;; Citations:

;; - Show me your ~/.emacs and I will tell you who you are.
;;   -- Bogdan Maryniuk
;;
;; - Emacs is like a laser guided missile. It only has to be slightly
;;   mis-configured to ruin your whole day.
;;   -- Sean McGrath
;;
;; - While any text editor can save your files, only Emacs can save your soul.
;;   -- Per Abrahamsen

;;; Code:

;; This file is only provided as an example. Customize it to your own taste!

(message "* --[ Loading Emacs Leuven 20130809.1045]--")

;; uptimes
(when (string-match "XEmacs" (version))
  ;; XEmacs doesn't have `float-time'
  (defun float-time ()
    "Convert `current-time' to a floating point number."
    (multiple-value-bind (s0 s1 s2) (current-time)
      (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))

(defconst leuven-before-time (float-time)
  "Value of `float-time' before loading the Emacs Leuven library.")

;; turn on Common Lisp support
(eval-when-compile (require 'cl)) ;; provide useful things like `loop' and `setf'

;;; User Customizable Internal Variables

(defgroup emacs-leuven nil
  "Emacs Leuven."
  :group 'files
  :group 'comm)

(defcustom leuven-load-verbose nil
  "If non-nil, means show messages describing progress of loading Emacs Leuven."
  :group 'emacs-leuven
  :type 'integer)

(when (and (string-match "GNU Emacs" (version))
           leuven-load-verbose)
  (defadvice message (before leuven-when-was-that activate)
    "Add timestamps to `message' output."
    (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T.")
                          (substring (format-time-string "%N") 0 3)
                          (format-time-string "] ")
                          (ad-get-arg 0)))))

;; allow quick include/exclude of setup parts -- DO NOT EDIT the DEFVAR!
(defvar leuven-chapter-0-environment t) ; required
(defvar leuven-chapter-0-loading-libraries t) ; required
(defvar leuven-chapter-0-debugging t)
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
(defvar leuven-chapter-47-packages t)
(defvar leuven-chapter-48-customization t)
(defvar leuven-chapter-AppG-ms-dos t)
(defvar leuven-chapter-XX-emacs-display t)
(defvar leuven-chapter-99-debugging t)

(defvar leuven--load-times-list nil
  "List of chapters and time to load them.")

(defmacro leuven--chapter (chapterid chaptername &rest body)
  "Wrap Lisp expressions as CHAPTERNAME, evaluated only when CHAPTERID is not
  nil. Save execution times in the global list `leuven--load-times-list'."
  `(when ,chapterid
     (let (before-chapter-time
           this-chapter-time)
       (when leuven-load-verbose
         (message "** %s" ,chaptername))
       (setq before-chapter-time (float-time))
       (setq leuven--before-section-time (float-time)) ;; init section time
       (progn ,@body)
       (leuven--section (concat "[" ,chaptername " ends here]") 'end-of-chapter) ;; add fake closing section
       (setq this-chapter-time
             (format "%.3f" (- (float-time) before-chapter-time)))
       (add-to-list 'leuven--load-times-list
                    (concat "| " ,chaptername " "
                            "| " this-chapter-time " |")))))

(defvar leuven--before-section-time
  "Value of `float-time' before loading some section.")

(defun leuven--section (sectionname &optional end-of-chapter)
  "Output time taken since last saved time (in global variable
  `leuven--before-section-time')."
  (let ((this-section-time (- (float-time)
                              leuven--before-section-time)))
    (when leuven-load-verbose
      (when (not (equal this-section-time 0.000))
        (message "    Section time: %.3f s" this-section-time))
      (unless end-of-chapter (message "*** %s" sectionname)))
    ;; for next one
    (setq leuven--before-section-time (float-time))))

;;* Environment

(leuven--chapter leuven-chapter-0-environment "0 Environment"

;;** Type of OS

  (leuven--section "Type of OS")

  (defconst running-ms-windows
    (eq system-type 'windows-nt)
    "Running a native Microsoft Windows version of Emacs.")

  (defconst running-cygwin
    (eq system-type 'cygwin)
    "Running a Cygwin version of Emacs.")

  (defconst running-gnu-linux
    (eq system-type 'gnu/linux)
    "Running a GNU/Linux version of Emacs.")

  (defmacro MSWindows (&rest body)
    (list 'if running-ms-windows
          (cons 'progn body)))

  (defmacro GNULinux (&rest body)
    (list 'if running-gnu-linux
          (cons 'progn body)))

;;** Window system

  (leuven--section "Window system")

  (defconst running-x-window
    (eq window-system 'x)
    "Running X Window System.")

  (defmacro XWindow (&rest body)
    "Execute any number of forms if running X Window System."
    (list 'if running-x-window
          (cons 'progn body)))

;;** MS Windows

  ;; FIXME The path is not correct under Cygwin Emacs (gsprint.exe not found)
  (defconst windows-program-files-dir
    (if running-ms-windows
        (concat (file-name-as-directory (getenv "PROGRAMFILES")) "/") ;; double slash?
      "/usr/local/bin/")
    "Defines the default Windows Program Files folder.")

;;** Emacs version

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

) ;; chapter 0 ends here

;;* Loading Libraries of Lisp Code for Emacs

(leuven--chapter leuven-chapter-0-loading-libraries "0 Loading Libraries"

  ;; You have to put the `load-path' stuff as early as you can...

  ;; This is a list of directories where Emacs Lisp libraries (`.el' and
  ;; `.elc' files) are installed.

  ;; The most important directories are the last!

  ;; load-path enhancement
  ;; TODO Check that added directories do have a trailing slash
  ;; The GNU Emacs FAQ includes a trailing slash in the instructions
  ;; on how to add directories to one's load path via .emacs [1]:
  ;; (add-to-list 'load-path "/dir/subdir/")
  ;; [1] See 5.17 here: http://www.gnu.org/software/emacs/emacs-faq.html
  (defun leuven-add-to-load-path (this-directory)
    "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
    (when (and this-directory
               (file-directory-p this-directory))
      ;; TODO Add warning if directory does not exist
      (let* ((this-directory (expand-file-name this-directory)))

        ;; directories containing a `.nosearch' file (such as
        ;; `auctex-11.86\style') should not made part of `load-path'.
        ;; TODO `RCS' and `CVS' directories should also be excluded.
        (unless (file-exists-p (concat this-directory "/.nosearch"))
          (add-to-list 'load-path this-directory)
          (when leuven-load-verbose
            (message "(info) Added `%s' to `load-path'" this-directory))))))

  ;; wrapper around `eval-after-load' (added in GNU Emacs 24.4)
  (defmacro with-eval-after-load (mode &rest body)
    "`eval-after-load' MODE evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,mode
       '(progn ,@body)))

;;*** Site-lisp

  ;; 1.
  (defvar leuven-local-site-lisp-directory "~/.emacs.d/site-lisp/"
    "Directory containing additional Emacs Lisp packages (from the Internet).")

  ;; (leuven-add-to-load-path leuven-local-site-lisp-directory
  ;;                      'add-subdirs) ;; not recursive

  (leuven-add-to-load-path leuven-local-site-lisp-directory)
  (XEmacs
    (leuven-add-to-load-path (concat leuven-local-site-lisp-directory "color-theme-6.6.0")))
  (leuven-add-to-load-path (concat leuven-local-site-lisp-directory "ecb-2.40"))

  ;; 2.
  (defvar leuven-site-lisp-directory "~/emacs/site-lisp/"
    "Directory containing my personal additional Emacs Lisp packages.")

  ;; (leuven-add-to-load-path leuven-site-lisp-directory
  ;;                      'add-subdirs)

  (leuven-add-to-load-path leuven-site-lisp-directory)
  ;; (leuven-add-to-load-path (concat leuven-site-lisp-directory "mode-abbrevs"))

  ;; remember this directory
  (setq leuven--directory
        (file-name-directory (or load-file-name (buffer-file-name))))

;;*** Development code

  (defvar leuven-local-repos-directory "~/Public/Repositories/"
    "Directory containing additional Emacs Lisp public repositories.")

  ;; No Gnus (unstable Gnus branch)
  (leuven-add-to-load-path (concat leuven-local-repos-directory "gnus/lisp"))
  ;; dev version currently necessary for BBDB mail splitting!  why?

  (leuven-add-to-load-path (concat leuven-local-repos-directory "babel"))
  (leuven-add-to-load-path (concat leuven-local-repos-directory "emacs-bookmark-extension"))

  (defun leuven--file-exists-and-executable-p (file)
    "Make sure the file FILE exists and is executable."
    (if file
        (if (file-executable-p file)
            file
          (message "(warning) Can't find executable `%s'" file)
          ;; sleep 0.5 s so that you can see the warning
          (sit-for 0.5))
      (error "leuven--file-exists-and-executable-p: missing operand")))

) ;; chapter 0-loading-libraries ends here

;;* 47 Emacs Lisp (info "(emacs)Packages")

(leuven--chapter leuven-chapter-47-packages "47 Emacs Lisp Packages"

;;** 47.2 Package Installation

  (leuven--section "47.2 Package Installation")

  ;; simple package system for GNU Emacs
  (GNUEmacs
    (when (require 'package)

      ;; archives from which to fetch
      (setq package-archives
            (append '(("org"       . "http://orgmode.org/elpa/")
                      ("melpa"     . "http://melpa.milkbox.net/packages/")
                      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("ELPA"      . "http://tromey.com/elpa/"))
                    package-archives))

      ;; ;; download the ELPA archive description if needed
      ;; (unless package-archive-contents
      ;;   (package-refresh-contents))

      ;; load the latest version of all installed packages,
      ;; and activate them (= add to `load-path' and load `XXX-autoloads.el'?)
      (package-initialize) ;; automatically called by Emacs 24 AFTER the init.el
                           ;; is loaded???

      (defcustom leuven-packages
        '(auctex
          auto-complete
          bbdb
          calfw
          circe
          dictionary
          ess
          fuzzy
          git-commit-mode
          gnuplot-mode ;; or gnuplot?
          graphviz-dot-mode
          helm
          idle-require
          ;; jabber
          leuven-theme
          ;; htmlize
          org
          org-mime ;; (from contrib)
          pager
          ;; paredit
          rainbow-mode
          redshank
          w3m
          yasnippet)
        "A list of packages to ensure are installed at Emacs startup."
        :group 'emacs-leuven
        :type '(repeat (string)))

      ;; install all packages specified in `leuven-packages' which are not built-in nor
      ;; already installed (must be run after initializing `package-initialize')
      (dolist (pkg leuven-packages)
        (if (package-installed-p pkg)
            (message "(info) Package `%s' built-in or already installed..." pkg)
          (if (yes-or-no-p (format "Install package `%s'? " pkg))
              (ignore-errors
                (package-install pkg))
            (message "Customize `leuven-packages' to ignore this package at next startup...")
            (sit-for 1.5))))

      ;; don't truncate package names in Emacs package list
      (add-hook 'package-menu-mode-hook
                (lambda ()
                  (setq tabulated-list-format
                        [("Package" 28 package-menu--name-predicate)
                         ("Version" 14 nil)
                         ("Status"  10 package-menu--status-predicate)
                         ("Description" 0 nil)])
                  (tabulated-list-init-header)))))

) ;; chapter 47 ends here

;;* Debugging

(leuven--chapter leuven-chapter-0-debugging "0 Debugging"

  ;; get the backtrace when uncaught errors occur
  (setq debug-on-error t) ;; will be unset at the end

  (XEmacs
    (setq stack-trace-on-error t))

  ;; hit `C-g' while it's frozen to get an ELisp backtrace
  (setq debug-on-quit nil)

) ;; chapter 0 ends here

  (defadvice find-file (around leuven-find-file activate)
    "Open the file named FILENAME and report time spent."
    (let ((filename (ad-get-arg 0))
          (find-file-time-start (float-time)))
      (message "(info) Finding file %s..." filename)
      ad-do-it
      (message "(info) Found file %s in %.2f s." filename
               (- (float-time) find-file-time-start))))

  ;; make loaded files give a message
  (when leuven-load-verbose
    (GNUEmacs
      (defadvice load (around leuven-load activate)
        "Execute a file of Lisp code named FILE and report time spent."
        (let ((filename (ad-get-arg 0))
              (find-file-time-start (float-time)))
          (message "(info) Loading %s..." filename)
          ad-do-it
          (message "(info) Loaded %s in %.3f s." filename
                   (- (float-time) find-file-time-start))))

      (defadvice require (around leuven-require activate)
        "Leave a trace of packages being loaded."
        (let* ((feature (ad-get-arg 0))
               (require-depth (or (and (boundp 'require-depth) require-depth)
                                  0))
               (prefix (concat (make-string (* 2 require-depth) ? ) "+-> ")))
          (cond ((featurep feature)
                 (message "(info) %sRequiring `%s'... already loaded"
                          prefix feature)
                 ;; in the case `ad-do-it' is not called, you have to set the
                 ;; return value yourself!
                 (setq ad-return-value feature))
                (t
                 (let ((time-start))
                   (message "(info) %sRequiring `%s'... %s"
                            prefix feature
                            (locate-library (symbol-name feature)))
                   (setq time-start (float-time))
                   (let ((require-depth (1+ require-depth)))
                     ad-do-it)
                   (message "(info) %sRequiring `%s'... loaded in %.3f s"
                            prefix feature
                            (- (float-time) time-start)))))))))

  (defvar leuven--missing-packages nil
    "List of packages that `try-require' or `locate-library' can't find.")

  (defadvice locate-library (around leuven-locate-library activate)
    "Locate Emacs library named LIBRARY and report time spent."
    (let ((filename (ad-get-arg 0))
          (find-file-time-start (float-time)))
      (when leuven-load-verbose
        (message "(info) Locating library %s..." filename))
      (if ad-do-it
          (when leuven-load-verbose
            (message "(info) Located library %s in %.3f s." filename
                     (- (float-time) find-file-time-start)))
        (add-to-list 'leuven--missing-packages filename 'append)
        (when leuven-load-verbose
          (message "(info) Locating library %s... missing" filename
                   (- (float-time) find-file-time-start))))))

  ;; require a feature/library if available; if not, fail silently
  (defun try-require (feature)
    "Attempt to load a library or module. Return true if the
  library given as argument is successfully loaded. If not, instead
  of an error, just add the package to a list of missing packages."
    (let (time-start)
      (condition-case err
          ;; protected form
          (progn
            (when leuven-load-verbose
              (message "(info) Checking for `%s'..." feature))
            (if (stringp feature)
                (load-library feature)
              (setq time-start (float-time))
              (require feature))
            ;; (when leuven-load-verbose
            ;;   (message "(info) Checking for `%s'... %s (loaded in %.3f s)"
            ;;            feature
            ;;            (locate-library (symbol-name feature))
            ;;            (- (float-time) time-start)))
            ;; return t (necessary for correct behavior in conditions,
            ;; when leuven-load-verbose is nil)
            t)
        ;; error handler
        (file-error ;; condition
         (progn
           (when leuven-load-verbose
             (message "(info) Checking for `%s'... missing" feature))
           (add-to-list 'leuven--missing-packages feature 'append))
         nil))))

  (if (try-require 'idle-require)

      (progn
        ;; idle time in seconds after which autoload functions will be loaded
        (setq idle-require-idle-delay 5)

        ;; time in seconds between automatically loaded functions
        (setq idle-require-load-break 2))

    ;; fail-safe for `idle-require'
    (defun idle-require (feature &optional file noerror)
      (try-require feature)))

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

) ;; chapter 1 ends here

;;* 6 (info "(emacs)Exiting") Emacs

(leuven--chapter leuven-chapter-6-exiting "6 Exiting Emacs"

  ;; quit with Alt + F4
  (global-set-key
    (kbd "<M-f4>") 'save-buffers-kill-terminal)

) ;; chapter 6 ends here

;;* 7 (info "(emacs)Basic") Editing Commands

(leuven--chapter leuven-chapter-7-basic "7 Basic Editing Commands"

;;** 7.1 (info "(emacs)Inserting Text")

  (leuven--section "7.1 (emacs)Inserting Text")

  ;; enter characters by their code in octal (for `C-q xxx <RET>')
  (setq read-quoted-char-radix 8) ;; 16 for hexadecimal (for Unicode char)

;;** 7.2 (info "(emacs)Moving Point") Location

  (leuven--section "7.2 (emacs)Moving Point Location")

  ;; don't add newlines to end of buffer when scrolling
  (setq next-line-add-newlines nil)

  ;; move point to a given line number (XEmacs default)
  (GNUEmacs
    (global-set-key
      (kbd "M-g") 'goto-line))

  ;; print the current buffer line number
  (global-set-key
    (kbd "M-G") 'what-line)

;;** 7.4 (info "(emacs)Basic Undo")ing Changes

  (leuven--section "7.4 (emacs)Basic Undoing Changes")

  ;; undo some previous changes
  (global-set-key
    (kbd "<f11>") 'undo)

  ;; redo the most recent undo
  (when (locate-library "redo")
    (autoload 'redo "redo" nil t)
    (global-set-key
      (kbd "<S-f11>") 'redo))

) ;; chapter 7 ends here

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

) ;; chapter 8 ends here

;;* 10 (info "(emacs)Help")

(leuven--chapter leuven-chapter-10-help "10 Help"

;;** 10.1 (info "(emacs)Help Summary")

  (leuven--section "10.1 (emacs)Help Summary")

  ;; find convenient unbound keystrokes (undefined key bindings)
  (autoload 'describe-unbound-keys "unbound" nil t)

  ;; avoid the description of all minor modes
  (defun describe-major-mode ()
    "Describe only `major-mode'."
    (interactive)
    (describe-function major-mode))

  ;; look up subject in (the indices of the) ELisp manual
  (global-set-key
    (kbd "C-h E") 'elisp-index-search)

;;** 10.4 (info "(emacs)Apropos")

  (leuven--section "10.4 (emacs)Apropos")

  (with-eval-after-load "apropos"

    ;; check all variables and non-interactive functions as well
    (setq apropos-do-all t))

  ;; show variables whose name matches the pattern
  (GNUEmacs
    (global-set-key
      (kbd "C-h A") 'apropos-variable))

;;** 10.8 (info "(emacs)Misc Help")

  (leuven--section "10.8 (emacs)Misc Help")

  ;; enter Info documentation browser
  (global-set-key
    (kbd "<f1>") 'info)

  ;; display symbol definitions, as found in the relevant manual
  ;; (for AWK, C, ELisp, LaTeX, M4, Makefile, Sh and other languages that have
  ;; documentation in Info)
  (global-set-key
    (kbd "<C-f1>") 'info-lookup-symbol)

  (with-eval-after-load "info"
    ;; Don't play with `Info-directory-list', it's not intended to be
    ;; settable by the user. When `info' is called, this variable is
    ;; populated from:
    ;; 1. the `INFOPATH' environment variable and/or
    ;; 2. the `Info-default-directory-list' variable -- non-existent
    ;;    directories will be removed when copied to `Info-directory-list'

    ;; list of additional directories to search for Info documentation
    ;; files (in the order they are listed)
    (setq Info-default-directory-list
          ;; FIXME `Info-additional-directory-list' does not work
          (cond (running-ms-windows
                 ;; EmacsW32 doesn't see `INFOPATH'...
                 `(
                   ,(expand-file-name
                     (concat (file-name-directory (locate-library "org"))
                             "../doc/"))
                   ,(expand-file-name (concat leuven-local-repos-directory "gnus/texi/"))
                   "c:/cygwin/usr/share/info/"
                   ,@Info-default-directory-list))
                (t
                 ;; best to set the `INFOPATH' environment variable
                 ;; (outside of Emacs, in the same shell from which you
                 ;; invoke Emacs)
                 nil)))

    (GNUEmacs
      ;; with `info+.el', you can merge an Info node with its subnodes
      ;; into the same buffer, by calling `Info-merge-subnodes' (bound to
      ;; `+')
      (try-require 'info+-XXX))  ;; error finding dir file

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
  (global-set-key
    (kbd "<S-f1>") 'man-follow)

  (with-eval-after-load "man"
    ;; make the manpage the current buffer in the current window
    (setq Man-notify-method 'pushy))

  ;; alias man to woman
  (defalias 'man 'woman)

  ;; decode and browse Unix man-pages "W.o. (without) Man"
  (with-eval-after-load "woman"
    (defalias 'man 'woman))

) ;; chapter 10 ends here

;;* 12 (info "(emacs)Killing") and Moving Text

(leuven--chapter leuven-chapter-12-killing "12 Killing and Moving Text"

;;** 12.1 (info "(emacs)Deletion and Killing")

  (leuven--section "12.1 (emacs)Deletion and Killing")

  (GNUEmacs
;; old ([2012-09-07 Fri] remove "compile" after "activate")

    ;; add the ability to copy or cut the current line without marking it
    ;; (no active region) -- idea stolen from SlickEdit
    (defadvice kill-ring-save (before leuven-slick-copy activate)
      "When called interactively with no active region, copy the current
    line instead."
      (interactive
       (if (use-region-p) (list (region-beginning) (region-end))
         (message "Copied the current line")
         (list (line-beginning-position)
               (line-beginning-position 2)))))

    (defadvice kill-region (before leuven-slick-cut activate)
      "When called interactively with no active region, kill the current
    line instead."
      (interactive
       (if (use-region-p) (list (region-beginning) (region-end))
         (list (line-beginning-position)
               (line-beginning-position 2))))))

;; new

    ;; (defadvice kill-ring-save (around leuven-slick-copy activate)
    ;;   "When called interactively with no active region, copy a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p)))
    ;;       ad-do-it
    ;;     (kill-new (buffer-substring (line-beginning-position)
    ;;                                 (line-beginning-position 2)))
    ;;     (message "Copied line")))
    ;;
    ;; (defadvice kill-region (around leuven-slick-cut activate)
    ;;   "When called interactively with no active region, kill a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p)))
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

  ;; auto-indentation of pasted code in the listed programming modes (fall
  ;; back to default, non-indented yanking by preceding the yanking
  ;; command `C-y' with `C-u')
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
    ;; (setq x-select-enable-clipboard t) ;; default in Emacs 24

    ;; make cut, copy and paste (keys and menu bar items) use the clipboard
    (menu-bar-enable-clipboard))

) ;; chapter 12 ends here

;;* 13 (info "(emacs)Registers")

(leuven--chapter leuven-chapter-13-registers "13 Registers"

  ;; ;; Enable position saving through shortcuts.
  ;; ;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
  ;; (global-set-key [C-f1] '(lambda () (interactive) (point-to-register ?1)))
  ;; (global-set-key [C-f2] '(lambda () (interactive) (point-to-register ?2)))
  ;; (global-set-key [C-f3] '(lambda () (interactive) (point-to-register ?3)))
  ;; (global-set-key [C-f4] '(lambda () (interactive) (point-to-register ?4)))

  ;; (defun jump-to-register-other (reg)
  ;; (other-window 1)
  ;; (jump-to-register reg)
  ;; (hilit-recenter (/ (window-height) 2)))

  ;; (defun jump-to-register-here (reg)
  ;; (jump-to-register reg)
  ;; (hilit-recenter (/ (window-height) 2)))

  ;; ;; Move to saved position with F1 F2 F3 and F4
  ;; (global-set-key [f1] '(lambda () (interactive) (jump-to-register-here ?1)))
  ;; (global-set-key [f2] '(lambda () (interactive) (jump-to-register-here ?2)))
  ;; (global-set-key [f3] '(lambda () (interactive) (jump-to-register-here ?3)))
  ;; (global-set-key [f4] '(lambda () (interactive) (jump-to-register-here ?4)))

;;** 13.7 (info "(emacs)Bookmarks")

  (leuven--section "13.7 (emacs)Bookmarks")

  (with-eval-after-load "bookmark"

    ;; where to save the bookmarks
    (setq bookmark-default-file "~/.emacs.d/bookmarks.bmk")
    ;;! a .txt extension would load Org at the time bookmark is required!

    ;; each command that sets a bookmark will also save your bookmarks
    (setq bookmark-save-flag 1))

  ;; extensions to standard library `bookmark.el'
  (with-eval-after-load "bookmark-XXX" ;; XXX here does not work!?

    (try-require 'bookmark+++XXX)

    ;; bookmarks to automatically highlight when jumped to
    (setq bmkp-auto-light-when-jump 'any-bookmark)

    ;; we will often be going back and forth between using Bookmark+ and
    ;; using vanilla Emacs
    (setq bmkp-propertize-bookmark-names-flag nil))

) ;; chapter 13 ends here

;;* 14 Controlling the (info "(emacs)Display")

(leuven--chapter leuven-chapter-14-display "14 Controlling the Display"

;;** 14.1 (info "(emacs)Scrolling")

  (leuven--section "14.1 (emacs)Scrolling")

  ;; keep screen position of point when scrolling
  (setq scroll-preserve-screen-position t)

  ;; better scrolling in Emacs (doing a <PageDown> followed by a <PageUp> will
  ;; place the point at the same place)
  (when (locate-library "pager")
    (autoload 'pager-page-up "pager" nil t)
    (autoload 'pager-page-down "pager" nil t)

    (global-set-key
      (kbd "<prior>") 'pager-page-up)
    (global-set-key
      (kbd "<next>") 'pager-page-down))

;;** 14.3 (info "(emacs)Auto Scrolling")

  (leuven--section "14.3 (emacs)Auto Scrolling")

  ;; ;; scroll one line at a time
  ;; (setq scroll-step 1) ;; should be on?

  ;; redisplay will never recenter point
  (setq scroll-conservatively 10000)

  ;; ;; number of lines of margin at the top and bottom of a window
  ;; (setq scroll-margin 1)

  ;; scrolling down looks much better
  (setq auto-window-vscroll nil)

  ;; ;; display update isn't paused when input is detected
  ;; (setq redisplay-dont-pause t) ;; default value

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
  (defvar leuven-highlight-regexps
    "\\(TODO\\|FIXME\\|BUG\\|XXX\\|[Ee]rror\\|ERROR\\|[Ww]arning\\|WARNING\\)"
    "Patterns to highlight.")

  (defvar leuven-highlight-org-regexps
    "\\(FIXME\\|BUG\\|XXX\\|[Ee]rror\\|[Ww]arning\\|WARNING\\)"
    "Patterns to highlight (for Org mode only, to ensure no conflict with the
  Org mode TODO keyword).")

  (defface leuven-highlight-face
    '((t (:weight normal :slant normal :box '(:line-width 1 :color "#CC0000")
          :foreground "#CC0000" :background "#FFFF88")))
    "Face for making FIXME and other warnings stand out.")

  ;; set up highlighting of special patterns for proper selected major modes
  ;; only
  (dolist (mode '(fundamental-mode
                  text-mode))
    (font-lock-add-keywords mode
     `((,leuven-highlight-regexps 1 'leuven-highlight-face prepend))))

  ;; set up highlighting of special patterns for Org mode only
  (dolist (mode '(org-mode))
    (font-lock-add-keywords mode
     `((,leuven-highlight-org-regexps 1 'leuven-highlight-face prepend))))

  ;; add fontification patterns (even in comments) to a selected major
  ;; mode *and* all major modes derived from it
  (defun leuven--highlight-special-patterns ()
    (interactive)
    (font-lock-add-keywords nil ;; in the current buffer
     `((,leuven-highlight-regexps 1 'leuven-highlight-face prepend))))
  ;; FIXME                    0                    t

  ;; set up highlighting of special patterns for selected major modes *and*
  ;; all major modes derived from them
  (dolist (hook '(prog-mode-hook
                  ;; text-mode-hook ;; avoid Org
                  css-mode-hook ;; [parent: fundamental]
                  latex-mode-hook
                  shell-mode-hook ;; [parent: fundamental] (works in *shell* buffers!)
                  ssh-config-mode-hook))
    (add-hook hook 'leuven--highlight-special-patterns))

  ;; stealth fontification should show status messages
  (setq jit-lock-stealth-verbose t)

  ;; colorize color names in buffers
  (GNUEmacs
    (when (locate-library "rainbow-mode")
      (autoload 'rainbow-mode "rainbow-mode" nil t)))

;;** 14.13 (info "(emacs)Highlight Interactively") by Matching

  (leuven--section "14.13 (emacs)Highlight Interactively by Matching")

  (GNUEmacs
    (autoload 'hlt-highlight-regexp-region "highlight"
      "Highlight regular expression REGEXP in region." t)

    (defun leuven-hlt-highlight-current-word ()
      "Highlight the word that point is on throughout the buffer."
      (interactive)
      (let ((cword (current-word t)))
        (when cword
          (save-excursion
            (hlt-highlight-regexp-region (point-min) (point-max)
                                         (regexp-quote cword))))))

    ;; emulation of Vim's `*' search
    (global-set-key
      (kbd "C-*") 'leuven-hlt-highlight-current-word)

    (with-eval-after-load "highlight"
      (global-set-key
        (kbd "C-S-p") 'hlt-previous-highlight)
      (global-set-key
        (kbd "C-S-n") 'hlt-next-highlight)))

;;** 14.15 (info "(emacs)Displaying Boundaries")

  (leuven--section "14.15 (emacs)Displaying Boundaries")

  ;; visually indicate buffer boundaries and scrolling
  (setq indicate-buffer-boundaries t)

;;** 14.16 (info "(emacs)Useless Whitespace")

  (leuven--section "14.16 (emacs)Useless Whitespace")

  ;; highlight trailing whitespaces in all modes
  (setq-default show-trailing-whitespace t)

  ;; delete all the trailing whitespaces and TABs across the current
  ;; buffer
  (defun leuven-delete-trailing-whitespaces-and-untabify ()
    "Delete all the trailing white spaces, and convert all TABs
  to multiple spaces across the current buffer."
    (interactive "*")
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))

  ;; ensure that your files have no trailing whitespace
  (add-hook 'before-save-hook
            (lambda ()
              ;; except for Message mode where "-- " is the signature
              ;; separator
              (unless (eq major-mode 'message-mode)
                (delete-trailing-whitespace))))

  ;; visually indicate empty lines after the buffer end
  (setq-default indicate-empty-lines t)

  ;; ;; control highlighting of non-ASCII space and hyphen chars, using the
  ;; ;; `nobreak-space' or `escape-glyph' face respectively
  ;; (setq nobreak-char-display t) ;; default

  (GNUEmacs
    ;; whitespace mode
    (add-hook 'text-mode-hook
              (lambda ()
                (whitespace-mode 1)))

    (add-hook 'prog-mode-hook
              (lambda ()
                (whitespace-mode 1)))

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
            '((space-mark ?\xA0 [?\u00B7] [?.]) ;; hard space - centered dot
              (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))) ;; tab - left quote mark
      ))

  ;; ;; show zero-width spaces
  ;; (font-lock-add-keywords nil
  ;;  `((,(format "\\(%c\\)" ?\u200b) ; #\ZERO_WIDTH_SPACE
  ;;     (1 (progn (compose-region (match-beginning 1) (match-end 1)
  ;;                               ?\u2b1b ; #\BLACK_LARGE_SQUARE
  ;;                               'decompose-region)
  ;;               nil)))))

;;** 14.18 (info "(emacs)Optional Mode Line") Features

  (leuven--section "14.18 (emacs)Optional Mode Line Features")

  ;; show the line number in each mode line
  (line-number-mode 1)

  ;; show the column number in each mode line
  (column-number-mode 1)

  ;; use inactive face for mode-line in non-selected windows
  (setq mode-line-in-non-selected-windows t)

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
                       ;; insert mode
                       leuven-default-cursor-color))))
        (set-cursor-color color)))

    (add-hook 'post-command-hook 'leuven--set-cursor-color-according-to-mode))

;;** 14.21 (info "(emacs)Line Truncation")

  (leuven--section "14.21 (emacs)Line Truncation")

  ;; switch wrap mode from "wrap long lines to next screen line" (continued
  ;; line) to "non-wrap", or vice-versa
  (global-set-key
    (kbd "C-c t") 'toggle-truncate-lines)

  ;; respect the value of `truncate-lines' in all windows less than the
  ;; full width of the frame
  (setq truncate-partial-width-windows nil)

;;** 14.23 (info "(emacs)Display Custom")ization

  (leuven--section "14.23 (emacs)Display Customization")

  ;; echo what I'm typing *immediately*
  (setq echo-keystrokes 0.1)

  ;; exhaustive log of interactions with Emacs (display keystrokes, etc.)
  (with-eval-after-load "interaction-log"

    ;; maximum number of lines to keep in the *Emacs Log* buffer
    (setq ilog-log-max 10)

    ;; enable logging of keys, commands, file loads and messages
    (interaction-log-mode 1)

    ;; hotkey for showing the log buffer
    (global-set-key
      (kbd "<C-f2>")
      (lambda ()
        (interactive)
        (display-buffer ilog-buffer-name))))

) ;; chapter 14 ends here

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
    ;; incremental search will open the contents
    (setq search-invisible 'open) ;; XXX

    ;; re-hide an invisible match right away
    (setq isearch-hide-immediately nil)) ;; XXX

  ;; scrolling commands are allowed during incremental search
  (setq isearch-allow-scroll t)

  (GNUEmacs
    ;; fuzzy matching utilities (a must-have)
    (when (locate-library "fuzzy")

      (add-hook 'isearch-mode-hook
                (lambda ()
                  (require 'fuzzy)))

      (with-eval-after-load "fuzzy"
       (turn-on-fuzzy-isearch))))

;;** 15.4 (info "(emacs)Regexp Search")

  (leuven--section "15.4 (emacs)Regexp Search")

  (defun leuven--buffer-matched-strings ()
    (interactive)
    (mapcar 'leuven--buffer-matched-string-nth '(0 1 2 3 4 5 6 7 8 9)))

  (defun leuven--buffer-matched-string-nth (n)
    "Return the Nth pattern-matched string from the current buffer."
    (if (and (match-beginning n) (match-end n))
        (if (> (match-end n) (match-beginning n))
            (buffer-substring (match-beginning n) (match-end n))
          "")
      nil))

;;** 15.8 (info "(emacs)Search Case")

  (leuven--section "15.8 (emacs)Search Case")

  ;; searches should ignore case by default (in all buffers that do not
  ;; override this)
  (setq-default case-fold-search t)

;;** 15.10 (info "(emacs)Other Repeating Search") Commands

  (leuven--section "15.10 (emacs)Other Repeating Search Commands")

  (defun leuven--isearch-occur ()
    "Invoke `occur' from within isearch."
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur
       (if isearch-regexp
           isearch-string
         (regexp-quote isearch-string)))))

  ;; activate `occur' easily while at the `I-search:' prompt
  (define-key isearch-mode-map
    (kbd "C-o") 'leuven--isearch-occur)

  ;; activate `occur' easily globally
  (global-set-key
    (kbd "C-o") 'occur) ;; [default: "M-s o"]

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
            moccur-grep-find))

    (setq moccur-split-word t))

) ;; chapter 15 ends here

;;* 16 Commands for (info "(emacs)Fixit") Typos

(leuven--chapter leuven-chapter-16-fixit "16 Commands for Fixing Typos"

   ;; ;; load flyspell-guess every time you start Emacs.
   ;; (require 'flyspell-guess)
   ;;
   ;; (with-eval-after-load "flyspell-guess"
   ;;   (flyspell-insinuate-guess-indicator))

;;** 16.4 Checking and Correcting (info "(emacs)Spelling")

  (leuven--section "16.4 (emacs)Checking and Correcting Spelling")

  ;; spelling checker program
  (setq ispell-program-name ;; XXX undefined
        (or (executable-find "aspell")
            (executable-find "ispell")
            ;; nil [default: "ispell"]
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
    (global-set-key
      (kbd "C-$") 'ispell-region-or-buffer)
    (global-set-key
      (kbd "C-M-$") 'ispell-change-dictionary)

    ;; ;; default dictionary to use (if `ispell-local-dictionary' is nil, that is
    ;; ;; if there is no local dictionary to use in the buffer)
    ;; (setq ispell-dictionary "american")

    ;; enable on-the-fly spell checking when changing a buffer which was
    ;; unmodified
    (add-hook 'first-change-hook
              (lambda ()
                ;; skip temporary buffers
                ;; (XXX another test could be skipping buffers not associated with a file)
                (unless (eq (aref (buffer-name) 0) ?\s) ;; buffer starts with " "
                  (cond ((derived-mode-p 'text-mode) ;; org-mode
                         (flyspell-mode 1))
                        ((derived-mode-p 'prog-mode)
                         (flyspell-prog-mode)) ;; `ispell-comments-and-strings'
                        ;; prevent flyspell from finding mistakes in the code,
                        ;; which is pretty cool
                        ))))

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

     )

    ;; don't use `M-TAB' to auto-correct the current word (only use `C-.')
    (setq flyspell-use-meta-tab nil)
    ;; FIXME M-TAB is still bound to `flyspell-auto-correct-word' when this
    ;; chunk of code is placed within (with-eval-after-load "flyspell"...)

    (with-eval-after-load "flyspell"

     ;; TEMP
     (message "flyspell has been loaded!!!")
     (sit-for 3)

     ;; don't consider that a word repeated twice is an error
     (setq flyspell-mark-duplications-flag nil)

     ;; fix the "enabling flyspell mode gave an error" bug
     (setq flyspell-issue-welcome-flag nil)

     ;; ;; don't print messages for every word (when checking the
     ;; ;; entire buffer) as it causes an enormous slowdown
     ;; (setq flyspell-issue-message-flag nil)

     ;; dash character (`-') is considered as a word delimiter
     (setq-default flyspell-consider-dash-as-word-delimiter-flag t)
     ;; '("francais" "deutsch8" "norsk")

     ;; spell-check your XHTML (by adding `nxml-text-face' to the list of
     ;; faces corresponding to text in programming-mode buffers)
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

     (defun leuven-flyspell-toggle-dictionary ()
       "Change the dictionary."
       (interactive)
       (let ((dict (or ispell-local-dictionary
                       ispell-dictionary)))
         (setq dict (if (string= dict "francais") "american" "francais"))
         (message "Switched to %S" dict)
         (sit-for 0.5)
         (ispell-change-dictionary dict)
         (when flyspell-mode
           ;; (flyspell-delete-all-overlays)
           ;; if above is executed, the advised `org-mode-flyspell-verify'
           ;; won't work anymore
           (flyspell-buffer))))

     ;; key binding
     (global-set-key
       (kbd "C-$") 'flyspell-buffer)
     (global-set-key
       (kbd "C-M-$") 'leuven-flyspell-toggle-dictionary)))

  (when (locate-library "dictionary-init")

     ;; ;; autoloads
     ;; (load-library "dictionary-init")

     (autoload 'dictionary-search "dictionary"
       "Ask for a word and search it in all dictionaries" t)
     (autoload 'dictionary-match-words "dictionary"
       "Ask for a word and search all matching words in the dictionaries" t)
     (autoload 'dictionary-lookup-definition "dictionary"
       "Unconditionally lookup the word at point." t)

     (autoload 'dictionary "dictionary"
       "Create a new dictionary buffer" t)
     (autoload 'dictionary-mouse-popup-matching-words "dictionary"
       "Display entries matching the word at the cursor" t)
     (autoload 'dictionary-popup-matching-words "dictionary"
       "Display entries matching the word at the point" t)
     (autoload 'dictionary-tooltip-mode "dictionary"
       "Display tooltips for the current word" t)
     (unless (boundp 'running-xemacs)
       (autoload 'global-dictionary-tooltip-mode "dictionary"
         "Enable/disable dictionary-tooltip-mode for all buffers" t))

     (global-set-key
       (kbd "C-c d l") 'dictionary-lookup-definition)
     (global-set-key
       (kbd "C-c d s") 'dictionary-search)
     (global-set-key
       (kbd "C-c d m") 'dictionary-match-words)

     (with-eval-after-load "dictionary"

      (global-dictionary-tooltip-mode 1)

      ;; ;; server contacted for searching the dictionary
      ;; (setq dictionary-server "localhost")

      ;; ;; connect via a HTTP proxy (using the CONNECT command)
      ;; (setq dictionary-use-http-proxy t)
      ;;
      ;; ;; name of the HTTP proxy to use
      ;; (setq dictionary-proxy-server "hellman") ; XXX
      ;;
      ;; ;; port of the proxy server
      ;; (setq dictionary-proxy-port 8080) ; XXX

      ;; use proxy
      (setq url-proxy-services
            `(("http"     . ,(getenv "http_proxy"))
              ("ftp"      . ,(getenv "http_proxy"))
              ("no_proxy" . "^.*example.com")))
              ;; disable proxy for some hosts
     ))

  ;; XXX excellent!
  (defun leuven-answers-define ()
    "Look up the word under cursor in a browser."
    (interactive)
    (browse-url
     (concat "http://www.answers.com/main/ntquery?s=" (find-tag-default))))

  (defun leuven-lookup-word-definition-in-w3m ()
    "Look up the word's definition in a emacs-w3m.\n
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

) ;; chapter 16 ends here

;;* 17 (info "(emacs)Keyboard Macros")

(leuven--chapter leuven-chapter-17-keyboard-macros "17 Keyboard Macros"

;;** 17.1 (info "(emacs)Basic Keyboard Macro") Use

  (leuven--section "17.1 (emacs)Basic Keyboard Macro Use")

  (defun leuven-toggle-kbd-macro-recording-on ()
    "Start recording a keyboard macro and toggle functionality of
  key binding."
    (interactive)
    (global-set-key
      (kbd "<S-f8>") 'leuven-toggle-kbd-macro-recording-off)
    (start-kbd-macro nil))

  (defun leuven-toggle-kbd-macro-recording-off ()
    "Stop recording a keyboard macro and toggle functionality of
  key binding."
    (interactive)
    (global-set-key
      (kbd "<S-f8>") 'leuven-toggle-kbd-macro-recording-on)
    (end-kbd-macro))

  ;; start/stop recording a keyboard macro
  (global-set-key
    (kbd "<S-f8>") 'leuven-toggle-kbd-macro-recording-on)

  ;; execute the most recent keyboard macro
  (global-set-key
    (kbd "<f8>") 'call-last-kbd-macro)

;;** 17.5 Name and (info "(emacs)Save Keyboard Macro")s

  (leuven--section "17.5 (emacs)Name and Save Keyboard Macros")

  ;; assign a name to the last keyboard macro defined
  (global-set-key
    (kbd "<C-f8>") 'name-last-kbd-macro)

) ;; chapter 17 ends here

;;* 18 (info "(emacs)Files") Handling

(leuven--chapter leuven-chapter-18-files "18 Files Handling"

;;** 18.2 (info "(emacs)Visiting") Files

  (leuven--section "18.2 (emacs)Visiting Files")

  ;; visit a file
  (global-set-key
    (kbd "<f3>") 'find-file)

;;** 18.3 (info "(emacs)Saving") Files

  (leuven--section "18.3 (emacs)Saving Files")

  ;; make your changes permanent
  (global-set-key
    (kbd "<f2>") 'save-buffer)

  ;; ensure a file ends in a newline when it is saved
  (setq require-final-newline t)
  ;; TODO Do this only for text and Fundamental modes, because I could
  ;; edit binary files (see `mode-require-final-newline')

  ;; directory used for temporary files
  (XEmacs
    (setq temporary-file-directory
          (or (file-name-as-directory (getenv "TEMP"))
              "/tmp/")))

  ;; update time stamps every time you save a buffer
  (add-hook 'before-save-hook 'time-stamp)

  ;; maintain last change time stamps (`Time-stamp: <>' occurring within
  ;; the first 8 lines) in files edited by Emacs
  (with-eval-after-load "time-stamp"

   ;; format of the string inserted by `M-x time-stamp':
   ;; `YYYY-MM-DD Weekday HH:MM' (see `system-time-locale' for non-numeric
   ;; formatted items of time)
   (setq-default time-stamp-format "%:y-%02m-%02d %3a %02H:%02M"))

  (GNUEmacs
    ;; update the copyright notice in current buffer
    (add-hook 'before-save-hook 'copyright-update))

;;** 18.4 (info "(emacs)Reverting") a Buffer

  (leuven--section "18.4 (emacs)Reverting a Buffer")

  ;; replace current buffer text with the text of the visited file on disk
  (defun leuven-revert-buffer-without-query ()
    "Unconditionally revert current buffer."
    (interactive)
    (revert-buffer t t))

  ;; key binding
  (global-set-key
    (kbd "<C-f12>") 'leuven-revert-buffer-without-query)

;;** 18.6 (info "(emacs)Auto Save"): Protection Against Disasters

  (leuven--section "18.6 (emacs)Auto Save: Protection Against Disasters")

  ;; auto-save every 100 input events
  (setq auto-save-interval 100)

  ;; auto-save after 10 seconds idle time
  (setq auto-save-timeout 10)

  ;; save backup files (i.e., `foo~' or `foo.~i~') in one common directory
  ;; (instead of in the local directory)
  (GNUEmacs
    ;; filenames matching a regexp are backed up in the corresponding
    ;; directory
    (setq backup-directory-alist
          ;; Emacs will `make-directory' it, if necessary
          '((".*" . "~/.emacs.d/backups/")))) ;; regexp => directory mappings

  (XEmacs
    (when (try-require 'backup-dir)
      ;; FIXME Use a `set' construction, with `make-local-variable'
      (make-variable-buffer-local 'backup-inhibited)
      (setq bkup-backup-directory-info
            '((t "~/.saves" ok-create full-path prepend-name)))))

  ;; always use copying to create backup files (don't clobber symlinks)
  (setq backup-by-copying t)

  ;; make numbered backups
  (setq version-control t)

  ;; ;; number of oldest versions to keep when a new numbered backup is made
  ;; (setq kept-old-versions 0) ;; 2

  ;; number of newest versions to keep when a new numbered backup is made
  (setq kept-new-versions 20) ;; 2

  ;; delete excess backup versions silently
  (setq delete-old-versions t)

  (define-minor-mode sensitive-mode
    "For sensitive files like password lists.
  It disables backup creation and auto saving in the current buffer.

  With no argument, this command toggles the mode. Non-null
  prefix argument turns on the mode. Null prefix argument
  turns off the mode."
    nil ;; initial value
    " Sensitive" ;; indicator for the mode line
    nil ;; minor mode bindings
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
  (global-set-key
    (kbd "C-=") 'compare-windows)

;;** 18.10 (info "(emacs)Diff Mode")

  (leuven--section "18.10 (emacs)Diff Mode")

  ;; extensions to `diff-mode.el' ("*Diff*" buffer is highlighted differently)
  (try-require 'diff-mode-)
  ;; this library should be loaded *before* library `diff-mode.el'

  ;; highlight the changes with better granularity
  (defun leuven--diff-make-fine-diffs ()
    "Enable Diff Auto Refine mode."
    (interactive)
    (require 'diff-mode)
    (let (diff-auto-refine-mode)
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (diff-hunk-next)
              (diff-refine-hunk)))
        (error nil))
      (run-at-time 0.0 nil
                   (lambda ()
                     (if (eq major-mode 'diff-mode)
                         ;; put back the cursor only if still in a diff buffer
                         ;; after the delay
                         (goto-char (point-min)))))))

  (defun leuven--diff-make-fine-diffs-if-necessary ()
    "Auto-refine only the regions of 14,000 bytes or less."
    ;; check for auto-refine limit
    (unless (> (buffer-size) 14000)
      (leuven--diff-make-fine-diffs)))

  (add-hook 'diff-mode-hook
            'leuven--diff-make-fine-diffs-if-necessary)

  ;; ;; ediff, a comprehensive visual interface to diff & patch
  ;; ;; setup for Ediff's menus and autoloads
  ;; (try-require 'ediff-hook)
  ;; already loaded (by Emacs?)

  (with-eval-after-load "ediff"

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
    (setq epa-file-encrypt-to "john@doe.com")
    ;; if no one is selected (""), symmetric encryption will always be
    ;; performed

    ;; cache passphrase for symmetric encryption (VERY important)
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)
    ;; Not to sound paranoid. But if you want caching, it's recommended to
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
    (setq ange-ftp-try-passive-mode t)) ;; needed for Ubuntu

;;*** TRAMP - Transparent Remote Access, Multiple Protocols

  (leuven--section "TRAMP")

  (with-eval-after-load "tramp" ;; the autoloads are predefined

;;* 4 (info "(tramp)Configuration") of TRAMP for use

;;** 4.6 Selecting a (info "(tramp)Default Method")

    ;; default transfer method
    (setq tramp-default-method ;; [default: "scp"]
          (cond (running-ms-windows
                 ;; (issues with Cygwin `ssh' which does not cooperate
                 ;; with Emacs processes -> use `plink' from PuTTY, it
                 ;; definitely does work under Windows)
                 ;;
                 ;; `C-x C-f /plink:user@host:/some/directory/file'
                 "plink")
                (t
                 "ssh")))

    ;; You might try out the `rsync' method, which saves the remote
    ;; files quite a bit faster than SSH. It's based on SSH, so it
    ;; works the same, just saves faster.

    ;; ;; 2011-07-25 New test on Windows XP
    ;; (setq tramp-default-method "ssh")
    ;;
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
    ;; that host afterwards. It is important to know that the given
    ;; method is applied on the host which has been reached so far. The
    ;; trick is to think from the end.

;;** 4.12 (info "(tramp)Password handling") for several connections

    ;; how many seconds passwords are cached
    (setq password-cache-expiry 60) ;; [default: 16]

;;** 4.15 (info "(tramp)Remote shell setup") hints

    ;; string used for end of line in rsh connections
    (setq tramp-rsh-end-of-line ;; [default: "\n"]
          (cond (running-ms-windows "\n")
                (t "\r")))

;;** 4.16 (info "(tramp)Auto-save and Backup") configuration

    ;; faster auto saves
    (setq tramp-auto-save-directory temporary-file-directory)

;;* 9 How to Customize (info "(tramp)Traces and Profiles")

    ;; debugging Tramp
    (setq tramp-verbose 6) ;; [maximum: 10]

    ;; "turn off" the effect of `backup-directory-alist' for TRAMP
    ;; files
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))

    ;; make Emacs beep after reading from or writing to the remote host
    (defadvice tramp-handle-write-region ;; XXX
      (after leuven-tramp-write-beep-advice activate)
      "Make Tramp beep after writing a file."
      (interactive)
      (beep))

    (defadvice tramp-handle-do-copy-or-rename-file ;; XXX
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
  ;;! has to be set before your require recentf
  (setq recentf-exclude
        '(
          "~$" ;; Emacs (and others) backup
          "\\.log$" ;; LaTeX
          "\\.toc$" ;; LaTeX
          "\\.aux$" ;; LaTeX
          "/tmp/"
          ))

  ;; setup a menu of recently opened files
  (GNUEmacs
    (idle-require 'recentf))

  (with-eval-after-load "recentf"

    ;; maximum number of items that will be saved
    (setq recentf-max-saved-items 100)

    ;; file to save the recent list into
    (setq recentf-save-file "~/.emacs.d/.recentf")

    ;; (when using Tramp) turn off the cleanup feature of `recentf'
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!

    ;; save file names relative to my current home directory
    (setq recentf-filename-handlers '(abbreviate-file-name))

    ;; enable `recentf' mode
    (recentf-mode 1))

  (leuven--section "FFAP")

  ;; visit a file
  (global-set-key
    (kbd "<f3>") 'find-file-at-point)

  ;; find file (or URL) at point
  (with-eval-after-load "ffap"

    ;; function called to fetch an URL
    (setq ffap-url-fetcher 'browse-url))
    ;; could be `browse-url-emacs' or `w3m-browse-url'

  (GNUEmacs

    (leuven--section "Helm")

    ;; open Helm (QuickSilver-like candidate-selection framework)
    (when (try-require 'helm-config)

      ;; sort locate results by full path
      (when running-ms-windows
        (setq helm-locate-command "es -s %s %s"))

      (global-set-key
        (kbd "<f3>") 'helm-for-files)

      (global-set-key
        (kbd "M-x") 'helm-M-x)

      ;; buffers only
      (global-set-key
        (kbd "C-x b") 'helm-buffers-list)

      ;; awesome wearing a hat
      (global-set-key
        (kbd "<f4>") 'helm-org-headlines)
      ;; (or `helm-imenu', better than `org-toc')

      (global-set-key
        (kbd "C-c o") 'helm-occur)

      (global-set-key
        (kbd "C-x r l") 'helm-bookmarks)

      ;; install from https://github.com/thierryvolpiatto/emacs-bmk-ext
      (global-set-key
        (kbd "C-x r b") 'helm-bookmark-ext)

      ;; prefix key for all Helm commands in the global map
      (setq helm-command-prefix-key "C-c C-f") ;; `C-x c'?

      ;; use the current window (no popup) to show the candidates
      (setq helm-full-frame nil)

      (defface leuven-separator-face
        '((t (:weight bold :foreground "slate gray")))
        "Face used to display state NEW.")

      ;; candidates separator of `multiline' source
      (setq helm-candidate-separator
            (propertize "--separator-------------------------------"
                        'face 'leuven-separator-face))

      ;; suppress displaying sources which are out of screen at first
      (setq helm-quick-update t)

      ;; time that the user has to be idle for, before candidates from
      ;; DELAYED sources are collected
      (setq helm-idle-delay 0.1)
      ;; useful for sources involving heavy operations, so that
      ;; candidates from the source are not retrieved unnecessarily if
      ;; the user keeps typing

      ;; time that the user has to be idle for, before ALL candidates
      ;; are collected (>= `helm-idle-delay')
      (setq helm-input-idle-delay 0.1)
      ;; also effective for NON-DELAYED sources

      ;; do not show more candidates than this limit from individual
      ;; sources
      (setq helm-candidate-number-limit 100) ;; more than one screen page

      ;;   ;; don't save history information to file
      ;;   (remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

      ;; don't show only basename of candidates in `helm-find-files'
      (setq helm-ff-transformer-show-only-basename nil)

      ;; max length of buffer names before truncate
      (setq helm-buffer-max-length 40)
      ;; (setq helm-buffer-max-length nil) ;; don't truncate anymore

      (defun helm-toggle-debug ()
        (interactive)
        (setq helm-debug (not helm-debug))
        (message "Helm Debug is now %s" (if helm-debug
                                            "Enabled"
                                          "Disabled")))))

  (leuven--section "Image mode")

  ;; show image files as images (not as semi-random bits)
  (GNUEmacs
    (add-hook 'find-file-hook
              (lambda ()
                (auto-image-file-mode 1))))

) ;; chapter 18 ends here

;;* 19 Using Multiple (info "(emacs)Buffers")

(leuven--chapter leuven-chapter-19-buffers "19 Using Multiple Buffers"

;;** 19.2 (info "(emacs)List Buffers")

  (leuven--section "19.2 (emacs)List Buffers")

  ;; rebind `C-x C-b'
  (global-set-key
    (kbd "C-x C-b") 'electric-buffer-list)
  ;; `buffer-menu' moves point in the window which lists your buffers
  ;; `electric-buffer-list' pops up a buffer describing the set of buffers

  ;; operate on buffers like Dired
  (global-set-key
    (kbd "C-x C-b") 'ibuffer)

  (with-eval-after-load "ibuffer"

    ;; completely replaces `list-buffer'
    (defalias 'ibuffer-list-buffers 'list-buffer)

    ;; don't show the names of filter groups which are empty
    (setq ibuffer-show-empty-filter-groups nil)

    ;; filtering groups
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("Chat" (mode . circe-mode))
             ("Org" (or
                     (mode . diary-mode)
                     (mode . org-mode)
                     (mode . org-agenda-mode)))
             ("LaTeX" (or (mode . latex-mode)
                          (mode . LaTeX-mode)
                          (mode . bibtex-mode)
                          (mode . reftex-mode)))
             ("Gnus & News" (or
                             (mode . message-mode)
                             (mode . bbdb-mode)
                             (mode . mail-mode)
                             (mode . gnus-group-mode)
                             (mode . gnus-summary-mode)
                             (mode . gnus-article-mode)
                             (name . "^\\(\\.bbdb\\|dot-bbdb\\)$")
                             (name . "^\\.newsrc-dribble$")
                             (mode . newsticker-mode)))
             ("Files" (filename . ".*"))
             ("Dired" (mode . dired-mode))
             ("Shell" (mode . shell-mode))
             ("Version Control" (or (mode . svn-status-mode)
                                    (mode . svn-log-edit-mode)
                                    (name . "^\\*svn-")
                                    (name . "^\\*vc\\*$")
                                    (name . "^\\*Annotate")
                                    (name . "^\\*git-")
                                    (name . "^\\*vc-")))
             ("Emacs" (or (name . "^\\*scratch\\*$")
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
             ("Emacs Source" (mode . emacs-lisp-mode))
             ("Documentation" (or
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
  (global-set-key
    (kbd "<S-f12>") 'leuven-kill-this-buffer-without-query)

;;** 19.5 (info "(emacs)Several Buffers")

  (leuven--section "19.5 (emacs)Several Buffers")

  ;; put the current buffer at the end of the list of all buffers
  (global-set-key
    (kbd "<f12>") 'bury-buffer)

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

) ;; chapter 19 ends here

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
  (global-set-key
    (kbd "<f6>") 'other-window)

  ;; reverse operation of `C-x o' (or `f6')
  (global-set-key
    (kbd "<S-f6>")
    (lambda ()
      (interactive)
      (other-window -1)))

;;** 20.5 (info "(emacs)Change Window")

  (leuven--section "20.5 (emacs)Change Window")

  ;; delete all windows in the selected frame except the selected window
  (global-set-key
    (kbd "<f5>") 'delete-other-windows)

  ;; enlarge or shrink windows more easily than with `C-x {', `C-x }' and the
  ;; like
  (global-set-key
    (kbd "<C-S-up>") 'enlarge-window)
  (global-set-key
    (kbd "<C-S-down>") 'shrink-window)
  (global-set-key
    (kbd "<C-S-left>") 'enlarge-window-horizontally)
  (global-set-key
    (kbd "<C-S-right>") 'shrink-window-horizontally)

  ;; make all visible windows the same height (approximately)
  (global-set-key
    (kbd "<C-f6>") 'balance-windows)

  ;; swap 2 windows
  (defun leuven-swap-windows ()
    "If you have 2 windows, it swaps them."
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

  (global-set-key
    (kbd "C-c ~") 'leuven-swap-windows)

  (defun leuven-toggle-window-split ()
    "Vertical split shows more of each line, horizontal split shows
  more lines. This code toggles between them. It only works for
  frames with exactly two windows."
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

  (global-set-key
    (kbd "C-c |") 'leuven-toggle-window-split)

;;** 20.6 (info "(emacs)Displaying Buffers")

  (leuven--section "20.6 (emacs)Pop Up Window")

  ;; don't allow splitting windows vertically
  (setq split-height-threshold nil)

  ;; minimum width for splitting windows horizontally
  (setq split-width-threshold 160)

  ;; (GNUEmacs
  ;;
  ;;  (defun leuven--display-buffer (buffer force-other-window)
  ;;    "If BUFFER is visible, select it.
  ;;
  ;;  If it's not visible and there's only one window, split the
  ;;  current window and select BUFFER in the new window. If the
  ;;  current window (before the split) is more than 160 columns wide,
  ;;  split horizontally, else split vertically.
  ;;
  ;;  If the current buffer contains more than one window, select
  ;;  BUFFER in the least recently used window.
  ;;
  ;;  This function returns the window which holds BUFFER.
  ;;
  ;;  FORCE-OTHER-WINDOW is ignored."
  ;;    (or (get-buffer-window buffer)
  ;;        (if (one-window-p)
  ;;            (let ((new-win (if (> (window-width) 160)
  ;;                               (split-window-horizontally)
  ;;                             (split-window-vertically))))
  ;;              (set-window-buffer new-win buffer)
  ;;              new-win)
  ;;          (let ((new-win (get-lru-window)))
  ;;            (set-window-buffer new-win buffer)
  ;;            new-win))))
  ;;
  ;;  (setq display-buffer-function 'leuven--display-buffer))

) ;; chapter 20 ends here

;;* 21 (info "(emacs)Frames") and Graphical Displays

(leuven--chapter leuven-chapter-21-frames "21 Frames and Graphical Displays"

;;** 21.1 (info "(emacs)Mouse Commands")

  (leuven--section "21.1 (emacs)Mouse Commands")

  ;; scroll one line at a time
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 1)))

;;** 21.5 (info "(emacs)Creating Frames")

  (leuven--section "21.5 (emacs)Creating Frames")

  ;; put Emacs exactly where you want it, every time it starts up, by
  ;; auto-detecting the screen dimensions and computing where it should be
  (when (display-graphic-p)
    (GNUEmacs
      ;; list of default values for frame creation
      (add-to-list 'default-frame-alist
                   (cons 'height
                         (/ (- (x-display-pixel-height) 106)
                            (frame-char-height)))))

    ;; avoid Emacs hanging for a while (old hack from 2001)
    (add-to-list 'default-frame-alist
                 '(wait-for-wm . nil))

    ;; list of frame parameters for creating the initial frame
    (setq initial-frame-alist
          '((top . 0)
            (left . 0)))

    ;; default value of `vertical-scroll-bar' for buffers that don't
    ;; override it
    (setq default-vertical-scroll-bar 'right))

  (XEmacs
    (set-frame-width (buffer-dedicated-frame) 80)
    (set-frame-height (buffer-dedicated-frame) 42)
    (set-frame-position (buffer-dedicated-frame) 0 0))

  ;; title bar display of visible frames
  (setq frame-title-format "Emacs") ;; XXX for StumpWM?  Dunno anymore...

  (setq frame-title-format
        (format "Emacs %s pid:%d"
                emacs-version (emacs-pid)))

;;** 21.6 (info "(emacs)Frame Commands")

  (leuven--section "21.6 (emacs)Frame Commands")

  (XWindow
   (defun toggle-full-screen ()
     "Toggle between full screen and partial screen display on X11"
     (interactive)
     ;; WM must support EWMH
     ;; http://standards.freedesktop.org/wm-spec/wm-spec-latest.html
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

   (global-set-key
     (kbd "C-c z") 'toggle-full-screen))

  (GNUEmacs
    (when running-ms-windows
      (defun w32-maximize-frame ()
        "Maximize the current frame."
        (interactive)
        (w32-send-sys-command 61488)
        (global-set-key
          (kbd "C-c z") 'w32-restore-frame))

      (global-set-key
        (kbd "C-c z") 'w32-maximize-frame)

      (defun w32-restore-frame ()
        "Restore a minimized frame."
        (interactive)
        (w32-send-sys-command 61728)
        (global-set-key
          (kbd "C-c z") 'w32-maximize-frame))))

;;** 21.8 (info "(emacs)Speedbar")

  (leuven--section "21.8 (emacs)Speedbar Frames")

  ;; TODO don't bind F4 if already bound to helm... If helm not there, OK do it.
  ;; ;; jump to speedbar frame
  ;; (global-set-key (kbd "<f4>") 'speedbar-get-focus)

  ;; everything browser (into individual source files), or Dired on
  ;; steroids
  (with-eval-after-load "speedbar"

    ;; number of spaces used for indentation
    (setq speedbar-indentation-width 2)

    ;; add new extensions for speedbar tagging (allow to expand/collapse
    ;; sections, etc.) -- do this BEFORE firing up speedbar?
    (speedbar-add-supported-extension
     '(".bib" ".css" ".jpg" ".js" ".nw" ".org" ".php" ".png" ".tex" ".txt"
       ".w" "README"))

    ;; bind the arrow keys in the speedbar tree
    (define-key speedbar-key-map
      (kbd "<right>") 'speedbar-expand-line)
    (define-key speedbar-key-map
      (kbd "<left>") 'speedbar-contract-line)

    ;; parameters to use when creating the speedbar frame in Emacs
    (setq speedbar-frame-parameters '((width . 30)
                                      (height . 45)
                                      (foreground-color . "blue")
                                      (background-color . "white")))

    ;; speedbar in the current frame (vs in a new frame)
    (when (locate-library "sr-speedbar")
      (autoload 'sr-speedbar-toggle "sr-speedbar" nil t)
      ;; TODO don't bind F4 if already bound to helm... If helm not there, OK do it.
      ;; (global-set-key (kbd "<f4>") 'sr-speedbar-toggle)
      ))

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

  ;; use the echo area for help and GUD tooltips
  (setq tooltip-use-echo-area t)

) ;; chapter 21 ends here

;;* 22 (info "(emacs)International") Character Set Support

(leuven--chapter leuven-chapter-22-international "22 International Character Set Support"

;;** 22.3 (info "(emacs)Language Environments")

  (leuven--section "22.3 (emacs)Language Environments")

  ;; specify your character-set locale
  (setenv "LANG" "en_US.utf8") ;; for svn not to report warnings

  ;; system locale to use for formatting time values
  (setq system-time-locale "C") ;; make sure that timestamps in your Org mode
                                ;; files appear in English

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
    (cond (running-ms-windows
           (prefer-coding-system 'utf-8-unix))
          ;; XXX Unix flavor for script executing in Org-Babel
          ;; FIXME Prefer ISO Latin 1 (temporarily, for PFlow)
          (t
           (prefer-coding-system 'utf-8))))

;;** 22.8 (info "(emacs)Specify Coding") System of a File

  (leuven--section "22.8 (emacs)Specify Coding System of a File")

  (GNUEmacs
    ;; to copy and paste to and from Emacs through the clipboard (with
    ;; coding system conversion)
    (cond (running-ms-windows
           (set-selection-coding-system 'compound-text-with-extensions))
          (t
           (set-selection-coding-system 'utf-8))))

) ;; chapter 22 ends here

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

  ;; ledger
  (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
  (autoload 'ledger-mode "ledger"
    "A mode for editing ledger data files.")

  ;; major mode for editing comma-separated value files
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t)
  (with-eval-after-load "csv-mode"
    ;; field separators: a list of *single-character* strings
    (setq csv-separators '("," ";")))

  (autoload 'ssh-config-mode "ssh-config-mode" t)

  ;; list of interpreters specified in the first line (starts with `#!')
  (push '("expect" . tcl-mode) interpreter-mode-alist)

  ;; ;; load generic modes which support e.g. batch files
  ;; (try-require 'generic-x)

) ;; chapter 23 ends here

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

) ;; chapter 24 ends here

;;* 25 Commands for (info "(emacs)Text") Human Languages

(leuven--chapter leuven-chapter-25-text "25 Commands for Human Languages"

;;** 25.1 (info "(emacs)Words")

  (leuven--section "25.1 (emacs)Words")

  ;; GNU Emacs default for killing back to the beginning of a word
  (XEmacs
    (global-set-key
      (kbd "<C-backspace>") 'backward-kill-word))

;;** 25.2 (info "(emacs)Sentences")

  (leuven--section "25.2 (emacs)Sentences")

  ;; a single space does end a sentence
  (setq-default sentence-end-double-space nil)

;;** 25.5 (info "(emacs)Filling") Text

  (leuven--section "25.5 (emacs)Filling Text")

  ;; line-wrapping beyond that column (when pressing `M-q')
  (setq-default fill-column 78)

  ;; ;; `M-q' runs the command `fill-paragraph'. `C-u M-q' runs
  ;; ;; "unfill-paragraph"
  ;; (defun leuven-fill-paragraph (&optional arg)
  ;;   (interactive "P")
  ;;   (let ((fill-column (if arg
  ;;                          (point-max)
  ;;                        fill-column)))
  ;;     (fill-paragraph nil)))
  ;;
  ;; (global-set-key (kbd "M-q") 'leuven-fill-paragraph)


  ;; (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
  ;; (add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

  ;; activate Auto Fill for all text mode buffers
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; automatically fill comments (but not code) in programming modes
  (add-hook 'prog-mode-hook
            (lambda ()
              (auto-fill-mode 1)
              (set (make-local-variable 'fill-nobreak-predicate)
                   (lambda ()
                     (not (eq (get-text-property (point) 'face)
                              'font-lock-comment-face))))))

  (defun leuven-good-old-fill-paragraph ()
    (interactive)
    (let ((fill-paragraph-function nil)
          (adaptive-fill-function nil))
      (fill-paragraph)))

  ;; replace space by nobreak-space where it fits well
  (defun leuven--replace-space-before-colon ()
    "Replace space by nobreak-space in front of a colon."
    (interactive)
    (require 'org-element)
    (cond ((eq (char-before) ?\ ) ;; normal space
           (backward-delete-char 1)
           (cond ((equal mode-name "PDFLaTeX")
                  (insert "~:"))
                 ((equal mode-name "Org")
                  (if (member (org-element-type (org-element-at-point))
                              ;; list of exceptions
                              '(src-block keyword table dynamic-block))
                      (insert " :")
                    (insert " :")))
                 (t
                  (insert " :")))) ;; non-breaking space

          ;; remove nobreak-space if two colons are put one after the
          ;; other (for terms and definitions in Org)
          ((and (eq (char-before) ?\:)
                (eq (char-before (- (point) 1)) ?\ ))
           (backward-delete-char 2)
           (insert " ::")) ;; normal space

          (t
           (insert ":"))))

  (defun leuven--replace-space-before-question-mark ()
    "If any, replace space by nobreak-space in front of a question mark."
    (interactive)
    (if (eq (char-before) ?\ ) ; normal space
        (progn
          (backward-delete-char 1)
          (if (equal mode-name "PDFLaTeX")
              (insert "\,?")
            (insert " ?"))) ; non-breaking space
      (insert "?")))

  (defun leuven--replace-space-before-exclamation-mark ()
    "If any, replace space by nobreak-space in front of an exclamation
  mark."
    (interactive)
    (if (eq (char-before) ?\ ) ; normal space
        (progn
          (backward-delete-char 1)
          (if (equal mode-name "PDFLaTeX")
              (insert "\,!")
            (insert " !"))) ; non-breaking space
      (insert "!")))

  (defun leuven--replace-space-before-semi-colon ()
    "If any, replace space by nobreak-space in front of a semi-colon."
    (interactive)
    (require 'org-element)
    (if (eq (char-before) ?\ ) ; normal space
        (progn
          (backward-delete-char 1)
          (cond ((equal mode-name "PDFLaTeX")
                 (insert "\,;"))
                ((equal mode-name "Org")
                 (if (member (org-element-type (org-element-at-point))
                             ;; list of exceptions
                             '(src-block))
                     (insert " ;")
                   (insert " ;")))
                (t
                 (insert " ;")))) ; non-breaking space
      (insert ";")))

  (defun leuven--replace-double-double-quote ()
    "Replace two following double quotes by French quotes with nobreak-spaces."
    (interactive)
    (if (eq (char-before) ?\")
        (progn
          (backward-delete-char 1)
          (insert "«  »")
          (backward-char 2))
      (insert "\"")))

  ;; French typography
  (defun leuven--nobreak-keys ()
    "If any, replace space in front of colons, question marks, exclamation
  marks, etc. to avoid line break problems."
    (interactive)
    (local-set-key ":" 'leuven--replace-space-before-colon)
    (local-set-key "?" 'leuven--replace-space-before-question-mark)
    (local-set-key "!" 'leuven--replace-space-before-exclamation-mark)
    (local-set-key ";" 'leuven--replace-space-before-semi-colon)
    (local-set-key "\"" 'leuven--replace-double-double-quote))

  (add-hook 'text-mode-hook 'leuven--nobreak-keys)
  (add-hook 'message-mode-hook 'leuven--nobreak-keys)

  (defun insert-one-quote-or-two ()
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
      (delete-backward-char 1)
      (unless (looking-back "`") (insert-and-inherit "`"))
      (save-excursion
        (skip-syntax-forward "w_")
        (unless (looking-at "'") (insert-and-inherit "'"))))))

  (global-set-key
   [39] 'insert-one-quote-or-two)

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
    (setq outline-minor-mode-prefix (kbd "M-o"))

    ;; ;; make other `outline-minor-mode' files (LaTeX, etc.) feel the Org
    ;; ;; mode outline navigation (written by Carsten Dominik)
    ;; (when (try-require 'outline-magic)
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
      (interactive)
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
    ;; or expand all headings respectively. I am guessing you mean to make
    ;; segments such as `;; SHORTCUTS' and `;; VARIABLES', this will do
    ;; that, but not too much more.
    )

  ;; (add-hook 'outline-minor-mode-hook
  ;;   (lambda ()
  ;;     (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
  ;;     (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle))) ;; backtab?

  (global-set-key
    (kbd "<S-tab>") 'org-cycle) ; that works (but on level 1+)
  ;; TODO Look at org-cycle-global and local below, they work better, but
  ;; still on level 1+
  ;; TODO Replace it by a function which alternatively does `hide-body' and
  ;; `show-all'

  ;; from Bastien

  ;; XXX 2010-06-21 Conflicts with outline-minor-mode bindings
  ;; add a hook to use `orgstruct-mode' in Emacs Lisp buffers
  (add-hook 'emacs-lisp-mode-hook 'orgstruct-mode)

  (defun org-cycle-global ()
    (interactive)
    (org-cycle t))

  (global-set-key ;; ok on Elisp, not on LaTeX
    (kbd "C-M-]") 'org-cycle-global) ;; <S-tab>?

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

  (global-set-key ;; ok on Elisp, not on LaTeX
    (kbd "M-]") 'org-cycle-local)

;; C-M-] and M-] fold the whole buffer or the current defun.

  ;; unified user interface for Emacs folding modes, bound to Org
  ;; key-strokes
  (GNUEmacs
    (try-require 'fold-dwim-org-XXX))

  ;; 25.8.2
  (global-set-key
    (kbd "<M-f6>") 'visible-mode)

;;** (info "(emacs-goodies-el)boxquote")

  (leuven--section "(emacs-goodies-el)boxquote")

  (when (locate-library "boxquote")

    ;; quote text with a semi-box
    (autoload 'boxquote-region "boxquote" nil t)

    (with-eval-after-load "boxquote"
      (setq boxquote-top-and-tail "────")
      (setq boxquote-title-format " %s")
      (setq boxquote-top-corner    "  ╭")
      (setq boxquote-side          "  │ ")
      (setq boxquote-bottom-corner "  ╰")))

;;** (info "phonetic")

  (leuven--section "phonetic")

  ;; phonetic spelling
  (when (locate-library "phonetic")
    (autoload 'phonetize-region "phonetic" nil t))

) ;; chapter 25 ends here

;;* 25.9 Org Mode

;; (info "(org)Top") outline-based notes management and organizer

(leuven--chapter leuven-chapter-25.9-org-mode "25.9 Getting Things Done (with Org mode)"

;;* 1 (info "(org)Introduction")

;;** 1.2 (info "(org)Installation")

  ;; autoload functions
  (GNUEmacs
    (unless (try-require 'org-loaddefs)
      (try-require 'org-install))) ;; obsolete since Emacs 24.3

  ;; getting started
  (GNUEmacs
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode)))

  (define-key global-map
    (kbd "C-c l") 'org-store-link)
  (define-key global-map
    (kbd "C-c c") 'org-capture)
  (define-key global-map
    (kbd "C-c b") 'org-switchb)
  (define-key global-map
    (kbd "C-c a") 'org-agenda)

  ;; using links outside Org
  (global-set-key
    (kbd "C-c L") 'org-insert-link-global)
  (global-set-key
    (kbd "C-c o") 'org-open-at-point-global)

  (defun org-switch-to-agenda ()
    (interactive)
    (let ((buffer (get-buffer "*Org Agenda*")))
      (if buffer
          (switch-to-buffer buffer))))
  (global-set-key
    (kbd "C-c C-b") 'org-switch-to-agenda)


  ;; This must be set before loading Org...


  ;; face to be used by `font-lock' for highlighting in Org-mode Emacs
  ;; buffers, and tags to be used to convert emphasis fontifiers for HTML
  ;; export
  (setq org-emphasis-alist ;; remove the strike-through emphasis
        '(("*" bold "<b>" "</b>")
          ("/" italic "<i>" "</i>")
          ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
          ("=" org-code "<code>" "</code>" verbatim)
          ("~" org-verbatim "<code>" "</code>" verbatim)))

  (setq org-emphasis-alist
        '(("&" (:weight ultra-bold :foreground "#000000" :background "#FBFF00"))
          ("?" (:box t))
          ("^" (:weight ultra-bold :foreground "#393D90"))
          ("¶" (:height 1.3 :weight ultra-bold :underline t))
          ("¡" (:weight ultra-bold :foreground "#FFA500"))
          ("`" (:weight bold :slant italic :foreground "#155640" :background "#E2FFE8"))
          ("!" (:weight ultra-bold :foreground "#B40000")) ;; = alert in some Wikis
          ;; ("$" (:weight ultra-bold :foreground "#000000" :background "#DDDDFF"))
          ;; ("@" (:weight bold :foreground "#B40000" :background "#FFDDDD"))
          ("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-code verbatim)
          ("~" org-verbatim verbatim)))


  ;; Unhiding edited areas
  ;;??? I like the idea of clustering undo but find it disconcerting
  (setf org-self-insert-cluster-for-undo nil) ;; XXX undefined
  ;; somebody, I think Carsten, suggested this, and it might work for
  ;; you, but for some reason I commented it out. I don't remember what
  ;; the reason was. Maybe speed.
  (defadvice undo (after leuven-org-undo-reveal activate)
    "Make point and context visible after an undo command in Org mode."
    (message "Using adviced undo") (sit-for 1)
    (and (derived-mode-p 'org-mode)
         (org-reveal)))
  ;;(ad-unadvise 'undo)


  (add-hook 'org-mode-hook
            (lambda ()
              ;; (local-set-key "\M-n" 'outline-next-visible-heading)
              ;; (local-set-key "\M-p" 'outline-previous-visible-heading)

              (local-set-key
                (kbd "C-c h")
                'hide-other)

              ;; table
              (local-set-key "\M-\C-w" 'org-table-copy-region)
              (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
              (local-set-key "\M-\C-l" 'org-table-sort-lines)))

;;** 1.3 (info "(org)Activation")

  (leuven--section "1.3 (org)Activation")

  ;; insert the first line setting Org mode in empty files
  (setq org-insert-mode-line-in-empty-file t)

  ;; libraries that should (always) be loaded along with `org.el'
  ;; (loaded when opening the first Org file)
  (when (boundp 'org-modules)

    (setq org-modules nil)

    ;; globally unique ID for Org-mode entries (see `org-store-link')
    ;; (takes care of automatically creating unique targets for internal
    ;; links, see `C-h v org-id-link-to-org-use-id <RET>')
    (add-to-list 'org-modules 'org-id)

    ;; support for links to Gnus groups and messages from within Org-mode
    (add-to-list 'org-modules 'org-gnus)

    ;; habit tracking code for Org-mode
    (add-to-list 'org-modules 'org-habit)

    ;; make sure to turn `org-info' on in order to link to info nodes
    (add-to-list 'org-modules 'org-info))

  (with-eval-after-load "org-id"

    ;; storing a link to an Org file will use entry IDs
    (setq org-id-link-to-org-use-id
          'create-if-interactive-and-no-custom-id))

;;* 2 (info "(org)Document Structure")

  ;; ellipsis to use in the Org mode outline
  (with-eval-after-load "org"
    (setq org-ellipsis
          (if (char-displayable-p ?\u25B7) ;; white right-pointing triangle
              ;; this test takes ~ 0.40s; hence, wrapped in eval-after-load
              " \u25B7" ;; string
            'org-ellipsis))) ;; face

  ;; <RET> follows links
  (setq org-return-follows-link t)

  ;; blank lines
  (setq org-blank-before-new-entry
        '(
          ;; insert  a blank line before new heading
          (heading . t)

          ;; try to make an intelligent decision whether to insert a
          ;; blank line or not before a new item
          (plain-list-item . auto)))

;;** (info "(org)Headlines")

  (leuven--section "2.2 (org)Headlines")

  ;; ;; `C-a' and `C-e' behave specially in headlines and items
  (setq org-special-ctrl-a/e 'reversed)

  (with-eval-after-load "org"

    ;; insert an inline task (independent of outline hierarchy)
    (when (try-require 'org-inlinetask) ;; needed

      ;; initial state (TODO keyword) of inline tasks
      (setq org-inlinetask-default-state "TODO")

      ;; (defun org-html-format-inlinetask (todo type priority name tags
      ;;                                    contents)
      ;;   "Format an inline task element for HTML export."
      ;;   (let ((full-title
      ;;          (concat
      ;;           (when todo
      ;;             (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
      ;;           (when priority (format "\\framebox{\\#%c} " priority))
      ;;           title
      ;;           (when tags (format "\\hfill{}\\textsc{%s}" tags)))))
      ;;     (format (concat "<div class=\"inlinetask\">\n"
      ;;                     "  <b>%s</b><br/>\n"
      ;;                     "  %s\n"
      ;;                     "</div>")
      ;;             full-title
      ;;             contents)))

      ;; template for inline tasks in LaTeX exporter
      (defun org-latex-format-inlinetask (todo type priority name tags contents)
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
                   (format "{\\color{red}\\textbf{\\textsf{\\textsc{%s}}}} " todo))
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
            'org-latex-format-inlinetask))

    ) ;; eval-after-load "org" ends here

;;** (info "(org)Visibility cycling")

  (leuven--section "2.3 (org)Visibility cycling")

  ;; do not switch to OVERVIEW at startup
  (setq org-startup-folded nil)

  ;; ;; create a binding for `org-show-subtree'
  ;; must be in eval-after-load "org"?
  ;; (org-defkey org-mode-map (kbd "C-c C-S-s") 'org-show-subtree)
  ;; (org-defkey org-mode-map (kbd "C-c s") 'org-show-subtree)

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
              (local-set-key (kbd "\C-\M-n") 'outline-next-visible-heading)
              (local-set-key (kbd "\C-\M-p") 'outline-previous-visible-heading)
              (local-set-key (kbd "\C-\M-u") 'outline-up-heading)))

  ;; headlines in the current buffer are offered via completion
  ;; (interface also used by the `refile' command)
  (setq org-goto-interface 'outline-path-completion)

  ;; must be in eval-after-load "org"?
  ;; (define-key org-mode-map
  ;;   (kbd "C-c C-r") 'leuven-org-reveal)

  (defun leuven-org-reveal (&optional all-siblings)
    "For `C-u C-c C-r', it does the same as default org-mode --- shows all
  hidden siblings, and for `C-c C-r' --- all siblings of current level."
    (interactive "P")
    (if all-siblings
        (org-reveal t)
      (org-show-siblings)))

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
  (setq org-cycle-include-plain-lists nil)

  ;; an empty line does not end all plain list levels
  (setq org-empty-line-terminates-plain-lists nil)

;;** (info "(org)Footnotes")

  (leuven--section "2.10 (org)Footnotes")

  ;; use `C-c C-x f' to add a footnote, to go back to the message
  ;; *and* to go to a footnote
  (global-set-key
    (kbd "C-c C-x f") 'org-footnote-action)

;;* 3 (info "(org)Tables")

  (setq org-table-use-standard-references 'from)

;;** 3.1 The (info "(org)Built-in table editor")

  (leuven--section "3.1 The (org)Built-in table editor")

  ;; default export parameters for `org-table-export'
  (setq org-table-export-default-format "orgtbl-to-csv")

;;** 3.5 (info "(org)The spreadsheet")

  (leuven--section "3.5 (org)The spreadsheet")

  ;; FIXME Only set `calc-internal-prec' to 12 [default: 8]
  (setq org-calc-default-modes
        '(calc-internal-prec 12
          calc-float-format  (float 12)
          calc-angle-mode    deg
          calc-prefer-frac   nil
          calc-symbolic-mode nil
          calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm))
          calc-display-working-message t))

;;* 4 (info "(org)Hyperlinks")

  ;; create web links to Google groups or Gmane (instead of Gnus
  ;; messages)
  (setq org-gnus-prefer-web-links t)

  ;; open non-existing files
  (setq org-open-non-existing-files t)

  ;; 4.3 function and arguments to call for following `mailto' links
  (setq org-link-mailto-program '(compose-mail "%a" "%s"))

  ;; setup the frame configuration for following links
  (setq org-link-frame-setup
        '((vm   . vm-visit-folder)
          (gnus . org-gnus-no-new-news)
          (file . find-file-other-window))) ;; open link in other window

  ;; 4.4 show inline images when loading a new Org file
  (setq org-startup-with-inline-images t)

  ;; 4.4 try to get the width from an #+ATTR.* keyword and fall back on the
  ;; original width if none is found
  (setq org-image-actual-width nil)

;;** 4.6 (info "(org)Link abbreviations")

  ;; 4.6 shortcut links
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
           "http://fr.wikipedia.org/wiki/%s")))

;;* 5 (info "(org)TODO Items")

;;** 5.1 (info "(org)TODO basics") functionality

  (leuven--section "5.1 (org)TODO basics functionality")

  ;; 5.1 select a TODO state and bypass any logging associated with that
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; some commands act upon headlines in the active region
  (setq org-loop-over-headlines-in-active-region 'start-level)

;;** 5.2 Use of (info "(org)TODO extensions")

  (leuven--section "5.2 Use of (org)TODO extensions")

  ;; list of TODO entry keyword sequences and their interpretation (for
  ;; the different task states)
  (setq org-todo-keywords
        '((sequence "NEW(n!)"   ;; proposal
                    "TODO(t!)"
                    "STRT(s!)"  ;; in progress
                    "WAIT(w!)"  ;; feedback
                    "DLGT(l!)"  ;; assigned
                    "DFRD(f!)"  ;; future, someday
                    ;; DFRD is *not* a completion state (in order not to
                    ;; be struck through)
                    "MAYB(m!)"  ;; maybe, perhaps, wish
                    ;; actions that may be undertaken in the future
                    "|"
                    "DONE(d!)"  ;; resolved, closed
                    "CANX(x!)") ;; wontfix, rejected
          (sequence "QTE(q!)" "QTD(Q!)" "|"
                    "APP(A!)" "EXP(E!)" "REJ(R!)")
          (sequence "OPENPO(O!)" "|"
                    "CLSDPO(C!)")))

  ;; TODO Check coherence of the faces with the above defined states
  ;; faces for specific TODO keywords
  (GNUEmacs
    (setq org-todo-keyword-faces
          '(("NEW"  . leuven-org-created-kwd-face)
            ("TODO" . org-todo)
            ("STRT" . leuven-org-inprogress-kwd-face)
            ("DLGT" . leuven-org-delegated-kwd-face)
            ("WAIT" . leuven-org-delegated-kwd-face)
            ("DFRD" . leuven-org-deferred-kwd-face)
            ("MAYB" . leuven-org-deferred-kwd-face)
            ("DONE" . org-done)
            ("CANX" . org-done)

            ("QTE" . leuven-org-quote-kwd-face)
            ("QTD" . leuven-org-quoted-kwd-face)
            ("APP" . leuven-org-approved-kwd-face)
            ("EXP" . leuven-org-expired-kwd-face)
            ("REJ" . leuven-org-rejected-kwd-face)

            ("OPENPO" . leuven-org-openpo-kwd-face)
            ("CLSDPO" . leuven-org-closedpo-kwd-face))))

  ;; Org standard faces
  (with-eval-after-load "org-faces"
    (set-face-attribute 'org-todo nil
                        :weight 'bold :box '(:line-width 1 :color "#D8ABA7")
                        :foreground "#D8ABA7" :background "#FFE6E4")

    (set-face-attribute 'org-done nil
                        :weight 'bold :box '(:line-width 1 :color "#BBBBBB")
                        :foreground "#BBBBBB" :background "#F0F0F0")

    ;; Org non-standard faces
    (defface leuven-org-created-kwd-face
      '((t (:weight normal :box (:line-width 1 :color "#EEE9C3")
            :foreground "#1A1A1A" :background "#FDFCD8")))
      "Face used to display state NEW.")
    (defface leuven-org-inprogress-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#D9D14A")
            :foreground "#D9D14A" :background "#FCFCDC")))
      "Face used to display state STRT.")
    (defface leuven-org-delegated-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#89C58F")
            :foreground "#89C58F" :background "#E2FEDE")))
      "Face used to display state DLGT or WAIT.")
    (defface leuven-org-deferred-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#9EB6D4")
            :foreground "#9EB6D4" :background "#E0EFFF")))
      "Face used to display state DFRD or MAYB.")

    (defface leuven-org-quote-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#FC5158")
            :foreground "#FC5158" :background "#FED5D7")))
      "Face used to display .")
    (defface leuven-org-quoted-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#55BA80")
            :foreground "#55BA80" :background "#DFFFDF")))
      "Face used to display .")
    (defface leuven-org-approved-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#969696")
            :foreground "#969696" :background "#F2F2EE")))
      "Face used to display .")
    (defface leuven-org-expired-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#42B5FF")
            :foreground "#42B5FF" :background "#D3EEFF")))
      "Face used to display state EXPIRED.")
    (defface leuven-org-rejected-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#42B5FF")
            :foreground "#42B5FF" :background "#D3EEFF")))
      "Face used to display state REJECTED.")

    (defface leuven-org-openpo-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#FC5158")
            :foreground "#FC5158" :background "#FED5D7")))
      "Face used to display OPEN purchase order.")
    (defface leuven-org-closedpo-kwd-face
      '((t (:weight bold :box (:line-width 1 :color "#969696")
            :foreground "#969696" :background "#F2F2EE")))
      "Face used to display CLOSED purchase order."))

  ;; block switching entries to DONE if
  ;; 1) there are undone child entries, or
  ;; 2) the parent has an `:ORDERED:' property and there are prior
  ;;    siblings not yet done
  (setq org-enforce-todo-dependencies t)

  ;; don't dim blocked tasks in the agenda display -- agenda optimization
  (setq org-agenda-dim-blocked-tasks nil) ; XXX not sure about this one

  ;; inhibit startup when preparing agenda buffers -- agenda optimization
  (setq org-agenda-inhibit-startup t)

  ;; block switching the parent to DONE if
  ;; there are unchecked checkboxes
  (setq org-enforce-todo-checkbox-dependencies t)

;;** 5.3 (info "(org)Progress logging")

  (leuven--section "5.3 (org)Progress logging")

  ;; 5.3.1 insert a CLOSED time stamp each time a TODO entry is marked DONE
  (setq org-log-done nil)

  ;; 5.3.2 the notes will be ordered according to time
  (setq org-log-states-order-reversed nil)

  ;; 5.3.2 insert state change notes and time stamps into a LOGBOOK drawer
  (setq org-log-into-drawer t) ;; should be the default

  ;; ~5.3.2 heading for state change added to entries
  (with-eval-after-load "org"
    (setcdr (assq 'state org-log-note-headings)
            "State %-12S  ->  %-12s %t"))

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
    (let (org-log-done org-log-states) ;; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;* 6 (info "(org)Tags")

  (setq org-tags-column -78)

  ;; 6.2 list of tags ("contexts") allowed in Org mode files
  (setq org-tag-alist '((:startgroup . nil)
                         ("home" . ?h)
                         ("work" . ?w)
                        (:endgroup . nil)
                        ("errands" . ?e)
                        ;; ("computer" . ?c)
                        ("phone" . ?p)
                        ("mail" . ?m)

                        ("reading" . ?r)
                        ;; ("finance" . ?f)
                        ("bank" . ?b)
                        ("note" . ?n)
                        ("blog" . ?B)

                        ("ARCHIVE" . ?A)
                        ("crypt" . ?C)
                        ("FLAGGED" . ??)))

  ;; ("PROJ" . ?P)v

  ;; faces for specific tags
  ;; XXX This generates an error when C-x C-w'ing the agenda view
  (setq org-tag-faces-XXX
        '(("refile" .
           (:weight normal :slant italic
            :box (:line-width 1 :color "#A0A1A4")
            :foreground "#A0A1A4" :background "#444444'"))
          ("home" .
           (:weight normal :slant italic
            :box (:line-width 1 :color "#4488BB")
            :foreground "#5C88D3" :background "#BBDDFF"))
          ("work" .
           (:weight normal :slant italic
            :box (:line-width 1 :color "#44AA44")
            :foreground "#44AA44" :background "#CCFFAA"))
          ("blog" .
           (:weight normal :slant italic
            :box (:line-width 1 :color "#A48CC4")
            :foreground "#FFFFFF" :background "#A48CC4"))
          ("note" .
           (:weight normal :slant italic
            :box (:line-width 1 :color "#7F7F7F")
            :foreground "#FFFFFF" :background "#989898"))))

  ;; 6.2 exit fast tag selection after first change (toggle this with `C-c')
  (setq org-fast-tag-selection-single-key t)

  ;; remove redundant tags of headlines (from David Maus)
  (defun leuven--org-remove-redundant-tags ()
    "Remove redundant tags of headlines in current buffer.
  A tag is considered redundant if it is local to a headline and inherited by
  a parent headline."
    (interactive)
    (when (eq major-mode 'org-mode)
      (save-excursion
        (org-map-entries
         '(lambda ()
            (let ((alltags (split-string
                            (or (org-entry-get (point) "ALLTAGS") "")
                            ":"))
                  local inherited tag)
              (dolist (tag alltags)
                (if (get-text-property 0 'inherited tag)
                    (push tag inherited) (push tag local)))
              (dolist (tag local)
                (if (member tag inherited) (org-toggle-tag tag 'off)))))
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
           )))

;;* 8 (info "(org)Dates and Times")

  (leuven--section "8 (org)Dates and Times")

  ;; insinuate appt if Org mode is loaded
  (with-eval-after-load "org"
    (try-require 'appt))

;;** 8.2 (info "(org)Creating timestamps")

  (leuven--section "8.2 (org)Creating timestamps")

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


  ;; (setq org-time-clocksum-use-fractional t)

  ;; format string used when creating CLOCKSUM lines and when generating a
  ;; time duration (avoid showing days)
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;** 8.3 (info "(org)Deadlines and scheduling")

  (leuven--section "8.3 (org)Deadlines and scheduling")

  ;; information to record when the scheduling date is modified
  (setq org-log-reschedule 'time)

  ;; information to record when the deadline date is modified
  (setq org-log-redeadline 'time)

  ;; skip deadline prewarning (up to 7 days before the actual deadline)
  ;; when entry is also scheduled
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 7)

  ;; don't show deadlines when the corresponding item is done
  (setq org-agenda-skip-deadline-if-done t)

  ;; skip scheduling line if same entry shows because of deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; don't show scheduled items in agenda when they are done
  (setq org-agenda-skip-scheduled-if-done t)

  ;; ~8.3 don't select item by timestamp or -range if it is DONE
  (setq org-agenda-skip-timestamp-if-done t)

  ;; ;; show all days between the first and the last date
  ;; (setq org-timeline-show-empty-dates t)

  ;; TODO state to which a repeater should return the repeating task
  (setq org-todo-repeat-to-state "TODO")

;;** 8.4 (info "(org)Clocking work time")

  (leuven--section "8.4 (org)Clocking work time")

  (global-set-key
    (kbd "C-c C-x C-i") 'org-clock-in)
  (global-set-key
    (kbd "C-c C-x C-j") 'org-clock-goto)
  (global-set-key
    (kbd "C-c C-x C-o") 'org-clock-out)

  ;; the time clocking code for Org mode
  ;; (try-require 'org-clock)
  ;;! needed for trying to automatically re-clock at Emacs startup
  ;; Alternative: resume clocks when opening the first Org file

  (with-eval-after-load "org-clock"

    ;; 8.4 save both the running clock and the entire clock history when Emacs
    ;; is closed, and resume it next time Emacs is started up
    (setq org-clock-persist t)

    ;; 8.4 set up hooks for clock persistence
    (org-clock-persistence-insinuate)

    ;; resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)

    ;; number of clock tasks to remember in history: 1-9A-Z
    (setq org-clock-history-length 35)

    ;; 8.4.2 include the current clocking task time in clock reports
    (setq org-clock-report-include-clocking-task t)

    (defun leuven-org-clock-in-interrupted-task ()
      "Clock back into the task that has been interrupted, if there is one."
      (interactive)
      (if (and (not org-clock-resolving-clocks-due-to-idleness)
               (marker-buffer org-clock-marker)
               (marker-buffer org-clock-interrupted-task))
          (org-with-point-at org-clock-interrupted-task
            (org-clock-in nil))
        (org-clock-out)))

    (global-set-key
      (kbd "C-c C-x C-q") 'leuven-org-clock-in-interrupted-task)

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

    ;; time included for the modeline clock is all time clocked into this
    ;; task today
    (setq org-clock-modeline-total 'today)
    (setq org-clock-modeline-total 'all)

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
        t)) ;; only fails on keyboard quit or error

    (add-hook 'kill-emacs-query-functions 'leuven--org-query-clock-out)

    ;; format string for the total time cells
    (setq org-clock-total-time-cell-format "%s")

    ;; format string for the file time cells
    (setq org-clock-file-time-cell-format "%s")

    ) ;; with-eval-after-load "org-clock" ends here

;;** 8.5 (info "(org)Effort estimates")

  (leuven--section "8.5 (org)Effort estimates")

  ;; add an effort estimate on the fly when clocking in
  (defun leuven--org-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Time estimate (H:MM): "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))

  (add-hook 'org-clock-in-prepare-hook
            'leuven--org-ask-effort)

;;* 9 (info "(org)Capture - Refile - Archive")

  (leuven--section "9.1 (org)Capture")

  ;; 9.1.2 directory with Org files
  (setq org-directory
        (directory-file-name ;; this function removes the final slash
         (cond ((file-directory-p "~/org/") "~/org/")
               (t "~/"))))

  ;; 9.1.2 default target for storing notes
  (setq org-default-notes-file
        (concat org-directory "/refile.org")) ;; Inbox for collecting

  ;; 9.1.2 templates for the creation of capture buffers

  ;; ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")
  ;; ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Personal/someday.org")

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
                 `("m" "Mail to task" entry
                   (file+headline ,org-default-notes-file "Tasks")
                   "* TODO %:subject%? (from %:fromname) :mail:
SCHEDULED: %t
%:date-timestamp-inactive

#+begin_verse
%i
#+end_verse

From %a"
                   :empty-lines 1 :immediate-finish t) t)
       ;; `immediate-finish' = immediately store note without
       ;; further prompt (skipping `C-c C-c'), which is very handy
       ;; for quick storing of emails

    (add-to-list 'org-capture-templates
                 `("M" "mailtodo" entry (file+datetree "~/Personal/mails.org")
                   "* TODO %^{Task} %^G
SCHEDULED: %t
- From :: %:from
- Subject :: %:subject
- Email :: %a
%?"
                   :kill-buffer t) t)

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
                   (file+headline ,org-default-notes-file "Shopping list")) t)

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
                   "* %T %?  :blog:

%U

%i

From %a"
                   ;; "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x"
                   :empty-lines 1) t)

    (add-to-list 'org-capture-templates
                 `("S" "secure" entry
                   (file+datetree+prompt "~/git/notes/secure.org.gpg")
                   "* %(format-time-string \"%H:%M\") %^{Entry} %^G
%i%?") t)

    (defun leuven--org-capture-template (keys description file headline)
      "Create template for captured elements."
      `(,keys ,description entry
              (file+headline ,file ,headline)
              "* %^{Title}
:PROPERTIES:
:Created: %:date-timestamp-inactive
:END:
%?
%i

From %a"
              :empty-lines 1))

    ;; notes
    (add-to-list 'org-capture-templates
                 `("N" "Templates adding notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Ne" "Emacs"
                  "~/org/notes/Notes-on-Emacs.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "No" "Org"
                  "~/org/notes/Notes-on-Org.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "NL" "Lisp"
                  "~/org/notes/Notes-on-Lisp.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Ng" "Gnus"
                  "~/org/notes/Notes-on-Gnus.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Nl" "LaTeX"
                  "~/org/notes/Notes-on-LaTeX.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "NT" "TikZ"
                  "~/org/notes/Notes-on-TikZ.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Nb" "Beamer"
                  "~/org/notes/Notes-on-Beamer.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "NS" "StumpWM"
                  "~/org/notes/Notes-on-StumpWM.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Nu" "Unix"
                  "~/org/notes/Notes-on-Unix.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Nc" "Ledger"
                  "~/org/notes/Notes-on-Ledger.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Nr" "RFID" "~/org/notes/Notes-on-RFID.txt" "Notes") t)
    (add-to-list 'org-capture-templates
                 (leuven--org-capture-template
                  "Ns" "Security"
                  "~/org/notes/Notes-on-Security.txt" "Notes") t)

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

    (defadvice org-capture-destroy ;; XXX
      (after leuven-delete-capture-destroy-frame activate)
      "Advise capture-destroy to close the frame (if it is the capture frame)."
      (if (equal "capture" (frame-parameter nil 'name))
          (delete-frame)))

    ) ;; with-eval-after-load "org-capture" ends here

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

    ;; 9.1.4 any headline with level <= 2 is a target
    (defvar leuven-org-refile-extra-files
      (if (file-exists-p "~/org/notes/")
          (directory-files "~/org/notes/" t "^[^\\.#].*\\.\\(txt\\|org\\)$")
        nil)
      "List of extra files to be used as targets for refile commands.")

    ;; cache refile targets to speed up the process
    (setq org-refile-use-cache t)

    ;; 9.1.4 provide refile targets as paths, including the file name
    ;; (without directory) as level 1 of the path
    (setq org-refile-use-outline-path 'file)

    ;; 9.1.4 allow to create new nodes (must be confirmed by the user) as
    ;; refile targets
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; refile only within the current buffer
    (defun leuven-org-refile-within-current-buffer ()
      "Move the entry at point to another heading in the current buffer."
      (interactive)
      (let ((org-refile-targets '((nil :maxlevel . 8))))
        (org-refile)))
    ;; FIXME Add a smart key binding

    (leuven--section "9.6 (org)Archiving")

    ;; 9.6.1 subtrees should be archived in the current file
    (setq org-archive-location "::* Archive")

    )

  (leuven--section "10 (org)Agenda Views")

;;* 10 (info "(org)Agenda Views")

  ;; multiple same-day timestamps in entry make multiple agenda lines
  (setq org-agenda-skip-additional-timestamps-same-entry nil)

  ;; show outline path in echo area after line motion (though, may bring
  ;; some slowness)
  (setq org-agenda-show-outline-path t)

  ;; 10.0 restore the window configuration when exiting the agenda
  (setq org-agenda-restore-windows-after-quit t)

;;** 10.1 (info "(org)Agenda files")

  (leuven--section "10.1 (org)Agenda files")

  (when (boundp 'org-agenda-files)
    (message "(info) Found %s entries in `org-agenda-files'"
             (length org-agenda-files))
    (sit-for 0.5)

    (setq org-refile-targets
          `((nil
             :maxlevel . 8) ;; current file
            (,(append org-agenda-files leuven-org-refile-extra-files)
             :maxlevel . 4))))

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

  ;; show entries with a date in the global `todo' list
  (setq org-agenda-todo-ignore-with-date nil) ;;!! tricky setting

  ;; show entries with a timestamp in the global `todo' list
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; 10.3.2 don't show scheduled entries in the global `todo' list
  (setq org-agenda-todo-ignore-scheduled 'future) ;;!! tricky setting
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; 10.3.2 don't show entries scheduled in the future in the global
  ;; `todo' list (until they are within the warning period)
  (setq org-agenda-todo-ignore-deadlines 'near) ;;!! tricky setting
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
  (setq org-agenda-text-search-extra-files nil)
  ;; (setq leuven-org-search-extra-files ...) to list extra files to be searched

  ;; turn on individual word search (for Google addicts)
  (setq org-agenda-search-view-always-boolean t
        org-agenda-search-view-search-words-only t)

  ;; match part of a word
  (setq org-agenda-search-view-force-full-words nil)

  ;; don't search headline for a time-of-day
  (setq org-agenda-search-headline-for-time nil)

  ;; 10.3.6 how to identify stuck projects
  (setq org-stuck-projects
        '("+LEVEL=2/-DONE" ;; identify a project
          ("TODO" "STRT") ;; TODO keywords
          nil "")) ;; tags, regexp

;;** 10.4 (info "(org)Presentation and sorting")

  (leuven--section "10.4 (org)Presentation and sorting")

  ;; 10.4 format specifications for the prefix of items in the agenda views
  (setq org-agenda-prefix-format
        '((agenda . " %-11s%i %?-12t") ;; agenda
          (timeline . " % s") ;; timeline
          (todo . " %i %-12:c") ;; todo, alltodo
          (tags . " %i %-12:c") ;; tags, tags-todo, stuck
          (search . " %i %-12:c"))) ;; search

  ;; text preceding scheduled items in the agenda view
  (setq org-agenda-scheduled-leaders
        '("Today      "
          "           "))

  ;; text preceding item pulled into the agenda by inactive time stamps
  (setq org-agenda-inactive-leader "[")

  ;; text preceding deadline items in the agenda view
  (setq org-agenda-deadline-leaders
        '("Deadline   "
          "In %d d" ;; or "%d d left"
          "%d d ago"))

  ;; faces for showing deadlines in the agenda
  (setq org-agenda-deadline-faces
        '((1.0001 . leuven-org-deadline-yesterday-or-before-face)
          (0.9999 . leuven-org-deadline-today-face)
          (0.0000 . leuven-org-deadline-tomorrow-or-later-face)))

  (with-eval-after-load "org-faces"

    ;; Org non-standard faces
    (defface leuven-org-deadline-yesterday-or-before-face
      '((t (:weight bold :foreground "#D24231" :background "#F8D3D4")))
      "Face used to highlight tasks whose deadline is in the past.")

    (defface leuven-org-deadline-today-face
      '((t (:weight bold :foreground "#BF8239" :background "#F8D1A9")))
      "Face used to highlight tasks whose deadline is today.")

    (defface leuven-org-deadline-tomorrow-or-later-face
      '((t (:weight bold :foreground "#45A856" :background "#B8E9B1")))
      "Face used to highlight tasks whose deadline is for later."))

  ;; 10.4 column to shift tags to (in agenda items)
  (setq org-agenda-tags-column -132)

  ;; type "L" in agenda and todo buffers to show category name and task
  ;; length for each task
  (defvar leuven--org-agenda-show-task-details nil)
  (defun leuven-org-agenda-toggle-task-details ()
    "Toggle inclusion of category and estimate in agenda views."
    (interactive)
    (if leuven--org-agenda-show-task-details
        (progn
          (setq leuven--org-agenda-show-task-details nil)
          (setq org-agenda-prefix-format
                '((agenda  . " %-11s%i %?-12t")
                  (timeline  . " % s")
                  (todo  . " ")
                  (search . " ")
                  (tags  . " "))))
      (setq leuven--org-agenda-show-task-details t)
      (setq org-agenda-prefix-format
            '((agenda . " %-11s%i %-12:c%?-12t%7e ")
              (timeline . " % s")
              (todo . " %i %-12:c")
              (search . " %i %-12:c")
              (tags . " %i %-12:c"))))
    (org-agenda-redo))

  (with-eval-after-load "org-agenda"

    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-agenda-keymap
                  (kbd "L") 'leuven-org-agenda-toggle-task-details)
                (define-key org-agenda-mode-map
                  (kbd "L") 'leuven-org-agenda-toggle-task-details))))

  ;; 10.4.2 settings for time grid for agenda display
  (setq org-agenda-time-grid '((daily remove-match)
                               ""
                               (0800 1000 1200 1400 1600 1800 2000)))

  ;; string for the current time marker in the agenda
  (setq org-agenda-current-time-string "now")

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

  ;; ;; enable Follow mode
  ;; (setq org-agenda-start-with-follow-mode t)
  ;; ;; XXX Seems nice, but first solve problem with decrypt question (auto-save)

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
        (concat ;; "\n"
                "%Y-%m-%d" " %a "
                (make-string (1- (window-width)) (string-to-char "_"))))

  ;; 10.5 only show clocked entries in agenda log mode (no closed
  ;; entries, no state changes)
  (setq org-agenda-log-mode-items '(clock))

  ;; 10.5 parameters for the clocktable in clockreport mode
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 3 :fileskip0 t))
  (setq org-agenda-clockreport-parameter-plist
        '(:link nil :maxlevel 3 :fileskip0 t))

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
  ;; XXX This generates an error when C-x C-w'ing the agenda view
  (setq org-priority-faces-XXX
        '((?A . (:weight bold :slant italic :underline t
                 :foreground "#6E0000" :background "#F67777"))
          (?B . (:slant italic
                 :foreground "#005606" :background "#B6E864"))
          (?C . (:slant italic
                 :foreground "#00337B" :background "#C3DCFF"))))

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
          ((string= tag "home")
           (with-temp-buffer
             (call-process "/sbin/ifconfig" nil t nil "en0" "inet")
             (goto-char (point-min))
             (not (re-search-forward "inet 192\\.168\\.9\\." nil t))))
          ((or (string= tag "errands")
               (string= tag "phone"))
           (let ((hour (nth 2 (decode-time))))
             (or (< hour 8) (> hour 21)))))
         (concat "-" tag)))

  ;;! ensure that `:refile:' tags never will be excluded!
  (defun leuven--org-auto-exclude-function (tag)
    (and (cond
          ((string= tag "home")
           (leuven--working-p))
          ((string= tag "work")
           (not (leuven--working-p)))
          ((or (string= tag "errands")
               (string= tag "phone"))
           (not (leuven--calling-hours-p))))
         (concat "-" tag)))

  (setq org-agenda-auto-exclude-function 'leuven--org-auto-exclude-function)

  ;; make the block agenda more compact (no agenda span name, no week
  ;; number, no separator line)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-compact-blocks nil)

  (setq org-agenda-block-separator
        ;; (concat (make-string 132 (string-to-char "_")) "\n\n")
        "\n")

;;** 10.6 (info "(org)Custom agenda views")

  (leuven--section "10.6 (org)Custom agenda views")

  (with-eval-after-load "org-agenda"

    ;; custom commands for the agenda -- start with a clean slate
    (setq org-agenda-custom-commands nil)

    (add-to-list 'org-agenda-custom-commands
                 '("f" "Like s, but with extra files"
                   search ""
                   ((org-agenda-text-search-extra-files
                     leuven-org-search-extra-files))) t)
                 ;; FIXME Add (agenda-archives)

    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("A" . "0. Agenda...") t)
    ;;
    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("AF" "Agenda of upcoming due dates (6 months)"
    ;;                ;; FIXME We don't see DEADLINE with `-1m' (or so)
    ;;                ;; specifications (if they are more than 1m ahead of now)!
    ;;                agenda ""
    ;;                ((org-agenda-include-all-todo nil)
    ;;                 (org-agenda-skip-function
    ;;                  '(org-agenda-skip-entry-if 'notdeadline))
    ;;                 (org-agenda-span 'day)
    ;;                 (org-agenda-time-grid nil)
    ;;                 (org-deadline-warning-days 183))) t)
    ;;              ;; Some SCHEDULED are shown (when paired with a deadline and
    ;;              ;; scheduled in the past or for today)

    (add-to-list 'org-agenda-custom-commands
                 '("b" . "All active tasks...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("b=" "List of all TODO entries (column view)"
                   alltodo ""
                   ((org-agenda-overriding-columns-format
                     "%65ITEM(Task) %4TODO %PRIORITY %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
                    (org-agenda-view-columns-initially t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("bT" "Filtered list of TODO entries"
                   tags-todo "TODO<>{DFRD\\|MAYB\\|PROJ}"
                   ((org-agenda-overriding-header
                     "List of TODO items of all types but DFRD/MAYB/PROJ, and PRIORITY >= #B")
                    (org-agenda-sorting-strategy
                     '(category-up priority-down todo-state-up alpha-up)))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("p" . "2. Process/Clarify...") t)

    (add-to-list 'org-agenda-custom-commands
                 `("ps" "Inbox"
                   tags "CATEGORY=\"@Inbox\"&LEVEL=2"
                   ((org-agenda-overriding-header "Level 2 stuff in inbox"))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("o" . "3. Organize...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("or" "Thoughts to refile"
                   tags "refile|capture"
                   ((org-agenda-overriding-header "Refile stuff"))) t)

    ;; XXX
    (add-to-list 'org-agenda-custom-commands
                 `("oc" "XXX Tasks to refile (column view)"
                   tags-todo ""
                   ((org-agenda-files ,org-default-notes-file)
                    (org-agenda-overriding-columns-format
                     "%65ITEM(Task) %4TODO %PRIORITY %20TAGS %6Effort(Estim.) %14SCHEDULED %14DEADLINE(Due Date)")
                    (org-agenda-view-columns-initially t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("r" . "4. Review...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("rt" "Unscheduled TODO entries"
                   alltodo ""
                   ((org-agenda-entry-types '(:scheduled :deadline :timestamp))
                    (org-agenda-overriding-header "Unscheduled TODO entries: "))) t)

    ;; all TODO entries, but DFRD/MAYB
    (add-to-list 'org-agenda-custom-commands
                 '("rT" "List of unscheduled TODO entries (no DFRD/MAYB)"
                   tags-todo "TODO<>{DFRD\\|MAYB}"
                   ((org-agenda-overriding-header
                     "Global list of unscheduled TODO items of all types but DFRD/MAYB")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
                    (org-agenda-sorting-strategy '(priority-down)))) t)

    ;; only TODO entries
    (add-to-list 'org-agenda-custom-commands
                 '("rs" "TODO without SCHEDULED or DEADLINE"
                   tags-todo "TODO=\"TODO\"+SCHEDULED=\"\"+DEADLINE=\"\"") t)
    ;; identical entries? (diff for inherited tags; do C-x C-w of the buffers and compare)
    (add-to-list 'org-agenda-custom-commands
                 ;; show unscheduled TODO items as a TODO list
                 '("rS"
                   todo "TODO"
                   ((org-agenda-todo-ignore-with-date t)
                    (org-agenda-tags-todo-honor-ignore-options t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("r^" "Calendar for current month"
                   (lambda (&rest ignore)
                     (cfw:open-org-calendar))) t)

    ;; show what happened today
    (add-to-list 'org-agenda-custom-commands
                 '("rL" "Timeline (with clock) for today"
                   agenda ""
                   ((org-agenda-clockreport-mode t)
                    (org-agenda-entry-types '(:timestamp :sexp))
                    (org-agenda-log-mode-items '(clock closed))
                    (org-agenda-show-log t)
                    (org-agenda-span 'day))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("r$" "Cleanup"
                   todo "DONE|CANX|DFRD"
                   ((org-agenda-overriding-header "Old tasks to delete or archive")
                    (org-agenda-todo-ignore-deadlines nil)
                    (org-agenda-todo-ignore-scheduled nil)
                    (org-agenda-todo-ignore-with-date nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rp" "Projects"
                   tags-todo "project-DONE-CANX"
                   ((org-agenda-overriding-header "Projects (High Level)")
                    (org-agenda-sorting-strategy nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rC" "Clock Review"
                   agenda ""
                   ((org-agenda-archives-mode t)
                    (org-agenda-clockreport-mode t)
                    (org-agenda-overriding-header "Clocking Review")
                    (org-agenda-show-log 'clockcheck)
                    (org-agenda-span 'day))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rn" "Now (unscheduled tasks in progress)"
                   todo "STRT"
                   ((org-agenda-todo-ignore-with-date t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rw" "Weekly review"
                   (
                    (tags "CATEGORY=\"@Inbox\"&LEVEL=2|TODO=\"NEW\""
                          ((org-agenda-overriding-header "Inbox")))

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
                             (org-agenda-start-day "-1w") ;; recently done
                             (org-deadline-warning-days 0)))

                    (agenda ""
                            ((org-agenda-overriding-header "Next month")
                             (org-agenda-span 'month)
                             (org-agenda-start-day "+0d")
                             (org-deadline-warning-days 0) ;; XXX
                             ))

                    (todo "PROJ"
                          ((org-agenda-overriding-header "Project list")))

                    (todo "DONE|PROJDONE"
                          ((org-agenda-overriding-header
                            "Candidates to be archived")))

                    ;; (stuck ""
                    ;;        ((org-agenda-overriding-header "Stuck projects")))

                    (todo "STRT"
                          ((org-agenda-overriding-header "In progress")
                           (org-agenda-tags-todo-honor-ignore-options nil)
                           (org-agenda-todo-ignore-scheduled nil)))

                    (todo "TODO" ;; don't include items from Inbox! XXX
                          ((org-agenda-overriding-header "Action list")))

                    (todo "WAIT|DLGT"
                          ((org-agenda-format-date "")
                           (org-agenda-overriding-header "Waiting for")
                           ;; (org-agenda-tags-todo-honor-ignore-options nil)
                           ;; (org-agenda-todo-ignore-deadlines nil)
                           ;; (org-agenda-todo-ignore-scheduled nil)))
                           ;; (org-deadline-warning-days 7)
                           ))

                    (todo "WAIT|DLGT"
                          ((org-agenda-include-all-todo nil)
                           (org-agenda-time-grid nil)
                           (org-agenda-todo-ignore-deadlines t)
                           (org-agenda-todo-ignore-with-date t)
                           (org-deadline-warning-days 360)))

                    (todo "DFRD|MAYB"
                          ((org-agenda-format-date "")
                           (org-agenda-overriding-header "Someday/maybe")
                           ;; (org-deadline-warning-days 7)
                           ))

                    (todo "DFRD|MAYB"
                          ((org-agenda-include-all-todo nil)
                           (org-agenda-time-grid nil)
                           (org-agenda-todo-ignore-deadlines t)
                           (org-agenda-todo-ignore-with-date t)
                           (org-deadline-warning-days 360)))

                   ;; ((org-agenda-block-separator "\n")
                   ;;  (org-agenda-clockreport-mode nil)
                   ;;  (org-agenda-prefix-format " %i %?-12t% s")
                   ;;  (org-agenda-span 'day)
                   ;;  (org-agenda-use-time-grid nil)
                   ;;  (org-agenda-write-buffer-name "Weekly task review"))
                   ;; "~/org-weekly-review.html") t)
                    )) t)

    (add-to-list 'org-agenda-custom-commands
                 '("rd" "Daily review"
                   ((agenda ""
                            ((org-agenda-entry-types '(:timestamp :sexp))
                             (org-agenda-overriding-header "Calendar")
                             (org-agenda-span 'day)
                             (org-agenda-start-on-weekday nil)))
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
                   ((org-agenda-format-date "%Y-%m-%d %a")
                    (org-agenda-start-with-clockreport-mode nil))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("d" . "5. Do the work...") t)

;;*** Calendar style views

    (add-to-list 'org-agenda-custom-commands
                 '("dc" "Calendar for current day"
                   agenda ""
                   ((org-agenda-entry-types '(:timestamp :sexp))
                    (org-agenda-include-all-todo nil)
                    (org-agenda-overriding-header "Calendar for today")
                    (org-agenda-prefix-format " %i %-12:t ")
                    (org-agenda-repeating-timestamp-show-all t)
                    (org-agenda-span 'day)
                    (org-agenda-use-time-grid t))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("dC" "Calendar for current week"
                   agenda ""
                   ((org-agenda-entry-types '(:timestamp :sexp))
                    (org-agenda-include-all-todo nil)
                    (org-agenda-overriding-header "Calendar for this week")
                    (org-agenda-prefix-format " %i %12:t ")
                    (org-agenda-repeating-timestamp-show-all t)
                    (org-agenda-span 'week)
                    (org-agenda-time-grid nil))) t)

;;*** Other views

    (add-to-list 'org-agenda-custom-commands
                 '("dh" "Hotlist"
                   tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY=\"A\"|FLAGGED"
                   ((org-agenda-todo-ignore-scheduled 'future))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("de" "Effort less than 1 hour"
                   tags-todo "Effort<1") t)

    (add-to-list 'org-agenda-custom-commands
                 '("dE" "Effort less than 1 hour"
                   tags-todo "Effort<\"1:00\"") t)

    ;; checking tasks that are assigned to me
    (add-to-list 'org-agenda-custom-commands
                 `("dm" "Tasks assigned to me"
                   tags ,(concat "Assignee=\"" user-login-name "\"")
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
                   nil ;; ((org-tag-faces nil))
                   ("~/org-agenda.txt" "~/org-agenda.html" "~/org-agenda.pdf")) t)

    (add-to-list 'org-agenda-custom-commands
                 '("Ep" "Call list"
                   tags-todo "phone"
                   ((org-agenda-prefix-format " %-20:c \u25A1 [ ] " )
                    (org-agenda-remove-tags t)
                    (org-agenda-with-colors nil)
                    (org-agenda-write-buffer-name "Org -- My Call list")
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
                    (tags-todo "+PRIORITY=\"A\"")
                    ;; medium important things to do
                    (tags-todo "+PRIORITY=\"B\"")
                    ;; other things to do
                    (tags-todo "+PRIORITY=\"C\""))) t)

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

;;*** Tags

    (add-to-list 'org-agenda-custom-commands
                 '("v:" . "Contexts...") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:h" "Home realm"
                   tags-todo "home") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:w" "Work realm"
                   tags-todo "work") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:e" "Errands context"
                   tags-todo "errands") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:p" "Phone context"
                   tags-todo "phone") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:m" "Mail context"
                   tags-todo "mail") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v:r" "Reading context"
                   tags-todo "reading") t)

    (add-to-list 'org-agenda-custom-commands
                 '("v::" "Next/Started and Delegated/Waiting For Actions Grouped by Context"
                   (;; Next/Started Actions
                    (tags-todo "phone&TODO={TODO\\|STRT}")
                    (tags-todo "mail&TODO={TODO\\|STRT}")
                    (tags-todo "reading&TODO={TODO\\|STRT}")
                    (tags-todo "errands&TODO={TODO\\|STRT}")
                    ;; Delegated/Waiting For Actions
                    (tags-todo "phone&TODO={WAIT\\|DLGT}")
                    (tags-todo "mail&TODO={WAIT\\|DLGT}")
                    (tags-todo "reading&TODO={WAIT\\|DLGT}")
                    (tags-todo "errands&TODO={WAIT\\|DLGT}"))) t)

    (add-to-list 'org-agenda-custom-commands
                 '("vp"
                   tags-todo ""
                   ((org-agenda-view-columns-initially t))) t)

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
                            ((org-agenda-entry-types '(:deadline))
                             (org-agenda-overriding-header
                              "Unscheduled upcoming due dates:")
                             (org-agenda-skip-entry-if 'scheduled)
                             (org-agenda-sorting-strategy '(deadline-up))
                             (org-agenda-span 'day)
                             (org-agenda-time-grid nil)
                             (org-deadline-warning-days 365))))
                   ((org-agenda-block-separator "\n")
                    (org-agenda-clockreport-mode nil)
                    (org-agenda-format-date "")
                    (org-agenda-span 'day)
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
                             (org-agenda-overriding-header "Overdue")
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
                   ((org-agenda-block-separator "\n")
                    (org-agenda-clockreport-mode nil)
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
                (and (or (not dl) ;; no deadline
                         (equal dl ""))
                     (or (not sd) ;; nor scheduled
                         (equal sd ""))))
            (progn (outline-next-heading) (point)))))

    (add-to-list 'org-agenda-custom-commands
                 '("dT" "Agenda for all TODO entries"
                   ((agenda ""
                            ((org-agenda-overriding-header "Overdue")
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
                            ((org-agenda-overriding-header "Next 12 days")
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
                   ((org-agenda-block-separator "\n")
                    (org-agenda-clockreport-mode nil)
                    (org-agenda-prefix-format " %i %?-12t% s")
                    (org-agenda-span 'day)
                    (org-agenda-use-time-grid nil)
                    (org-agenda-write-buffer-name "List Review"))
                   "org-agenda-all-todo-entries.html") t)

    ) ;; with-eval-after-load "org-agenda" ends here

;;** 10.7 (info "(org)Exporting Agenda Views")

  (leuven--section "10.7 (org)Exporting Agenda Views")

  ;; 10.7 alist of variable/value pairs that should be active during
  ;; agenda export
  (setq org-agenda-exporter-settings
        '((ps-number-of-columns 1)
          (ps-landscape-mode t)
          (htmlize-output-type 'css)))

;;** 10.8 (info "(org)Agenda column view")

  (leuven--section "10.8 (org)Agenda column view")

  ;; 10.8 default column format, if no other format has been defined
  (setq org-columns-default-format
        ;; "%65ITEM(Task) %DEADLINE(Due Date) %PRIORITY %6CLOCKSUM(Spent) %6Effort(Estim.){:}")
        ;; "%1BLOCKED %4TODO %CATEGORY %5Effort{:} %50ITEM %20TAGS %21ALLTAGS")
        "%60ITEM(Details) %5PRIORITY(Prio) %14SCHEDULED(Scheduled) %15TAGS(Context) %7TODO(To Do) %6CLOCKSUM(Clock) %5Effort(Effort){:} ")

  ;; DUPLICATE Obey `eval' variables -- RISKY!
  (setq enable-local-eval t)

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (remove-text-properties (point-min) (point-max)
                                      '(mouse-face t))))

;;* 11 (info "(org)Markup")

  (leuven--section "11 (org)Markup")

  (with-eval-after-load "org-faces"

    ;; add a face to #+begin_quote and #+begin_verse blocks
    (setq org-fontify-quote-and-verse-blocks t))

  (with-eval-after-load "org"

    ;;??? change the face of a headline (as an additional information) if
    ;; it is marked DONE (to face `org-headline-done')
    (setq org-fontify-done-headline t)

    ;; 11.1 hide the emphasis marker characters
    (setq org-hide-emphasis-markers t) ;; impact on table alignment!

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
      (org-display-inline-images)))

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

  ;; 11.7 interpret "_" and "^" for export when braces are used
  (setq org-export-with-sub-superscripts '{})

  ;; 11.7 convert LaTeX fragments to images when exporting to HTML (using MathJax)
  (setq org-export-with-LaTeX-fragments t) ;; XXX undefined?

  ;; highlight LaTeX and related syntax
  (setq org-highlight-latex-and-related
        '(latex
          script
          entities))

;;* 12 (info "(org)Exporting")

  ;; bind the exporter dispatcher to a key sequence
  (with-eval-after-load "org"

    ;; libraries in this list will be loaded once the export framework is needed
    (setq org-export-backends '(ascii html icalendar latex odt))

    ;; (define-key org-mode-map
    ;;   (kbd "C-c C-e") 'org-export-dispatch)

    ;; XXX temporary (until Org 8 is bundled within Emacs)
    (define-key org-mode-map
      (kbd "C-c C-e")
      (lambda ()
        (interactive)
        (if (fboundp 'org-export-dispatch)
            (org-export-dispatch)
          (message "Please upgrade to Org 8...")
          (sit-for 1)))))

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

    ;; activate smart quotes during export
    (setq org-export-with-smart-quotes t)

    ;; allow #+BIND to define local variable values for export
    (setq org-export-allow-bind-keywords t)

    ;; exported stuff will not be pushed onto the kill ring
    (setq org-export-copy-to-kill-ring nil)

    ;; ;; export and publishing commands will run in background
    ;; (setq org-export-in-background t)

    ;; file used to initialize external export process
    (setq org-export-async-init-file
          (expand-file-name "~/src/org-batch/bin/org-batch-init.el"))

    ;; ;; use a non-intrusive export dispatcher
    ;; (setq org-export-dispatch-use-expert-ui t)

    ) ;; with-eval-after-load "ox" ends here

  ;; execute buffer when exporting it (see some thread with Eric Schulte,
  ;; end of December 2010)
  ;;;;;;;;;; (add-hook 'org-export-first-hook 'org-babel-execute-buffer)

;;** 12.5 (info "(org)HTML export")

  ;; Org HTML export engine
  (with-eval-after-load "ox-html"

    ;; output type to be used by htmlize when formatting code snippets
    (setq org-export-htmlize-output-type 'css) ;; XXX

    ;; ;; URL pointing to a CSS file defining text colors for htmlized Emacs buffers
    ;; (setq org-export-htmlized-org-css-url "style.css")

    ;; XML declaration
    (setq org-html-xml-declaration
          '(("html" . "<!-- <xml version=\"1.0\"> -->")
            ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
            ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

    ;; coding system for HTML export
    (setq org-html-coding-system 'utf-8)

    ;; format for the HTML postamble
    (setq org-html-postamble
          "  <div id=\"copyright\">\n    &copy; %d %a\n  </div>")

    ;; 13.1.5 don't include the JavaScript snippets in exported HTML files
    (setq org-html-style-include-scripts nil)

    ;; 12.5.9 turn inclusion of the default CSS style off
    (setq org-html-style-include-default nil)

    ;; check that `tidy' is in PATH, and that configuration file exists
    (when (and (executable-find "tidy")
               (file-exists-p "~/.tidyrc"))

      (defun leuven--export-html-final-filter (contents backend info)
        (if (not (eq backend 'html)) contents
          (let* ((in-file "~/tidy-stdin.html")
                          ;; this filepath must be readable by Cygwin
                 (err-file "~/tidy.log")
                 new-contents)
            (with-temp-file in-file
              (insert contents))
            (setq new-contents
                  (shell-command-to-string
                   (format "tidy -config ~/.tidyrc -f %s %s"
                           err-file in-file)))
            (message "Buffer tidy'ed")
            (message "%s" (org-file-contents err-file))
            new-contents)))

      (add-to-list 'org-export-filter-final-output-functions
                   'leuven--export-html-final-filter))

    ) ;; eval-after-load "ox-html" ends here

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
  This forces the `css' style and only returns the HTML body, but
  without the BODY tag. This should make it useful for inserting
  the text to another HTML buffer."
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
    (global-set-key
      (kbd "M-P") 'htmlize-buffer)

    ) ;; with-eval-after-load "htmlize" ends here

  ;; quick print preview (to Web browser) with `htmlize-view-buffer'
  (GNUEmacs
    (autoload 'htmlize-view-buffer "htmlize-view" nil t)

    ;; same key binding as Org export to HTML (open in browser)
    (global-set-key
      (kbd "C-c C-e h o") 'htmlize-view-buffer)

    ;; view current buffer as html in web browser
    (with-eval-after-load "htmlize-view"

      ;; add "Quick Print" entry to file menu
      (htmlize-view-add-to-files-menu)))

;;** 12.6 (info "(org)LaTeX and PDF export")

  (leuven--section "12.6 (org)LaTeX and PDF export")

  ;; LaTeX back-end
  (with-eval-after-load "ox-latex"

    ;; markup for TODO keywords and for tags, as a printf format
    (defun org-latex-format-headline (todo todo-type priority text tags)
      "Default format function for an headline."
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

    ;; format string for links with unknown path type
    (setq org-latex-link-with-unknown-path-format "\\colorbox{red}{%s}")

    (defun leuven--change-pdflatex-program (backend)
      "When exporting an Org document to LaTeX, automatically run XeLaTeX,
    if asked."

      ;; default (in Windows binary)
      (setq org-latex-pdf-process
            (if (executable-find "latexmk")
                '("latexmk -pdf %f && rm -f %b.fdb_latexmk %b.fls %b.ilg %b.ind %b.*.vrb")
              '("pdflatex -interaction=nonstopmode -output-directory=%o %f"
                "pdflatex -interaction=nonstopmode -output-directory=%o %f"
                "pdflatex -interaction=nonstopmode -output-directory=%o %f")))

      (when (string-match "^#\\+LATEX_CMD: xelatex" (buffer-string))
        (setq org-latex-pdf-process
              (if (executable-find "latexmk")
                  '("latexmk -pdf -pdflatex=xelatex %f && rm -f %b.fdb_latexmk %b.fls %b.ilg %b.ind %b.*.vrb")
                '("xelatex -interaction=nonstopmode -output-directory=%o %f"
                  "xelatex -interaction=nonstopmode -output-directory=%o %f"
                  "xelatex -interaction=nonstopmode -output-directory=%o %f")))))

    ;; hook run before parsing an export buffer
    (add-hook 'org-export-before-parsing-hook
              'leuven--change-pdflatex-program)

    ;; export source code using `listings' (instead of `verbatim')
    (setq org-latex-listings t)

    ;; 12.6.2 default packages to be inserted in the header
    ;; include the `listings' package for fontified source code
    (add-to-list 'org-latex-packages-alist '("" "listings") t)

    ;; include the `xcolor' package for colored source code
    (add-to-list 'org-latex-packages-alist '("" "xcolor") t)

    ;; include the `babel' package for language-specific hyphenation and
    ;; typography
    (add-to-list 'org-latex-packages-alist '("frenchb" "babel") t)

    (defun leuven--change-pdflatex-packages (backend)
      "When exporting an Org document to LaTeX, automatically select the
    LaTeX packages to include (depending on PDFLaTeX vs XeLaTeX)."

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
    (add-hook 'org-export-before-parsing-hook
              'leuven--change-pdflatex-packages)

      ;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

      (defun leuven--latex-filter-nbsp (text backend info)
        "Ensure that non-breaking spaces are properly handled in LaTeX/Beamer export."
        (when (memq backend '(latex beamer))
          (replace-regexp-in-string " " "~" text)))

      ;; check that it's defined (in org-export.el)
      (add-to-list 'org-export-filter-plain-text-functions
                   'leuven--latex-filter-nbsp)

    ;; 12.6.5 default position for LaTeX figures
    (setq org-latex-default-figure-position "!htbp")

    ) ;; with-eval-after-load "ox-latex" ends here

  ;; 12.6.6 Beamer class export
  (with-eval-after-load "ox-beamer"

    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass[presentation,t]{beamer}
\\usepackage{etex}% avoid `too many packages' (for PDFTeX)
% default packages ---------------------
[DEFAULT-PACKAGES]
% packages -----------------------------
[PACKAGES]
% extra (#+LaTeX_HEADER: lines) --------
[EXTRA]
\\ifdefined\\DeclareUnicodeCharacter{\\DeclareUnicodeCharacter{00A0}{~}}\\fi
% end of `org-latex-classes' -------------------------------------------------"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) t)

    ;; default title of a frame containing an outline
    (setq org-beamer-outline-frame-title "Plan"))

  (with-eval-after-load "ox-odt"

    ;; convert "odt" format to "doc" format
    (setq org-odt-preferred-output-format "doc"))

;;* 13 (info "(org)Publishing")

  (leuven--section "13 (org)Publishing")

  (with-eval-after-load "ox-publish"

    ;; show message about files *not* published
    (setq org-publish-list-skipped-files nil)

    ;; ;; 13.2 always publish all files
    ;; ;; (do not use timestamp checking for skipping unmodified files)
    ;; (setq org-publish-use-timestamps-flag nil)

    ;; 13.4 force publishing all files
    (defun org-publish-all-force ()
      (interactive)
      (org-publish-all t)))

;;* 14 (info "(org)Working With Source Code")

  (with-eval-after-load "ob-core"

    ;; make the images in the Emacs buffer automatically refresh after
    ;; execution
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

  (with-eval-after-load "ob-sh"

    ;; command used to invoke a shell
    (setq org-babel-sh-command "bash")

    ;; use plain old syntax (instead of `$(...)') for Cygwin
    (setq org-babel-sh-var-quote-fmt
          "`cat <<'BABEL_TABLE'\n%s\nBABEL_TABLE\n`"))

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
  (defun leuven--org-babel-expand-src-block ()
    (interactive)
    (let ((org-src-window-setup 'reorganize-frame))
      (org-babel-expand-src-block)))

  ;; indent the content of a source code block
  (setq org-edit-src-content-indentation 2)

  ;; fontify code in code blocks (highlight syntax in the org-buffer)
  (setq org-src-fontify-natively t)
  ;;! create overlay `org-block-background' and remove text property
  ;;! `org-block'

  ;; preserve spaces and `tab' characters in source code blocks
  (setq org-src-preserve-indentation t)

  ;; same effect for `tab' as in the language major mode buffer
  (setq org-src-tab-acts-natively t)


  (with-eval-after-load "org"

    ;; allow comment region in the code edit buffer (according to language)
    (defun leuven-org-comment-dwim (&optional arg)
      (interactive "P")
      (or (org-babel-do-key-sequence-in-edit-buffer (kbd "M-;"))
          (comment-dwim arg)))

    ;; make `C-c C-v C-x M-;' more convenient
    (define-key org-mode-map
      (kbd "M-;") 'leuven-org-comment-dwim)

    ;; allow indent region in the code edit buffer (according to language)
    (defun leuven-org-indent-region (&optional arg)
      (interactive "P")
      (or (org-babel-do-key-sequence-in-edit-buffer (kbd "C-M-\\"))
          (indent-region arg)))

    ;; make `C-c C-v C-x C-M-\' more convenient
    (define-key org-mode-map
      (kbd "C-M-\\") 'leuven-org-indent-region))

  ;; prevent auto-filling in src blocks
  (setq org-src-prevent-auto-filling t)

  (global-set-key
    (kbd "C-c C-v C-d") 'org-babel-demarcate-block)

  (defvar only-code-overlays nil "Overlays hiding non-code blocks.")
  (make-variable-buffer-local 'only-code-overlays)

  (defun hide-non-code ()
    "Hide non-code-block content of the current Org-mode buffer."
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
    "Show non-code-block content of the current Org-mode buffer."
    (interactive)
    (mapc 'delete-overlay only-code-overlays))

;;** 14.5 (info "(org)Evaluating code blocks")

  (leuven--section "14.5 (org)Evaluating code blocks")

  ;; I don't want to execute code blocks with `C-c C-c' (evaluate code
  ;; block only with `C-c C-v e')
  (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

  ;; languages for which Babel will raise literate programming errors
  ;; when noweb references can not be resolved.

  (with-eval-after-load "ob-core"
    (add-to-list 'org-babel-noweb-error-langs "emacs-lisp"))

;;** 14.6 (info "(org)Library of Babel")

  (leuven--section "14.6 (org)Library of Babel")

  (with-eval-after-load "ob-lob"

    ;; load the NAMED code blocks defined in Org-mode files into the
    ;; library of Babel (global `org-babel-library-of-babel' variable)
    (let ((lob-file (concat (file-name-directory (locate-library "org"))
                            "../doc/library-of-babel.org")))
      (when (file-exists-p lob-file)
        (org-babel-lob-ingest lob-file))))

  ;; template used to export the body of code blocks
  (setq org-babel-exp-code-template
        ;; (concat "\n=%name=:\n"
                "#+BEGIN_SRC %lang%flags\n%body\n#+END_SRC")
        ;; )

  ;; keep lower-case
  (setq org-babel-results-keyword "results")

;;** 14.7 (info "(org)Languages")

  (leuven--section "14.7 (org)Languages")

  ;; FIXME Test executable-find (of Rterm, gnuplot, ruby, etc.) before
  ;; setting language to yes...

  (with-eval-after-load "org"

    (org-babel-do-load-languages ;; loads org, gnus-sum, etc...
     'org-babel-load-languages
     '((C . nil)
       (R . t) ;; requires R and ess-mode
       (awk . t)
       (calc . t)
       (ditaa . t) ;; sudo aptitude install openjdk-6-jre
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t) ;; requires gnuplot-mode
       (haskell . nil)
       (latex . t)
       (ledger . t) ;; requires ledger
       (ocaml . nil)
       (octave . nil)
       (org . t)
       (perl . nil)
       (python . t)
       (ruby . nil)
       (screen . nil)
       (sh . t)
       (sql . t)
       (sqlite . nil))))

;;* 15 (info "(org)Miscellaneous")

  ;; from Dan Davison
  (defun leuven-switch-to-org-scratch ()
    "Switch to a temp Org buffer. If the region is active, insert it."
    (interactive)
    (let ((contents (and (region-active-p)
                         (buffer-substring (region-beginning)
                                           (region-end)))))
      (find-file "/tmp/org-scratch.org")
      (if contents (insert contents))))

  ;; ;; flyspell should not check babel blocks
  ;; (defadvice org-mode-flyspell-verify
  ;;   (after leuven-org-mode-flyspell-verify activate)
  ;;   "Don't spell check src blocks."
  ;;   (require 'org-element)
  ;;   (setq ad-return-value
  ;;         (and ad-return-value
  ;;              (not (eq (org-element-type (org-element-at-point))
  ;;                       'src-block)))))

  (defun leuven--org-switch-language ()
    "Switch language for Org file, if a `#+LANGUAGE:' meta-tag is
  on top 8 lines."
    (save-excursion
      (goto-line (1+ 8))
      (if (re-search-backward "#\\+LANGUAGE: +\\([[:alpha:]_]*\\)" 1 t)
          (ispell-change-dictionary (match-string 1)))))

  ;; ;; guess language
  ;; (add-hook 'org-mode-hook 'leuven--org-switch-language)

;;** 15.2 (info "(org)Easy Templates")

  (leuven--section "15.2 (org)Easy Templates")

  (with-eval-after-load "org"

    ;; keep lower-case (easy templates)
    (setq org-structure-template-alist
          '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
            ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
            ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
            ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
            ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
            ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
            ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
            ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
            ("H" "#+html: " "<literal style=\"html\">?</literal>")
            ("a" "#+begin_ascii\n?\n#+end_ascii")
            ("A" "#+ascii: ")
            ("i" "#+index: ?" "#+index: ?")
            ("I" "#+include: %file ?" "<include file=%file markup=\"?\">"))))

;;** 15.3 (info "(org)Speed keys")

  (leuven--section "15.3 (org)Speed keys")

  (with-eval-after-load "org"

    ;; activate single letter commands at beginning of a headline
    (setq org-use-speed-commands t))

;;** 15.4 (info "(org)Code evaluation security") issues

  (leuven--section "15.4 (org)Code evaluation security issues")

  (with-eval-after-load "ob-core"

    ;;!! don't be prompted on every code block evaluation
    (setq org-confirm-babel-evaluate nil))

;;** 15.8 A (info "(org)Clean view")

  (with-eval-after-load "org"

    ;; 15.8 don't skip even levels for the outline
    (setq org-odd-levels-only nil))

;;** 15.10 (info "(org)Interaction")

  (leuven--section "15.10 (org)Interaction")

  ;; extension of Imenu
  (when (and ;; `org-babel' has been loaded
             (fboundp 'org-babel-execute-src-block)

             ;; `imenu' has been loaded
             (fboundp 'try-to-add-imenu))

    (try-require 'imenu+)

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
      (other-window 1)))

  ;; allow YASnippet to do its thing in Org files
  ;;! make sure you initialise YASnippet *before* Org mode
  (when (try-require 'yasnippet)

    (defun yas/org-very-safe-expand ()
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand)))

    (add-hook 'org-mode-hook
              (lambda ()
                ;; YASnippet (using the new org-cycle hooks)
                (set (make-local-variable 'yas/trigger-key) (kbd "tab")) ;; needed?
                (add-to-list 'org-tab-first-hook
                             'yas/org-very-safe-expand)
                (define-key yas/keymap
                  (kbd "tab") 'yas/next-field) ;; `yas/next-field-or-maybe-expand'?
                )))

  ;; keep my encrypted data (like account passwords) in my Org mode
  ;; files with a special tag instead
  (with-eval-after-load "org"

    (when (try-require 'org-crypt) ;; loads org, gnus-sum, etc...

      ;; encrypt all entries before saving
      (org-crypt-use-before-save-magic)

      ;; which tag is used to mark headings to be encrypted
      (setq org-tags-exclude-from-inheritance '("crypt"))))

  ;; don't pad tangled code with newlines
  (setq org-babel-tangle-pad-newline nil)

  ;; how to combine blocks of the same name during tangling
  (setq org-babel-tangle-named-block-combination 'append)


  ;; minimum number of lines for output *block* (placed in a
  ;; #+begin_example...#+end_example) vs output marked as literal by
  ;; inserting a *colon* at the beginning of the lines
  (setq org-babel-min-lines-for-block-output 2)

  ;; ;; FIXME Make this the default behavior
  ;; ;; grab the last line too, when selecting a subtree
  ;; (org-end-of-subtree nil t)

  ;; backend aware export preprocess hook
  (defun leuven--org-export-preprocess-hook ()
    "My backend aware export preprocess hook."
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
      (delete-backward-char 1)
      (unless (looking-back "=") (insert-and-inherit "="))
      (save-excursion
        (skip-syntax-forward "w_")
        (unless (looking-at "=") (insert-and-inherit "="))))))

  ;; must be in eval-after-load "org"?
  ;; (define-key org-mode-map
  ;;   (kbd "=") 'insert-one-equal-or-two)

  (with-eval-after-load "org"

    ;; using Org mode to send buffer/subtree per mail
    (when (try-require 'org-mime)

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
                                 (current-time-string))))))

;;** A.3 (info "(org)Adding hyperlink types")

  ;; ;; define a new link type (`latex') whose path argument can hold the name of
  ;; ;; any LaTeX command
  ;; (org-add-link-type
  ;;  "latex" nil
  ;;  (lambda (path desc format)
  ;;    (cond
  ;;     ((eq format 'html)
  ;;      (format "<span class=\"%s\">%s</span>" path desc))
  ;;     ((eq format 'latex)
  ;;      (format "\\%s{%s}" path desc)))))

  (with-eval-after-load "org"

    ;; add background color by using custom links like [[bgcolor:red][Warning!]]
    (org-add-link-type
      "bgcolor" nil
      (lambda (path desc format)
       (cond
        ((eq format 'html)
         (format"<span style=\"background-color:%s;\">%s</span>" path desc))
        ((eq format 'latex)
         (format"\\colorbox{%s}{%s}" path desc))
        (t
         (format"BGCOLOR LINK (%s): {%s}{%s}" format path desc))))))

;;** A.6 (info "(org)Dynamic blocks")

  ;; make sure that all dynamic blocks and all tables are always
  ;; up-to-date
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (org-align-all-tags)
                (org-update-all-dblocks)
                (when (fboundp 'org-table-iterate-buffer-tables)
                  (org-table-iterate-buffer-tables))
                (when (file-exists-p (buffer-file-name (current-buffer)))
                  (leuven--org-remove-redundant-tags)))))

  (GNUEmacs
    ;; add weather forecast in your Org agenda
    (autoload 'org-google-weather "org-google-weather" nil t)

    (with-eval-after-load "org-google-weather"
      ;; (try-require 'url)

      ;; add the city
      (setq org-google-weather-format "%C %i %c, %l°-%h°")))

) ;; chapter 25.9-org-mode ends here

;;** 25.10 (info "(emacs)TeX Mode")

(leuven--chapter leuven-chapter-25.10-tex-mode "25.10 TeX Mode"

  (leuven--section "25.10 (emacs)TeX Mode")

  ;; get colored PDFLaTeX output
  (define-derived-mode latex-output-mode fundamental-mode "LaTeX-Output"
    "Simple mode for colorizing LaTeX output."
    (set (make-local-variable 'font-lock-defaults)
         '((("^!.*" .
             compilation-error-face) ;; LaTeX error
            ("^-+$" .
             compilation-info-face) ;; latexmk divider
            ("^Package .* Warning: .*" .
             compilation-warning-face)
            ("Reference .* undefined" .
             compilation-warning-face)
            ("^\\(?:Overfull\\|Underfull\\|Tight\\|Loose\\).*" .
             font-lock-string-face)
            ("^LaTeX Font Warning:" .
             font-lock-string-face)
            ;; .....
            ))))

  (defadvice TeX-recenter-output-buffer
    (after leuven-colorize-latex-output activate)
    (with-selected-window (get-buffer-window (TeX-active-buffer))
      (latex-output-mode)))

  (leuven--section "25.10 (emacs)AUCTeX Mode")

;;** 1.2 (info "(auctex)Installation") of AUCTeX

  (ignore-errors
    (load "auctex.el"))

  ;; support for LaTeX documents
  (GNUEmacs
    (with-eval-after-load "latex"
      ;; if "tex", error when loading the TeX-mode, because we add-to-list in a
      ;; LaTeX variable (not loaded yet)

      ;; ;; TEST ??
      ;; (add-hook 'tex-mode-hook 'imenu-add-menubar-index)

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
      ;; cause problems with font-lock in latex-mode
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

      ;; use PDF mode by default (instead of DVI)
      (setq-default TeX-PDF-mode t)

;;** 4.2 (info "(auctex)Viewing") the formatted output

      (leuven--section "4.2 (auctex)Viewing the formatted output")

      (defvar sumatrapdf-command
        (concat windows-program-files-dir "SumatraPDF/SumatraPDF.exe")
        "Path to the SumatraPDF executable.")

      ;; use a saner PDF viewer (evince, SumatraPDF)
      (setcdr (assoc "^pdf$" TeX-output-view-style)
              (cond (running-ms-windows
                     `("." (concat "\"" ,sumatrapdf-command "\" %o")))
                    ;; under Windows, we could open the PDF file with
                    ;; `start "" xxx.pdf' (in a command prompt)
                    (t
                     '("." "evince %o"))))

      ;; for AUCTeX 11.86
      (when (boundp 'TeX-view-program-list)
        (add-to-list 'TeX-view-program-list
                     `("SumatraPDF"
                       (concat "\"" ,sumatrapdf-command "\" %o"))))

      (when running-ms-windows
        (setcdr (assoc 'output-pdf TeX-view-program-selection)
                '("SumatraPDF")))

;;** 4.3 (info "(auctex)Debugging") Catching the errors

      (leuven--section "4.3 (auctex)Debugging Catching the errors")

      ;; don't show output of TeX compilation in other window
      (setq TeX-show-compilation nil)

;;** 5.2 (info "(auctex)Multifile") Documents

      ;; AUCTeX will will assume the file is a master file itself
      (setq-default TeX-master t)

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
      (setq TeX-auto-local
            ;; must end with a slash
            "~/.emacs.d/auctex-auto-generated-info/")

;;** (info "(preview-latex)Top")

      (leuven--section "(preview-latex)Top")

      (when (locate-library "preview-latex")
        (load "preview-latex.el") t)

      (with-eval-after-load "preview"

        ;; path to `gs' command (for conversion from EPS)
        (setq preview-gs-command
          (cond (running-ms-windows
                 (concat windows-program-files-dir
                         "gs/gs9.01/bin/gswin32c.exe")) ;; XXX Not up-to-date!!!
                (t
                 "/usr/bin/gs")))
        (leuven--file-exists-and-executable-p preview-gs-command)

        ;; scale factor for included previews
        (setq preview-scale-function 1.2))

      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
                                 ;; with AUCTeX LaTeX mode

      ;; minor mode with distinct support for `\label', `\ref', `\cite'
      ;; and `\index' in LaTeX
      (with-eval-after-load "reftex"

        ;; turn all plug-ins on
        (setq reftex-plug-into-AUCTeX t)

        ;; use a separate selection buffer for each label type -- so the
        ;; menu generally comes up faster
        (setq reftex-use-multiple-selection-buffers t))

      )) ;; with-eval-after-load "latex" ends here

) ;; chapter 25.10-tex-mode ends here

(leuven--chapter leuven-chapter-25-text "25 Commands for Human Languages"

;;** 25.11 (info "(emacs)HTML Mode")

  (leuven--section "25.11 (emacs)HTML Mode")

  (with-eval-after-load "nxml-mode"

    ;; shortcut to view the current file in browser
    (define-key nxml-mode-map
      (kbd "C-c C-v") 'browse-url-of-buffer)

    ;; remove the binding of `C-c C-x', used by Org timeclocking commands
    ;; (add-hook 'nxml-mode-hook
    ;;           (lambda ()
                (define-key nxml-mode-map
                  (kbd "C-c C-x") nil)
    ;;             ))
                )

  (add-to-list 'auto-mode-alist '("\\.s?html?\\'" . xml-mode)) ;; alias for `nxml-mode'

;;** HTML Tidy

  (leuven--section "HTML Tidy")

  (when (executable-find "tidy")
    ;; interface to the HTML Tidy program
    (autoload 'tidy-buffer "tidy"
      "Run Tidy HTML parser on current buffer" t)
    (autoload 'tidy-parse-config-file "tidy"
      "Parse the `tidy-config-file'" t)
    (autoload 'tidy-save-settings "tidy"
      "Save settings to `tidy-config-file'" t)
    (autoload 'tidy-build-menu  "tidy"
      "Install an options menu for HTML Tidy." t)

    (with-eval-after-load "nxml"

      (defun leuven--nxml-mode-hook ()
        "Customize my nxml-mode."
        (tidy-build-menu nxml-mode-map)
        (local-set-key
          (kbd "C-c C-c") 'tidy-buffer)
        (setq sgml-validate-command "tidy"))

      (add-hook 'nxml-mode-hook 'leuven--nxml-mode-hook)))

) ;; chapter 25 ends here

;;** Weblint

;; (setq load-path (cons "path_to_weblint_directory/" load-path))
;; (autoload 'weblint "weblint" "Weblint syntax checker" t)

;;* 26 Editing (info "(emacs)Programs")

(leuven--chapter leuven-chapter-26-programs "26 Editing Programs"

;;** 26.1 Major Modes for (info "(emacs)Program Modes")

  (leuven--section "26.1 Major Modes for (emacs)Program Modes")

  (autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;** 26.2 Top-Level Definitions, or (info "(emacs)Defuns")

  (leuven--section "26.2 Top-Level Definitions, or (emacs)Defuns")

  (GNUEmacs
    ;; making buffer indexes as menus (awesome!)
    (when (try-require 'imenu-XXX)
      ;; imenu-add-to-menubar: Command attempted to use minibuffer while in
      ;; minibuffer

      ;; automatically add Imenu to the menu bar in /any/ mode that supports
      ;; it
      (defun try-to-add-imenu ()
        (condition-case nil
            (imenu-add-to-menubar "Imenu")
          (error nil)))
      (add-hook 'font-lock-mode-hook 'try-to-add-imenu)

      ;; show current function in mode line (based on Imenu)
      (which-func-mode 1))) ;; ~ Stickyfunc mode (in header line)

;;** 26.3 (info "(emacs)Program Indent")ation

    (leuven--section "26.3 (emacs)Program Indentation")

    ;; turn on auto-fill mode in Lisp modes
    (add-hook 'lisp-mode-hook 'turn-on-auto-fill)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

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

;;** 26.4 Commands for Editing with (info "(emacs)Parentheses")

  (leuven--section "26.4 Commands for Editing with (emacs)Parentheses")

  ;; jump to matching parenthesis
  (defun match-paren (arg)
    "Go to the matching parenthesis, if on a parenthesis."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

  (global-set-key
    (kbd "C-)") 'match-paren)

  ;; advanced highlighting of matching parentheses
  (if (try-require 'mic-paren)

      ;; activate `mic-paren' parenthesis highlighting (if the matching paren
      ;; is offscreen, show the matching line in the echo area + many other
      ;; useful things)
      (paren-activate)

    ;; enable Show Paren mode (highlight matching parenthesis)
    (GNUEmacs
      (show-paren-mode 1)
      (setq show-paren-style 'mixed)
      (setq show-paren-ring-bell-on-mismatch t))
    (XEmacs
      (paren-set-mode 'paren)))

  ;; highlight surrounding parentheses
  (GNUEmacs
    (autoload 'highlight-parentheses-mode "highlight-parentheses")

    ;; (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)

    (with-eval-after-load "highlight-parentheses"

      (define-globalized-minor-mode global-highlight-parentheses-mode
        highlight-parentheses-mode
        (lambda ()
          (highlight-parentheses-mode t)))
      (global-highlight-parentheses-mode t)

      (setq hl-paren-background-colors
            '("#FF993F" "#FFF33F" "#B0FF3F" "#4BFF4B"
              "#3FFFB0" "#3FF3FF" "#3F99FF" "#3F3FFF"))

      (setq hl-paren-colors
            '("black" "black" "black" "black" "black"
              "black" "black" "white"))))

;;** 26.5 (info "(emacs)Comments")

  (leuven--section "26.5 (emacs)Comments")

  ;; always comments out empty lines
  (setq comment-empty-lines t)

;;** 26.6 (info "(emacs)Documentation") Lookup

  (leuven--section "26.6 (emacs)Documentation Lookup")

  ;; show the function arglist or the variable docstring in the echo area
  (GNUEmacs
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;;** 26.7 (info "(emacs)Hideshow") minor mode

  (leuven--section "26.7 (emacs)Hideshow minor mode")

  ;; enable hideshow for programming modes
  (add-hook 'prog-mode-hook
            (lambda ()
              (hs-minor-mode 1)))

  ;; Especially after changing a couple of those really awkward
  ;; key bindings with `@' in the middle.
  ;; Changing: C-c @ c-s  to C-c s  (hs-show-block)
  ;;           C-c @ c-h  to C-c h  (hs-hide-block)
  ;; Seems not to collide with anything when in cperl-mode at least.

  ;; (define-key hs-minor-mode-map
  ;;   [?\C-c ?\C-\M-h] 'hs-hide-all)
  ;; (define-key hs-minor-mode-map
  ;;   [?\C-c ?\C-\M-s] 'hs-show-all)

  ;; (global-set-key (kbd "C-c @ @") 'hs-hide-all)
  ;; (global-set-key (kbd "C-c @ @") 'hs-show-all)
  (global-set-key
    (kbd "C-c @ h") 'hs-hide-block)
  (global-set-key
    (kbd "C-c @ s") 'hs-show-block)

  ;; if hideshowvis is not installed, do not attempt to configure it,
  ;; as this will prevent packages (including hideshowvis itself)
  ;; from compiling
  (when (and (display-graphic-p)
             (try-require 'hideshowvis-XXX))

    (autoload 'hideshowvis-enable
      "hideshowvis"
      "Highlight foldable regions")

    (autoload 'hideshowvis-minor-mode
      "hideshowvis"
      "Will indicate regions foldable with hideshow in the fringe."
      'interactive)

    ;; enable hideshowvis for programming modes
    (add-hook 'prog-mode-hook
              (lambda ()
                ;; more syntax definitions
                (require 'fold-dwim)
                (hideshowvis-enable)))

    ;; +/- fold buttons
    (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

    (defcustom hs-fringe-face 'hs-fringe-face
      "*Specify face used to highlight the fringe on hidden regions."
      :type 'face
      :group 'hideshow)

    (defface hs-fringe-face
      '((t (:box (:line-width 2 :color "#808080" :style released-button)
            :foreground "#999999")))
      "Face used to highlight the fringe on folded regions"
      :group 'hideshow)

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
        (let* ((marker-string "*fringe-dummy*")
               (marker-length (length marker-string))
               (display-string
                ;; (format "(%d)..."
                ;; (count-lines (overlay-start ov) (overlay-end ov)))
                "..."))
          (overlay-put ov 'help-echo "Hidden text. C-c,= to show")
          (put-text-property 0 marker-length
                             'display (list 'left-fringe
                                            'hs-marker
                                            'hs-fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (put-text-property 0 (length display-string)
                             'face 'hs-face display-string)
          (overlay-put ov 'display display-string))))

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

) ;; chapter 26 ends here

;;* 27 (info "(emacs)Building") Compiling and Testing Programs

(leuven--chapter leuven-chapter-27-building "27 Compiling and Testing Programs"

  (autoload 'flymake-mode "flymake" nil t)

  (with-eval-after-load "flymake"

    ;; set up `flymake'
    (defun activate-flymake ()
      "Activates flymake when real buffer and you have write access"
      (if (and (buffer-file-name) (file-writable-p buffer-file-name))
          (flymake-mode t)))

    ;; add errors to modeline -- With this the error output of othe current
    ;; line will appear right below in the modeline XXX
    (defun leuven--flymake-show-help ()
      (when (get-char-property (point) 'flymake-overlay)
        (let ((help (get-char-property (point) 'help-echo)))
          (if help (message "%s" help)))))

    (add-hook 'post-command-hook 'leuven--flymake-show-help))

;;** 27.1 Running (info "(emacs)Compilation")s under Emacs

  (leuven--section "27.1 Running (emacs)Compilations under Emacs")

  ;; http://www.emacswiki.org/emacs-en/eproject allows to define projects, and
  ;; in each project to define menu commands and shortcut keys as you like.
  ;; For example:
  ;;
  ;; make (f9)               : `-in src make' OR `make'
  ;; clean (C-f9)            : `rm -vf src/emacs-23.* etc/DOC* && make clean' OR `make clean'
  ;; run (f8)                : `src/emacs' OR `./my-program'
  ;; stop (C-f8)             : `-e kill-compilation'
  ;; ---
  ;; configure               : `./configure'
  ;; install                 : `echo root-pass | sudo -S make install'


  ;; invoke a compiler with the same command as in the last invocation of
  ;; `compile'
  (autoload 'recompile "compile" nil t)
  (global-set-key
    (kbd "<f9>") 'recompile)

  ;; scroll the `*compilation*' buffer window to follow output as it
  ;; appears
  (setq compilation-scroll-output t)

  ;; number of lines in a compilation window
  (setq compilation-window-height (* 2 5))

  ;; ;; I also don't like that the compilation window sticks around after
  ;; ;; a successful compile. After all, most of the time, all I care
  ;; ;; about is that the compile completed cleanly. Here's how I make the
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
    (require 'compile) ;; needed for compile-internal
    (if arg
        (setq make-clean-command
              (read-string "Command: " make-clean-command)))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-internal make-clean-command "No more errors"))

  (global-set-key
    (kbd "<S-f9>") 'make-clean)

;;** 27.2 (info "(emacs)Compilation Mode")

  (leuven--section "27.2 (emacs)Compilation Mode")

  ;; automatically jump to the first error during compilation
  (setq compilation-auto-jump-to-first-error t)

  ;; display the next compiler error message
  (global-set-key
    (kbd "<f10>") 'next-error)

  ;; display the previous compiler error message
  (global-set-key
    (kbd "<S-f10>") 'previous-error)

  ;; display the first compiler error message
  (global-set-key
    (kbd "<C-f10>") 'first-error)

  ;; highlight and parse the whole compilation output as soon as it
  ;; arrives
  (setq compile-auto-highlight t)

;;** 27.4 (info "(emacs)Grep Searching") under Emacs

  (leuven--section "27.4 (emacs)Grep Searching under Emacs")

  ;; ignore case distinctions in the default `grep' command
  (setq grep-command "grep -i -H -n -e ")

  ;; do not append `null-device' (`/dev/null' or `NUL') to `grep' commands
  (setq grep-use-null-device nil)
  ;; not necessary if the `grep' program used supports the `-H' option

  ;; ;; for Windows
  ;; (setq grep-find-command '("findstr /sn *" . 13))

  ;; use `find -print0' and `xargs -0'
  (setq grep-find-use-xargs 'gnu)

  ;; run `grep' via `find', with user-friendly interface
  (global-set-key
    (kbd "C-c 3") 'rgrep)

  ;; 10.3.5 Org keyword search
  (defun leuven-org-grep (regexp &optional context)
    "Recursively search for REGEXP in Org files in directory tree rooted at `org-directory'.
  Prefix argument determines number of lines of output context."
    (interactive "sSearch regexp: \nP")
    (let ((grep-find-ignored-files '("#*" ".#*"))
          (grep-template (concat "grep <X> -i -nH "
                                 (when context
                                   (concat "-C" (number-to-string context)))
                                 " -e <R> <F>")))
      (rgrep regexp "*.org" org-directory)))

;;** 27.6 Running (info "(emacs)Debuggers") Under Emacs

  (leuven--section "27.6 Running (emacs)Debuggers Under Emacs")

  ;; Gdb integration works quite well already
  ;; (the only important parameter for GDB)
  (setq gdb-many-windows t)

  ;; Prolog[Mercury] mode
  (autoload 'mercury-mode "prolog"
    "Major mode for editing Mercury programs." t)

  (setq prolog-system 'mercury)

  (add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))

;;** Debugging Lisp programs

  (add-hook 'cl-load-hook
            (lambda ()
              (add-hook 'edebug-setup-hook
                        (lambda ()
                          ;; edebug specs for cl.el
                          (load-library "cl-specs.el")))))

  (with-eval-after-load "edebug"

    (defadvice edebug-overlay-arrow (around leuven-highlight-line activate)
      "Highlight line currently being edebugged."
      (require 'hl-line)
      (hl-line-mode)
      ad-do-it)

    (defun leuven-edebug-quit ()
      "Stop edebugging and remove highlighting."
      (interactive)
      (hl-line-mode -1)
      (top-level))

    (define-key edebug-mode-map
      [remap top-level] 'leuven-edebug-quit))

  (defun autocompile nil
    "compile itself if ~/.emacs"
    (interactive)
    (require 'bytecomp)
    (let ((dotemacs (expand-file-name "~/.emacs"))) ;; emacs-leuven.el
      (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))

  (add-hook 'after-save-hook 'autocompile)

;;** 27.9 (info "(emacs)Lisp Eval") Expressions

  (leuven--section "27.9 (emacs)Lisp Eval Expressions")

  ;; enable the use of the command `eval-expression' without confirmation
  (put 'eval-expression 'disabled nil)

  ;; maximum depth of lists to print in the result of the evaluation
  ;; commands before abbreviating them
  (setq eval-expression-print-level nil) ;; no limit

  ;; maximum length of lists to print in the result of the evaluation
  ;; commands before abbreviating them
  (setq eval-expression-print-length nil) ;; no limit

;;** 27.10 Lisp Interaction Buffers

  (leuven--section "27.10 (emacs)Lisp Interaction Buffers")

  ;; inhibit the initial startup message in the `*scratch*' buffer
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)

  ;; major mode command symbol to use for the initial `*scratch*' buffer
  (setq initial-major-mode 'fundamental-mode)

  ;;;_ * eldoc

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (require 'edebug)))

  ;;;_ * elint

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

  ;;;_ * emacs-lisp

  (add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

  ;; (defun elisp-indent-or-complete (&optional arg)
  ;;   (interactive "p")
  ;;   (call-interactively 'lisp-indent-line)
  ;;   (unless (or (looking-back "^\\s-*")
  ;;          (bolp)
  ;;          (not (looking-back "[-[:alnum:]_*+/=<>!?]+")))
  ;;     (call-interactively 'lisp-complete-symbol)))
  ;;
  ;; (with-eval-after-load "lisp-mode"
  ;;   (define-key emacs-lisp-mode-map
  ;;     (kbd "<tab>") 'elisp-indent-or-complete))

  ;;;_  + paredit

  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)
  (autoload 'enable-paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)

  ;;;;;;;;;;;;;;(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

  ;;;_  + redshank

  (when (locate-library "redshank")
      (autoload 'redshank-mode "redshank"
        "Minor mode for restructuring Lisp code (i.e., refactoring)." t)

      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (redshank-mode 1))))

  ;; lang-emacs-lisp.el ends here

) ;; chapter 27 ends here

;;* 28 (info "(emacs)Maintaining") Programs

(leuven--chapter leuven-chapter-28-maintaining "28 Maintaining Programs"

;;** 28.1 (info "(emacs)Version Control")

  (leuven--section "28.1 (emacs)Version Control")

  ;; (try-require 'vc) ;; for defining function `vc-switches' (XXX autoload?)

;;*** 28.1.2 (info "(emacs)VC Mode Line")

  (leuven--section "28.1.2 Version Control and the Mode Line")

  (with-eval-after-load "vc"

    (GNUEmacs
      (when (image-type-available-p 'png)
        ;; http://www.emacswiki.org/emacs/VcIcon
        (defun vc-icon ()
          "Display a colored icon indicating the vc status of the current
        file."
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
    (when (try-require 'git-commit)

      ;; turn on on-the-fly spell-checking
      (add-hook 'git-commit-mode-hook 'turn-on-flyspell)

      ;; turn off save-place
      (add-hook 'git-commit-mode-hook
                (lambda ()
                  (toggle-save-place 0)))))

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
  (global-set-key
    (kbd "<C-f9>") 'leuven-vc-jump)

  ;; hide up-to-date and unregistered files
  (add-hook  'vc-dir-mode-hook
             (lambda ()
               (define-key vc-dir-mode-map
                 (kbd "x") 'leuven-vc-dir-hide-up-to-date-and-unregistered)
               (define-key vc-dir-mode-map
                 (kbd "E") 'vc-ediff)
               (define-key vc-dir-mode-map
                 (kbd "#") 'vc-ediff-ignore-whitespace) ;; ediff-windows-wordwise?
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
      (call-interactively 'vc-ediff))) ;; XXX does not work yet

;;*** 28.1.12 (info "(emacs)Customizing VC")

  (leuven--section "28.1.12 Customizing VC")

  ;; ;; display run messages from back-end commands
  ;; (setq vc-command-messages t) ;; [default: nil]

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
               (y-or-n-p (format "Buffer %s is modified. Save buffer? "
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

    (define-key vc-prefix-map
      (kbd "=") 'leuven-vc-diff))

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
          "/usr/local/lib/emacs/src/TAGS"
          ;; "/usr/share/texmf-texlive/tex/latex/TAGS"
          ))

  (defun find-next-tag ()
    (interactive)
    (find-tag nil t))

  (with-eval-after-load "etags"

    ;; select from multiple tags
    (when (try-require 'etags-select)

      ;; do a `find-tag-at-point', and display all exact matches
      (global-set-key
        (kbd "M-?") 'etags-select-find-tag-at-point)))

  ;; find the definition of the Emacs Lisp function or variable near point
  (GNUEmacs
    (find-function-setup-keys))

;;** 28.4 (info "(emacs)EDE")

  (leuven--section "28.4 Emacs Development Environment")

  (unless (string< emacs-version "23.2")
    ;; ;; enable global EDE (project management) features
    ;; (global-ede-mode 1)

    (setq semantic-default-submodes
          '(
            ;; turn Semantic DB mode on (Semantic parsers store the
            ;; results of parsing source code in a database file, which can
            ;; be saved for future Emacs sessions)
            global-semanticdb-minor-mode

            ;; the idle scheduler with automatically reparse buffers in idle
            ;; time
            global-semantic-idle-scheduler-mode ;; [minimum-features]

            ;; display a summary of the symbol at point in the echo area
            ;; (~ ElDoc)
            global-semantic-idle-summary-mode ;; [code-helpers]

            ;; display a tooltip with a list of possible completions near
            ;; the cursor
            global-semantic-idle-completions-mode ;; [gaudy-code-helpers]

            ;; turn Semantic MRU Bookmarks on (keep track of the Most
            ;; Recently Used tags)
            global-semantic-mru-bookmark-mode

            ;; enable Semantic-Stickyfunc mode (display a header line that
            ;; shows the declaration line of the function or tag)
            global-semantic-stickyfunc-mode ;; [gaudy-code-helpers]

            ;; enable Semantic-Highlight-Func mode
            global-semantic-highlight-func-mode ;; [excessive-code-helpers]

            ;; turn on all active decorations
            global-semantic-decoration-mode ;; [gaudy-code-helpers]
            ))

    ;; XXX if prog-mode, then Semantic will be launched after Emacs init, as
    ;; the scratch buffer is in Emacs Lisp...
    (add-hook 'java-mode-hook
              (lambda ()
                ;; enable parser features (Semantic mode) and install a
                ;; `Development' menu on the menu-bar
                (semantic-mode 1)))

    ;; ;; smart completion, and display of information for tags & classes
    ;; (require 'semantic/ia)
    ;;
    ;; (require 'semantic/db)

    (with-eval-after-load "semantic"

      (defun leuven--semantic ()
        ;; automatically complete whatever symbol you are typing
        (local-set-key "\C-c?" 'semantic-ia-complete-symbol) ;; better binding: `M-/'?

        ;; jump to the definition of the symbol under cursor
        (local-set-key "\C-cj" 'semantic-ia-fast-jump) ;; where a symbol is declared

        ;; show the documentation of the symbol under cursor
        (local-set-key "\C-cq" 'semantic-ia-show-doc) ;; show javadoc of the right method

        ;; show a summary about the symbol under cursor
        (local-set-key "\C-cs" 'semantic-ia-show-summary)


        ;; show possible public member functions or data members (when at `.'
        ;; or `->' after an object name)
        (local-set-key "\C-c>" 'semantic-complete-analyze-inline)

        ;; toggle between the implementation and a prototype of symbol under
        ;; cursor
        (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)

        ;; visit the header file under cursor
        (local-set-key "\C-c=" 'semantic-decoration-include-visit)


        ;; unfold the block under cursor
        (local-set-key "\C-c+" 'semantic-tag-folding-show-block)

        ;; fold the block under cursor
        (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)

        ;; C-c C-c is not a prefix key!
        ;; ;; unfold all
        ;; (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
        ;;
        ;; ;; fold all
        ;; (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
        )

      (add-hook 'prog-mode-hook 'leuven--semantic)

      (defun leuven--c-mode-semantic ()
        "Completion on `.' or `->'."
        (local-set-key "." 'semantic-complete-self-insert)
        (local-set-key ">" 'semantic-complete-self-insert)
        (local-set-key "\C-c\C-r" 'semantic-symref))

      (add-hook 'c-mode-common-hook 'leuven--c-mode-semantic))

    ;; hooks, specific for Semantic
    (defun leuven--semantic-imenu ()
      (imenu-add-to-menubar "TAGS"))

    (add-hook 'semantic-init-hooks 'leuven--semantic-imenu)

    )

  ;; Emacs Code Browser
  (custom-set-variables '(ecb-options-version "2.40"))
  (when (try-require 'ecb-autoloads-XXX)

    ;; trick for starting ECB 2.40 (with CEDET merged in Emacs since 23.2)
    (GNUEmacs
      (require 'semantic/analyze))
    (provide 'semantic-analyze)
    (provide 'semantic-ctxt)
    (provide 'semanticdb)
    (provide 'semanticdb-find)
    (provide 'semanticdb-mode)
    (provide 'semantic-load)

    (setq stack-trace-on-error t)

    ;; don't show tip of the day at start time of ECB
    (setq ecb-tip-of-the-day nil)

    ;; toggle activation of ECB (between `ecb-activate' and
    ;; `ecb-deactivate')
    (global-set-key
      (kbd "C-c e") 'ecb-minor-mode)

    ;; (global-set-key (kbd "<M-left>") 'ecb-goto-window-methods)
    ;; (global-set-key (kbd "<M-right>") 'ecb-goto-window-edit1)
    )

) ;; chapter 28 ends here

;;* 29 (info "(emacs)Abbrevs")

(leuven--chapter leuven-chapter-29-abbrevs "29 Abbrevs"

  ;; See (info "(autotype)") as well

;;** 29.3 Controlling (info "(emacs)Expanding Abbrevs")

  (leuven--section "29.3 Controlling Expanding Abbrevs")

  ;; Yet Another Snippet extension for Emacs
  (GNUEmacs
    ;; use the "standard" package (NOT `yasnippet-bundle'!)
    (when (try-require 'yasnippet)

      (defvar leuven-yasnippet-my-snippets-dir
        "~/src/yasnippet/snippets"
        "Directory containing my personal additional YASnippets.")

      ;; root directories that store the snippets
      (let ((my-snippets
             leuven-yasnippet-my-snippets-dir)
            (org-snippets
             (concat leuven-local-repos-directory "yasnippet-org-mode")))

        (when (file-directory-p org-snippets)
          (add-to-list 'yas-snippet-dirs org-snippets))

        ;; the first element is always the user-created snippets directory
        (when (file-directory-p my-snippets)
          (add-to-list 'yas-snippet-dirs my-snippets)))

      ;; use Snippet mode for files with a `yasnippet' extension
      (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

      ;; enable the YASnippet menu and tab-trigger expansion in *all*
      ;; buffers
      (yas-global-mode 1)

      ;; ;; text that will be used in menu to represent the trigger
      ;; (setq yas-trigger-symbol " <tab>")

      (add-hook 'snippet-mode-hook
                (lambda ()
                  (setq require-final-newline nil)))

      ;; automatically reload snippets after saving
      (defun recompile-and-reload-all-snippets ()
        (interactive)
        (when (eq major-mode 'snippet-mode)
          (yas-recompile-all)
          (yas-reload-all)
          (message "Reloaded all snippets")))

      (add-hook 'after-save-hook 'recompile-and-reload-all-snippets)))

;;** 29.7 (info "(emacs)Dabbrev Customization")

  (leuven--section "29.7 Dabbrev Customization")

  ;; preserve case when expanding the abbreviation
  (setq dabbrev-case-replace nil)

  ;; expand text trying various ways to find its expansion
  (global-set-key
    (kbd "M-/") 'hippie-expand)

  (with-eval-after-load "hippie-exp"

    ;; list of expansion functions tried (in order) by `hippie-expand'
    ;; (completion strategy)
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev ;; current buffer
            try-expand-dabbrev-visible ;; visible (parts of all) buffers
            try-expand-dabbrev-all-buffers ;; all opened buffers
            try-expand-dabbrev-from-kill ;; kill ring
            try-complete-file-name-partially ;; file names
            try-complete-file-name
            try-expand-all-abbrevs ;; abbreviations
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-whole-kill))

    (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill))

    ;; integrate YASnippet with `hippie-expand'
    (with-eval-after-load "yasnippet"

      (add-to-list 'hippie-expand-try-functions-list
                    'yas/hippie-try-expand)))

  (GNUEmacs

    ;; Auto Completion
    (when (locate-library "auto-complete-config")
      (idle-require 'auto-complete-config)

      (with-eval-after-load "auto-complete-config"

        ;; avoid flyspell processes when auto completion is being started
        (ac-flyspell-workaround)

        ;; optimize sources to use
        (ac-config-default)

        ;; change default sources
        (setq-default ac-sources
                      ;; buffers whom major-mode is same to of a current
                      ;; buffer
                      '(ac-source-words-in-same-mode-buffers))

        ;; change sources for Emacs Lisp mode
        (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (setq ac-sources
                          '(ac-source-words-in-same-mode-buffers
                            ac-source-words-in-buffer
                            ac-source-symbols))))

        ;; enable auto-complete-mode automatically for Sword mode
        (add-to-list 'ac-modes 'sword-mode)

        ;; ;; delay to completions will be available
        ;; (setq ac-delay 0.1)

        ;; completion menu will be automatically shown
        (setq ac-auto-show-menu 0.3)

        ;; ;; start auto-completion at current point
        ;; (define-key ac-mode-map
        ;;   (kbd "M-TAB") 'auto-complete)

        ;; try expand
        (define-key ac-complete-mode-map
          (kbd "<tab>") 'ac-expand))))

) ;; chapter 29 ends here

;;* 30 (info "(emacs)Dired"), the Directory Editor

(leuven--chapter leuven-chapter-30-dired "30 Dired, the Directory Editor"

;;** (info "(emacs)Dired Enter")

  ;; directory-browsing commands
  (with-eval-after-load "dired"

    (leuven--section "30.1 (emacs)Dired Enter")

    ;; switches passed to `ls' for Dired
    (setq dired-listing-switches
          ;; "-a -F --group-directories-first -l -p --time-style=long-iso"
          ;; causes display problems on Windows
          "-a -F -l -p"
          )

;;** (info "(emacs)Dired Deletion")

    (leuven--section "30.3 (emacs)Dired Deletion")

    ;; recursive deletes allowed, after asking for each directory at top
    ;; level
    (setq dired-recursive-deletes 'top)

;;** (info "(emacs)Dired Visiting")

    (leuven--section "30.5 (emacs)Dired Visiting")

    ;; reuse Dired buffers, by running the command
    ;; `dired-find-alternate-file' (bound to `a') on a directory
    (put 'dired-find-alternate-file 'disabled nil)

    ;; reuse the current Dired directory buffer to visit another directory
    ;; (limit Dired to 1 single buffer)
    (when (try-require 'dired-single)

      (define-key dired-mode-map
        (kbd "<return>") 'joc-dired-single-buffer)

      (define-key dired-mode-map
        (kbd "<mouse-1>") 'joc-dired-single-buffer-mouse)

      (define-key dired-mode-map
        (kbd "^")
        (lambda ()
          (interactive)
          (joc-dired-single-buffer "..")))

      ;; (define-key dired-mode-map
      ;;   (kbd "C-x C-j")
      ;;   (lambda ()
      ;;     (interactive)
      ;;     (joc-dired-single-buffer "..")))
      )

    (define-key dired-mode-map
      (kbd "e") 'browse-url-of-dired-file) ;; <C-RET>?

    ;; open files using Windows associations
    (GNUEmacs
      (when running-ms-windows
        (defun w32-dired-open-files-externally (&optional arg)
          "In Dired, open the marked files (or directories) with the default
        Windows tool."
          (interactive "P")
          (mapcar
           #'(lambda (file)
               (w32-shell-execute "open" (convert-standard-filename file)))
           (dired-get-marked-files nil arg)))

        ;; bind it to `E' in Dired mode
        (define-key dired-mode-map
          (kbd "E") 'w32-dired-open-files-externally)))

    ;; open current file with w3m
    (when (executable-find "w3m")
      (defun dired-find-w3m ()
        "In Dired, visit (with find-w3m) the file named on this line."
        (interactive)
        (w3m-find-file (file-name-sans-versions (dired-get-filename) t)))

      ;; add a binding "W" -> `dired-find-w3m' to Dired
      (define-key dired-mode-map
        "W" 'dired-find-w3m))

;;** (info "(emacs)Operating on Files")

    (leuven--section "30.7 (emacs)Operating on Files")

    ;; try to guess a default target directory
    (setq dired-dwim-target t)

    ;; copy recursively without asking
    (setq dired-recursive-copies 'always)

;;** (info "(emacs)Dired Updating")

    (leuven--section "30.15 (emacs)Dired Updating")

    ;; add-on for sorting
    (try-require 'dired-sort-map)
    ;; press `s' then `s', `x', `t', `n' or `d' to sort by
    ;;  Size, eXtension, Time, Name or name grouping Dirs

;;** (info "(emacs)Dired and Find")

    (leuven--section "30.16 (emacs)Dired and Find")

    ;; ;; what to use in place of `-ls' as the final argument
    ;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
    ;; ;; quicker to collate the matches and then use `xargs' to run the
    ;; ;; command (variable defined in `find-dired.el')

;; (when Cygwin... XXX
    ;; search for files with names matching a wild card pattern and Dired
    ;; the output
    (global-set-key
      (kbd "C-c 1") 'find-name-dired)
      ;; case insensitive if `read-file-name-completion-ignore-case' is non-nil

    ;; `find-grep-dired' case insensitivity
    (setq find-grep-options "-i -q")

    ;; search for files with contents matching a wild card pattern and Dired
    ;; the output
    (global-set-key
      (kbd "C-c 2") 'find-grep-dired)

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

    ) ;; with-eval-after-load "dired" ends here

;;** Dired+

  (leuven--section "30.XX Dired+")

  ;; extensions to Dired (provides fancy highlighting, etc.)
  (add-hook 'dired-load-hook
            (lambda ()
              (try-require 'dired+)))

;;** (info "(emacs)ls in Lisp")

  (leuven--section "G.4 (emacs)ls in Lisp")

  ;; disable the case sensitive sort of file names
  (setq ls-lisp-ignore-case t)

  ;; sort directories first
  (setq ls-lisp-dirs-first t)

  ;; use ISO 8601 dates (on MS-Windows)
  (setq ls-lisp-format-time-list
        '("%Y-%m-%d %H:%M"
          "%Y-%m-%d %H:%M"))

  ;; use localized date/time format
  (setq ls-lisp-use-localized-time-format t)

) ;; chapter 30 ends here

;;* 31 The (info "(emacs)Calendar/Diary")

(leuven--chapter leuven-chapter-31-calendar-diary "31 The Calendar and the Diary"

;;** 31.1 (info "(emacs)Calendar Motion")

  (leuven--section "31.1 (emacs)Calendar Motion")

  ;; years must be written in full
  (setq diary-abbreviated-year-flag nil)

  ;; set the style of calendar and diary dates to ISO (how to interpret
  ;; the dates)
  (setq calendar-date-style 'iso) ;; or 'european

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
              (define-key calendar-mode-map
                (kbd ">") 'scroll-calendar-left)
              (define-key calendar-mode-map
                (kbd "<") 'scroll-calendar-right)))

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

  (defun leuven--diary-sunrise () ;; XXX
    (let ((dss (diary-sunrise-sunset)))
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (search-forward ",")
        (buffer-substring (point-min) (match-beginning 0)))))

  (defun leuven--diary-sunset ()
    (let ((dss (diary-sunrise-sunset))
          start end)
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (search-forward ", ")
        (setq start (match-end 0))
        (search-forward " at")
        (setq end (match-beginning 0))
        (goto-char start)
        (capitalize-word 1)
        (buffer-substring start end))))

;;** 31.11 (info "(emacs)Appointments")

  (leuven--section "31.11 (emacs)Appointments")

  ;; insinuate appt if `diary-file' exists
  (if (file-readable-p "~/diary")
      (try-require 'appt) ;; requires `diary-lib', which requires `diary-loaddefs'
    (message "Appointment reminders library `appt' not required (no diary file found)"))

  (with-eval-after-load "appt"

    ;; send the first warning 60 minutes before an appointment
    (setq appt-message-warning-time 60) ;; [default: 12]

    ;; warn every 15 minutes
    (setq appt-display-interval 15) ;; [default: 3]

    ;; use a separate window to display appointment reminders
    (setq appt-display-format 'window)

    ;; function called to display appointment reminders *in a window*
    (setq appt-disp-window-function (function leuven--appt-display))

    (defun leuven--appt-display (mins-to-appt current-time notification-string)
      "Display a reminder for appointments.

    Use libnotify if available and if display is graphical, or fall back on a
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
    (when leuven-load-verbose (message "(info) Enable appointment reminders..."))
    (GNUEmacs
      (appt-activate 1))
    (XEmacs
      (appt-initialize))
    (when leuven-load-verbose (message "(info) Enable appointment reminders... Done"))

    ;; enable appointment notification, several minutes beforehand
    (add-hook 'diary-hook 'appt-make-list)

    (with-eval-after-load "org-agenda"

      ;; keep your appointment list clean: if you delete an appointment from
      ;; your Org agenda file, delete the corresponding alert
      (defadvice org-agenda-to-appt (before leuven-org-agenda-to-appt activate)
        "Clear the existing appt-time-msg-list."
        (setq appt-time-msg-list nil))

      ;; add today's appointments (found in `org-agenda-files') each time the
      ;; agenda buffer is (re)built
      (add-hook 'org-finalize-agenda-hook
                'org-agenda-to-appt)
      ;;! don't use the `org-agenda-mode-hook' because the Org agenda files
      ;;! would be opened once by `org-agenda-to-appt', and then killed by
      ;;! `org-release-buffers' (because `org-agenda-to-appt' closes all the
      ;;! files it opened itself -- as they weren't already opened), to be
      ;;! finally re-opened!

      ;; add today's appointments (found in `org-agenda-files') each time such a
      ;; file is saved
      (add-hook 'after-save-hook
                (lambda ()
                  (when (and (eq major-mode 'org-mode)
                             (org-agenda-file-p))
                    (org-agenda-to-appt)))))

    ) ;; with-eval-after-load "appt" ends here

;;** 31.15 (info "(emacs)Advanced Calendar/Diary Usage")

  (leuven--section "31.15 (emacs)Advanced Calendar/Diary Usage")

  ;; get rid of some holidays
  (setq holiday-general-holidays nil) ;; too U.S.-centric holidays
  (setq holiday-oriental-holidays nil) ;; Oriental holidays
  (setq holiday-hebrew-holidays nil) ;; religious holidays
  (setq holiday-islamic-holidays nil) ;; religious holidays
  (setq holiday-bahai-holidays nil) ;; Baha'i holidays
  (setq holiday-solar-holidays nil) ;; sun-related holidays

  ;; mark dates of holidays in the calendar window
  (setq calendar-mark-holidays-flag t)

  (defun leuven-insert-current-date (prefix)
    "Insert the current date in ISO format. With one PREFIX argument,
  add day of week. With two PREFIX arguments, add day of week and
  time."
    (interactive "P")
    (let ((format (cond ((not prefix) "%Y-%m-%d")
                        ((equal prefix '(4)) "%Y-%m-%d %a")
                        ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
      (insert (format-time-string format))))

  (global-set-key
    (kbd "C-c .") 'leuven-insert-current-date)

;;* Calendar view framework on Emacs

  ;; calendar view for org-agenda
  (when (locate-library "calfw")

    ;; Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)

    (autoload 'cfw:open-org-calendar "calfw-org"
      "Open an Org schedule calendar." t)

    (with-eval-after-load "calfw-org"

      ;; remove some strings (tags and filenames) from item summary
      (defun cfw:org-summary-format (item)
        "Format an item. (How should be displayed?)"
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
           ;; table layout.
           'display nil)))))

) ;; chapter 31 ends here

;;* 32 (info "(emacs)Sending Mail")

(leuven--chapter leuven-chapter-32-sending-mail "32 Sending Mail"

  ;; full name of this user
  (setq user-full-name "John Doe")

  ;; full mailing address of this user
  ;; (used in MAIL envelope FROM, and to select the default personality
  ;; ID)
  (setq user-mail-address "john@doe.com")

  ;; sending mail
  (setq send-mail-function 'smtpmail-send-it)

  (with-eval-after-load "smtpmail"

    ;; name of the host running SMTP server
    (setq smtpmail-smtp-server
          (or (getenv "SMTPSERVER")
              "mail"))

    ;; SMTP service port number
    (setq smtpmail-smtp-service 25))

) ;; chapter 32 ends here

;;* 34 (info "(emacs)Gnus")

(leuven--chapter leuven-chapter-34-gnus "34 Gnus"

  ;; from NoGnus
  (GNUEmacs
    (try-require 'gnus-load)) ;; generated by `make'

  (defun switch-or-start (function buffer)
    "If the buffer is current, bury it. If there is a buffer with that
    name, switch to it; otherwise invoke the function."
    (if (equal (buffer-name (current-buffer)) buffer)
        (bury-buffer)
      (if (get-buffer buffer)
          (switch-to-buffer buffer)
        (funcall function))))

  (global-set-key
    (kbd "C-c n")
    (lambda ()
      (interactive)
      (switch-or-start 'gnus "*Group*")))

  ;; directory beneath which additional per-user Gnus-specific files are
  ;; placed
  (setq gnus-directory "~/.gnus.d/")
  ;; note that this should end with a directory separator

  ;; a newsreader for GNU Emacs
  (with-eval-after-load "gnus"

    ;; package to compose an outgoing mail (Message, with Gnus
    ;; paraphernalia)
    (setq mail-user-agent 'gnus-user-agent)
    (XEmacs
      (setq toolbar-mail-reader 'gnus))

    ;; reading mail with Gnus
    (setq read-mail-command 'gnus))

;;** Insidious bbdb

  (leuven--section "Insidious bbdb")

;;* (info "(bbdb)Installation")

  (unless (ignore-errors (load-library "bbdb-autoloads")) ;; "hand-made"
    (autoload 'bbdb         "bbdb-com"
      "Insidious Big Brother Database" t)
    (autoload 'bbdb-name    "bbdb-com"
      "Insidious Big Brother Database" t)
    (autoload 'bbdb-company "bbdb-com"
      "Insidious Big Brother Database" t)
    (autoload 'bbdb-net     "bbdb-com"
      "Insidious Big Brother Database" t)
    (autoload 'bbdb-notes   "bbdb-com"
      "Insidious Big Brother Database" t)

    (autoload 'bbdb-insinuate-gnus "bbdb-gnus"
      "Hook BBDB into Gnus.")
    ;; (autoload 'bbdb-insinuate-message "bbdb"
    ;;   "Hook BBDB into `message-mode'.") ;; BBDB 2.35
    (autoload 'bbdb-insinuate-message "bbdb-message"
      "Hook BBDB into `message-mode'."))

  ;; search the BBDB
  (global-set-key
    (kbd "<C-f11>") 'bbdb)

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

    ;; - add bindings for the default keys to Gnus and configure Gnus to
    ;;   notify the BBDB when new messages are loaded (required if the
    ;;   BBDB is to be able to display BBDB entries for messages displayed
    ;;   in Gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

    ;; - add a binding for `M-TAB' to Message mode. This will enable
    ;;   completion of addresses based on BBDB records
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)


    ;; (bbdb-mua-auto-update-init 'gnus 'message)


    ;; (define-key gnus-summary-mode-map
    ;;   (kbd ":") 'bbdb-mua-display-sender)


    ;; ;; customizable completion in message headers
    ;; ;; (to avoid conflict between `flyspell' and `BBDB')
    ;; (try-require 'message-x)

    ;; FIXME Does not work (still ask to add address)
      ;; don't ask about fake addresses
      ;; NOTE: there can be only one entry per header (such as To, From)
      ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
      (setq bbdb-ignore-some-messages-alist
            '(("From" . "no.?reply\\|public.gmane.org")))

;;* (info "(bbdb)Interfaces")

    ;; mail aliases (local mailing lists)
    ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases) ;; BBDB 2.35
    (add-hook 'message-setup-hook 'bbdb-get-mail-aliases) ;; BBDB 3

    ;; always use full name when sending mail
    ;; (even if User Name has an address of the form <user.name@domain>)
    (setq bbdb-dwim-net-address-allow-redundancy t) ;; BBDB 2.35
    (setq bbdb-mail-avoid-redundancy nil) ;; BBDB 3

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

    ;; replace the information provided in the From header with data from
    ;; the BBDB if we have one
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
    (setq bbdb-north-american-phone-numbers-p nil) ;; BBDB 2.35
    (setq bbdb-phone-style nil) ;; BBDB 3

    ;; restoration of the window configuration
    (setq bbdb-electric-p t) ;; BBDB 2.35
    (setq bbdb-electric t) ;; BBDB 3

    ;; don't display a continuously-updating BBDB window while in GNUS
    ;; (setq bbdb-use-pop-up nil) ;; BBDB 2.35
    ;; (setq bbdb-pop-up-layout nil) ;; BBDB 3

    ;; desired number of lines in a GNUS pop-up BBDB window
    (setq bbdb-pop-up-target-lines 1) ;; BBDB 2.35
    (setq bbdb-pop-up-window-size 1) ;; BBDB 3

    ;; default display layout
    (setq bbdb-display-layout 'multi-line)

    ;; default display layout pop-up BBDB buffers
    (setq bbdb-pop-up-display-layout 'one-line)

    ;; omit creation-date and timestamp from BBDB display
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
    (setq bbdb-complete-name-allow-cycling t) ;; BBDB 2.35
    (setq bbdb-complete-mail-allow-cycling t) ;; BBDB 3

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
                             "\\1\\3\\5\\7"))))

;;* (info "(bbdb)Utilities")

    ;; use BBDB to store PGP preferences
    (when (try-require 'bbdb-pgp)
      ;; what to do if the recipient is not in the BBDB
      (setq bbdb/pgp-default-action nil)))

) ;; chapter 34 ends here

;;* 35 (info "(emacs)Document View")

(leuven--chapter leuven-chapter-35-document-view "35 Document Viewing"

  ;; view PDF/PostScript/DVI files in Emacs

;;** 35.1 (info "(emacs)Navigation")

  (leuven--section "35.1 (emacs)Navigation")

  ;; `doc-view' integrates with the usual bookmark facility. So simply
  ;; use `C-x r m' (`bookmark-set') to jump back to the last page you've
  ;; read in a PDF document.

  ;; antiword will be run on every `.doc' file you open
  ;; TODO sudo aptitude install antiword (or via Cygwin setup)
  (autoload 'no-word "no-word"
    "word to txt")
  (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))



  (add-to-list 'auto-mode-alist '("\\.docx\\'" . docx2txt))

  (defun docx2txt ()
    "Run docx2txt on the entire buffer."
    (shell-command-on-region (point-min) (point-max) "docx2txt.pl" t t))



  ;; un-xls files
  ;; TODO sudo aptitude install xlhtml
  (add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
  (defun no-xls (&optional filename)
    "Run xlhtml and w3m -dump on the entire buffer.
  Optional FILENAME says what filename to use.
  This is only necessary for buffers without
  proper `buffer-file-name'.  FILENAME should
  be a real filename, not a path."
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
    "Run ppthtml and w3m -dump on the entire buffer.
  Optional FILENAME says what filename to use.
  This is only necessary for buffers without
  proper `buffer-file-name'.  FILENAME should
  be a real filename, not a path."
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

) ;; chapter 35 ends here

;;* 36 Running (info "(emacs)Shell") Commands from Emacs

(leuven--chapter leuven-chapter-36-shell "36 Running Shell Commands from Emacs"

  ;; transform shell names to what they really are
  (with-eval-after-load "sh-script"

    (add-to-list 'sh-alias-alist '(sh . bash)))

  (defun set-shell-cmdproxy()
    (interactive)
    (setq shell-file-name "cmdproxy")
    (setq explicit-shell-file-name "cmdproxy")
    (setenv "SHELL" explicit-shell-file-name)
    (setq w32-quote-process-args t)
    (setq shell-command-switch "/c"))

  ;; (set-shell-cmdproxy)

  ;; XXX Test the following (added on 2011-08-03)
  ;; (when (eq system-type 'windows-nt)
  ;;   ;; Workaround for Cygwin shell, when set 'CYGWIN=noglob'. By default
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

  ;; for single shell commands
  (setq shell-file-name ;; must be in the `PATH' (Windows users)
        (or ;; (executable-find "zsh") ;; problems to compile with AUCTeX
            (executable-find "bash")
            (executable-find "cmdproxy.exe")
            "cmd.exe")) ;; default (= system shell)

  ;; use `shell-file-name' as the default shell
  (setenv "SHELL" shell-file-name)

  ;; switch used to have the shell execute its command line argument
  ;; (`/c' does not work with XEmacs)
  (setq shell-command-switch
        (cond
         ;; using a system shell
         ((string-match "cmd.exe" shell-file-name) "/c")
         (t "-c")))

  ;; quote process arguments to ensure correct parsing on Windows
  (setq w32-quote-process-args
        (cond ((string-match "cmd.exe" shell-file-name) nil) ;; using a system shell
              (t t)))

  ;; name of shell used to parse TeX commands
  (setq TeX-shell shell-file-name) ;; ???

  ;; shell argument indicating that next argument is the command
  (setq TeX-shell-command-option shell-command-switch) ;; ???

;;** 36.2 Interactive Shell

  (leuven--section "36.2 Interactive Shell")

  ;; for the interactive (sub)shell (and AUCTeX compilation?)
  (setq explicit-shell-file-name shell-file-name)

  ;; ;; args passed to inferior shell by `M-x shell', if the shell is bash
  ;; (setq explicit-bash-args '("--noediting" "--login"))
  ;; ;; FIXME This ensures that /etc/profile gets read (at least for Cygwin).
  ;; ;; Is this good?

  ;; regexp to match prompts in the inferior shell
  (setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;;** 36.3 Shell Mode

  (leuven--section "36.3 Shell Mode")

  ;; general command-interpreter-in-a-buffer stuff (lisp, shell, R, ...)
  ;; (when (try-require 'comint)

    ;; regexp to recognize prompts in the inferior process
    ;; (set it for Org-babel sh session to work!)
    ;; (defun set-shell-prompt-regexp ()
      (setq comint-prompt-regexp shell-prompt-pattern)
    ;;   )
    ;; (add-hook 'shell-mode-hook 'set-shell-prompt-regexp)

    ;; no duplicates in command history
    (setq-default comint-input-ignoredups t)

    ;; input to interpreter causes windows showing the buffer to scroll
    ;; (inserting at the bottom)
    (setq-default comint-scroll-to-bottom-on-input t)

    ;; output to interpreter causes windows showing the buffer to scroll
    (setq-default comint-move-point-for-output t)

    ;; remove the `^M' characters
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t)
    ;; XXX [2011-11-23 Wed] Added `nil t'!???

    ;; show completion list when ambiguous
    (setq comint-completion-autolist t)

    ;; use the `up' and `down' arrow keys to traverse through the previous
    ;; commands
    (defun leuven-up-down-keys ()
      "Customize my shell-mode."
      (local-set-key
        (kbd "<up>") 'comint-previous-input)
      (local-set-key
        (kbd "<down>") 'comint-next-input))

    (add-hook 'shell-mode-hook 'leuven-up-down-keys)
;; )

  ;; translate ANSI escape sequences into faces (within shell mode)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (setenv "PAGER" "/usr/bin/cat")

;;** 36.4 Shell Prompts

  (leuven--section "36.4 Shell Prompts")

;;** 36.5 History

  (leuven--section "36.5 History")

;;** 36.6 Directory Tracking

  (leuven--section "36.6 Directory Tracking")

;;** 36.7 Options

  (leuven--section "36.7 Options")

;;** 36.8 Terminal emulator

  (leuven--section "36.8 Terminal emulator")

;;** 36.9 Term Mode

  (leuven--section "36.9 Term Mode")

  ;; managing multiple terminal buffers in Emacs
  ;; (and fixing some troubles of `term-mode': key bindings, etc.)

  ;; "multi-term on POSIX hosts has let me switch from using screen, with
  ;; one Emacs screen and lots of shell screens; to just using Emacs, with
  ;; lots of terminals inside it."

  (when (locate-library "multi-term")

    (autoload 'multi-term "multi-term" nil t)
    (autoload 'multi-term-next "multi-term" nil t)

    (setq multi-term-program shell-file-name)

    ;; (global-set-key (kbd "C-c t") 'multi-term-next)
    (global-set-key
      (kbd "C-c T") 'multi-term)) ;; create a new one

  ;; run an inferior shell, with I/O through buffer `*shell*'
  (global-set-key
    (kbd "C-c !")
    (cond (running-ms-windows 'shell)
          (t 'term)))

;;** 36.10 Paging in Term

  (leuven--section "36.10 Paging in Term")

;;** 36.11 Remote Host

  (leuven--section "36.11 Remote Host")

  ;; load ssh.el file
  (add-to-list 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")
  (autoload 'ssh "ssh"
    "Open a network login connection via `ssh'" t)
    ;; this is to run ESS remotely on another computer in my own Emacs, or
    ;; just plain old reading remote files

  ;; See http://emacs.1067599.n5.nabble.com/SSH-inside-Emacs-td225528.html
  ;; - plink (with `dumb' terminal option?) as interactive shell
  ;; - ssh -t -t user@host
  ;; - Cygwin'ized Emacs
  ;; - MSYS (MinGW)

    ;; ;; let Emacs recognize Cygwin paths (e.g. /usr/local/lib)
    ;; (when (and running-ms-windows
    ;;            (executable-find "mount")) ;; Cygwin bin directory found
    ;;   (when (try-require 'cygwin-mount)
    ;;     (cygwin-mount-activate)))

)

;;* Proced

  ;; start Proced in a similar manner to Dired
  (global-set-key
    (kbd "C-x p") 'proced)

  (with-eval-after-load "proced"

    ;; current sort scheme for proced listing
    (setq-default proced-sort 'start)

    ;; display of Proced buffer as process tree
    (setq-default proced-tree-flag t))

;;* 37 (info "(emacs)Emacs Server")

(leuven--chapter leuven-chapter-37-emacs-server "37 Using Emacs as a Server"

  ;; use Emacs as a server (with the `emacsclient' program)
  (GNUEmacs
    (idle-require 'server) ;; after init
    (with-eval-after-load "server"

      ;; test whether server is (definitely) running, avoiding the message of
      ;; "server-start" while opening another Emacs session
      (or (equal (server-running-p) t)

          ;; start the Emacs server
          (server-start)))) ;; ~ 0.20 s

) ;; chapter 37 ends here

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
          (cond (running-ms-windows "//PRINT-SERVER/Brother HL-4150CDN") ; XXX
                (t t))))

  (defun leuven-ps-print-buffer-with-faces-query ()
    "Query user before printing the buffer."
    (interactive)
    (when (y-or-n-p "Are you sure you want to print this buffer? ")
      (ps-print-buffer-with-faces)))

  ;; generate and print a PostScript image of the buffer
  (GNUEmacs
    (when running-ms-windows
      ;; override `Print Screen' globally used as a hotkey by Windows
      (w32-register-hot-key (kbd "<snapshot>"))
      (global-set-key
        (kbd "<snapshot>") 'leuven-ps-print-buffer-with-faces-query)))

  (global-set-key
    (kbd "M-p") 'leuven-ps-print-buffer-with-faces-query)

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
          ;; tell Emacs where ghostscript print utility is located
          (setq ps-lpr-command gsprint-program)

          ;; list of extra switches to pass to `ps-lpr-command'
          ;; tell Ghostscript to query which printer to use
          (setq ps-lpr-switches '("-query"))) ;; '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")

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
    (setq ps-print-header-frame nil) ;; no box around the header
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
    (setq ps-print-footer-frame nil) ;; no box around the footer
    (setq ps-footer-frame-alist '((fore-color . "#666666")))
    (setq ps-footer-lines 1)
    (setq ps-footer-font-family 'Helvetica)
    (setq ps-footer-font-size 8)
    (setq ps-left-footer nil)
    (setq ps-right-footer (list "/pagenumberstring load")) ;; Page n of m

    (setq ps-font-family 'Courier) ;; see `ps-font-info-database'
    ;; legitimate values include Courier, Helvetica, NewCenturySchlbk,
    ;; Palatino and Times
    (setq ps-font-size 9.1)

    (setq ps-use-face-background t)

    (setq ps-line-spacing 3))

) ;; chapter 38 ends here

;;* 39 (info "(emacs)Sorting") Text

(leuven--chapter leuven-chapter-39-sorting "39 Sorting Text"

  ;; key binding
  (global-set-key
    (kbd "C-c ^") 'sort-lines)

) ;; chapter 39 ends here

;;* 42 (info "(emacs)Saving Emacs Sessions")

(leuven--chapter leuven-chapter-42-saving-emacs-sessions "42 Saving Emacs Sessions"

  (idle-require 'saveplace)
  (with-eval-after-load "saveplace"

    ;; automatically save place in each file
    (setq-default save-place t) ;; default value for all buffers

    ;; name of the file that records `save-place-alist' value
    (setq save-place-file
          (convert-standard-filename "~/.emacs.d/.places"))
    ;;! a .txt extension would load Org at the time Emacs is killed (if not
    ;;! already loaded)!

    ;; do not make backups of master save-place file
    (setq save-place-version-control "never"))

) ;; chapter 42 ends here

;;* 45 (info "(emacs)Hyperlinking")

(leuven--chapter leuven-chapter-45-hyperlinking "45 Hyperlinking and Navigation Features"

;;** pass a URL to a WWW browser

  (leuven--section "pass a URL to a WWW browser")

  ;; default browser started when you click on some URL in the buffer
  (setq browse-url-browser-function
        (if (not (display-graphic-p))
            'w3m-browse-url
          (if running-ms-windows
              'browse-url-default-windows-browser
            'browse-url-generic)))
  ;; (setq browse-url-browser-function
  ;;       '(("file:///usr/share/doc/hyperspec/" . w3m-browse-url)
  ;;         ("emacswiki.org" . w3m-browse-url)
  ;;         ("lispdoc.com" . w3m-browse-url)
  ;;         ( "." . browse-url-firefox)))
  ;; that let me use `w3m' for EmacsWiki/Common Lisp documentation and
  ;; Firefox otherwise.

  ;; ;; name of the browser program used by `browse-url-generic'
  ;; (setq browse-url-generic-program
  ;;       (when (and (display-graphic-p)
  ;;                  (not running-ms-windows))
  ;;         (executable-find "firefox"))) ;; could be `google-chrome'

  (defun leuven--browse (url)
    "If prefix is specified, use the system default browser else use the
  configured emacs one."
    (require 'browse-url)
    (if current-prefix-arg
        ;; open in your desktop browser (firefox here)
        (when url (browse-url-default-browser url))
      ;; open using your Emacs browser (whatever that is configured to)
      (if url (browse-url url) (call-interactively 'browse-url))))

  (defun leuven--browse-url (&optional url)
    "Browse the url passed in."
    (interactive)
    (require 'w3m)
    (require 'browse-url)
    (require 'thingatpt+)
    (setq url (or url
                  (w3m-url-valid (w3m-anchor))
                  (browse-url-url-at-point)
                  (region-or-word-at-point))) ;; see thingatpt+.el
    (setq url (read-string (format "Url \"%s\" :" url) url nil url))
    (leuven--browse url))

;;** Web search

  (leuven--section "Web search")

  (when t ;; (try-require 'browse-url)

    (defconst leuven--google-maxlen (* 32 7)
      "Maximum length of search string to send. This prevents you from
    accidentally sending a 5 MB query string.")

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
    ;;               (setq term (buffer-substring start (+ start leuven--google-maxlen)))
    ;;             (setq term (buffer-substring start end)))
    ;;           (google-it term))
    ;;       (beep)
    ;;       (message "Region not active"))))


         (defun google (what)
           "Use google to search for WHAT."
           (interactive "sSearch: ")
           (save-window-excursion
             (delete-other-windows)
             (let ((dir default-directory))
               (w3m-browse-url (concat "http://www.google.com/search?q="
                                       (w3m-url-encode-string what)))
               (cd dir)
               (recursive-edit))))
         (global-set-key
           (kbd "C-c g s") 'google)

  (defun pm/region-or-word (prompt)
    "Read a string from the minibuffer, prompting with PROMPT.
  If `transient-mark-mode' is non-nil and the mark is active,
  it defaults to the current region, else to the word at or before
  point. This function returns a list (string) for use in `interactive'."
    (list (read-string prompt (or (and (use-region-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                                  (current-word)))))

  (defun pm/google (string)
    "Ask a WWW browser to google string.
  Prompts for a string, defaulting to the active region or the current word at
  or before point."
    (interactive (pm/region-or-word "Google: "))
    (browse-url (concat "http://google.com/search?num=100&q=" string)))

    (defvar leuven--google-prefix-map (make-sparse-keymap)
      "Keymap for my Google commands.")

    ;;     (global-set-key (kbd "M-s") 'leuven-google-search-region)

    (global-set-key
      (kbd "C-c g") leuven--google-prefix-map)

    (define-key leuven--google-prefix-map
      (kbd "g") 'leuven-google-search)

    (define-key leuven--google-prefix-map
      (kbd "w") 'leuven-google-search-word-at-point)

    (define-key leuven--google-prefix-map
      (kbd "r") 'leuven-google-search-region)
)

;;** Emacs-w3m

  (leuven--section "Emacs-w3m")

  ;; only use if `w3m' command is available on system
  (when (executable-find "w3m")

    ;; name of the executable file of the `w3m' command
    (setq w3m-command "w3m") ;; I don't want `/usr/bin/w3m' (which
                             ;; requires `cygwin-mount')

    ;; `w3m' slows down the startup process dramatically
    (unless (try-require 'w3m-load)
      (autoload 'w3m "w3m"
        "Visit the WWW page using w3m" t)
      (autoload 'w3m-find-file "w3m"
        "Find a local file using emacs-w3m." t)
      (autoload 'w3m-browse-url "w3m"
        "Ask emacs-w3m to show a URL." t))

    (with-eval-after-load "w3m"

;;*** 3.1 Browsing Web Pages

      ;; go ahead, just try it
      (defun leuven-w3m-goto-url ()
        "Type in directly the URL I would like to visit (avoiding to
      hit `C-k')."
        (interactive)
        (let ((w3m-current-url ""))
          (call-interactively 'w3m-goto-url)))

      ;; make w3m stop "stealing" my arrow keys, allowing to move the
      ;; cursor down the lines of an HTML email (in Gnus)
      (setq w3m-minor-mode-map nil)

      (define-key w3m-mode-map
        (kbd "U") 'leuven-w3m-goto-url)

      ;; fix inappropriate key bindings for moving from place to place in a
      ;; page (let the cursor keys behave normally, don't jump from link to
      ;; link)
      (define-key w3m-mode-map
        (kbd "<up>") 'previous-line)
      (define-key w3m-mode-map
        (kbd "<down>") 'next-line)
      (define-key w3m-mode-map
        (kbd "<left>") 'backward-char)
      (define-key w3m-mode-map
        (kbd "<right>") 'forward-char)

      (define-key w3m-mode-map
        (kbd "<tab>") 'w3m-next-anchor)

      ;; moving from page to page
      (define-key w3m-mode-map
        (kbd "F") 'w3m-view-next-page)

;;*** 3.5 Using Tabs

      (define-key w3m-mode-map
        (kbd "<C-tab>") 'w3m-next-buffer)
      (define-key w3m-mode-map
        (kbd "<C-S-tab>") 'w3m-previous-buffer)

      (defun w3m-new-tab ()
        (interactive)
        (w3m-copy-buffer nil nil nil t))

      (define-key w3m-mode-map
        (kbd "C-t") 'w3m-new-tab)

      (define-key w3m-mode-map
        (kbd "C-w") 'w3m-delete-buffer)

;;*** 5.1 General Variables

      ;; send referers only when both the current page and the target
      ;; page are provided by the same server
      (setq w3m-add-referer 'lambda)

      ;; home page
      (setq w3m-home-page "http://www.emacswiki.org/")

      ;; number of steps in columns used when scrolling a window
      ;; horizontally
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
      (eval-after-load "w3m-cookie"

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
      (when (try-require 'w3m-lnum)

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

        (define-key w3m-mode-map
          (kbd "f") 'leuven-w3m-go-to-link-number)

        ;; enable link numbering mode by default
        (add-hook 'w3m-mode-hook 'w3m-lnum-mode))

      ))

;;** Babel

  (leuven--section "Babel")

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
      (let (source)
        (autoload 'babel-work "babel" nil t)
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
        (insert (babel-work source "en" "fr" 'babel-free-fetch 'babel-free-wash))))

    (global-set-key
      (kbd "<f7>") 'leuven-babel-translate))
)

;;* 46 Other (info "(emacs)Amusements")

(leuven--chapter leuven-chapter-46-amusements "46 Other Amusements"

  (GNUEmacs
    ;; get rid of the Games in the Tools menu
    (define-key menu-bar-tools-menu
      [games] nil))

) ;; chapter 46 ends here

;;* 48 (info "(emacs)Customization")

(leuven--chapter leuven-chapter-48-customization "48 Customization"

  (GNUEmacs24
    (ignore-errors
      (load-theme 'leuven t)))

  ;; color sort order for `list-colors-display'
  (setq list-colors-sort '(rgb-dist . "#FFFFFF"))

  (XEmacs
    ;; the real color theme functions
    (when (try-require 'color-theme-autoloads)

      ;; `color-theme-print' allows to keep what you see

      ;; initialize the color theme package
      (if (fboundp 'color-theme-initialize)
          (color-theme-initialize))

      ;; color themes will be installed for all frames
      (setq color-theme-is-global t)

      ;; set my default color theme
      (when (try-require 'color-theme-leuven)
        (color-theme-leuven)))

    (setq options-save-faces t))

  ;; ;; limit serving to catch infinite recursions for you before they
  ;; ;; cause actual stack overflow in C, which would be fatal for Emacs
  ;; (setq max-lisp-eval-depth 600) ;; 1000?

  ;; limit on number of Lisp variable bindings & unwind-protects
  (setq max-specpdl-size 3000) ;; XEmacs 21.5.29

  ;; speed up things by preventing garbage collections
  (setq gc-cons-threshold 3500000) ;; make Gnus fast
  ;; from (info "(gnus)FAQ 9-2")

  ;; don't display messages at start and end of garbage collection (as it
  ;; hides too many interesting messages)
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
          ;; (ac-user-dictionary-files . ("../dictionary.txt"))
          (balloon-help-mode . -1)
          (before-save-hook . nil)
          (flyspell-mode . -1)
          (flyspell-mode . 1)
          (ispell-local-dictionary . "american")
          (ispell-local-dictionary . "en_US")
          (ispell-local-dictionary . "fr_FR")
          (ispell-local-dictionary . "francais")
          (ispell-mode . t)
          (org-tags-column . -80)))

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

  (defmacro rloop (clauses &rest body)
    (if (null clauses)
        `(progn ,@body)
      `(loop ,@(car clauses) do (rloop ,(cdr clauses) ,@body))))

  (defun leuven-all-bindings ()
    (interactive)
    (message "leuven-all-bindings: wait a few seconds please...")
    (let ((data
           (with-output-to-string
             (let ((bindings '()))
               (rloop ((for C in '("" "C-")) ;; Control
                       (for M in '("" "M-")) ;; Meta
                       (for A in '("" "A-")) ;; Alt
                       (for S in '("" "S-")) ;; Shift
                       (for H in '("" "H-")) ;; Hyper
                       (for s in '("" "s-")) ;; super
                       (for x from 32 to 127))
                      (let* ((k (format "%s%s%s%s%s%s%c" C M A S H s x))
                             (key (ignore-errors (read-kbd-macro k))))
                        (when key
                          (push
                           (list k
                                 (format "%-12s  %-12s  %S\n" k key
                                         (or
                                          ;; (string-key-binding key)
                                          ;; What is this string-key-binding?
                                          (key-binding key))))
                           bindings))))
               (dolist (item
                        (sort bindings
                              (lambda (a b)
                                (or (< (length (first a))
                                       (length (first b)))
                                    (and (= (length (first a))
                                            (length (first b)))
                                         (string< (first a)
                                                  (first b)))))))
                 (princ (second item)))))))
      (switch-to-buffer (format "Key bindings in %s" (buffer-name)))
      (erase-buffer)
      (insert data)
      (goto-char (point-min))
      (values)))

;;** 48.5 The (info "(emacs)Syntax") Table

  (leuven--section "48.5 The (emacs)Syntax Table")

  ;; now '-' is not considered a word-delimiter
  ;; (add-hook 'emacs-lisp-mode-hook
  ;;           (lambda ()
  ;;             (modify-syntax-entry ?- "w")))

) ;; chapter 48 ends here

;;* Emacs Display

(leuven--chapter leuven-chapter-XX-emacs-display "XX Emacs Display"

;;** (info "(elisp)Faces")

  (leuven--section "Faces")

  ;; enable more bold and italic fonts to be displayed
  (setq w32-enable-synthesized-fonts t)

  (defun merge-x-resources ()
    (let ((file (file-name-nondirectory (buffer-file-name))))
      (when (or (string= file ".Xdefaults")
                (string= file ".Xresources"))
        (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
        (message (format "Merged %s into X resource database" file)))))

  (add-hook 'after-save-hook 'merge-x-resources)

  ;; allow any scalable font
  (when running-ms-windows
    (setq scalable-fonts-allowed t))

)

;;** 38.16 (info "(elisp)Images")

  (leuven--section "Utilities -- ESS")

  ;; (setq-default inferior-ess-program "R") ;; XXX undefined

  ;;
  (setq ess-directory-containing-R "C:/Program Files")
  ;; needs to be done before loading `ess-site'

  ;; add to list of prefixes recognized by ESS
  (setq ess-r-versions '("R-2.15.2")) ;; R-current
  ;; matching versions will appear after doing M-x R <tab> <tab>

  ;; ESS: Emacs Speaks Statistics
  (try-require 'ess-site-XXX)

  ;; set default R version, (i.e. the one launched by typing M-x R <RET>)
  (setq inferior-R-program-name ;; R-current
        "C:/Program Files/R/R-2.15.2/bin/x64/Rterm.exe")

  (global-set-key
    (kbd "M--") 'ess-smart-underscore)

  ;; don't request the process directory each time R is run
  (setq ess-ask-for-ess-directory nil)

  ;; name of the ESS process associated with the current buffer
  (setq ess-local-process-name "R")

  ;; Use `<S-RET>' to split window & launch R (if not running), execute
  ;; highlighted region (if R running & area highlighted), or execute current
  ;; line (and move to next line, skipping comments). Nice.
  ;;
  ;; See http://www.emacswiki.org/emacs/EmacsSpeaksStatistics,
  ;; FelipeCsaszar. Adapted to split vertically instead of
  ;; horizontally.

  ;; XXX This should be useless, at least if R code is well written
  ;; inside Org documents!
  (defun leuven--ess-start-R ()
    (interactive)
    (if (not (member "*R*"
                     (mapcar (function buffer-name) (buffer-list))))
        (progn
          (delete-other-windows)
          (setq w1 (selected-window))
          (setq w1name (buffer-name))
          (setq w2 (split-window w1 nil t))
          (R)
          (set-window-buffer w2 "*R*")
          (set-window-buffer w1 w1name))))

  (defun leuven-ess-eval ()
    (interactive)
    (leuven--ess-start-R)
    (if (use-region-p)
        (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))

  (add-hook 'ess-mode-hook
            (lambda ()
              (local-set-key
                (kbd "<S-return>") 'leuven-ess-eval)))

  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (local-set-key
                (kbd "<C-up>") 'comint-previous-input)
              (local-set-key
                (kbd "<C-down>") 'comint-next-input)))

  ;; tooltip with some information about the R object at point
  ;; (invoked by `C-c C-g'). See
  ;; http://www.kieranhealy.org/blog/archives/2010/02/16/easily-display-information-about-r-objects-in-emacsess/
  (try-require 'ess-R-object-tooltip-XXX) ;; or popup?

;;* App G Emacs and (info "(emacs)Microsoft Windows/MS-DOS")

(leuven--chapter leuven-chapter-AppG-ms-dos "Appendix G Emacs and MS-DOS"

  ;; divide key (needed in GNU Emacs for Windows)
  (GNUEmacs
    (global-set-key
      (kbd "<kp-divide>") (kbd "/")))

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

) ;; chapter G ends here

;; Recovery from Problems

;;* Reporting Bugs

(leuven--chapter leuven-chapter-99-debugging "99 Debugging"

  ;; get the backtrace when uncaught errors occur
  (setq debug-on-error nil)) ;; was set to `t' at beginning of buffer

(when (and (string-match "GNU Emacs" (version))
           leuven-load-verbose)
  (ad-disable-advice 'message 'before 'leuven-when-was-that)
  (ad-update 'message))

(when leuven-load-verbose
  (message "| Chapter | Time |")
  (message "|---------+------|")
  (mapcar (lambda (el) (message el))
          (nreverse leuven--load-times-list))
  (message "|---------+------|")
  (message "|         | =vsum(@-I..@-II) |"))

;; warn that some packages were missing
(dolist (pkg leuven--missing-packages)
  (message "(warning) Package `%s' not found" pkg))

(message "Loading `%s'...done (in %.3f s)"
         load-file-name
         (- (float-time) leuven-before-time))
(sit-for 0.3)

(message "* --[ Loaded Emacs Leuven 20130809.1045]--")

(provide 'emacs-leuven)


;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8
;; eval: (when (locate-library "rainbow-mode") (require 'rainbow-mode) (rainbow-mode))
;; ispell-local-dictionary: "american"
;; End:

;;; emacs-leuven.el ends here
