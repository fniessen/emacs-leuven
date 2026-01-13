;;; nuweb.el --- major mode to edit nuweb files with AucTex
;;;  Time-stamp: <Tue 2008-06-10 14:21 sva on mundaneum>
;;;  $Id: nuweb.el 1676 2008-06-10 12:21:53Z fni $ 
;;;
;;;
;; LCD Archive Entry:
;; nuweb|Dominique de Waleffe|ddw@acm.org|
;; Major mode to edit nuweb files with AucTex|
;; $Date: 2008-06-10 14:21:53 +0200 (Tue, 10 Jun 2008) $|$Revision: 1676 $|~/modes/nuweb.el.Z|
;;
;;
;; Author: Dominique de Waleffe <ddw@acm.org>
;; Maintainer: Dominique de Waleffe <ddw@acm.org>
;; Version: $Revision: 1676 $
;; Keywords: nuweb programming  tex tools languages
;;
;; This nuweb support package is free software, just as GNU Emacs; you
;; can redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by ;the Free Software Foundation;
;; either version 2, or (at your option) any later version.
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; 
;;; Bug reports , suggestions are  welcome. I'll see if I can do anything in
;;; my copious spare time :-)

;;; DOCUMENTATION (short) there is no long version yet:-)
;;;  To install:
;;;     ; if your version of nuweb does not know about @% comments 
;;;     (setq nuweb-comment-leader "") 
;;;     ; if you want to activate the mode for .w files
;;;     (push auto-mode-alist '( "\\.w" . nuweb-mode))
;;;     ; To load it
;;;     (require 'nuweb)
;;; 
;;; When called, nuweb-mode calls latex-mode and adds the following
;;; bindings: 
;;;
;;; A) Help for editing scraps in language dependent mode
;;;
;;;       C-c C-z nuweb-edit-this scrap
;;;           Edit the scrap point is on in its own buffer *Source*
;;;           put into the mode specified  as follows:
;;;           a) if the first line contains -*-mode-name-*- that is
;;;		 used,
;;;           b) otherwise, for @o <filename> scraps, the mode is
;;;		 derived from the value of auto-mode-alist
;;;           c) otherwise it is taken from the (buffer local) variable nuweb-source-mode
;;;           (which defaults to "emacs-lisp")
;;;
;;;           The *Source* buffer is then put into Nuweb minor mode
;;;           which adds two bindings:
;;;           C-c C-z nuweb-install-this-scrap
;;;               Which takes the *Source* buffer contents and puts
;;;               it back into the web buffer in place of the old
;;;               version of the scrap.
;;;           C-c M-k nuweb-kill-this-scrap
;;;               Which restores the old scrap, ignoring changes
;;;               made.
;;;
;;;           C-c t i Toggles auto-index mode (default on)
;;;
;;;           The original buffer is put in read-only mode until you
;;;           call one of the two above functions or kill the
;;;           *Source* buffer. 
;;;       C-c @ nuweb-insert-scrap
;;;           With no argument: inserts an empty scrap template at
;;;           point.
;;;           With an argument: prompt for scrap type (oDdD), scrap
;;;           name and language mode. A new scrap is inserted and
;;;           edited as if nuweb-edit-this-scrap had been called.
;;;
;;;       C-c t i toggle-auto-index mode (default on)
;;;
;;; B) Help for navigation on definitions and uses
;;;     (not normally allowed by the guidelones for elisp modes, I
;;;	could not think of any other other than C-c C-d [rpsn] but
;;;	auctex now uses C-c C-d....)
;;;
;;;       C-c d r nuweb-compute-d-u
;;;           Recompute all defs and uses in current buffer.
;;;       C-c d p
;;;           Pop back to previous def/use
;;;       C-c d s
;;;           Find first definition for scrap name at point
;;;       C-c d n
;;;           Find next definition of same scrap name
;;;       C-c u s
;;;           Find first use of scrap name at point
;;;       C-c u n
;;;           Find next use of smae scrap name
;;;       M-mouse3
;;;           Find first def or first use (if on a def -> use, if on a
;;;           use -> def)
;;;       M-mouse1
;;;           Make an imenu for symbols (@|) , scrap names (@d) and
;;;	      files (@o). (imenu.el comes with 19.25, and should be on archive
;;;	      sites).
;;;
;;; C) The commands to run nuweb and latex on the file are known as
;;;    Tangle: generate output files only
;;;    Weave: generate tex file then run latex on it
;;;    Web: does it all.
;;;    Provided your version of nuweb does output !name(f.w) and
;;;    !offset(<num>) in the generated tex file Auctex will take you
;;;    right back to Tex errors but at the correct stop in the web file.
;;;  
;;; CUSTOMISATION: 
;;;
;;; Change language mode for scraps
;;; (setq-default nuweb-source-mode "mode-name-function(without -mode)")
;;; (setq-default nuweb-source-mode "prolog") ;default for all buffers
;;; or (setq nuweb-source-mode "emacs-lisp") ; current one only
;;;
;;; To get a menu with scraps/files/index entries call:
;;; (imenu-add-to-menubar "Nuweb")

;;; Support for nuweb comments @% (I have patches to nuweb for that)
;;; (setq nuweb-comment-leader "@%")
;;;
;;;
;;; 
;;; PROBLEMS:                    SOLUTION:
;;;  
;;; -) Relies on FSF Emacs 19    Upgrade or make the package
;;; 				 back-compatible 
;;; -) Functions are not well    I should have used nuweb for this
;;;    documented
;;; -) Bindings may not suit     Change as you like
;;;    every one
;;; -) Does not yet support the
;;;  standard TeX mode of Emacs

;;; WISH LIST:
;;; -) Menus active on scrap def/use point providing navigation
;;; functions (I'm thinking of looking at imenu.el for this
;;;    (Experimental support for this at the end) Bound to C-M-mouse-1.
;;; -) Better support for standard tex mode (or that GNU adopts AucTeX..)
;;;
;;; CONTRIBUTIONS:
;;; Thorbj{\o}rn Andersen <ravn@imada.ou.dk> suggested the use of C-c C-z
;;;      I used it to get into the *source* and as normal exit key.
;;;      Also suggested the simple (no prompts) insertion  of a scrap
;;;      template, and other things.
;;; 
;;; AVAILABILITY:
;;; 
;;; elisp-archive directory: elisp-archive/modes/nuweb.el.Z
;;;  from main site and mirrors
;;; or from by email from me
;;;

(require 'cl)

(defvar AucTeX-used t)

(if AucTeX-used
    (progn
      (require 'tex-site)
      (require 'latex))
  (require 'latex-mode)
  (setq TeX-command-list nil)
  (setq LaTeX-mode-map tex-mode-map))

;;; Extend the list of commands
;;; Web extracts source files, creates LaTeX document, and run `latex' on it
(push
 (list "Web"
       "sh -c \"nuweb %s && latex \\\\\\\\nonstopmode\\\\\\\\input\\{%s\\}\""
       'TeX-run-LaTeX nil
       t)
 TeX-command-list)
;;; Weave creates LaTeX document, and run `latex' on it
(push
 (list "Weave"
       "sh -c \"nuweb -o %s && latex %s\""
       'TeX-run-LaTeX nil
       t)
 TeX-command-list)
;;; Tangle extracts source files
(push
 (list "Tangle"
       "sh -c \"nuweb -t %s && echo 'Sources updated'\""
       'TeX-run-LaTeX nil
       t)
 TeX-command-list)

;;; allow .w as extension
(if (boundp 'TeX-file-extensions)
    (push "w" TeX-file-extensions)
  (setq TeX-file-extensions '("tex" "sty" "w")))

(defvar nuweb-mode-map nil)
(defvar nuweb-source-mode "emacs-lisp")

(defvar nuweb-defs nil
  "List of defs found and where")
(defvar nuweb-uses nil
  "List of uses found and where")
(defvar nuweb-names nil
  "List of canonical names of scraps")
(defvar *def-list* nil)
(defvar *use-list* nil)
(defvar nuweb-def-use-stack nil
  "Record locations we have visited so far")

; only one of those in effect....
(defvar *nuweb-last-scrap-pos* nil)
(defvar *nuweb-last-scrap-begin* nil)
(defvar *nuweb-last-scrap-end* nil)
(defvar *nuweb-last-index-begin* nil)
(defvar *nuweb-last-index-end* nil)

(defvar *nuweb-source-buffer* nil)

(defun nuweb-mode ()
  "Major mode to edit nuweb source files.
Adds the following bindings to the normal LaTeX ones.
Commands:
\\{nuweb-mode-map}"
  (interactive)
  (latex-mode)
  (setq major-mode 'nuweb-mode)
  (setq mode-name "nuweb")
  ;; Make sure the nuweb map exist
  (cond ((or (not (boundp 'nuweb-mode-map))
	     (null nuweb-mode-map))
	 ;; this keymap inherit the current local bindings
	 (setq nuweb-mode-map (cons 'keymap LaTeX-mode-map))
	 (define-key nuweb-mode-map 
	   "\C-c\C-z" 'nuweb-edit-this-scrap)
	 (define-key nuweb-mode-map 
	   "\C-c@" 'nuweb-insert-scrap)
	 ;; This section contains bindings for @-<key> 
	 (define-key nuweb-mode-map "@d"
	   (function (lambda()(interactive)
		       (insert "@d  @{@%\n@| @}\n")
		       (forward-char -12))))
	 (define-key nuweb-mode-map "@D"
	   (function (lambda()(interactive)
		       (insert "@D @{@%\n@| @}\n")
		       (forward-char -12))))
	 (define-key nuweb-mode-map "@o"
	   (function (lambda()(interactive)
		       (insert "@o @{@%\n@| @}\n")
		       (forward-char -12))))
	 (define-key nuweb-mode-map "@O"
	   (function (lambda()(interactive)
		       (insert "@O @{@%\n@| @}\n")
		       (forward-char -12))))
	 (define-key nuweb-mode-map "@<"
	   (function (lambda()(interactive)
		       (insert "@<  @>\n")
		       (forward-char -4))))
	 (define-key nuweb-mode-map [ ?@ t ]
	   (function(lambda()
		      (interactive)
		      (insert "@")
		      (insert last-input-char))))
	 (define-key nuweb-mode-map "@@"
	   (function (lambda()(interactive)(insert "@@"))))
	 (define-key nuweb-mode-map "\C-cti"
	   (function (lambda()
		       (interactive)
		       (setq nuweb-auto-index-scrap
			     (not nuweb-auto-index-scrap))
		       (message "Now %sauto-indexing scraps"
				(if nuweb-auto-index-scrap "" "not ")))))
	 (define-key nuweb-mode-map "\C-cds" 'nuweb-find-def)
	 (define-key nuweb-mode-map "\C-cus" 'nuweb-find-use)
	 (define-key nuweb-mode-map "\C-cdn" 'nuweb-find-next-def)
	 (define-key nuweb-mode-map "\C-cun" 'nuweb-find-next-use)
	 (define-key nuweb-mode-map "\C-cdr" 'nuweb-compute-d-u)
	 (define-key nuweb-mode-map "\C-cdp" 'nuweb-pop-d-u)
	 (define-key nuweb-mode-map [M-mouse-3]
	   'nuweb-find-def-or-use)
	 ;;; we now fix a problem with the map we inherited (it has been
	 ;;; unexpectedly modified and now contains a binding for "@"
	 ;;; as an empty keymap. (BUG in 19.30)
	 (define-key LaTeX-mode-map "@" nil)
	 ))
  ;; make sure we have our own keymap
  ;; we use a copy for in buffer so that outline mode is 
  ;; properly initialized
  (use-local-map (copy-keymap nuweb-mode-map))
  (make-local-variable 'nuweb-source-mode)
  (make-local-variable 'nuweb-defs)
  (make-local-variable 'nuweb-uses)
  (make-local-variable 'nuweb-names)
  (make-local-variable '*def-list*)
  (make-local-variable '*use-list*)
  (make-local-variable 'nuweb-def-use-stack)
  (make-local-variable '*nuweb-last-scrap-pos*)
  (make-local-variable '*nuweb-last-scrap-begin*)
  (make-local-variable '*nuweb-last-scrap-end*)
  (make-local-variable '*nuweb-source-buffer*)
  (make-local-variable '*nuweb-win-config*)
  (setq TeX-default-extension "w")
  (setq TeX-auto-untabify nil)
  (setq TeX-command-default "Web")
  (run-hooks 'nuweb-mode-hook))

;; set this to "" if you dont have comments in nuweb
;; (I have patches to support @%)
(defvar nuweb-comment-leader "")

(defvar nuweb-scrap-name-hist nil
  "History list for scrap names used")

(defun nuweb-insert-scrap(arg)
  "Insert a scrap at current cursor location. With an argument, 
prompts for the type, name and editing mode then directly enter
the *Source* buffer. If no argument given, simply inserts a template
for a scrap"
  (interactive "P")
  (if arg
      (apply 'nuweb-insert-scrap-intern 
	     (list 
	      (concat (read-from-minibuffer "Type of scrap: " "d")" ")

	      (concat (read-from-minibuffer
			     "Scrap title:" 
			     (car nuweb-scrap-name-hist)
			     nil	;no keymap
			     nil	;dont use read
			     (cons 'nuweb-scrap-name-hist 0))
		      " ")
	      (read-from-minibuffer "Mode name:" nuweb-source-mode)
	      ;; edit if interactive
	      t))
    (save-excursion 
      (nuweb-insert-scrap-intern "" "\n" nuweb-source-mode nil))
    (forward-char 1)))

(defun nuweb-insert-scrap-intern(type title modename editp)
  (save-excursion 
    (insert (format "@%s%s@\{%s%s\n@| @\}\n"
                    type title
                    nuweb-comment-leader
                    (if (or (equal modename "")
			    (equal modename nuweb-source-mode))
                        ""
                      (concat " -*-" modename "-*-")))))
  (cond ( editp
          (forward-line 1)
          (nuweb-edit-this-scrap))))

;;;
;;; !!! The file re does not check for nuweb options that may follow
(defun nuweb-edit-this-scrap()
  (interactive)
  (barf-if-buffer-read-only)
  (cond ((or (null *nuweb-last-scrap-pos*)
             (y-or-n-p 
              "You did not finish editing the previous scrap. Continue"))
         (setq *nuweb-last-scrap-pos* (point-marker))
	 ;; ddw- Thu Dec 14 11:59:20 2000 
	 ;; ain't perfect yet as @@@d brol @{x@} is a looser for this one...
         (let* ((s-begin (and (re-search-backward "[^@]@[dDoO]" nil t)
			      ;; (search-forward "@\{" nil t)
			      (forward-char 1)
			      (point)))
                (file (if (looking-at "@[oO][ \t]*\\([^ \t]+\\)[ \t]*")
                          (buffer-substring-no-properties (match-beginning 1)
                                            (match-end 1))
                        nil))
                (b-begin
                 (and (re-search-forward
                       (concat "@\{[ \t]*\\("
                               nuweb-comment-leader
                               "\\)?[ \t]*\\(-\\*-[ \t]*\\([^ \t]+\\)[ \t]*-\\*-\\)?") nil t)
                      (prog2 (if (and (match-beginning 1)(match-end 1))
                                 (skip-chars-forward "[^\n]"))
                          (point))))
                (mode-spec (if (and b-begin (match-beginning 3) (match-end 3))
                               (buffer-substring-no-properties (match-beginning 3)
                                                 (match-end 3))
                             nil))
                (offset (- (marker-position *nuweb-last-scrap-pos*)
                           b-begin))
                ;; fool nuweb-mode using char class instead of
		;; explicit match.
                (s-end (and (re-search-forward "@[}]" nil t)
			    (prog1 t (forward-char -2))
			    (>= (point) (marker-position *nuweb-last-scrap-pos*))
			    (point)))
		;; find position of user index entries
		(i-begin (re-search-backward "@[|]" b-begin t))
		;; fix scrap source part to correct position
		(b-end (if i-begin
			   i-begin
			 s-end))
                (text "")
		;; get value cuurent in .w buffer
		(auto-index-value nuweb-auto-index-scrap)
                (nuweb-source-mode-orig nuweb-source-mode)
		(saved-return-location *nuweb-last-scrap-pos*)
		source-mode)
           (cond ( (and b-begin b-end)
                   (setq *nuweb-last-scrap-begin* b-begin)
                   (setq *nuweb-last-scrap-end* b-end)
		   (setq *nuweb-last-index-begin* ( or i-begin s-end))
		   (setq *nuweb-last-index-end* s-end)
                   (setq text (buffer-substring-no-properties b-begin b-end))
                   (setq buffer-read-only t)
                   (setq *nuweb-win-config*
			 (current-window-configuration))
                   (pop-to-buffer (nuweb-get-source-buf))
                   (erase-buffer)
                   (insert text)
                   (setq source-mode nil)
                   (if (and (not source-mode)
                            mode-spec)
                       (setq source-mode
                             (intern (concat (downcase mode-spec) "-mode"))))
                   
                   (if (and (not source-mode) file)
                       (let* ((case-fold-search nil)
                              (mode (cdr (find file auto-mode-alist
                                               :key 'car 
                                               :test (function
                                                      (lambda(a b)
                                                        (string-match b a)))))))
                         (if mode (setq source-mode mode))))
                   
                   (if (not source-mode)
                       (setq source-mode
                             (if (stringp nuweb-source-mode-orig)
                                 (intern (concat 
                                          (downcase nuweb-source-mode-orig)
                                          "-mode"))
                               source-mode)))
                   
                   (funcall source-mode)

		   ;; set indexing var in source buffer according to
		   ;; original one, after calling the origianl mode
		   ;; since those almost surely call
		   ;; kill-all-local-variables 
		   (setq nuweb-auto-index-scrap auto-index-value)
                   
                   ;; go to same relative position
                   (goto-char (+ (point-min) (max offset 0)))
                   ;; clean up when killing the *source* buffer
                   (make-local-variable 'kill-buffer-hook)
                   (make-local-variable '*nuweb-last-scrap-pos*)
                   (make-local-variable 'nuweb-minor-mode)
                   (make-local-variable 'nuweb-return-location)
		   (setq nuweb-return-location saved-return-location)
                   (add-hook 'kill-buffer-hook 
                             (function (lambda()
                                         (save-excursion
                                           (nuweb-kill-this-scrap)))))
                   (nuweb-minor-mode 1)
                   (message "C-c C-z to use source, C-c M-k to abort"))
                 (t (goto-char (marker-position *nuweb-last-scrap-pos*))
                    (setq *nuweb-last-scrap-pos* nil)
                    (error "Could not identify scrap")))
           )
         )
        (t (message "Use C-x b and select buffer %s to finish"
		    (nuweb-get-source-buf)))))

(defun nuweb-get-source-buf ()
  (if *nuweb-source-buffer*
      *nuweb-source-buffer*
    (get-buffer-create (concat "*Source <"
			       (buffer-name)
			       ">*"))))

(defvar nuweb-minor-mode-map nil)
(cond ((or (not (boundp 'nuweb-minor-mode-map))
	   (null nuweb-minor-mode-map))
       (setq nuweb-minor-mode-map (make-sparse-keymap))
       (define-key nuweb-minor-mode-map "\C-c\C-z"
	 'nuweb-install-this-scrap)
       (define-key nuweb-minor-mode-map "\C-cti"
	 (function (lambda()
		       (interactive)
		       (setq nuweb-auto-index-scrap
			     (not nuweb-auto-index-scrap))
		       (message "Now %sauto-indexing scraps"
				(if nuweb-auto-index-scrap "" "not ")))))
       (define-key nuweb-minor-mode-map "\C-c\M-k" 'nuweb-kill-this-scrap)
       ))

(defvar nuweb-minor-mode nil)

(make-variable-buffer-local 'nuweb-minor-mode)

(or (assq 'nuweb-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(nuweb-minor-mode " Nuweb")
				 minor-mode-alist)))
(or (assq 'nuweb-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'nuweb-minor-mode 
					   nuweb-minor-mode-map)
				     minor-mode-map-alist)))



;;; Support for automatically indexing a scrap
;;;
(defvar nuweb-auto-index-scrap t
  "When t, inserting the scrap into the nuweb buffer automatically
insert index entries")

(make-variable-buffer-local 'nuweb-auto-index-scrap)

(defvar nuweb-index-functions nil
  "alist associating mode names with function returning the list of
index entries for the buffer."  ) 




;;; The function is there but has nothing to do (thanks to Emacs 19 
;;; function for minor mode bindings
;;; It is here if anyone cares to make it Emacs 18 compatible.
(defun nuweb-minor-mode (arg)
  "This minor mode provides two additional bindings to the current
language mode. The bindings allow to install the contents of the
*Source* buffer as current scrap body or to kill the buffer and
re-install the old scrap  body.

The bindings installed by this minor mode are
\\{nuweb-minor-mode-map}"
  (interactive "P")
  (setq nuweb-minor-mode
	(if (null arg) (not nuweb-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (cond (nuweb-minor-mode
	 ;; turn it on
	 (make-local-variable 'nuweb-index-function)
	 (setq nuweb-index-function
	       (cdr (assoc mode-name nuweb-index-functions)))
	 t
	 )
	(t
	 ;; turn it off
	 ;; Nothin to do yet...
	 nil
	 )))

(defun nuweb-install-this-scrap()
  (interactive)
  (let ((offset (point))
	;; collect entries should that be needed, if at all possible
	(index-entries (if (and nuweb-auto-index-scrap
				nuweb-index-function)
			   (progn
			     (message "Computing index entries")
			     (condition-case err
				 (funcall nuweb-index-function)
			       (error (progn (message (cadr err))
					     (sit-for 3)
					     nil))))
			 ())))
    (setq kill-buffer-hook nil)
    ;; use *Source <>* buffer's  local variable to return to correct
    ;; buffer 
    (switch-to-buffer (marker-buffer nuweb-return-location))
    (setq buffer-read-only nil)		;it was not writable
    (set-window-configuration *nuweb-win-config*)
    ;; now we can use the last-scrap-pos variable
    (goto-char (marker-position *nuweb-last-scrap-pos* ))
    (set-marker *nuweb-last-scrap-pos* nil)
    (recenter)
    ;; fix index entries (which are further down in the buffer)
    ;; before putting in the code (this might invalidates saved
    ;; positions)  
    (if nuweb-auto-index-scrap
	(progn
	  (goto-char *nuweb-last-index-begin*)
	  (delete-region *nuweb-last-index-begin*
			 *nuweb-last-index-end*)
	  (insert "@|")
	  (mapc (function (lambda(e) (insert " " e))) index-entries)
	  (insert " ")))
    (goto-char *nuweb-last-scrap-begin*)
    (delete-region *nuweb-last-scrap-begin* *nuweb-last-scrap-end*)
    (insert-buffer (nuweb-get-source-buf))
    ;; since it's been use, might as well be put it low in the list
    (bury-buffer (nuweb-get-source-buf))
    (forward-char (- offset 1))
    (setq *nuweb-last-scrap-pos* nil)))

(defun nuweb-kill-this-scrap() 
  (interactive)
  (nuweb-back-to-pos) 
  (setq *nuweb-last-scrap-pos* nil))

(defun nuweb-back-to-pos()
  (setq kill-buffer-hook nil)
  (switch-to-buffer (marker-buffer nuweb-return-location))
  (setq buffer-read-only nil)
  (delete-other-windows)
  (goto-char (marker-position *nuweb-last-scrap-pos*))
  (recenter))


;;; Below is code to support movements based on scrap uses and
;;; definitions.
;;;
;; structure to hold the scrap def descriptors
;; The name field
;; contains the name as found in buffer, then will hold the cononical
;; name (after second pass). While text will contain the whole text
;; matched when the def or use is found.

(defstruct scrap-def name text type loc)
;; structure to hold the scrap def descriptors (see above)

(defstruct scrap-use name text loc)

(defvar nuweb-scrap-def-re "@\\([oOdD]\\)[ \t]*\\(.*[^ \t\n]\\)[ \t]*\n?@{"
  "Way to match a scrap definition")

(defun nuweb-make-scrap-def-list()
  "Collect a list of all the scrap definitions"
  (save-excursion
    (goto-char (point-min))
    (loop
     while (re-search-forward nuweb-scrap-def-re nil t)
     collect (make-scrap-def :name (buffer-substring-no-properties (match-beginning 2)
						     (match-end 2))
			     :type (buffer-substring-no-properties (match-beginning 1)
						     (match-end 1))
			     :text (buffer-substring-no-properties (match-beginning 0)
						     (match-end 0))
			     :loc (match-beginning 0)))))

(defvar nuweb-scrap-use-re "@<[ \t]*\\(\\(@@\\|[^@]\\)*[^ \t\n]\\)[ \t]*@>"
  "How to recognize a usage")

(defun nuweb-make-scrap-use-list()
  "Collect list of scrap usages"
  (save-excursion
    (goto-char (point-min))
    (loop
     while (re-search-forward nuweb-scrap-use-re nil t)
     collect (make-scrap-use :name (buffer-substring-no-properties (match-beginning 1)
						     (match-end 1))
			     :text (buffer-substring-no-properties (match-beginning 0)
						     (match-end 0))
			     :loc (match-beginning 0)))))


(defun nuweb-merge-names ( defs uses)
  "Builds a list of full names use as scrap names"
  (remove-duplicates
   (sort*

    (append
     (loop for x in  defs 
	   when (not (string-match ".*\\.\\.\\."  (scrap-def-name x)))
	   collect (scrap-def-name x))
     (loop for x in  uses 
	   when (not (string-match ".*\\.\\.\\."  (scrap-use-name x)))
	   collect (scrap-use-name x)))
    'string-lessp)
   :test 'equal))
    


(defun nuweb-canonic-name(name)
  "Returns the full name corresponding to the one given"
  (if (string-match "\\(.*\\)\\.\\.\\." name)
      (let ((prefix (substring name 0 -3)))
	;; then finds it in name list
	(find (regexp-quote prefix) nuweb-names :test 'string-match))
    name))

(defun nuweb-compute-d-u()
  "Recompute all the defs and uses point in the current file"
  (interactive)
  (message "Looking for defs")
  ; gets defs,
  (setq nuweb-defs (nuweb-make-scrap-def-list))
  (message "Looking for uses")
  ; gets uses
  (setq nuweb-uses (nuweb-make-scrap-use-list))
  ;; compute list of names
  (message "Fixing names")
  (setq nuweb-names (nuweb-merge-names nuweb-defs nuweb-uses))
  ;; Now change all name fields back to their canonical names
  ;; in both lists
  (loop for def in nuweb-defs
	when (string-match "\\(.*\\)\\.\\.\\." (scrap-def-name def))
	do (setf (scrap-def-name def)
	      (nuweb-canonic-name (scrap-def-name def))))
  (loop for use in nuweb-uses
	when (string-match "\\(.*\\)\\.\\.\\." (scrap-use-name use))
	do (setf (scrap-use-name use)
	      (nuweb-canonic-name (scrap-use-name use))))
  (setq nuweb-def-use-stack nil)
  (message "Done."))

(defun nuweb-scrap-name-at-point()
  "Gathers the scrap name under the cursor. Returns a pair whose car
is the scrap name found and whose cdr is either 'def or 'use"
  (interactive)
  (save-excursion
    ;; God this code is not the nicest I've written...
    ;; Comprenne qui pourras
    (let* ((here (point))
	   (beg (if (re-search-backward "@[oOdD<]" nil t)
		    (match-end 0)
		  nil))
	   (to-match (if beg
			 (if (equal (char-after (+ (match-beginning 0) 1))
				    ?<)
			     "@>"
			   "@{")
		       nil))
	   (context (if (equal to-match "@{") 'def 'use))
	   (end (if (search-forward to-match nil t)
		    (match-beginning 0)
		  nil)))
      (cond ((and beg end
		  (<= beg here)
		  (>= end here))
	     (cons (buffer-substring-no-properties
		    (progn
		      (goto-char beg)
		      (skip-chars-forward " \t\n")
		      (point))
		    (progn
		      (goto-char end)
		      (skip-chars-backward" \t\n")
		      (point)))
		   context))
	    (t (error "Not on a possible scrap name"))))))

(defun nuweb-find-next-use()
  (interactive)
  (nuweb-find-use t))

(defun nuweb-find-use(arg)
  "Find use of scrap name at point. With argument, find next use"
  (interactive "P")
  (if (not arg)
      (nuweb-find-use-internal
       (nuweb-canonic-name (car(nuweb-scrap-name-at-point)))
       t)
    (nuweb-find-use-internal *nuweb-prev-use* nil)))

(defun nuweb-find-use-internal (name first)
  (if first
      (setq *use-list*
	    (remove* name nuweb-uses :test-not 'equal :key 'scrap-use-name)
	    ;; list of use points for scrap name at point
	    *nuweb-prev-use* name))
  (if (not *use-list*)
      (error "No more uses for of <%s>" name)
    (push (point) nuweb-def-use-stack)
    (nuweb-position-search (scrap-use-loc (car *use-list*))
			   (scrap-use-text (pop *use-list*)))))


(defun nuweb-find-next-def()
  (interactive)
  (nuweb-find-def t))

(defun nuweb-find-def(arg)
  "Find def of scrap name at point. With argument, find next def"
  (interactive "P")
  (if (not arg)
      (nuweb-find-def-internal
       (nuweb-canonic-name (car (nuweb-scrap-name-at-point)))
       t)
    (nuweb-find-def-internal *nuweb-prev-def* nil)))

(defun nuweb-find-def-internal (name first)
  (if first
      (setq *def-list*
	    (remove* name nuweb-defs :test-not 'equal :key 'scrap-def-name)
	    ;; list of def points for scrap name at point
	    *nuweb-prev-def* name))
  (if (not *def-list*)
      (error "No more defs for <%s>" name)
    (push (point) nuweb-def-use-stack)
    (nuweb-position-search (scrap-def-loc (car *def-list*))
			   (scrap-def-text (pop *def-list*)))))


(defun nuweb-position-search( loc expected-text)
  (goto-char loc)
  
  (let ((offset 250)
	(found (looking-at expected-text))
	up down)
    (while (and (not found)
		(not (equal up (point-min)))
		(not (equal down (point-max))))
      (setq up (max (- loc offset) (point-min)))
      (setq down (min (+ loc offset) (point-max)))
      (setq offset (* 2  offset))
      (goto-char up)

      (setq found (search-forward expected-text down t)))
    (if (not found)
	(error "Time to resynchronize defs and uses")
      (goto-char (+ 2 (match-beginning 0)))
      (recenter))))
    

(defun nuweb-find-def-or-use(arg)
  (interactive "e")
  (mouse-set-point arg)
  (let ((scrap (nuweb-scrap-name-at-point)))
    (message "Looking for: %S"  scrap)
    (cond ((eq (cdr scrap) 'def)
	   (nuweb-find-use-internal (nuweb-canonic-name (car scrap)) t))
	  ((eq (cdr scrap) 'use)
	   (nuweb-find-def-internal (nuweb-canonic-name (car scrap)) t))
	  (t (error "nuweb mode: should never get here")))))


(defun nuweb-pop-d-u()
  (interactive)
  (goto-char (pop nuweb-def-use-stack)))

;;;
;;; Some support for use of imenu. Experimental.
;;; Provides a menu of scrap definitions (one for files, one for
;;; macros). 


(eval-when (compile )
  (require 'imenu))

(defvar imenu-wanted t
  "Specifies whether imenu functions are useable in nuweb mode")

(if imenu-wanted
    (progn
      (require 'imenu)
      (add-hook 'nuweb-mode-hook 'nuweb-imenu-setup)))

(defun nuweb-imenu-setup ()
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'nuweb-create-imenu-index)
  (local-set-key [C-M-mouse-1] 'imenu))

(defvar imenu-nuweb-scrap-regexp
  "\\(@|[ \t]*\\([^@]*\\)@}\\)\\|\\(@[dDoO][ \t]*\\(.*[^ \t\n]\\)[ \t\n]*@{\\)"
  "How do we match a scrap def point or a index defining entry")

(defun imenu-nuweb-name-and-position-scrap()
  (if (and (match-beginning 4) (match-end 4))
      (cons (buffer-substring-no-properties (match-beginning 4) (match-end 4))
	    (set-marker (make-marker) (match-beginning 0)))
    nil))

;;; The symbol information is not used as is...
(defun imenu-nuweb-name-and-position-symbol()
  (if (and (match-beginning 2) (match-end 2))
      (cons (buffer-substring-no-properties (match-beginning 2) (match-end 2))
	    (set-marker (make-marker) (match-beginning 0)))
    nil))

(defmacro imenu-create-submenu-name(n) n)

(defun nuweb-create-imenu-index(&optional regexp)
  (let ((index-macros-alist '())
	(index-files-alist '())
	(index-symbols-alist '())
	(index-position 0)
	(char))
    (goto-char (point-min))
    (imenu-progress-message index-position 0)
    ;; Search for the function
    (save-match-data
      (while (re-search-forward
	      (or regexp imenu-nuweb-scrap-regexp)
	      nil t)
	(imenu-progress-message index-position)
	(let ((scrap-name-pos (imenu-nuweb-name-and-position-scrap))
	      (symbol-name-pos (imenu-nuweb-name-and-position-symbol)))
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (cond ((looking-at "@[oO]")
		   (push scrap-name-pos index-files-alist))
		  ( (looking-at "@|")
		    (if (looking-at "@|[ \t]*@}")
			t		; do nothing
		      (forward-char 2)
		      (let ((pos (point))
			    (ref-pos (cdr symbol-name-pos)))
			(while (not (looking-at "[\t\n ]*@}"))
			  (if (re-search-forward "[\t\n ]*\\([^@ \t\n]+\\)"
						 nil t)
			      (push (cons (buffer-substring-no-properties
					   (match-beginning 1)
					   (match-end 1))
					  ref-pos)
				    index-symbols-alist))))))
		  (t (push scrap-name-pos index-macros-alist)))))))
    (imenu-progress-message index-position 100)
    (list
     ;; the list of symbols is sorted as it is easier to search in an
     ;; alpha sorted list
     (cons (imenu-create-submenu-name "Symbols")
	   (sort (nreverse index-symbols-alist)
		 (function(lambda(pair1 pair2)
			    (string< (car pair1) (car pair2))))))
     ;; the scrap and file def point are left in their order of
     ;; appearance in the file.
     (cons (imenu-create-submenu-name "Macros")
	   (nreverse index-macros-alist))
     (cons (imenu-create-submenu-name "Files")
	   (nreverse index-files-alist)))))

;;; Support for font-lock by way of extending font-latex mode
;;;
(if
    (require 'font-latex)
    (progn
      ;; we replace the 3 symbols pointing now to our extended def for
      ;; keywords. This assumes 
    
      (defun nuweb-font-lock-setup ()
	(setq font-lock-defaults
	      (cons '(nuweb-font-keywords nuweb-font-keywords-1
					  nuweb-font-keywords-2)
		    (cdr font-lock-defaults))))
      
      (defconst nuweb-font-keywords-data
	'( "@[@oOdD<>{}|umf]"			;basic nuweb stuff
	   ("@[oO][ \t]*\\(.*\\)[ \t]*@{" 1 font-lock-function-name-face nil t)
	   ("@[dD][ \t]*\\(.*\\)[ \t]@{" 1 font-lock-function-name-face nil t)
	   ("@<[ \t]*\\(.*\\)[ \t]*@>" 1 font-lock-reference-face nil t)
	   ("@|[ \t]*\\(.*\\)[ \t]*@}" 1 font-lock-reference-face nil t)))
      
      (defvar nuweb-font-keywords
	(append nuweb-font-keywords-data
		font-latex-keywords))
      (defvar nuweb-font-keywords-1
	(append nuweb-font-keywords-data
		font-latex-keywords-1))
      (defvar nuweb-font-keywords-2
	(append nuweb-font-keywords-data
		font-latex-keywords-2))
      
      (add-hook 'nuweb-mode-hook 'nuweb-font-lock-setup)))
;;;
;;; Language specific entries
;;;
;;; A) A first extension is to extract from a given buffer all the
;;; predicate names and arities so that an indexing entry be quickly
;;; made up which introduces both the name and the name/arity
;;; pairs. This will help in the prevention of wrong arity
;;; calls/defs.


;;; This function should still ignore stuff in between @< @>

(defun nuweb-prolog-get-index-entries ()
  (save-excursion
    (goto-char (point-min))
    (loop
     
     for pred-arity = (progn
			(forward-comment (buffer-size))
			(skip-syntax-forward " ")
			(find-pred))
     then (find-pred)
     if (not-junk-predicate pred-arity)
        collect (format "%s/%d"
		     (car pred-arity)
		     (cdr pred-arity))
	and collect (car pred-arity)
	and do (message "%s/%d" (car pred-arity) (cdr pred-arity))
     do
     (forward-comment (buffer-size))
     (skip-syntax-forward " ")
     until (eobp)
     )))

(defun not-junk-predicate(pred-arity)
  (let ((p (car pred-arity))(a (cdr pred-arity)))
    (cond ( (equal p ":-") nil)
	  ( t t))))

(eval-when (load)
  (push (cons "ISO-prolog" 'nuweb-prolog-get-index-entries)
	nuweb-index-functions)
  (push (cons "BIMcompat" 'nuweb-prolog-get-index-entries)
	nuweb-index-functions))

(provide 'nuweb)
;;; end of nuweb.el
