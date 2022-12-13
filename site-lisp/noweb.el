;;; noweb.el --- noweb mode for literate programming

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages, tools
;; Created: Oct 2003
;; $Revision: 1.6 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A sort of major mode for the noweb literate programming system.
;; noweb files contain documentation regions which should be edited in
;; a major mode `noweb-doc-mode' and code regions which should be
;; edited in major mode `noweb-code-mode'.  These are separated by
;; marker regions which are edited in a separate `noweb-mode'.  (It's
;; arguable whether this is better than treating the marker as the
;; same mode as its following `chunk', but may cause less disturbance,
;; even with font-lock and `parse-sexp-lookup-properties' in force.)
;; The code and doc modes can be customized globally or set in file
;; local variables, e.g.:
;; 
;; Local variables:
;; mode: noweb
;; noweb-code-mode: c-mode
;; noweb-doc-mode: nroff-mode
;; End:

;; This is mostly compatible command- and keymap-wise with Thorsten
;; Ohl's old noweb-mode.el.  It is based on my multi-mode.el, and
;; inherits issues that has, especially in Emacs < 21.4.

;; Inter-chunk Outline and Imenu support is provided only in the noweb
;; buffer, i.e. when point is in a chunk header.  This could probably
;; be improved.

;; Perhaps needs commands for weaving and tangling the buffer rather
;; than relying on make.  Could use dealing with gerd's
;; <<name -*-mode-*->> notation for multiple code modes.

;;; Code:

(require 'multi-mode)
(eval-when-compile
  (require 'cl)
  (require 'easy-mmode))

(autoload 'font-lock-set-defaults "font-lock")

(defgroup noweb nil
  "Editing noweb documents"
  :link '(emacs-commentary-link "noweb")
  :group 'languages)

(defcustom noweb-doc-mode 'latex-mode
  "Major mode for documentation chunks."
  :type 'function
  :group 'noweb)
(make-variable-buffer-local 'noweb-doc-mode)

(defcustom noweb-code-mode 'fundamental-mode
  "Major mode for code chunks."
  :type 'function
  :group 'noweb)
(make-variable-buffer-local 'noweb-code-mode)

(defcustom noweb-select-code-mode-hook nil
  "Hook run after the code mode is selected."
  :group 'noweb
  :type 'hook)

(defcustom noweb-select-doc-mode-hook nil
  "Hook run after the documentation mode is selected."
  :group 'noweb
  :type 'hook)

(defcustom noweb-select-noweb-mode-hook nil
  "Hook run after noweb mode is selected (on header lines)."
  :group 'noweb
  :type 'hook)

(defconst noweb-chunk-header-pattern
  "^\\(?:\\(@ +%def .*\\)$\\|<<\\(.*\\)>>=$\\|\\(@[ \n]\\)\\)"
  "Regexp matching the start of a chunk header.")

;; Fixme: index %def items.
(defconst noweb-imenu-generic-expression
  '((nil "^<<\\(.*\\)>>=$" 1)))

(defconst noweb-font-lock-keywords
  `(("^\\(<<\\)\\(.*\\)\\(>>=\\)$" (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face) (3 'font-lock-keyword-face))
    ("^\\(@ +%def\\) +\\(.+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    "^@"))

;; It's not clear whether this should obey the major or minor mode
;; conventions, or something else.  M-n is what the old mode used, and
;; it still unused in the global map, so it's a reasonable choice.

(defcustom noweb-prefix-key "\M-n"
  "Prefix key for the Noweb mode keymap.
Not effective after loading the noweb library."
  :group 'noweb
  :type '(choice string vector))

(eval-when-compile
  (defvar noweb-electric-<)
  (defvar noweb-electric-@))

(defvar noweb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map noweb-prefix-key
      (let ((map (make-sparse-keymap)))
	(define-key map "\C-n" 'noweb-next)
	(define-key map "\C-p" 'noweb-prev)
	(define-key map "\M-n" 'noweb-goto-next)
	(define-key map "\M-p" 'noweb-goto-prev)
	(define-key map "c" 'noweb-code-next)
	(define-key map "C" 'noweb-code-prev)
	(define-key map "d" 'noweb-doc-next)
	(define-key map "D" 'noweb-doc-prev)
;; Use imenu.
;; 	(define-key map "\C-g" 'noweb-goto-chunk)
	(define-key map "k" 'noweb-kill-chunk)
	(define-key map "m" 'noweb-mark-chunk)
;; fixme: implement
;; 	(define-key map "M" 'noweb-mark-chunk-pair)
;; 	(define-key map "K" 'noweb-kill-chunk-pair)
	(define-key map "\t" 'noweb-complete-chunk)
	(define-key map "i" 'noweb-new-chunk)
	(define-key map "o" 'noweb-occur)
	(if (bound-and-true-p noweb-electric-<)
	    (define-key noweb-mode-map "<" #'noweb-electric-<))
	(if (bound-and-true-p noweb-electric-@)
	    (define-key noweb-mode-map "@" #'noweb-electric-@))
	map))
    (define-key map [menu-bar noweb]
      (cons "Noweb"
	    (let ((map (make-sparse-keymap "Noweb")))
	      (define-key-after map [next]
		'(menu-item "Next chunk" noweb-next))
	      (define-key-after map [prev]
		'(menu-item "Previous chunk" noweb-prev))
	      (define-key-after map [next-code]
		'(menu-item "Next code chunk" noweb-code-next))
	      (define-key-after map [prev-code]
		'(menu-item "Previous code chunk" noweb-code-prev))
	      (define-key-after map [next-doc]
		'(menu-item "Next doc chunk" noweb-doc-next))
	      (define-key-after map [prev-doc]
		'(menu-item "Previous doc chunk" noweb-doc-prev))
	      (define-key-after map [goto-prev]
		'(menu-item "Next matching chunk" noweb-goto-next))
	      (define-key-after map [goto-prev]
		'(menu-item "Previous matching chunk" noweb-goto-prev))
	      (define-key-after map [mark]
		'(menu-item "Mark chunk" noweb-mark-chunk))
	      (define-key-after map [kill]
		'(menu-item "Kill chunk" noweb-kill-chunk))
	      (define-key-after map [new]
		'(menu-item "New chunk" noweb-new-chunk))
	      (define-key-after map [occur]
		'(menu-item "Occurences of this chunk" noweb-occur))
	      map)))
    map))

(defcustom noweb-electric-keys nil
  "Non-nil means to make `<' and `@' electric.
This means that they get bound to `noweb-electric-<' and `noweb-electric-@'."
  :set (lambda (s v)
	 (set-default s v)
	 (if v
	     (progn (define-key noweb-mode-map "<" #'noweb-electric-<)
		    (define-key noweb-mode-map "@" #'noweb-electric-@))
	   (define-key noweb-mode-map "<" nil)
	   (define-key noweb-mode-map "@" nil)))
  :group 'noweb
  :type 'boolean)

;; Used to propagate the bindings to the indirect buffers.
(define-minor-mode noweb-minor-mode
  "Noweb minor mode, used in code and doc chunks."
  nil " Noweb" noweb-mode-map)

(defvar noweb-buffer nil
  "Internal use.")
(defvar noweb-doc-buffer nil
  "Internal use.")
(defvar noweb-code-buffer nil
  "Internal use.")

(defun noweb-set-local-variables ()
  (set (make-local-variable 'noweb-code-buffer) noweb-code-buffer)
  (set (make-local-variable 'noweb-doc-buffer) noweb-doc-buffer)
  (set (make-local-variable 'noweb-buffer) noweb-buffer)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (unless (eq noweb-buffer (current-buffer))
    (noweb-minor-mode 1)
    (font-lock-set-defaults))
  (add-hook (make-local-variable 'multi-select-mode-hook)
	    'noweb-select-noweb-mode-hook))

(define-derived-mode noweb-mode fundamental-mode "Noweb"
  "Mode for editing noweb documents.
Supports differnt major modes for doc and code chunks using multi-mode."
  ;; Extract values of local variables now, so we know the doc and
  ;; code modes.  Nullify noweb-mode while we process possible
  ;; `mode: noweb' line to avoid infinite regress.
  (flet ((noweb-mode ()))
    (hack-local-variables)
    (let ((multi-alist (list (cons 'noweb-mode #'noweb-select-mode)
			     (cons noweb-doc-mode nil)
			     (cons noweb-code-mode nil))))
      (multi-mode-install-modes)))
  (let ((noweb-doc-buffer (cdr (assq noweb-doc-mode
				     multi-indirect-buffers-alist)))
	(noweb-code-buffer (cdr (assq noweb-code-mode
				      multi-indirect-buffers-alist)))
	(noweb-buffer (cdr (assq 'noweb-mode
				 multi-indirect-buffers-alist))))
    (with-current-buffer noweb-code-buffer
      (noweb-set-local-variables)
      ;; Add font-lock stuff for chunk uses in code.  Add syntactic
      ;; keywords to treat them as comments with
      ;; `parse-sexp-lookup-properties' on (see
      ;; `noweb-set-local-variables') to try to avoid disturbing the
      ;; mode's idea of syntax.  (This used to be strings instead of
      ;; comments, but that doesn't work well with C, for instance.)
      ;; Then fontify the uses like their definitions, using
      ;; overriding patterns.
      (set (make-local-variable 'font-lock-syntactic-keywords)
	   (append (font-lock-eval-keywords font-lock-syntactic-keywords)
		   '(("\\(?:$\\|[^@]\\)\\(<\\)<[^>]+>\\(>\\)"
		      (1 "!") (2 "!")))))
      (set (make-local-variable 'font-lock-keywords)
	   (append (font-lock-eval-keywords font-lock-keywords)
		   '(("\\(?:$\\|[^@]\\|\\`\\)\\(<<\\)\\([^>]+\\)\\(>>\\)"
		      (1 'font-lock-keyword-face t)
		      (2 'font-lock-variable-name-face t)
		      (3 'font-lock-keyword-face t))))))
    (with-current-buffer noweb-doc-buffer
      (noweb-set-local-variables)
      ;; Treat noweb literals `[[...]]' as strings for syntax purposes.
      (set (make-local-variable 'font-lock-syntactic-keywords)
	   (append (font-lock-eval-keywords font-lock-syntactic-keywords)
		   '(("\\(\\[\\)\\[[^]]+]\\(]\\)" (1 "|") (2 "|"))))))
    (noweb-set-local-variables)
    ;; Use Imenu to navigate chunks.
    (set (make-local-variable 'imenu-generic-expression)
	 noweb-imenu-generic-expression)
    (set (make-local-variable 'font-lock-defaults)
	 '(noweb-font-lock-keywords nil nil nil nil))
    ;; Single level of outline.
    (set (make-local-variable 'outline-regexp)
	 noweb-chunk-header-pattern)
    (set (make-local-variable 'outline-level) (lambda () 1))
    (imenu-add-menubar-index)))

(defun noweb-select-mode (pos)
  "Mode-selecting function for use in `multi-alist'."
  (save-excursion
    (save-restriction
      (goto-char pos)
      (beginning-of-line)
      (cond
       ((looking-at noweb-chunk-header-pattern) ; on a header line
	;; A %def line or a code header line means neither doc or code.
	(cond
	 ((match-beginning 2)
	  (multi-make-list 'noweb-mode (match-beginning 0) (match-end 0)))
	 ((match-beginning 1)
	  (let (start)
	    (while (looking-at "@ +%def")
	      (setq start (match-beginning 0))
	      (previous-line 1))
	    (next-line 1)
	    (while (looking-at "@ +%def")
	      (next-line 1))
	    (multi-make-list 'noweb-mode start (line-end-position 0))))
	 (t 
	  ;; Else start of a doc chunk.  Mode is doc unless we're at bol.
	  (if (< pos (match-end 0))
	      (multi-make-list 'noweb-mode (match-beginning 0)
			       (1- (match-end 0)))
	    (goto-char (match-end 0))
	    (multi-make-list
	     noweb-doc-mode (point)
	     (if (re-search-forward noweb-chunk-header-pattern nil t)
		 (1- (match-beginning 0))
	       (point-max)))))))
       ((re-search-backward noweb-chunk-header-pattern nil t)
	(let ((start (if (match-beginning 3)
			 (match-end 0)
		       (1+ (match-end 0))))
	      (mode (if (match-beginning 2)
			noweb-code-mode
		      noweb-doc-mode))
	      (end (progn
		     (goto-char pos)
		     (if (re-search-forward noweb-chunk-header-pattern
					    nil t)
			 (1- (match-beginning 0))
		       (point-max)))))
	  (multi-make-list mode start end)))
       (t
	(multi-make-list
	 noweb-doc-mode (point-min)
	 (progn
	   (goto-char pos)
	   (if (re-search-forward noweb-chunk-header-pattern nil t)
	       (1- (match-beginning 0))
	     (point-max)))))))))

(easy-mmode-define-navigation noweb "^@[ \n]\\|^<<.*>>=$" "chunk")
(easy-mmode-define-navigation noweb-code "^<<.*>>=$" "code chunk")
(easy-mmode-define-navigation noweb-doc "^@[ \n]" "doc chunk")

;; Fixme:  Maybe this should mark the header part as well as the
;; following chunk.
(defun noweb-mark-chunk ()
  "Mark the current chunk."
  (interactive)
  (let ((r (multi-find-mode-at (point))))
    (goto-char (nth 2 r))
    (if (and (eolp) (not (eobp)))
	(forward-char))
    ;; Put a mark in all the buffers to avoid the worst effects of
    ;; losing the mark when moving point.
    (dolist (elt multi-indirect-buffers-alist)
      (with-current-buffer (cdr elt)
	(push-mark (nth 1 r) nil t)))))

(defun noweb-goto-next ()
  "Go to the first continuation of current code chunk."
  (interactive)
  (let ((chunk (multi-find-mode-at (point))))
    (if (eq 'noweb-doc-mode (car chunk))
      (error "Not in a code chunk"))
    (end-of-line)
    (noweb-code-prev)
    (unless (search-forward (buffer-substring (line-beginning-position)
					      (line-end-position))
			    nil t 2)
      (error "No next matching chunk"))))

(defun noweb-goto-prev ()
  "Go to the first continuation of current code chunk."
  (interactive)
  (let ((chunk (multi-find-mode-at (point))))
    (if (eq 'noweb-doc-mode (car chunk))
      (error "Not in a code chunk"))
    (end-of-line)
    (noweb-code-prev)
    (unless (search-backward (buffer-substring (line-beginning-position)
					       (line-end-position))
			     nil t)
      (error "No previous matching chunk"))))

(defun noweb-new-chunk (name)
  "Insert a new chunk with name NAME.
Interactively, prompt for name."
  (interactive "sChunk name: ")
  (insert "@\n")
  (save-excursion
    (insert "\n<<" name ">>=\n\n@ %def \n")))

(defun noweb-select-mode-hook ()
  "`multi-select-mode-hook' function.
Run the hooks `noweb-select-code-mode-hook', `noweb-select-doc-mode-hook'
and `noweb-select-noweb-mode-hook'."
  (cond ((eq major-mode noweb-code-mode)
	 (run-hooks 'noweb-select-code-mode-hook))
	((eq major-mode noweb-doc-mode)
	 (run-hooks 'noweb-select-doc-mode-hook))
	((eq major-mode 'noweb-mode)
	 (run-hooks 'noweb-select-noweb-mode-hook))))

(defun noweb-next-chunk (&optional N)
  "Go to Nth next chunk of this type.
N defaults to 1."
  (interactive)
  (cond ((eq major-mode noweb-code-mode)
	 (noweb-code-next N))
	((eq major-mode noweb-doc-mode)
	 (noweb-doc-next N))
	(t (noweb-next N))))

(defun noweb-prev-chunk (&optional N)
  "Go to Nth previous chunk of this type.
N defaults to 1."
  (interactive)
  (cond ((eq major-mode noweb-code-mode)
	 (noweb-code-prev N))
	((eq major-mode noweb-doc-mode)
	 (noweb-doc-prev N))
	(t (noweb-next N))))

(defun noweb-electric-@ (arg)
  "Smart incarnation of `@', starting a new documentation chunk, maybe.
If given an numerical argument, it will act just like the dumb `@'.
Otherwise, at the beginning of a line in a code chunk, insert \"@ \",
otherwise insert `@'."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (bolp)
	     (eq major-mode noweb-code-mode))
	(insert "@ ")
      (self-insert-command 1))))

(defun noweb-electric-< (arg)
  "Smart incarnation of `<', starting a new code chunk, maybe.
With numerical argument, just insert `<'.
Otherwise, at the beginning of a line in a documentation chunk,
insert \"<<>>=\" and a newline if necessary.  Otherwise insert `<'."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (if (and (bolp)
	     (not (eq major-mode noweb-code-mode)))
	(progn
	  (insert "<<")
	  (save-excursion
	    (insert ">>=")
	    (if (not (looking-at "\\s *$"))
		(newline))))
      (self-insert-command 1))))

(defalias 'noweb-narrow-to-chunk 'multi-narrow-to-chunk)

(defun noweb-complete-chunk ()
  "Complete name of used chunk.
The partial chunk name must be preceded by `<<'."
  (interactive)
  (let ((start (save-excursion
		 (if (re-search-backward "\\(?:$\\|[^@]\\)<<"
					 (line-end-position 0) t)
		     (match-end 0))))
	(end (point)))
    (if (not start)
	(error "Not at a chunk start")
      (with-current-buffer noweb-buffer
	(imenu--make-index-alist)
	(let* ((base (buffer-substring start end))
	       (completion (try-completion base imenu--index-alist)))
	  (goto-char end)
	  (cond ((eq completion t)
		 (unless (looking-at ">>")
		   (insert ">>")
		   (backward-char 2)))
		((not completion)
		 (error "No completion for \"%s\"" base))
		((not (string= base completion))
		 (delete-region start end)
		 (insert-before-markers completion)
		 (unless (looking-at ">>")
		   (insert ">>")
		   (backward-char 2)))
		(t
		 (message "Making completion list...")
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list
		    (all-completions base imenu--index-alist)))
		 (message "Making completion list...done"))))))))

(defun noweb-occur ()
  "Find occurrences of the current code chunk."
  (interactive)
  (unless (or (eq major-mode noweb-code-mode)
	      (and (eq major-mode 'noweb-mode)
		   (eq (char-after (line-beginning-position)) ?\<)))
    (error "Not in a code chunk"))
  (save-excursion
    (end-of-line)
    (noweb-code-prev)
    (occur (regexp-quote (buffer-substring (point) (line-end-position))))))

(defun noweb-kill-chunk ()
  "Kill the current chunk."
  (interactive)
  (apply #' delete-region (cdr (multi-find-mode-at (point)))))

(add-to-list 'debug-ignored-errors "Not at a chunk start")
(add-to-list 'debug-ignored-errors "No \\sw+ matching chunk")
(add-to-list 'debug-ignored-errors "No completion for ")
(add-to-list 'debug-ignored-errors "Not in a code chunk")

(provide 'noweb)
;;; noweb.el ends here
