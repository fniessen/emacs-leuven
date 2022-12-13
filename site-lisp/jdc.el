;; $Revision: 1.3 $

;; Copyright (C) 2000 by Ingo Koch
;; Copyright (C) 2000 by Craig McGeachie
;; Copyright (C) 2003 by Kevin J. Butler

;; Author: ingo Koch <ingo.koch@ikoch.de>
;; The Idea is stolen from the .emacs file of 
;; Jake Donham <jake@bitmechanic.com>
;; who did this for the mocha java decompiler
;; Maintainer: Ingo Koch <ingo.koch@ikoch.de>
;; Keywords: java, tools

;; Modified beyond all recognition: Craig McGeachie <craig@rhe.com.au>
;; The previous comment block is the original.  The original source is
;; almost unrecognisable here, so I couldn't say how much Ingo Koch
;; would be willing to maintain it.

;; Minor changes by Kevin J. Butler <emacs-kbutler@sabaydi.com>
;; 2003-10-08 linked decompilation to the CAFEBABE tag, hooked archive
;; extractions, made jdc-buffer interactive, updated text,
;; prefixed all variables with 'jdc-'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:

;; This package is an add-on to the Java Development Environment
;; (JDE) for Emacs. It automatically decompiles a class file and 
;; offers you a buffer to view or edit it.
;; javadecomp (currently) relies on the jad java decompiler to
;; do the actual work, but it should be possible to extend it to
;; whatever you like (sugestions are welcome).

;;; Used packages or utilities:

;; jad is developed by Pavel Kouznetsov <kpdus@yahoo.com>
;; available (depending on the tripod bandwidth limits)
;; http://kpdus.tripod.com/jad.html
;; JDEE is developed by Paul Kinnucan <paulk@mathworks.com>
;; available at http://jdee.sunsite.dk/


;;; Installation:

;; Put the following in your .emacs file:
;;   (require 'jdc)

;;; Usage:

;; Open a class file and feel happy. Open a .jar file, then open a .class
;; file inside that .jar file, and also feel happy.
;;
;; If you decide you want to get back to the binary class file, toggle read-only
;; (if needed) then 'undo':  C-x C-q C-_

;;; Support:

;; Any comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to Ingo Koch at ingo.koch@ikoch.de.
;;

;;; Bugs:

;; Possibly better to open the .java file in a split window?

;; jdc assumes the byte comes from a .class file on
;; disk and therefore the .java buffer name can be constructed
;; from this.  The byte code should be decompiled, and the
;; .java buffer name constructed by parsing the source for the
;; class declaration.

;; jdc demands that the decompiler output source code to
;; standard out.  This should be made more flexible.
;; There is a flag that, if set, means that the source code is placed
;; in a buffer that is modifable, associated with a Java filename,
;; and marked as modified.  The buffer isn't saved, so the Java file
;; isn't written to.  The flag should be extended to include a third
;; option to automatically save the reconstructed Java source.

;; Some thought is needed, about what to do if the intended Java file
;; already exists.

;;; Code:

(require 'jde)

(defconst jdc-cafebabe "\xCA\xFE\xBA\xBE"
  "The first four bytes of a Java class file.")

(defconst jdc-source-extension  ".java"
  "The extension which is used for the generated java files.")

(defconst jdc-object-extension-pattern "\\.class\\( (.*)\\)$"
  "The extension to look for, if a file is to be decompiled on the fly.")

(defgroup jdc nil
  "A Java object file (suffix of '.class') may be
opened up with Emacs, and automatically decompiled into understandable
Java source.  This uses an external decompilation tool, such as Jad."
  :tag "Java Decompilation"
  :group 'jde
  :prefix "jdc-")

(defcustom jdc-command  "jad"
  "The name of the decompiler if it's on your path, otherwise a full
qualified path to it.  The decompilation elisp glue code assumes that
the decompiler tool takes a file name as a last argument."
  :tag "Decompiler command"
  :group 'jdc
  :type 'string)

(defcustom jdc-parameter  '("-space"
                            "-t4"
                            "-lnc"
                            "-b"
                            "-p"
                            "-dead"
                            "-ff"
                            "-i"
                            "-l60"
                            "-nl")
  "Extra parameters which should be added to the call of the
decompiler.  Add one switch per entry.  If you try to put multiple
options into one line, then the options will be passed to the
decompiler as a single option, spaces and all.  If your decompiler
requires a switch to output the Java source text to standard out,
then make sure it is entered here."
  :tag "Command line options"
  :group 'jdc
  :type '(repeat string)
  )

(defcustom jdc-create-source-file-p nil
  "Create a modifible source buffer.  If this is non-nil, then the
buffer is set up as a Java source file that can be saved to file, with
or without modification.  If this is nil, then the buffer is a
read-only source view of the original file.  The read only file can
still be written to another file location."
  :tag "Create source buffer"
  :group 'jdc
  :type 'boolean)

(defun jdc-make-source-name (name-with-class-suffix)
  "If a strings ends with '.class', return it with '.java' as the
suffix.  Otherwise the string is unchanged."
  (if (string-match jdc-object-extension-pattern name-with-class-suffix)
      (replace-match (concat jdc-source-extension) nil nil name-with-class-suffix)
    name-with-class-suffix))


(defun jdc-buffer ()
  "Construct the command for decompiling a class file, call the resulting
command and load the decompiled file."
  (interactive)
  (let ((temp-file-name (concat temporary-file-directory (make-temp-name "jdc")))
        (orig-buffer-name (buffer-name))
        (orig-file-name (buffer-file-name)))
    (progn
      (write-file temp-file-name)
      (apply 'call-process-region 
             (point-min)
             (point-max)
             jdc-command
             t
             t
             nil
             (append jdc-parameter (list temp-file-name)))
      (if jdc-create-source-file-p
          (progn
            (set-visited-file-name (jdc-make-source-name orig-file-name))
            (let ((new-buffer-name (jdc-make-source-name orig-buffer-name)))
              (condition-case nil
                (rename-buffer new-buffer-name)
                (error (rename-buffer new-buffer-name t)))))
        (progn 
          (set-visited-file-name nil)
          (rename-buffer orig-buffer-name)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (jde-mode)))
      (delete-file temp-file-name))))

(defun jdc-hook ()
  "Function to call to conditionally extract a class file.
Does nothing unless the first four bytes of the file are CAFEBABE."
  (interactive)
  (if (string-match jdc-cafebabe (buffer-substring 1 (min 5 (point-max))))
      (jdc-buffer)))

(add-hook 'find-file-hooks 'jdc-hook)
(add-hook 'archive-extract-hooks 'jdc-hook)

(provide 'jdc)
