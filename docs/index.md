<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">Description</a></li>
<li><a href="#sec-2">Header</a></li>
<li><a href="#sec-3">Code</a></li>
<li><a href="#sec-4">Loading Libraries of Lisp Code for Emacs</a>
<ul>
<li><a href="#sec-4-1">Creating and selecting buffers</a></li>
</ul>
</li>
<li><a href="#sec-5">Environment</a></li>
<li><a href="#sec-6">Debugging</a></li>
<li><a href="#sec-7">Emacs Lisp Packages</a>
<ul>
<li><a href="#sec-7-1">The Package Menu Buffer</a></li>
<li><a href="#sec-7-2">Package Installation</a></li>
</ul>
</li>
<li><a href="#sec-8">The Organization of the Screen</a></li>
<li><a href="#sec-9">Exiting Emacs</a></li>
<li><a href="#sec-10">Basic Editing Commands</a>
<ul>
<li><a href="#sec-10-1">Inserting Text</a></li>
<li><a href="#sec-10-2">Moving Point Location</a></li>
<li><a href="#sec-10-3">Basic Undoing Changes</a></li>
</ul>
</li>
<li><a href="#sec-11">The Minibuffer</a>
<ul>
<li><a href="#sec-11-1">Editing in the Minibuffer</a></li>
<li><a href="#sec-11-2">Completion</a></li>
<li><a href="#sec-11-3">Repeating Minibuffer Commands</a></li>
</ul>
</li>
<li><a href="#sec-12">Help</a>
<ul>
<li><a href="#sec-12-1">Help Summary</a></li>
<li><a href="#sec-12-2">Apropos</a></li>
<li><a href="#sec-12-3">Misc Help Commands</a></li>
</ul>
</li>
<li><a href="#sec-13">The Mark and the Region</a>
<ul>
<li><a href="#sec-13-1">Setting the Mark</a></li>
<li><a href="#sec-13-2">Commands to Mark Textual Objects</a></li>
<li><a href="#sec-13-3">Operating on the Region</a></li>
<li><a href="#sec-13-4">Shift Selection</a></li>
<li><a href="#sec-13-5">Multiple selections</a></li>
</ul>
</li>
<li><a href="#sec-14">Killing and Moving Text</a>
<ul>
<li><a href="#sec-14-1">Deletion and Killing</a></li>
<li><a href="#sec-14-2">Yanking</a></li>
<li><a href="#sec-14-3">Cutting and Pasting on Graphical Displays</a></li>
<li><a href="#sec-14-4">Rectangles</a></li>
<li><a href="#sec-14-5">CUA Bindings</a></li>
</ul>
</li>
<li><a href="#sec-15">Registers</a>
<ul>
<li><a href="#sec-15-1">Saving Positions in Registers</a></li>
<li><a href="#sec-15-2">Bookmarks</a></li>
<li><a href="#sec-15-3">Ace Jump</a></li>
</ul>
</li>
<li><a href="#sec-16">Controlling the Display</a>
<ul>
<li><a href="#sec-16-1">Scrolling</a></li>
<li><a href="#sec-16-2">Automatic Scrolling</a></li>
<li><a href="#sec-16-3">Horizontal Scrolling</a></li>
<li><a href="#sec-16-4">Narrowing</a></li>
<li><a href="#sec-16-5">Font Lock mode</a></li>
<li><a href="#sec-16-6">Interactive Highlighting</a></li>
<li><a href="#sec-16-7">Window Fringes</a></li>
<li><a href="#sec-16-8">Displaying Boundaries</a></li>
<li><a href="#sec-16-9">Useless Whitespace</a></li>
<li><a href="#sec-16-10">Selective Display</a></li>
<li><a href="#sec-16-11">Optional Mode Line Features</a></li>
<li><a href="#sec-16-12">Displaying the Cursor</a></li>
<li><a href="#sec-16-13">Truncation of Lines</a></li>
<li><a href="#sec-16-14">Customization of Display</a></li>
</ul>
</li>
<li><a href="#sec-17">Searching and Replacement</a>
<ul>
<li><a href="#sec-17-1">Incremental Search (aka &ldquo;Live Search&rdquo;)</a></li>
<li><a href="#sec-17-2">Symbol Search</a></li>
<li><a href="#sec-17-3">Regexp Search</a></li>
<li><a href="#sec-17-4">Search Case</a></li>
<li><a href="#sec-17-5">Replacement Commands</a></li>
<li><a href="#sec-17-6">Other Search-and-Loop Commands</a></li>
</ul>
</li>
<li><a href="#sec-18">Commands for Fixing Typos</a>
<ul>
<li><a href="#sec-18-1">Checking and Correcting Spelling</a></li>
</ul>
</li>
<li><a href="#sec-19">Keyboard Macros</a>
<ul>
<li><a href="#sec-19-1">Basic Use</a></li>
<li><a href="#sec-19-2">Naming and Saving Keyboard Macros</a></li>
</ul>
</li>
<li><a href="#sec-20">Files Handling</a>
<ul>
<li><a href="#sec-20-1">Visiting Files</a></li>
<li><a href="#sec-20-2">Saving Files</a></li>
<li><a href="#sec-20-3">Reverting a Buffer</a></li>
<li><a href="#sec-20-4">Auto Reverting Non-File Buffers</a></li>
<li><a href="#sec-20-5">Auto-Saving: Protection Against Disasters</a></li>
<li><a href="#sec-20-6">Comparing Files</a></li>
<li><a href="#sec-20-7">Diff mode</a></li>
<li><a href="#sec-20-8">Miscellaneous File Operations</a></li>
<li><a href="#sec-20-9">Accessing Compressed Files</a></li>
<li><a href="#sec-20-10">Auto Encryption</a></li>
<li><a href="#sec-20-11">Remote Files</a></li>
<li><a href="#sec-20-12">Convenience Features for Finding Files</a></li>
</ul>
</li>
<li><a href="#sec-21">Using Multiple Buffers</a>
<ul>
<li><a href="#sec-21-1">Listing Existing Buffers</a></li>
<li><a href="#sec-21-2">Killing Buffers</a></li>
<li><a href="#sec-21-3">Operating on Several Buffers</a></li>
<li><a href="#sec-21-4">Convenience Features and Customization of Buffer Handling</a></li>
</ul>
</li>
<li><a href="#sec-22">Multiple Windows</a>
<ul>
<li><a href="#sec-22-1">Concepts of Emacs Windows</a></li>
<li><a href="#sec-22-2">Using Other Windows</a></li>
<li><a href="#sec-22-3">Deleting and Rearranging Windows</a></li>
<li><a href="#sec-22-4">Dedicated windows</a></li>
<li><a href="#sec-22-5">Displaying a Buffer in a Window</a></li>
<li><a href="#sec-22-6">Window Handling Convenience Features and Customization</a></li>
</ul>
</li>
<li><a href="#sec-23">Frames and Graphical Displays</a>
<ul>
<li><a href="#sec-23-1">Mouse Commands for Editing</a></li>
<li><a href="#sec-23-2">Creating Frames</a></li>
<li><a href="#sec-23-3">Frame Commands</a></li>
<li><a href="#sec-23-4">Speedbar Frames</a></li>
<li><a href="#sec-23-5">Scroll Bars</a></li>
<li><a href="#sec-23-6">Tool Bars</a></li>
<li><a href="#sec-23-7">Using Dialog Boxes</a></li>
<li><a href="#sec-23-8">Tooltips</a></li>
</ul>
</li>
<li><a href="#sec-24">International Character Set Support</a>
<ul>
<li><a href="#sec-24-1">Introduction to International Character Sets</a></li>
<li><a href="#sec-24-2">Language Environments</a></li>
<li><a href="#sec-24-3">Input Methods</a></li>
<li><a href="#sec-24-4">Recognizing Coding Systems</a></li>
<li><a href="#sec-24-5">Specifying a File&rsquo;s Coding System</a></li>
<li><a href="#sec-24-6">Bidirectional Editing</a></li>
</ul>
</li>
<li><a href="#sec-25">Major and Minor Modes</a>
<ul>
<li><a href="#sec-25-1">How Major Modes are Chosen</a></li>
</ul>
</li>
<li><a href="#sec-26">Indentation</a>
<ul>
<li><a href="#sec-26-1">Indentation Commands</a></li>
<li><a href="#sec-26-2">Tabs vs. Spaces</a></li>
</ul>
</li>
<li><a href="#sec-27">Commands for Human Languages</a>
<ul>
<li><a href="#sec-27-1">Words</a></li>
<li><a href="#sec-27-2">Sentences</a></li>
<li><a href="#sec-27-3">Filling Text</a></li>
<li><a href="#sec-27-4">Case Conversion Commands</a></li>
<li><a href="#sec-27-5">Outline Mode</a></li>
<li><a href="#sec-27-6">Boxquote</a></li>
<li><a href="#sec-27-7">Phonetic</a></li>
</ul>
</li>
<li><a href="#sec-28">Org Mode (Getting Things Done)</a>
<ul>
<li><a href="#sec-28-1">Document Structure</a></li>
<li><a href="#sec-28-2">Tables</a></li>
<li><a href="#sec-28-3">Hyperlinks</a></li>
<li><a href="#sec-28-4">&ldquo;TODO&rdquo; Items</a></li>
<li><a href="#sec-28-5">Tags</a></li>
<li><a href="#sec-28-6">Properties and Columns</a></li>
<li><a href="#sec-28-7">Dates and Times</a></li>
<li><a href="#sec-28-8">Capture - Refile - Archive</a></li>
<li><a href="#sec-28-9">Agenda Views</a></li>
<li><a href="#sec-28-10">Markup for rich export</a></li>
<li><a href="#sec-28-11">Exporting</a></li>
<li><a href="#sec-28-12">Publishing</a></li>
<li><a href="#sec-28-13">Working With Source Code</a></li>
<li><a href="#sec-28-14">Miscellaneous</a></li>
<li><a href="#sec-28-15">Other</a></li>
<li><a href="#sec-28-16">A.3 Adding hyperlink types</a></li>
<li><a href="#sec-28-17">A.5 Tables and lists in arbitrary syntax</a></li>
<li><a href="#sec-28-18">A.6 Dynamic blocks</a></li>
<li><a href="#sec-28-19">Org-contrib</a></li>
</ul>
</li>
<li><a href="#sec-29">TeX</a>
<ul>
<li><a href="#sec-29-1">Native TeX Mode</a></li>
<li><a href="#sec-29-2">AUCTeX</a></li>
<li><a href="#sec-29-3">Preview-LaTeX</a></li>
<li><a href="#sec-29-4">RefTeX</a></li>
<li><a href="#sec-29-5">BibTeX</a></li>
</ul>
</li>
<li><a href="#sec-30">SGML and HTML Modes</a>
<ul>
<li><a href="#sec-30-1">HTML</a></li>
<li><a href="#sec-30-2">XHTML</a></li>
<li><a href="#sec-30-3">web-mode</a></li>
<li><a href="#sec-30-4">XML</a></li>
<li><a href="#sec-30-5">Highlight the closing tag</a></li>
<li><a href="#sec-30-6">CSS</a></li>
<li><a href="#sec-30-7">Skewer: live web development with Emacs</a></li>
<li><a href="#sec-30-8">JS2-mode + JS2-refactor</a></li>
</ul>
</li>
<li><a href="#sec-31">Editing Programs</a>
<ul>
<li><a href="#sec-31-1">Major Modes for Programming Languages</a></li>
<li><a href="#sec-31-2">Top-Level Definitions, or Defuns</a></li>
<li><a href="#sec-31-3">Indentation for Programs</a></li>
<li><a href="#sec-31-4">Commands for Editing with Parentheses</a></li>
<li><a href="#sec-31-5">Manipulating Comments</a></li>
<li><a href="#sec-31-6">Documentation Lookup</a></li>
<li><a href="#sec-31-7">Hideshow minor mode</a></li>
<li><a href="#sec-31-8">Completion for Symbol Names</a></li>
<li><a href="#sec-31-9">Glasses minor mode</a></li>
<li><a href="#sec-31-10">C and related modes</a></li>
</ul>
</li>
<li><a href="#sec-32">Compiling and Testing Programs</a>
<ul>
<li><a href="#sec-32-1">Running Compilations under Emacs</a></li>
<li><a href="#sec-32-2">Compilation Mode</a></li>
<li><a href="#sec-32-3">Searching with Grep under Emacs</a></li>
<li><a href="#sec-32-4">Finding Syntax Errors On The Fly</a></li>
<li><a href="#sec-32-5">Running Debuggers Under Emacs</a></li>
<li><a href="#sec-32-6">Debugging Lisp programs</a></li>
<li><a href="#sec-32-7">Executing Lisp Expressions</a></li>
<li><a href="#sec-32-8">Libraries of Lisp Code for Emacs</a></li>
<li><a href="#sec-32-9">Evaluating Emacs Lisp Expressions</a></li>
<li><a href="#sec-32-10">Lisp Interaction Buffers (<code>*scratch*</code>)</a></li>
</ul>
</li>
<li><a href="#sec-33">Maintaining Programs</a>
<ul>
<li><a href="#sec-33-1">Version Control</a></li>
<li><a href="#sec-33-2">Change Logs</a></li>
<li><a href="#sec-33-3">Tags Tables (navigating code)</a></li>
<li><a href="#sec-33-4">Emacs Development Environment</a></li>
</ul>
</li>
<li><a href="#sec-34">Abbrevs</a>
<ul>
<li><a href="#sec-34-1">Controlling Abbrev Expansion</a></li>
<li><a href="#sec-34-2">Dynamic Abbrev Expansion</a></li>
</ul>
</li>
<li><a href="#sec-35">Dired, the Directory Editor</a>
<ul>
<li><a href="#sec-35-1">Entering Dired</a></li>
<li><a href="#sec-35-2">Emulation of <code>ls</code> on MS-Windows</a></li>
<li><a href="#sec-35-3">Navigation in the Dired Buffer</a></li>
<li><a href="#sec-35-4">Deleting Files with Dired</a></li>
<li><a href="#sec-35-5">Visiting Files in Dired</a></li>
<li><a href="#sec-35-6">Operating on Files</a></li>
<li><a href="#sec-35-7">Updating the Dired Buffer</a></li>
<li><a href="#sec-35-8">Dired and <code>find</code></a></li>
<li><a href="#sec-35-9">Editing the Dired Buffer</a></li>
<li><a href="#sec-35-10">Viewing Image Thumbnails in Dired</a></li>
<li><a href="#sec-35-11">Other Dired features</a></li>
<li><a href="#sec-35-12">Dired &ldquo;extra&rdquo; features</a></li>
<li><a href="#sec-35-13">Dired+</a></li>
<li><a href="#sec-35-14">VC diff highlighting</a></li>
</ul>
</li>
<li><a href="#sec-36">The Calendar and the Diary</a>
<ul>
<li><a href="#sec-36-1">Calendar Motion</a></li>
<li><a href="#sec-36-2">Scroll Calendar</a></li>
<li><a href="#sec-36-3">Times of Sunrise/Sunset</a></li>
<li><a href="#sec-36-4">Diary</a></li>
<li><a href="#sec-36-5">Appointments</a></li>
<li><a href="#sec-36-6">Advanced Calendar/Diary Usage</a></li>
<li><a href="#sec-36-7">Calendar framework</a></li>
</ul>
</li>
<li><a href="#sec-37">Sending Mail</a></li>
<li><a href="#sec-38">Gnus</a>
<ul>
<li><a href="#sec-38-1">BBDB</a></li>
</ul>
</li>
<li><a href="#sec-39">Document Viewing</a>
<ul>
<li><a href="#sec-39-1">Navigation</a></li>
<li><a href="#sec-39-2">Conversion</a></li>
</ul>
</li>
<li><a href="#sec-40">Web Browsing</a>
<ul>
<li><a href="#sec-40-1">EWW (Emacs Web Browser)</a></li>
</ul>
</li>
<li><a href="#sec-41">Running Shell Commands from Emacs</a>
<ul>
<li><a href="#sec-41-1">Single Shell</a></li>
<li><a href="#sec-41-2">Interactive Subshell</a></li>
<li><a href="#sec-41-3">Shell Mode</a></li>
<li><a href="#sec-41-4">Shell Prompts</a></li>
<li><a href="#sec-41-5">Shell Command History</a></li>
<li><a href="#sec-41-6">Directory Tracking</a></li>
<li><a href="#sec-41-7">Options</a></li>
<li><a href="#sec-41-8">Term Mode</a></li>
<li><a href="#sec-41-9">Remote Host</a></li>
<li><a href="#sec-41-10">Serial Terminal</a></li>
<li><a href="#sec-41-11">Helper for GNU Emacs on w32</a></li>
<li><a href="#sec-41-12">Helper for Cygwin Emacs</a></li>
<li><a href="#sec-41-13">Emacs Speaks Statistics (ESS)</a></li>
<li><a href="#sec-41-14">Proced</a></li>
</ul>
</li>
<li><a href="#sec-42">Using Emacs Server</a></li>
<li><a href="#sec-43">Printing Hard Copies</a></li>
<li><a href="#sec-44">Sorting Text</a></li>
<li><a href="#sec-45">Saving Emacs Sessions</a></li>
<li><a href="#sec-46">Hyperlinking and Navigation Features</a>
<ul>
<li><a href="#sec-46-1">Following URLs</a></li>
<li><a href="#sec-46-2">Finding Files and URLs at Point</a></li>
<li><a href="#sec-46-3">Google search</a></li>
<li><a href="#sec-46-4">How do I?</a></li>
<li><a href="#sec-46-5">Babel translator</a></li>
</ul>
</li>
<li><a href="#sec-47">Other Amusements</a></li>
<li><a href="#sec-48">Customization</a>
<ul>
<li><a href="#sec-48-1">Color</a></li>
<li><a href="#sec-48-2">Variables</a></li>
<li><a href="#sec-48-3">Key Bindings</a></li>
<li><a href="#sec-48-4">Syntax Table</a></li>
</ul>
</li>
<li><a href="#sec-49">Emacs Display</a>
<ul>
<li><a href="#sec-49-1">Faces</a></li>
<li><a href="#sec-49-2">Images</a></li>
</ul>
</li>
<li><a href="#sec-50">Emacs Lisp</a>
<ul>
<li><a href="#sec-50-1">Variables</a></li>
<li><a href="#sec-50-2">GNU Emacs Internals</a></li>
</ul>
</li>
<li><a href="#sec-51">Calc</a>
<ul>
<li><a href="#sec-51-1">Introduction</a></li>
<li><a href="#sec-51-2">Embedded Mode</a></li>
</ul>
</li>
<li><a href="#sec-52">IRC client for Emacs</a></li>
<li><a href="#sec-53">Emacs and Microsoft Windows/MS-DOS</a></li>
<li><a href="#sec-54">Profiler</a></li>
<li><a href="#sec-55">Reporting Bugs</a></li>
<li><a href="#sec-56">Leuven</a>
<ul>
<li><a href="#sec-56-1">Feature</a></li>
<li><a href="#sec-56-2">File Local Variables</a></li>
</ul>
</li>
</ul>
</div>
</div>



# Description<a id="sec-1" name="sec-1"></a>

Emacs can be very difficult for the beginner, as many of the default choices may
seem (and sometimes are) ugly or awkward.

But &#x2013; that&rsquo;s the joy of Emacs &#x2013; nearly everything can be adjusted!  Hence,
this set of customizations (from one pathological Emacs tweaker) to &ldquo;short-cut&rdquo;
that initial process.

Citations:

-   &ldquo;Show me your ~/.emacs and I will tell you who you are.&rdquo;   
      &#x2013; Bogdan Maryniuk

-   &ldquo;Emacs is like a laser guided missile.  It only has to be slightly
    mis-configured to ruin your whole day.&rdquo;   
      &#x2013; Sean McGrath

-   &ldquo;While any text editor can save your files, only Emacs can save your soul.&rdquo;   
      &#x2013; Per Abrahamsen

# Header<a id="sec-2" name="sec-2"></a>

    ;;; emacs-leuven.el --- Emacs configuration file with more pleasant defaults
    
    ;; Copyright (C) 1999-2017 Fabrice Niessen
    
    ;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
    ;; URL: https://github.com/fniessen/emacs-leuven
    ;; Version: 20160625.1211
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
    ;; line before requiring Leuven Emacs Config.
    ;;
    ;;     ;; show messages describing progress of loading Leuven Emacs Config
    ;;     (setq leuven-load-verbose t)
    ;;
    ;; To avoid be questioned about packages to add to your local Emacs
    ;; installation (though, I think you should install them), add the following
    ;; line before requiring Leuven Emacs Config.
    ;;
    ;;     ;; do not (try to) install extra Emacs packages
    ;;     (setq leuven-elpa-packages nil)
    ;;
    ;; For help on the Emacs Editor, see (info "(emacs)")  <== `C-x C-e' here!

# Code<a id="sec-3" name="sec-3"></a>

Some things are &ldquo;activated&rdquo; by default in `lisp/loaddefs.el`.

    ;;; Code:
    
    ;; This file is only provided as an example.  Customize it to your own taste!
    
    (defconst leuven--emacs-version "20160625.1211"
      "Leuven Emacs Config version (date of the last change).")
    
    (message "* --[ Loading Leuven Emacs Config %s]--" leuven--emacs-version)
    
    ;; Turn on Common Lisp support.
    (eval-when-compile (require 'cl))       ; Provide useful things like `setf'.
    
    (defconst leuven--before-time (float-time)
      "Value of `float-time' before loading the Leuven Emacs Config library.")

    (defmacro measure-time (message &rest body)
      "Measure the time it takes to evaluate BODY."
      `(let ((start (current-time)))
         ,@body
         (message "__%s (in %.02f s)___________________________"
                  ,message (float-time (time-since start)))))

    ;;; User Customizable Internal Variables
    
    (defgroup leuven nil
      "Set of Emacs customizations (better defaults)."
      :group 'convenience
      :group 'text)
    
    (defcustom leuven-load-verbose nil
      "If non-nil, means show messages describing progress of loading Leuven Emacs Config."
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

    ;; Allow quick include/exclude of setup parts -- DO NOT EDIT the DEFVAR!
    (defvar leuven-load-chapter-0-environment t) ; required
    (defvar leuven-load-chapter-0-loading-libraries t) ; required
    (defvar leuven-load-chapter-0-debugging t)
    (defvar leuven-load-chapter-48-packages t)
    (defvar leuven-load-chapter-1-screen t)
    (defvar leuven-load-chapter-6-exiting t)
    (defvar leuven-load-chapter-7-basic t)
    (defvar leuven-load-chapter-8-minibuffer t)
    (defvar leuven-load-chapter-10-help t)
    (defvar leuven-load-chapter-11-mark t)
    (defvar leuven-load-chapter-12-killing t)
    (defvar leuven-load-chapter-13-registers t)
    (defvar leuven-load-chapter-14-display t)
    (defvar leuven-load-chapter-15-search t)
    (defvar leuven-load-chapter-16-fixit t)
    (defvar leuven-load-chapter-17-keyboard-macros t)
    (defvar leuven-load-chapter-18-files t)
    (defvar leuven-load-chapter-19-buffers t)
    (defvar leuven-load-chapter-20-windows t)
    (defvar leuven-load-chapter-21-frames t)
    (defvar leuven-load-chapter-22-international t)
    (defvar leuven-load-chapter-23-major-and-minor-modes t)
    (defvar leuven-load-chapter-24-indentation t)
    (defvar leuven-load-chapter-25-text t)
    (defvar leuven-load-chapter-25.10-org-mode t)
    (defvar leuven-load-chapter-25.11-tex-mode t)
    (defvar leuven-load-chapter-26-programs t)
    (defvar leuven-load-chapter-27-building t)
    (defvar leuven-load-chapter-28-maintaining t)
    (defvar leuven-load-chapter-29-abbrevs t)
    (defvar leuven-load-chapter-30-dired t)
    (defvar leuven-load-chapter-31-calendar-diary t)
    (defvar leuven-load-chapter-32-sending-mail t)
    (defvar leuven-load-chapter-34-gnus t)
    (defvar leuven-load-chapter-36-document-view t)
    (defvar leuven-load-chapter-38-shell t)
    (defvar leuven-load-chapter-39-emacs-server t)
    (defvar leuven-load-chapter-40-printing t)
    (defvar leuven-load-chapter-41-sorting t)
    (defvar leuven-load-chapter-44-saving-emacs-sessions t)
    (defvar leuven-load-chapter-46-hyperlinking t)
    (defvar leuven-load-chapter-47-amusements t)
    (defvar leuven-load-chapter-49-customization t)
    (defvar leuven-load-chapter-AppG-ms-dos t)
    (defvar leuven-load-chapter-XX-emacs-display t)
    (defvar leuven-load-chapter-99-debugging t)

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
           (setq leuven--before-section-time (float-time)) ; Init section time.
           (progn ,@body)
           (leuven--section (concat "[" ,chaptername " ends here]") 'end-of-chapter)
                                            ; Add fake closing section.
           (setq this-chapter-time
                 (format "%.3f" (- (float-time) before-chapter-time)))
           (add-to-list 'leuven--load-times-list
                        (concat "| " ,chaptername " "
                                "| " this-chapter-time " |")))))
    
    (defvar leuven--before-section-time (float-time)
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
        ;; For next one.
        (setq leuven--before-section-time (float-time))))

# Loading Libraries of Lisp Code for Emacs<a id="sec-4" name="sec-4"></a>

Adding the right Lisp directories to your `load-path` (list of directories where
Emacs Lisp libraries &#x2013; `.el` and `.elc` files &#x2013; are installed) must be the very
**first thing** in your `.emacs` file, before the first time packages are required, to
make sure that you&rsquo;re not picking up bits and pieces from older files (bundled
with Emacs, and loaded before the path to the **newest** versions are set).

The most important directories are the last to be added to `load-path` (so that
they become the first of the list)!

Use `M-x list-load-path-shadows RET` to display a list of external Emacs Lisp
files that shadow Emacs builtins (listing potential load path problems).

Some Emacs modes are over 10K lines of code (e.g. `nxml-mode`, `CEDET`).  Many
packages (e.g. `Org`, `Gnus`) make use of the `autoload` feature, so that you only
need to load a single file that define autoloaded functions.

    ;;* Loading Libraries of Lisp Code for Emacs
    
    (leuven--chapter leuven-load-chapter-0-loading-libraries "0 Loading Libraries"

    ;; Load-path enhancement.
    (defun leuven--add-to-load-path (this-directory)
      "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
      (when (and this-directory
                 (file-directory-p this-directory))
        ;; TODO Add warning if directory does not exist.
        (let* ((this-directory (expand-file-name this-directory)))
    
          ;; directories containing a `.nosearch' file (such as
          ;; `auctex-11.88.6\style') should not made part of `load-path'.
          ;; TODO `RCS' and `CVS' directories should also be excluded.
          (unless (file-exists-p (concat this-directory "/.nosearch"))
            (add-to-list 'load-path this-directory)
            (when leuven-load-verbose
              (message "INFO- Added `%s' to `load-path'" this-directory))))))

    ;; Remember this directory.
    (defconst leuven--directory
      (file-name-directory (or load-file-name (buffer-file-name)))
      "Directory path of Leuven Emacs Config installation.")
    
    (leuven--add-to-load-path
     (concat leuven--directory "site-lisp"))

`leuven--local-repos-directory` is where you put Emacs Git/SVN/CSV repositories.

    (defvar leuven--local-repos-directory "~/Public/Repositories/"
      "Directory containing additional Emacs Lisp public repositories.")
    
    (leuven--add-to-load-path
     (concat leuven--local-repos-directory "babel"))
    (leuven--add-to-load-path
     (concat leuven--local-repos-directory "emacs-bookmark-extension") ; XXX?
     )

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Specify variables using \`defcustom&rsquo;</b><br  />
nil</div>

`leuven-user-lisp-directory` is there so that you have an easy way of installing
**your own Emacs add-ons** (which may be specific to the version of Emacs you&rsquo;re
running).  This keeps your local add-ons apart from distro supplied ones.

It also means you can do a complete re-install of Emacs (or even your Linux
distro) without impacting on stuff you have added by hand.

    (defvar leuven-user-lisp-directory (concat user-emacs-directory "lisp/")
      "Directory containing personal additional Emacs Lisp packages.")
    
    (leuven--add-to-load-path leuven-user-lisp-directory)

    ;; Require a feature/library if available; if not, fail silently.
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
              t)                          ; Necessary for correct behavior in
                                          ; conditional expressions.
          (file-error
           (message "Requiring `%s'... missing" feature)
           nil))))

    ;; TEMPORARY.
    (unless (fboundp 'with-eval-after-load)
      ;; Wrapper around `eval-after-load' (added in GNU Emacs 24.4).
      (defmacro with-eval-after-load (mode &rest body)
        "`eval-after-load' MODE evaluate BODY."
        (declare (indent defun))
        `(eval-after-load ,mode
           '(progn ,@body))))

## Creating and selecting buffers<a id="sec-4-1" name="sec-4-1"></a>

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

    )                                       ; Chapter 0-loading-libraries ends here.

# Environment<a id="sec-5" name="sec-5"></a>

    ;;* Environment
    
    (leuven--chapter leuven-load-chapter-0-environment "0 Environment"
    
    ;;** Type of OS
    
      (leuven--section "Type of OS")
    
      (defconst leuven--linux-p
        (eq system-type 'gnu/linux)
        "Running a GNU/Linux version of Emacs.")
    
      (defconst leuven--mac-p
        (eq system-type 'darwin)
        "Running a Mac OS version of Emacs.")
    
      (defconst leuven--win32-p
        (eq system-type 'windows-nt)
        "Running a native Microsoft Windows version of Emacs.")
    
      (defconst leuven--cygwin-p
        (eq system-type 'cygwin)
        "Running a Cygwin version of Emacs.")
    
    ;;** MS Windows
    
      (defconst leuven--windows-program-files-dir ; sys-path.
        (cond (leuven--win32-p
               (file-name-as-directory (getenv "ProgramFiles(x86)")))
              (leuven--cygwin-p
               "/cygdrive/c/Program Files (x86)/")
              (t
               "/usr/local/bin/"))
        "Default Windows Program Files folder.")

Use `(display-graphic-p)` (instead of `window-system`) in conditions.

    ;;** Window system
    
      (leuven--section "Window system")
    
      (defconst leuven--console-p
        (eq window-system nil)
        "Running a text-only terminal.")
    
      (defconst leuven--x-window-p
        (eq window-system 'x)
        "Running a X Window system.")
    
    ;;** Testing file accessibility
    
      (defun leuven--file-exists-and-executable-p (file)
        "Make sure the file FILE exists and is executable."
        (if file
            (if (file-executable-p file)
                file
              (message "WARN- Can't find executable `%s'" file)
              ;; Sleep 1.5 s so that you can see the warning.
              (sit-for 1.5))
          (error "Missing argument to \"leuven--file-exists-and-executable-p\"")))
    
    ;;** Init
    
      (leuven--section "Init")
    
      ;; Ensure that the echo area is always visible during the early stage of
      ;; startup (useful in case of error).
      (modify-all-frames-parameters
       '((height . 32)))
    
    )                                       ; Chapter 0 ends here.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Function for finding executables</b><br  />
See function `--find-program` at <http://www.visualco.de/src/elisp/.emacs.el.html>.
</div>

# Debugging<a id="sec-6" name="sec-6"></a>

    ;;* Debugging
    
    (leuven--chapter leuven-load-chapter-0-debugging "0 Debugging"

    ;; Get the backtrace when uncaught errors occur.
    (setq debug-on-error t)               ; Will be unset at the end.
    
    ;; Hit `C-g' while it's frozen to get an Emacs Lisp backtrace.
    (setq debug-on-quit t)                ; Will be unset at the end.

    )                                       ; Chapter 0 ends here.

# Emacs Lisp Packages<a id="sec-7" name="sec-7"></a>


Another **first thing** in your init script should be `(package-initialize)` so that
customization of Org variables (for example) doesn&rsquo;t auto-load the old version
that came with Emacs.

If you require something from Org mode, it must be after `package-initialize`.

## The Package Menu Buffer<a id="sec-7-1" name="sec-7-1"></a>

    ;;* 48 Emacs Lisp (info "(emacs)Packages")
    
    (leuven--chapter leuven-load-chapter-48-packages "48 Emacs Lisp Packages"

To upgrade packages after running `package-list-packages`, type:

-   `U` (*mark Upgradable packages*) and then
-   `x` (*eXecute the installs and deletions*).

When it&rsquo;s done installing all the packages, you delete the obsolete packages by
hitting `y` (*Yes*) when asked.

## Package Installation<a id="sec-7-2" name="sec-7-2"></a>

-   User&rsquo;s Emacs Lisp packages in `package-user-dir` (`~/.emacs.d/elpa`)
-   System-wide Emacs Lisp packages in `package-directory-list`

HTTP frontends for the Emacs repositories:
-   <http://elpa.gnu.org/packages/>
-   <http://melpa.org/packages/>

In Marmalade, the developers themselves do the packaging.  In MELPA, packages
are generated programmatically from upstream Git-based sources.

<div class="note">
The Marmalade repository is not recommended by Flycheck (due to frequent
outages).

</div>

To list packages which should *not* be installed by Emacs Leuven, add something
like this into your configuration file:

    (setq leuven-elpa-ignored-packages '(ess))

    ;;** 48.2 Package Installation
    
      (leuven--section "48.2 Package Installation")
    
      ;; Simple package system for GNU Emacs.
      (try-require 'package)
      (with-eval-after-load "package"
    
        ;; Archives from which to fetch.
        (setq package-archives
              (append '(("org"   . "http://orgmode.org/elpa/")
                        ("melpa" . "http://melpa.org/packages/"))
                      package-archives))
    
        ;; Load the latest version of all installed packages, and activate them.
        (package-initialize)                ; Add ALL ELPA subdirs to `load-path'
                                            ; and load `<pkg>-autoloads.el'.
    
        (defconst leuven-elpa-packages '(ace-jump-helm-line
                                         ace-jump-mode
                                         ace-link
                                         ace-window
                                         ;; aggressive-indent
                                         ant
                                         anzu
                                         auctex
                                         auto-complete
                                         bbdb
                                         bookmark+
                                         boxquote
                                         ;; calfw
                                         circe
                                         color-identifiers-mode
                                         company
                                         company-tern
                                         company-quickhelp
                                         csv-mode
                                         cygwin-mount
                                         dictionary
                                         diff-hl
                                         diminish
                                         dired+
                                         ;; emacs-eclim
                                         ess
                                         expand-region
                                         fancy-narrow
                                         fill-column-indicator
                                         flycheck
                                         flycheck-color-mode-line
                                         flycheck-ledger
                                         fuzzy
                                         git-commit
                                         git-messenger
                                         git-timemachine
                                         google-this
                                         google-translate
                                         goto-chg
                                         graphviz-dot-mode
                                         helm
                                         helm-ag
                                         helm-descbinds
                                         helm-ls-git
                                         helm-swoop
                                         hideshowvis
                                         highlight-symbol
                                         howdoi
                                         htmlize
                                         indent-guide
                                         ;; jabber
                                         js2-mode
                                         key-chord
                                         litable
                                         idle-require
                                         info+
                                         interaction-log
                                         ledger-mode
                                         leuven-theme
                                         ;; magit
                                         markdown-mode
                                         multi-term
                                         multiple-cursors
                                         ;; multi-term
                                         pager
                                         ;; paredit
                                         pdf-tools
                                         powerline
                                         rainbow-delimiters
                                         rainbow-mode
                                         ;; redshank
                                         tern
                                         tidy
                                         unbound
                                         undo-tree
                                         web-mode
                                         which-key
                                         ws-butler
                                         yasnippet)
          "A list of packages to ensure are installed at Emacs startup.")
    
        (defcustom leuven-elpa-ignored-packages
          nil
          "List of packages that should be ignored by Leuven Emacs Config."
          :group 'emacs-leuven
          :type '(repeat (string)))
    
        (defun leuven--missing-elpa-packages ()
          "List packages to install for a full blown Leuven installation.
    These packages are neither built-in nor already installed nor ignored."
          (let (missing-elpa-packages)
            (dolist (pkg leuven-elpa-packages)
              (unless (or (package-installed-p pkg)
                          (locate-library (symbol-name pkg))
                          (member pkg leuven-elpa-ignored-packages))
                (push pkg missing-elpa-packages)))
            missing-elpa-packages))
    
        ;; Propose to install all the packages specified in `leuven-elpa-packages'.
        ;; which are missing and which shouldn't be ignored.
        (let ((missing-elpa-packages (leuven--missing-elpa-packages)))
          (when missing-elpa-packages
            ;; Download once the ELPA archive description.
            (package-refresh-contents)      ; Ensure that the list of packages is
                                            ; up-to-date.  Otherwise, new packages
                                            ; (not present in the cache of the ELPA
                                            ; contents) won't install.
            (dolist (pkg (reverse missing-elpa-packages))
              (if (yes-or-no-p (format "Install ELPA package `%s'? " pkg))
                  (ignore-errors
                    (package-install pkg))
                                            ; Must be run after initializing
                                            ; `package-initialize'.
                (message (concat "Customize Emacs Leuven to ignore "
                                 "the `%s' package next times...") pkg)
                (sit-for 1.5))))))

The following code will be used when Emacs 25 will be made mandatory:

    (setq package-selected-packages
          '(auctex ess
            company
            js2-mode
            magit
            pdf-tools
            paredit
            visual-regexp))
    
    (package-initialize)
    (when (fboundp 'package-install-selected-packages) ; Emacs-v25
      (package-install-selected-packages))

    )                                       ; Chapter 48 ends here.

`Idle-require` must be loaded **after** `package-initialize`, and should not be
cancelable; hence, we put it out of a &ldquo;chapter&rdquo;.

    ;; Load elisp libraries while Emacs is idle.
    (try-require 'idle-require)
    
    ;; Fail-safe for `idle-require'.
    (if (not (featurep 'idle-require))
      (defun idle-require (feature &optional file noerror)
        (try-require feature)))
    
    (with-eval-after-load "idle-require"
    
      ;; Idle time in seconds after which autoload functions will be loaded.
      (setq idle-require-idle-delay 5)
    
      ;; Time in seconds between automatically loaded functions.
      (setq idle-require-load-break 2)
    
      ;; Starts loading.
      (add-hook 'after-init-hook #'idle-require-mode))

# The Organization of the Screen<a id="sec-8" name="sec-8"></a>

    ;;* 1 The Organization of the (info "(emacs)Screen")
    
    (leuven--chapter leuven-load-chapter-1-screen "1 The Organization of the Screen"
    
    ;;** 1.2 The (info "(emacs)Echo Area")
    
      (leuven--section "1.2 (emacs) The Echo Area")
    
      ;; Don't truncate the message log buffer when it becomes large.
      (setq message-log-max t)
    
    )                                       ; Chapter 1 ends here.

# Exiting Emacs<a id="sec-9" name="sec-9"></a>

    ;;* 6 (info "(emacs)Exiting") Emacs
    
    (leuven--chapter leuven-load-chapter-6-exiting "6 Exiting Emacs"
    
      ;; Unbind "minimize".
      (global-unset-key (kbd "C-z"))
    
      ;; Quit with Alt + F4.
      (global-set-key (kbd "<M-f4>") #'save-buffers-kill-terminal)
    
    )                                       ; Chapter 6 ends here.

# Basic Editing Commands<a id="sec-10" name="sec-10"></a>

    ;;* 7 (info "(emacs)Basic") Editing Commands
    
    (leuven--chapter leuven-load-chapter-7-basic "7 Basic Editing Commands"

## Inserting Text<a id="sec-10-1" name="sec-10-1"></a>

    ;;** 7.1 (info "(emacs)Inserting Text")
    
      (leuven--section "7.1 (emacs)Inserting Text")
    
      ;; Enter characters by their code in octal (for `C-q NNN RET').
      (setq read-quoted-char-radix 8)       ; 16 for hexadecimal (for Unicode char)

## Moving Point Location<a id="sec-10-2" name="sec-10-2"></a>

-   **`M-r`:** `move-to-window-line`.

-   **`M-g g`:** **Jump to** the beginning of **line** number *N* (`goto-line`).

    ;;** 7.2 (info "(emacs)Moving Point") Location
    
      (leuven--section "7.2 (emacs)Moving Point Location")
    
      ;; Don't add newlines to end of buffer when scrolling.
      (setq next-line-add-newlines nil)
    
      ;; Print the current buffer line number.
      (global-set-key (kbd "M-G") #'what-line)

Show line numbers temporarily &#x2013; just when you&rsquo;re going to a line number with
`goto-line`.

    (defun goto-line-with-feedback ()
      "Show line numbers temporarily, while prompting for the line number input"
      (interactive)
      (unwind-protect
          (progn
            (linum-mode 1)
            (goto-line (read-number "Goto line: ")))
        (linum-mode -1)))
    
    (global-set-key [remap goto-line] #'goto-line-with-feedback)

## Basic Undoing Changes<a id="sec-10-3" name="sec-10-3"></a>

    ;;** 7.4 (info "(emacs)Basic Undo")ing Changes
    
      (leuven--section "7.4 (emacs)Basic Undoing Changes")
    
      ;; Undo some previous changes.
      (global-set-key (kbd "C-z") #'undo)
      (global-set-key (kbd "<f11>") #'undo)

    (with-eval-after-load "volatile-highlights-autoloads"
      (volatile-highlights-mode 1))

    ;; Treat undo history as a tree.
    (with-eval-after-load "undo-tree-autoloads"
    
      ;; Enable Global-Undo-Tree mode.
      (global-undo-tree-mode 1))
    
    (with-eval-after-load "undo-tree"
    
      (with-eval-after-load "diminish-autoloads"
        (diminish 'undo-tree-mode))
    
      ;; Display times relative to current time in visualizer.
      (setq undo-tree-visualizer-relative-timestamps t)
    
      ;; Display time-stamps by default in undo-tree visualizer.
      (setq undo-tree-visualizer-timestamps t)
                                          ; Toggle time-stamps display using `t'.
    
      ;; Display diff by default in undo-tree visualizer.
      (setq undo-tree-visualizer-diff t)  ; Toggle the diff display using `d'.
    
      (define-key undo-tree-map (kbd "C-/") nil)
    
      ;; (defalias 'redo 'undo-tree-redo)
      (global-set-key (kbd "C-S-z") #'undo-tree-redo)
      (global-set-key (kbd "<S-f11>") #'undo-tree-redo))

    )                                       ; Chapter 7 ends here.

# The Minibuffer<a id="sec-11" name="sec-11"></a>

    ;;* 8 The (info "(emacs)Minibuffer")
    
    (leuven--chapter leuven-load-chapter-8-minibuffer "8 The Minibuffer"
    
      ;; How long to display an echo-area message when the minibuffer is active.
      (setq minibuffer-message-timeout 0.5)

## Editing in the Minibuffer<a id="sec-11-1" name="sec-11-1"></a>

If the output is short enough to display in the echo area (which is determined
by the variables `resize-mini-windows` and `max-mini-window-height`), it is shown in
echo area.

    ;;** 8.3 (info "(emacs)Minibuffer Edit")ing
    
      (leuven--section "8.3 (emacs)Minibuffer Editing")
    
      ;; Minibuffer and echo area windows resize vertically as necessary to fit
      ;; the text displayed in them.
      (setq resize-mini-windows t)

## Completion<a id="sec-11-2" name="sec-11-2"></a>

    ;;** 8.4 (info "(emacs)Completion")
    
      (leuven--section "8.4 (emacs)Completion")
    
      ;; Don't consider case significant in completion.
      (setq completion-ignore-case t)

Within `read-file-name`, this variable is overridden by
`read-file-name-completion-ignore-case`.

    ;; Ignore case when reading a file name.
    (setq read-file-name-completion-ignore-case t) ; [Default: t on Windows]

Within `read-buffer`, `completion-ignore-case` is overridden by
`read-buffer-completion-ignore-case`.

    ;; Ignore case when reading a buffer name.
    (setq read-buffer-completion-ignore-case t) ; [Default: nil].

    ;; Provide the same facility of `ls --color' inside Emacs.
    (when (locate-library "dircolors")
      (autoload 'dircolors "dircolors" nil t)
      (add-hook 'completion-list-mode-hook #'dircolors))

## Repeating Minibuffer Commands<a id="sec-11-3" name="sec-11-3"></a>

Use `M-x list-command-history` to display the entire command history, showing all
the commands `C-x <ESC> <ESC>` can repeat, most recent first.

    )                                       ; Chapter 8 ends here.

# Help<a id="sec-12" name="sec-12"></a>

In general, the following functions are very interesting for finding useful
functions and documentation.

-   **`apropos-documentation` (`C-h d`):** **Show symbols whose documentation contains matches for PATTERN.**
         For example, `C-h d yank properties RET`.

-   **`describe-function` (`C-h f`):** Used with TAB completion to show documentation of functions.

-   **`elisp-index-search` (`C-h E` in Leuven Emacs Config):** Look up documentation on broad Emacs Lisp topics (in the indices of the
    Emacs Lisp Reference Manual).

-   **`info-lookup-symbol` (`C-h S`, or `C-f1` in Leuven Emacs Config):** **Bring the relevant section from the manual, which describes the topic in
    more detail.**

With all prefix keys, if you follow them with `C-h` (`h` for help), a list of key
bindings in that prefix map is displayed.  This is automatic and is one of the
self-documenting features of Emacs.

Try, for example `C-x C-h` or `C-c C-x C-h` to see what it does.  I do use it to
remind myself of rarely used rectangle commands: `C-x r C-h`.

FYI, `catman` create the database files that are used by `apropos` or `man -k`.

Note about PHP documentation lookup &#x2013; PHP Mode has a nice feature to lookup the
function&rsquo;s definition in PHP&rsquo;s manual in your web browser (`M-x
php-search-documentation` or `C-c C-f`).  You can combine it with EWW to get the
relevant manual page without leaving Emacs.

**Close** the help buffer by pressing `Q`.

    ;;* 10 (info "(emacs)Help")
    
    (leuven--chapter leuven-load-chapter-10-help "10 Help"

## Help Summary<a id="sec-12-1" name="sec-12-1"></a>

    ;;** 10.1 (info "(emacs)Help Summary")
    
      (leuven--section "10.1 (emacs)Help Summary")
    
      ;; Avoid the description of all minor modes.
      (defun leuven-describe-major-mode ()
        "Describe only `major-mode'."
        (interactive)
        (describe-function major-mode))
    
      ;; Look up subject in (the indices of the) Emacs Lisp manual.
      (global-set-key (kbd "C-h E") #'elisp-index-search)

## Apropos<a id="sec-12-2" name="sec-12-2"></a>

You can ask what pertains to a given topic by typing `M-x apropos RET pattern
RET`.

    ;;** 10.4 (info "(emacs)Apropos")
    
      (leuven--section "10.4 (emacs)Apropos")
    
      (with-eval-after-load "apropos"
    
        ;; Apropos commands will search more extensively, checking all variables and
        ;; non-interactive functions as well.
        (setq apropos-do-all t))
    
      ;; (defun apropos-user-option (string)
      ;;   "Like apropos, but lists only symbols that are names of user
      ;; modifiable variables.  Argument REGEXP is a regular expression.
      ;;    Returns a list of symbols, and documentation found"
      ;;   (interactive "sVariable apropos (regexp): ")
      ;;   (let ((message
      ;;          (let ((standard-output (get-buffer-create "*Help*")))
      ;;            (print-help-return-message 'identity))))
      ;;     (if (apropos string  'user-variable-p)
      ;;         (and message (message message)))))
    
      ;; Show all variables whose name matches the pattern.
      (define-key help-map (kbd "A") #'apropos-user-option)

## Misc Help Commands<a id="sec-12-3" name="sec-12-3"></a>

`update-info-dir`

The utility `install-info` is used to maintain the `info/dir` file.

`C-h K` goes to the node in the Emacs manual describing the command bound to
a key.

In a manual, these key bindings will make your life easier:

-   **`^`:** Move &ldquo;up&rdquo; from this node.

-   **`]`:** Go forward one node, considering all nodes as forming one sequence.

-   **`l`:** Move back in history to the last node you were at.

-   **`L`:** Go to menu of visited nodes.

-   **`i`:** **Search for a topic in this manual&rsquo;s Index** and go to index entry.

-   **`s`:** **Search for a regexp** and select node it&rsquo;s found in.

    ;;** 10.8 (info "(emacs)Misc Help")
    
      (leuven--section "10.8 (emacs)Misc Help")
    
      ;; Enter Info documentation browser.
      (global-set-key (kbd "<f1>") #'info)
    
      (defun leuven-describe-elisp-symbol-at-point ()
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
    
      (global-set-key (kbd "<f1>") #'leuven-describe-elisp-symbol-at-point)
    
      ;; Display symbol definitions, as found in the relevant manual
      ;; (for AWK, C, Emacs Lisp, LaTeX, M4, Makefile, Sh and other languages that
      ;; have documentation in Info).
      (global-set-key (kbd "<C-f1>") #'info-lookup-symbol)

When Info is called, the variable `Info-directory-list` is populated from:

1.  the `INFOPATH` environment variable, or *(if unset)*

2.  the `Info-default-directory-list` variable &#x2013; non-existent directories will be
    removed when copied to `Info-directory-list`

The best would be to set the `INFOPATH` environment variable so that you see the
same manuals outside of Emacs, in the same shell from which you invoke Emacs.

However, as Windows Emacs doesn&rsquo;t see that Cygwin environment variable (it would
well work with a Windows environment variable), I prefer to try and put the
Windows configuration inside the Emacs configuration files.

Normally, `Info-directory-list` is not intended to be settable by the user. But we
must do so if we want to force our Info manuals before the standard ones (from
Emacs). XXX Could we set `Info-default-directory-list` instead???

    (with-eval-after-load "info"
      ;; List of directories to search for Info documentation files (in the order
      ;; they are listed).
      (when leuven--win32-p
        ;; (info-initialize)
        (setq Info-directory-list
              `(,(expand-file-name
                  (concat (file-name-directory (locate-library "org")) "../doc/"))
                "c:/cygwin/usr/share/info/"
                ,@Info-directory-list)))
    
      ;; XXX Replace by add-to-list to ensure we don't insert duplicates (if Cygwin was already there).

`Info+` fontifies entries for reference items (functions, macros, commands,
special forms, constants, options, other variables), and that includes their
parameters, even those on continuation lines.

With `Info+`, you can also merge an Info node with its subnodes into the same
buffer, by calling `Info-merge-subnodes` (bound to `+`).

    (with-eval-after-load "info+-autoloads"
      (idle-require 'info+))
    
    (with-eval-after-load "info+"
    
      ;; Show breadcrumbs in the header line.
      (setq Info-breadcrumbs-in-header-flag t)
    
      ;; Don't show breadcrumbs in the mode line.
      (setq Info-breadcrumbs-in-mode-line-mode nil))

In an Info page, `w` will copy the reference to the current node (the `(%s)%s` part
of your format string).

When called with a 0 argument (`M-0 w`), you get `(info \"(%s)%s\")`.

    )

    ;; Get a Unix manual page of the item under point.
    (global-set-key (kbd "<S-f1>") #'man-follow)
    
    (with-eval-after-load "man"
      ;; Make the manpage the current buffer in the current window.
      (setq Man-notify-method 'pushy))
    
    ;; Alias man to woman.
    (defalias 'man 'woman)
    
    ;; Decode and browse Unix man-pages "W.o. (without) Man".
    (with-eval-after-load "woman"
      (defalias 'man 'woman))

    )                                       ; Chapter 10 ends here.

# The Mark and the Region<a id="sec-13" name="sec-13"></a>

    ;;* 11 The (info "(emacs)Mark") and the Region
    
    (leuven--chapter leuven-load-chapter-11-mark "11 The Mark and the Region"

## Setting the Mark<a id="sec-13-1" name="sec-13-1"></a>

Instead of setting the **mark** in order to operate on a region, you can also type:

-   **`C-SPC C-SPC`:** **&ldquo;Remember&rdquo; a position** in the buffer (set the mark where point is), and

-   **`C-u C-SPC`:** Later **jump** back **to the mark** in the buffer.  You can use this repeatedly to
    navigate through the entire mark ring.

<div class="tip">
Some commands (especially ones which are liable to move you an an unknown or
arbitrary distance from your original location) will automatically push to the
mark ring so that you can use `C-uC-SPC` to return afterwards. This includes
`isearch`, so after using `C-s` to go somewhere, you can easily jump back again.

</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Look at \`back-button&rsquo;</b><br  />
Visual navigation through mark rings.
</div>

**Go to** the **place** where you **last changed** something:

    ;; Goto last change.
    (with-eval-after-load "goto-chg-autoloads"
      (global-set-key (kbd "<C-S-backspace>") #'goto-last-change))

It moves point through `buffer-undo-list` positions.

## Commands to Mark Textual Objects<a id="sec-13-2" name="sec-13-2"></a>

-   **`M-@`:** Set mark after end of next **word** (`mark-word`) without moving point.

-   **`C-M-@`:** Set mark after end of following **balanced expression** (`mark-sexp`) without
    moving point.

    ;; Increase selected region by semantic units.
    (with-eval-after-load "expand-region-autoloads"
    
      (global-set-key (kbd "C-+") #'er/expand-region)
      (global-set-key (kbd "C--") #'er/contract-region))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Those key bindings are overridden by `text-scale-increase`</b><br  />
nil</div>

## Operating on the Region<a id="sec-13-3" name="sec-13-3"></a>

    ;; Inserting text while the mark is active causes the text in the region to be
    ;; deleted first.
    (delete-selection-mode 1)             ; Overwrite region.

## Shift Selection<a id="sec-13-4" name="sec-13-4"></a>

Shifted motion keys activate the mark momentarily.

## Multiple selections<a id="sec-13-5" name="sec-13-5"></a>

You can [watch an intro to multiple-cursors at Emacs Rocks](http://www.emacsrocks.com/e13.html).

In other editors:

-   <https://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html>
-   <https://docs.c9.io/multiple_cursors.html>
-   <http://komodoide.com/screencasts/watch/87286656-multiple-selections/>

    ;; Multiple cursors for Emacs.
    (with-eval-after-load "multiple-cursors-autoloads"

### Splitting the selection into lines<a id="sec-13-5-1" name="sec-13-5-1"></a>

Make **batch edits** with Multiple Cursors: select a block of lines, and then **split
the region into lines** which are then **edited simultaneously**.

    ;; Add a cursor to each (continuous) line in the current region.
    (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines) ;!

### Quick add next/previous<a id="sec-13-5-2" name="sec-13-5-2"></a>

Use Multiple Cursors to **rename variables quickly**: **add the next** or previous
**occurrence of the current word to the selection**.

    ;; Add a cursor and region at the next part of the buffer forwards that
    ;; matches the current region.
    (global-set-key (kbd "C->") #'mc/mark-next-like-this) ;!
    
    ;; Add a cursor and region at the next part of the buffer backwards that
    ;; matches the current region.
    (global-set-key (kbd "C-<") #'mc/mark-previous-like-this) ;!
    
    (global-set-key (kbd "C-M->") #'mc/skip-to-next-like-this)
    (global-set-key (kbd "C-M-<") #'mc/skip-to-previous-like-this)
    
    (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)

### Find All<a id="sec-13-5-3" name="sec-13-5-3"></a>

**Add all occurrences of the current word to the selection.**

    (global-set-key (kbd "C-;") #'mc/mark-all-like-this-dwim) ;! Like Iedit.
    ;; (global-set-key (kbd "C-c C-w") #'mc/mark-all-like-this-dwim)
    ;; (global-set-key (kbd "C-x C-;") #'mc/mark-all-like-this-dwim)
    
    ;; Mark all parts of the buffer that matches the current region.
    (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this) ;!

    ;; (global-set-key (kbd "<C-RET>") #'mc/mark-more-like-this-extended) ; useful for completion

      (global-set-key (kbd "C-S-SPC") #'set-rectangular-region-anchor)
      (global-set-key (kbd "C-M-=") #'mc/insert-numbers)
    )

### Single Selection<a id="sec-13-5-4" name="sec-13-5-4"></a>

To go from multiple selections to a single selection, press `C-g` or `RET`.

    ;; Multiple cursors for Emacs.
    (with-eval-after-load "multiple-cursors-core"
    
      ;; Commands to run for all cursors in multiple-cursors-mode.
      (setq mc/cmds-to-run-for-all
            '(cycle-spacing
              isearch-abort
              isearch-printing-char
              just-one-space
              kill-region
              leuven-fill-paragraph
              leuven-smart-punctuation-quotation-mark
              org-beginning-of-line
              org-end-of-line
              org-kill-line
              org-self-insert-command
              org-shiftdown
              org-shiftleft
              org-shiftright
              org-shiftup
              org-yank
              orgtbl-self-insert-command
              yas-expand)))

    )                                       ; Chapter 11 ends here.

# Killing and Moving Text<a id="sec-14" name="sec-14"></a>

    ;;* 12 (info "(emacs)Killing") and Moving Text
    
    (leuven--chapter leuven-load-chapter-12-killing "12 Killing and Moving Text"

## Deletion and Killing<a id="sec-14-1" name="sec-14-1"></a>

-   **`C-S-backspace`:** Kill an entire line at once.

    ;;** 12.1 (info "(emacs)Deletion and Killing")
    
      (leuven--section "12.1 (emacs)Deletion and Killing")
    
      ;; Manipulate whitespace around point in a smart way.
      (global-set-key (kbd "M-SPC") #'cycle-spacing) ; vs `just-one-space'.

    ;; old ([2012-09-07 Fri] remove "compile" after "activate")
    
      ;; Add the ability to copy the current line without marking it (no
      ;; selection).
      (defadvice kill-ring-save (before leuven-slick-copy activate)
        "When called with no active region, copy the current line instead."
        (interactive
         (if (use-region-p) (list (region-beginning) (region-end))
           (message "Copied the current line")
           (list (line-beginning-position)
                 (line-beginning-position 2)))))
    
      ;; Add the ability to cut the current line without marking it (no
      ;; selection).
      (defadvice kill-region (before leuven-slick-cut activate)
        "When called with no active region, kill the current line instead."
        (interactive
         (if (use-region-p) (list (region-beginning) (region-end))
           (list (line-beginning-position)
                 (line-beginning-position 2)))))
    
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

    (defun duplicate-current-line ()
      "Duplicate the line containing point."
      (interactive)
      (save-excursion
        (let (line-text)
          (goto-char (line-beginning-position))
          (let ((beg (point)))
            (goto-char (line-end-position))
            (setq line-text (buffer-substring beg (point))))
          (if (eobp)
              (insert ?\n)
            (forward-line))
          (open-line 1)
          (insert line-text))))
    
    (global-set-key (kbd "C-c d") #'duplicate-current-line)

## Yanking<a id="sec-14-2" name="sec-14-2"></a>

    ;;** 12.2 (info "(emacs)Yanking")
    
      (leuven--section "12.2 (emacs)Yanking")
    
      ;; Auto-indentation of pasted code in the programming modes (fall back to
      ;; default, non-indented yanking by preceding the yanking command `C-y' with
      ;; `C-u').
      (dolist (command '(yank
                         yank-pop))
        (eval `(defadvice ,command (after leuven-indent-region activate)
                 "Indent `yank'ed text if programming mode (and no prefix)."
                 (let ((mark-even-if-inactive t))
                   (and (not current-prefix-arg)
                        (derived-mode-p 'prog-mode)
                        (indent-region (region-beginning) (region-end) nil))))))

    ;; Save clipboard strings into kill ring before replacing them.
    (setq save-interprogram-paste-before-kill t)
    
    ;; ;; Rotating the kill ring changes the window system selection.
    ;; (setq yank-pop-change-selection t)

### Yanking Earlier Kills<a id="sec-14-2-1" name="sec-14-2-1"></a>

Interactively insert items from kill ring with `M-x helm-show-kill-ring` (see
Helm (See section 20.12.1)).

## Cutting and Pasting on Graphical Displays<a id="sec-14-3" name="sec-14-3"></a>

    ;;** 12.3 (info "(emacs)Cut and Paste")
    
      (leuven--section "12.3 (emacs)Cut and Paste on Graphical Displays")
    
      ;; Make cut, copy and paste (keys and menu bar items) use the clipboard.
      (menu-bar-enable-clipboard)

## Rectangles<a id="sec-14-4" name="sec-14-4"></a>

To kill the text of a rectangular area (vertically selected text), use `C-x r k`
(`kill-rectangle`).  Or just &ldquo;delete&rdquo; the &ldquo;region-rectangle&rdquo; (without &ldquo;killing&rdquo;
it) with `C-x r d`.

To copy a (series of) Org column(s) while avoiding the use of registers:
1.  select the region-rectangle,
2.  use the command `copy-rectangle-as-kill` (bound to `C-x r M-w`), then
3.  paste the copied rectangle by doing `yank-rectangle` (`C-x r y`).

To do the same with registers:
1.  select the region-rectangle,
2.  use `C-x r r R` to copy the rectangle to the register named `R`,
3.  use `C-x r i R` to insert the rectangle that is being held in the register
    named `R`.

To shift cells up/down within a column of an Org table while leaving remaining
columns intact, use `kill-rectangle` and `yank-rectangle`.

To delete whitespace in each of the lines on the specified rectangle, use
`M-x delete-whitespace-rectangle`.

Use `C-x r t STRING RET` to replace each line of a region-rectangle with a given
string.

<div class="note">
Since Emacs 24.4, you can use `rectangle-mark-mode` (`C-x SPC`) and do:
-   `C-x SPC` <&#x2026;move around&#x2026;> `DEL` to delete the rectangle and
-   `C-x SPC` <&#x2026;move around&#x2026;> `C-t` to invoke `string-rectangle`.

</div>

## CUA Bindings<a id="sec-14-5" name="sec-14-5"></a>

CUA mode sets up key bindings used in many other applications (`C-x`, `C-c`, `C-v` and
`C-z`).

The `C-x` and `C-c` keys only do cut and copy when the region is active, so in most
cases, they do not conflict with the normal function of these prefix keys.

If you really need to perform a command which starts with one of the prefix keys
even when the region is active, you have three options:

-   press the prefix key twice very quickly (within 0.2 seconds),
-   press the prefix key and the following key within 0.2 seconds, or
-   use the Shift key with the prefix key, i.e. `C-S-x` or `C-S-c`.

You can customize `cua-enable-cua-keys` to completely disable the CUA bindings, or
`cua-prefix-override-inhibit-delay` to change the prefix fallback behavior.

CUA mode also provides enhanced rectangle support with visible rectangle
highlighting.  Though, since Emacs 24.4, `rectangle-mark-mode` is the new way.

-   `<C-RET>` runs the command `cua-set-rectangle-mark`
-   `M-n` runs the command `cua-sequence-rectangle`

    )                                       ; Chapter 12 ends here.

# Registers<a id="sec-15" name="sec-15"></a>

    ;;* 13 (info "(emacs)Registers")
    
    (leuven--chapter leuven-load-chapter-13-registers "13 Registers"

## Saving Positions in Registers<a id="sec-15-1" name="sec-15-1"></a>

    ;;** 13.1 (info "(emacs)Position Registers")
    
      (leuven--section "13.1 (emacs)Position Registers")

-   **`C-x r SPC`:** `point-to-register`. Type any character to specify a register when prompted.

-   **`C-x r j`:** `jump-to-register`.

    (global-set-key (kbd "C-j") #'jump-to-register) ; Also on `C-x r j'.

Setup registers for files I commonly edit.

    (set-register ?a '(file . "/sudo::/etc/apt/sources.list"))
    (set-register ?b '(file . "~/.bashrc"))
    (set-register ?e `(file . ,(concat leuven--directory "emacs-leuven.txt")))
    (when (file-exists-p "~/org/personal/Personal.org")
      (set-register ?p '(file . "~/org/personal/Personal.org")))
    (when (file-exists-p "~/org/refile.org")
      (set-register ?r '(file . "~/org/refile.org")))
    (when (file-exists-p "~/org/work/Work.org")
      (set-register ?w '(file . "~/org/work/Work.org")))
    (set-register ?z '(file . "~/.zshrc"))

## Bookmarks<a id="sec-15-2" name="sec-15-2"></a>

If you need to move to somewhere in your document and want a quick way to
return, you can place a bookmark on the current line.

This is done by pressing `C-x r m RET` or `C-x r m BOOKMARK RET`.

<div class="note">
Bookmarks are **persistent** across sessions and they have **names**; not *markers*.
Bookmarked positions can also be **relocated** (found) if they move slightly because
of text changes.

</div>

When you want to return to a bookmark, you can press `C-x r b`.  You&rsquo;ll be
prompted for the bookmark name, and it will open that file or directory.

    ;;** 13.7 (info "(emacs)Bookmarks")
    
      (leuven--section "13.7 (emacs)Bookmarks")
    
      (with-eval-after-load "bookmark"
    
        ;; Where to save the bookmarks.
        (setq bookmark-default-file (concat user-emacs-directory "bookmarks.bmk"))
                                            ;! A `.txt' extension would load Org at
                                            ;! the time `bookmark' is required!
    
        ;; Each command that sets a bookmark will also save your bookmarks.
        (setq bookmark-save-flag 1)

**Visible** bookmarks in buffer.

**Bookmark+** offers everything that `bm.el` offers, and quite a bit more.

-   You can bookmark arbitrary sets of files, from any locations.

-   You can bookmark Dired buffers (which, again, can actually list arbitrary
    files, not even necessarily in the same directory).  Markings, subdir
    inclusions, and omissions are all recorded, and restored when you access the
    bookmark.

-   You can tag bookmarks or files, using arbitrary strings as tags.  You can do
    this programmatically and interactively, by regexp, name, or Dired markings.
    Tags give you a great way to define sets of bookmarks or files &#x2013; sets that
    can overlap, etc.  They serve to categorize, but they can do more than that.
    You can use tags to, in effect, merge projects, split projects, define
    subprojects, and so on.

Key bindings:

-   **`C-x p RET` (`C-F2` in Emacs Leuven, Sublime Text and TextMate):** Toggle (**set** or delete) a **bookmark** at point.

-   **`C-x p C-down` (`S-F2` in Emacs Leuven, `F2` in Sublime Text and TextMate):** Cycle to the **next** highlighted **bookmark** in the current buffer.

-   **`C-x p C-up` (`S-F2` in Sublime Text and TextMate):** Cycle to the **previous** highlighted **bookmark** in the current buffer.

-   **`C-x (4) j h`:** Jump to a highlighted bookmark (in another window).

-   **C-x p ,:** Show the bookmark list just for bookmarks for the current file/buffer.

-   **`C-S-f2` (in Emacs Leuven):** **Clear** all **bookmarks** for the current file/buffer.

Buffer-local nature of the bookmarks.

Annotating bookmarks.

<div class="warning">
We bookmarks positions, not just lines!!

</div>

(setq bm-marker &lsquo;bm-marker-right
      bm-repository-file (concat emacs-persistence-directory &ldquo;bm-repository&rdquo;)
      bm-recenter t
      bm-highlight-style &lsquo;bm-highlight-only-fringe)

(global-set-key (kbd &ldquo;C-c m m&rdquo;) &lsquo;bm-toggle)
(global-set-key (kbd &ldquo;<right-fringe> <mouse-5>&rdquo;) &lsquo;bm-next-mouse)
(global-set-key (kbd &ldquo;<right-fringe> <mouse-4>&rdquo;) &lsquo;bm-previous-mouse)
(global-set-key (kbd &ldquo;<right-fringe> <mouse-1>&rdquo;) &lsquo;bm-toggle-mouse)

(with-eval-after-load &ldquo;bm&rdquo;
  (defvar bm-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd &ldquo;m&rdquo;) &lsquo;bm-toggle)
      (define-key map (kbd &ldquo;n&rdquo;) (make-repeatable-command &lsquo;bm-next))
      (define-key map (kbd &ldquo;p&rdquo;) (make-repeatable-command &lsquo;bm-previous))
      (define-key map (kbd &ldquo;L&rdquo;) &lsquo;bm-show-all)
      (define-key map (kbd &ldquo;l&rdquo;) &lsquo;bm-show)
      (define-key map (kbd &ldquo;s&rdquo;) &lsquo;bm-save)
      (define-key map (kbd &ldquo;r&rdquo;) &lsquo;bm-load-and-restore)
      map)
    &ldquo;Keymap for \`bm.el&rsquo;.")
  (global-set-key (kbd &ldquo;C-c m&rdquo;) bm-mode-map))

    ;; Extensions to standard library `bookmark.el'.
    (when (try-require 'bookmark+)
      (global-set-key (kbd "<C-f2>") #'bmkp-toggle-autonamed-bookmark-set/delete)
      (global-set-key (kbd "<S-f2>") #'bmkp-next-bookmark-this-file/buffer-repeat)
      (global-set-key (kbd "<C-S-f2>") #'bmkp-delete-all-autonamed-for-this-buffer))
    
    (with-eval-after-load "bookmark+"
    
      (setq bmkp-light-left-fringe-bitmap 'filled-square)
      (setq bmkp-light-right-fringe-bitmap 'filled-square)
    
      ;; Default highlight style for autonamed (= default) bookmarks.
      (setq bmkp-light-style-autonamed 'line+lfringe)
    
      ;; Default highlight style for non-autonamed bookmarks.
      (setq bmkp-light-style-non-autonamed 'lfringe)
    
      ;; Automatically highlight bookmarks when set.
      (setq bmkp-auto-light-when-set 'any-bookmark)
    
      ;; Automatically highlight bookmarks when jumped to.
      (setq bmkp-auto-light-when-jump 'any-bookmark)
    
      ;; Don't propertize bookmark names to hold full bookmark data.
      (setq bmkp-propertize-bookmark-names-flag nil)
                                        ; We will often be going back and forth
                                        ; between using Bookmark+ and using
                                        ; vanilla Emacs.
    
      ;; (setq bmkp-last-as-first-bookmark-file bookmark-default-file)
    
      ;; ;; Restoring bookmarks when on file find.
      ;; (add-hook 'find-file-hook #'bm-buffer-restore)
      ))

## Ace Jump<a id="sec-15-3" name="sec-15-3"></a>

    (with-eval-after-load "ace-jump-mode-autoloads"
    
      ;; Quickly jump to a position in the current view.
      (global-set-key (kbd "C-c SPC") #'ace-jump-mode)
    
      ;; Pop up a postion from ‘ace-jump-mode-mark-ring’, and jump back to that
      ;; position.
      (global-set-key (kbd "C-c C-SPC") #'ace-jump-mode-pop-mark))

    ;; Quickly follow links using `ace-jump-mode'.
    (with-eval-after-load "ace-link-autoloads"
    
      ;; Setup the defualt shortcuts.
      (ace-link-setup-default "f"))
    
    ;; Jump to things.
    (with-eval-after-load "avy"
    
      ;; Default keys for jumping.
      (setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                       ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))
    
      ;; Ace-jump during Isearch to one of the current candidates.
      (define-key isearch-mode-map (kbd "C-'") 'avy-isearch))

    )                                       ; Chapter 13 ends here.

# Controlling the Display<a id="sec-16" name="sec-16"></a>

    ;;* 14 Controlling the (info "(emacs)Display")
    
    (leuven--chapter leuven-load-chapter-14-display "14 Controlling the Display"

## Scrolling<a id="sec-16-1" name="sec-16-1"></a>

    ;;** 14.1 (info "(emacs)Scrolling")
    
      (leuven--section "14.1 (emacs)Scrolling")
    
      ;; Always keep screen position of point when scrolling.
      (setq scroll-preserve-screen-position 1)
    
      ;; Scroll window up/down by one line.
      (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
      (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

    ;; Better scrolling in Emacs (doing a <PageDown> followed by a <PageUp> will
    ;; place the point at the same place).
    (with-eval-after-load "pager-autoloads"
    
      (autoload 'pager-page-up "pager"
        "Like scroll-down, but moves a fixed amount of lines." t)
                                          ; These autoloads aren't defined in
                                          ; `pager-autoloads'!
      (autoload 'pager-page-down "pager"
        "Like scroll-up, but moves a fixed amount of lines." t)
    
      (global-set-key (kbd "<prior>") #'pager-page-up)
      (global-set-key (kbd "<next>") #'pager-page-down))

## Automatic Scrolling<a id="sec-16-2" name="sec-16-2"></a>

&ldquo;Smooth&rdquo; (civilized) scrolling.

    ;;** 14.3 (info "(emacs)Auto Scrolling")
    
      (leuven--section "14.3 (emacs)Auto Scrolling")
    
      ;; Scroll only one line at a time (redisplay will never recenter point).
      (setq scroll-conservatively 10000)    ; Or `most-positive-fixnum'.
    
      ;; Number of lines of margin at the top and bottom of a window.
      (setq scroll-margin 4) ; or 1 or 3?   ; Also for `isearch-forward'.
    
      ;; Scrolling down looks much better.
      (setq auto-window-vscroll nil)

## Horizontal Scrolling<a id="sec-16-3" name="sec-16-3"></a>

How to automatically add/remove horizontal scrollbars as needed by text width?

## Narrowing<a id="sec-16-4" name="sec-16-4"></a>

-   **`C-x n n`:** **Narrow** down to between point and mark.

-   **`C-x n d`:** **Narrow** down to the current **defun**.

-   **`C-x n s`:** **Narrow** buffer to the current **subtree** (bound in Org mode).

-   **`C-x n w`:** **Widen** to make the entire buffer accessible again.

    ;;** 14.5 (info "(emacs)Narrowing")
    
      (leuven--section "14.5 (emacs)Narrowing")
    
      ;; Enable the use of the command `narrow-to-region' without confirmation.
      (put 'narrow-to-region 'disabled nil)
    
      (with-eval-after-load "fancy-narrow-autoloads"
        (fancy-narrow-mode))

## Font Lock mode<a id="sec-16-5" name="sec-16-5"></a>

    ;;** 14.12 (info "(emacs)Font Lock")
    
      (leuven--section "14.12 (emacs)Font Lock")

Prior to running their own mode hooks,
-   all text-based major modes run `text-mode-hook`, and
-   all programming language modes run `prog-mode-hook`.

XXX Try to get Org TODO overriding the TODO defined in
leuven-highlight-keywords, so that only one expression can be used for **all** modes!

    ;; Highlight FIXME notes.
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
    
    ;; Add highlighting keywords for selected major modes only.
    (dolist (mode '(fundamental-mode
                    text-mode))
      (font-lock-add-keywords mode
       `((,leuven-highlight-keywords 1 'leuven-highlight-face prepend))
       'end))
    
    ;; Add highlighting keywords for Org mode only.
    (dolist (mode '(org-mode))
      (font-lock-add-keywords mode
       `((,leuven-highlight-keywords-in-org 1 'leuven-highlight-face prepend))
       'end))
    
    ;; Add highlighting keywords for selected major modes *and* all major modes
    ;; derived from them.
    (dolist (hook '(prog-mode-hook
                    ;; text-mode-hook     ; Avoid Org.
                    css-mode-hook         ; [parent: fundamental]
                    latex-mode-hook
                    shell-mode-hook       ; [parent: fundamental]
                    ssh-config-mode-hook))
      (add-hook hook
       (lambda ()
         (font-lock-add-keywords nil      ; In the current buffer.
          `((,leuven-highlight-keywords 1 'leuven-highlight-face prepend)) 'end))))
          ;; FIXME                      0                        t          t

See doc of `font-lock-keywords` and `font-lock-add-keywords`.

    (defun highlight-errors-in-logs ()
      "Highlight certain lines in log files."
      (interactive)
      (when (equal "log" (file-name-extension (buffer-file-name)))
            (hi-lock-mode 1)
            (highlight-lines-matching-regexp "ERROR" 'hi-pink)
            (highlight-lines-matching-regexp "WARN" 'hi-yellow)))
    
    (add-hook 'find-file-hook 'highlight-errors-in-logs)

Check out `log4j-mode` instead?  And nice colors at
<https://www.jetbrains.com/resharper/help/Regular_Expressions_Assistance.html>.

    ;; Just-in-time fontification.
    (with-eval-after-load "jit-lock"
    
      ;; Stealth fontification should show status messages.
      (setq jit-lock-stealth-verbose t))

## Interactive Highlighting<a id="sec-16-6" name="sec-16-6"></a>

    ;;** 14.13 (info "(emacs)Highlight Interactively") by Matching
    
      (leuven--section "14.13 (emacs)Highlight Interactively by Matching")

      ;; Highlight-Changes mode.
      (with-eval-after-load "hilit-chg"
        (defvar highlight-fringe-mark 'filled-rectangle
          "The fringe bitmap name marked at changed line.
    Should be selected from `fringe-bitmaps'.")
    
        (defadvice hilit-chg-make-ov (after hilit-chg-add-fringe activate)
          (mapc (lambda (ov)
                  (if (overlay-get ov 'hilit-chg)
                      (let ((fringe-anchor (make-string 1 ?x)))
                        (put-text-property 0 1 'display
                                           (list 'left-fringe highlight-fringe-mark)
                                           fringe-anchor)
                        (overlay-put ov 'before-string fringe-anchor))))
                (overlays-at (ad-get-arg 1)))))
    
      ;; Enable Global-Highlight-Changes mode.
      (global-highlight-changes-mode 1)
    
      ;; ;; Changes are initially NOT visible in Highlight Changes mode.
      ;; (setq highlight-changes-visibility-initial-state nil)

You can use **Hi Lock** (part of Emacs) to highlight a regexp (identical tokens)
throughout a buffer.

-   **`M-x hi-lock-mode RET`:** Enable or disable Hi Lock mode.

-   **`M-s h .`:** **Highlight** each instance of the **symbol at point**.

-   **`M-s h r REGEXP RET` (`highlight-regexp`):** Highlight text that **matches REGEXP**.

-   **`M-s h l REGEXP RET`:** Highlight **entire lines** containing a match for REGEXP.

-   **`M-s h p` (`highlight-phrase`):** When called interactively, replace whitespace in user-provided
    regexp with arbitrary whitespace, and make initial lower-case
    letters case-insensitive, before highlighting with ‘hi-lock-set-pattern’.
    
    `highlight-phrase` is just a bit of sugar around `highlight-regexp` that
    ignores case and translates a space in the regex to match arbitrary
    whitespace. Handy.
    
    Also, `highlight-phrase` is not fully case insensitive. Only initial
    lower-case letters (of words) ae made case insensitive. (I suppose it was
    intended to deal with fred vs Fred) .. eg it generates a regex like:
    `[Cc]at[ ]+[Dd]og`

-   **`M-s h u REGEXP RET`:** **Unhighlight** REGEXP.

    ;; Do not prompt for the face to use. Instead, cycle through them.
    (setq hi-lock-auto-select-face t)
    
    ;; ;; Enable Hi Lock mode for all buffers.
    ;; (global-hi-lock-mode 1)

It is complementary to the features of the library **Highlight-symbol** (from
Nikolaj Schumacher).

-   Hi Lock and Highlight-symbol both use **text properties**. Copying and pasting
    text will also paste the highlighting.

-   Hi Lock and Highlight-symbol both **highlight new text** as you type it.

-   Highlight-symbol lets you **navigate among highlights**: `highlight-symbol-next`,
    `highlight-symbol-prev`.

`Highlight-symbol` automatically chooses **rainbow colors** and makes it much easier
to follow some key variables through a block of code.

    (with-eval-after-load "highlight-symbol-autoloads"
    
      ;; Emulation of Vim's `*' search.
      (global-set-key (kbd "C-*") #'highlight-symbol-at-point)
      ;; (global-set-key (kbd "C-<f4>") #'highlight-symbol-next)
      ;; (global-set-key (kbd "S-<f4>") #'highlight-symbol-prev)
      (global-set-key (kbd "C-M-*") #'highlight-symbol-remove-all)
      ;; (global-set-key (kbd "+") #'highlight-symbol-query-replace)
      )
    
    (with-eval-after-load "highlight-symbol"
    
      ;; Number of seconds of idle time before highlighting the current symbol.
      (setq highlight-symbol-idle-delay 0.5)
    
      (setq highlight-symbol-colors '("#FFCDFF" "#CCCCFF" "#FFB6C6" "#84CFFF"))
    
      ;; Temporarily highlight the symbol when using `highlight-symbol-jump'
      ;; family of functions.
      (setq highlight-symbol-on-navigation-p t))

Use Auto-Highlight-Symbol to **automatically highlight** all found **usages of** the
**symbol at point** in the current file (if you pause on a symbol).  When you move
point, the highlighting goes away.

Toggle Auto-Highlight-Symbol using key chord `**`.

    ;; Automatic highlighting occurrences of the current symbol under cursor.
    (when (try-require 'auto-highlight-symbol)
    
      ;; Add R.
      (add-to-list 'ahs-modes 'ess-mode t)
    
      ;; Add js2-mode.
      (add-to-list 'ahs-modes 'js2-mode t)
    
      ;; ;; Toggle Auto-Highlight-Symbol mode in all buffers.
      ;; (global-auto-highlight-symbol-mode t)
    
      ;; ;; Toggle Auto-Highlight-Symbol mode in all programming mode buffers.
      ;; (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
      )

XXX Check out `light-symbol`.

Color Identifiers Mode: **color** every **variable** in its own, consistent color.

    ;; XXX Impact on Org's HTML export?
      ;; (with-eval-after-load "color-identifiers-mode-autoloads"
      ;;
      ;;   (add-hook 'after-init-hook #'global-color-identifiers-mode))

## Window Fringes<a id="sec-16-7" name="sec-16-7"></a>

Show an icon in the fringe of version-controlled files indicating which **lines**
have been **edited** (inserted, modified or deleted) **since the last commit**.

This feature gives you a really nice at-a-glance view of where you&rsquo;ve made
changes.

    (with-eval-after-load "diff-hl-autoloads"
      (idle-require 'diff-hl))
    
    ;; Indicate changes in the fringe.
    (with-eval-after-load "diff-hl"
    
      (global-diff-hl-mode 1)

If you&rsquo;re using some package other than `vc` to commit changes, it might not run
`vc-checkin-hook` after commits.  In that case, you&rsquo;ll need to either add
`diff-hl-update` to the hook it does run, or advise some function that&rsquo;s called in
the buffer after its state has changed.

<div class="note">
Of course, `diff-hl` is not updated when we commit changes with `sendpatch`&#x2026;

</div>

Jump between changes:

    ;; Jump to next hunk (also on `C-x v ]').
    (define-key diff-hl-mode-map (kbd "C-x v >") #'diff-hl-next-hunk)
    (define-key diff-hl-mode-map (kbd "@") #'diff-hl-next-hunk)
    
    ;; Jump to previous hunk (also on `C-x v [').
    (define-key diff-hl-mode-map (kbd "C-x v <") #'diff-hl-previous-hunk)

Act on changes:

    ;; Popup current diff.
    (define-key diff-hl-mode-map (kbd "C-x v =") #'diff-hl-diff-goto-hunk)
    
    ;; Revert current hunk (also on `C-x v n').
    (define-key diff-hl-mode-map (kbd "C-x v u") #'diff-hl-revert-hunk))

    (defadvice magit-refresh (after diff-hl-refresh-after-magit-refresh activate)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when diff-hl-mode
            (diff-hl-update)))))

## Displaying Boundaries<a id="sec-16-8" name="sec-16-8"></a>

    ;;** 14.15 (info "(emacs)Displaying Boundaries")
    
      (leuven--section "14.15 (emacs)Displaying Boundaries")
    
      ;; Visually indicate buffer boundaries and scrolling in the fringe.
      (setq-default indicate-buffer-boundaries '((top . left) (t . right)))

## Useless Whitespace<a id="sec-16-9" name="sec-16-9"></a>

    ;;** 14.16 (info "(emacs)Useless Whitespace")
    
      (leuven--section "14.16 (emacs)Useless Whitespace")

Make trailing whitespace at the end of a line visible.

    ;; ;; Highlight trailing whitespaces in all modes.
    ;; (setq-default show-trailing-whitespace t)

Remove trailing whitespace (right trim lines) **in all lines**:

    ;; Nuke all trailing whitespaces in the buffer.
    (add-hook 'before-save-hook
              (lambda ()                  ; Except for ...
                (let ((buffer-undo-list buffer-undo-list)) ; For goto-chg.
                  (unless (or (derived-mode-p 'message-mode)
                                          ; ... where "-- " is the signature
                                          ; separator (for when using emacsclient
                                          ; to compose emails and doing C-x #).
                              (derived-mode-p 'diff-mode))
                                          ; ... where the patch file can't be
                                          ; changed!
                    (delete-trailing-whitespace)))))

This command also deletes all **empty lines at the end of the buffer**.

Remove trailing whitespace **in edited lines** with `ws-butler`:

    ;; Unobtrusively remove trailing whitespace.
    (with-eval-after-load "ws-butler-autoloads"
    
      (add-hook 'text-mode-hook #'ws-butler-mode)
      (add-hook 'prog-mode-hook #'ws-butler-mode))
    
    (with-eval-after-load "ws-butler"
      (diminish 'ws-butler-mode))

XXX Check whether explicitly inserted tabs (at beginning of lines) are deleted
because of this.

    ;; Visually indicate empty lines after the buffer end in the fringe.
    (setq-default indicate-empty-lines t)

When Whitespace mode is on, it takes care of highlighting some special
characters over the default mechanism of `nobreak-char-display` and
`show-trailing-whitespace`.

    ;; Enable Whitespace mode in all file buffers (not in *vc-dir*, etc.).
    (add-hook 'text-mode-hook #'whitespace-mode)
    (add-hook 'prog-mode-hook #'whitespace-mode)
    
    (with-eval-after-load "whitespace"
    
      ;; Which kind of blank is visualized.
      (setq whitespace-style
            '(face
              trailing
              tabs
              ;; lines-tail
              indentation::space
              space-mark
              tab-mark))
    
      ;; Column beyond which the line is highlighted.
      (setq whitespace-line-column 80)
    
      ;; Mappings for displaying characters.
      (setq whitespace-display-mappings
            '((space-mark ?\u00A0         ; No-break space.
                          [?_]            ; Spacing underscore.
                          [?_])           ; Spacing underscore.
    
              (space-mark ?\u202F         ; Narrow no-break space.
                          [?\u00B7]       ; Middle dot.
                          [?.])
    
              (tab-mark ?\t               ; Tabulation.
                        [?\u25BA ?\t]     ; Black right-pointing pointer.
                        [?\\ ?\t]))))

Example :
VPATH=                /home/jt/src/cygwin/cygwin-packages/1.7/python-2.6.8-2/python-2.6.8-2/src/Python-2.6.8
LDLAST=
SGI\_ABI=
PYTHON\_OBJS=        \\
                Python/pythonrun.o \\
                Python/random.o \\
                Python/structmember.o

    ;; ;; Control highlighting of non-ASCII space and hyphen chars, using the
    ;; ;; `nobreak-space' or `escape-glyph' face respectively.
    ;; (setq nobreak-char-display t)      ; [Default]

    ;; ;; Show zero-width spaces.
    ;; (font-lock-add-keywords nil
    ;;  `((,(format "\\(%c\\)" ?\u200B) ; #\ZERO_WIDTH_SPACE
    ;;     (1 (progn (compose-region (match-beginning 1) (match-end 1)
    ;;                               ?\u2B1B ; #\BLACK_LARGE_SQUARE
    ;;                               'decompose-region)
    ;;               nil)))))

## Selective Display<a id="sec-16-10" name="sec-16-10"></a>

Emacs has the ability to **hide lines indented more than a given number of
columns**.  You can use this to get an overview of a part of a program.

To hide lines with at least 1 column of indentation (replacing them by an
ellipsis), type `C-1 C-x $` (or `C-x $` and give it `1` as depth argument).  It gives
a quick overview any source file.

To make all lines visible again, type `C-x $` with no argument.

See also **Outline Mode** for another way to hide part of the text in a buffer.

## Optional Mode Line Features<a id="sec-16-11" name="sec-16-11"></a>

    ;;** 14.18 (info "(emacs)Optional Mode Line") Features
    
      (leuven--section "14.18 (emacs)Optional Mode Line Features")
    
      ;; Show the column number in each mode line.
      (column-number-mode 1)

    ;; Unclutter the mode line.
    (with-eval-after-load "diminish-autoloads"
      (with-eval-after-load "abbrev"       (diminish 'abbrev-mode " Ab"))
      (with-eval-after-load "checkdoc"     (diminish 'checkdoc-minor-mode " Cd"))
      ;; (with-eval-after-load "company"      (diminish 'company-mode " Cp"))
                                          ; Company displays the currently used
                                          ; backend in the mode-line.
      (with-eval-after-load "eldoc"        (diminish 'eldoc-mode))
      (with-eval-after-load "fancy-narrow" (diminish 'fancy-narrow-mode))
      (with-eval-after-load "flycheck"     (diminish 'flycheck-mode " fC"))
      (with-eval-after-load "flyspell"     (diminish 'flyspell-mode " fS"))
      (with-eval-after-load "google-this"  (diminish 'google-this-mode))
      (with-eval-after-load "hilit-chg"    (diminish 'highlight-changes-mode))
      ;; (with-eval-after-load "isearch"      (diminish 'isearch-mode (string 32 ?\u279c)))
      (with-eval-after-load "paredit"      (diminish 'paredit-mode " Pe"))
      (with-eval-after-load "rainbow-mode" (diminish 'rainbow-mode))
      (with-eval-after-load "simple"       (diminish 'auto-fill-function))
      (with-eval-after-load "whitespace"   (diminish 'whitespace-mode))
      ;; (diminish-on-load hs-minor-mode-hook hs-minor-mode)
      ;; (with-eval-after-load "glasses"      (diminish 'glasses-mode))
      ;; (with-eval-after-load "redshank"     (diminish 'redshank-mode))
      (with-eval-after-load "smartparens"  (diminish 'smartparens-mode)))
      ;; (with-eval-after-load "whitespace"   (diminish 'whitespace-mode))

See <https://powerline.readthedocs.org/en/latest/overview.html#vim-statusline> for screenshots.

See <https://github.com/bling/vim-airline> for the original author.

    (defface powerline-modified-face
      '((((class color))
         (:background "#FFA335" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify modified files."
      :group 'powerline)
    
    (defface powerline-normal-face
      '((((class color))
         (:background "#4F9D03" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify unchanged files."
      :group 'powerline)
    
    (defface powerline-default-dictionary-active-face
      '((((class color))
         (:background "#8A2BE2" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify default dictionary in the active buffer."
      :group 'powerline)
    
    (defface powerline-default-dictionary-inactive-face
      '((((class color))
         (:background "thistle" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify default dictionary in inactive buffers."
      :group 'powerline)
    
    (defface powerline-other-dictionary-active-face
      '((((class color))
         (:background "yellow" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify another dictionary in the active buffer."
      :group 'powerline)
    
    (defface powerline-other-dictionary-inactive-face
      '((((class color))
         (:background "LightYellow1" :foreground "black" :weight bold))
        (t (:weight bold)))
      "Face to fontify another dictionary in inactive buffers."
      :group 'powerline)
    
    (defface powerline-buffer-position-face
      '((((class color))
         (:background "#D2D2D2" :foreground "#282828"))
        (t (:weight bold)))
      "Face to fontify buffer position."
      :group 'powerline)
    
    (defun powerline-simpler-vc-mode (s)
      (if s
          (replace-regexp-in-string "\\(Git\\|SVN\\)[-:]" "" s)
        s))
    
    (defun powerline-leuven-theme ()
      "Setup the leuven mode-line."
      (interactive)
      (setq-default mode-line-format
       '("%e"
         (:eval
          (let* ((active (powerline-selected-window-active))
                 (mode-line (if active
                                'mode-line
                              'mode-line-inactive))
                 (face1 (if active
                            'powerline-active1
                          'powerline-inactive1))
                 (face2 (if active
                            'powerline-active2
                          'powerline-inactive2))
                 (default-dictionary-face
                   (if active
                       'powerline-default-dictionary-active-face
                     'powerline-default-dictionary-inactive-face))
                 (other-dictionary-face
                  (if active
                      'powerline-other-dictionary-active-face
                    'powerline-other-dictionary-inactive-face))
                 (separator-left
                  (intern
                   (format "powerline-%s-%s"
                           powerline-default-separator
                           (car powerline-default-separator-dir))))
                 (separator-right
                  (intern
                   (format "powerline-%s-%s"
                           powerline-default-separator
                           (cdr powerline-default-separator-dir))))
                 (lhs (list
                       ;; VC mode.
                       (when (and (fboundp 'vc-switches)
                                  buffer-file-name
                                  vc-mode)
                         (if (eq (vc-state buffer-file-name) 'up-to-date)
                             (powerline-simpler-vc-mode (powerline-vc 'powerline-normal-face 'r))
                           (powerline-simpler-vc-mode (powerline-vc 'powerline-modified-face 'r))))
    
                       (when (and (not (fboundp 'vc-switches))
                                  buffer-file-name
                                  vc-mode)
                         (powerline-simpler-vc-mode (powerline-vc face1 'r)))
    
                       (when (and buffer-file-name
                                  vc-mode)
                         (if (eq (vc-state buffer-file-name) 'up-to-date)
                             (funcall separator-left 'powerline-normal-face mode-line)
                           (funcall separator-left 'powerline-modified-face mode-line)))
    
                       ;; "Modified" indicator.
                       (if (not (buffer-modified-p))
                           (powerline-raw "%*" nil 'l)
                         (powerline-raw "%*" 'mode-line-emphasis 'l))
    
                       (powerline-raw mode-line-mule-info nil 'l)
    
                       (powerline-buffer-id nil 'l)
    
                       (when (and (boundp 'which-func-mode) which-func-mode)
                         (powerline-raw which-func-format nil 'l))
    
                       (powerline-raw " ")
                       (funcall separator-left mode-line face1)
                       (when (boundp 'erc-modified-channels-object)
                         (powerline-raw erc-modified-channels-object face1 'l))
                       (powerline-major-mode face1 'l)
                       (powerline-process face1)
                       (powerline-raw " " face1)
                       (funcall separator-left face1 face2)
                       (powerline-minor-modes face2 'l)
                       (powerline-narrow face2 'l)
                       (powerline-raw " " face2)
                       (funcall separator-left face2 mode-line)))
                 (rhs (list (powerline-raw global-mode-string mode-line 'r)
                            (funcall separator-right mode-line face1)

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Show column in red when we go over the 80th character</b><br  />
See <http://www.elliotglaysher.org/emacs/>
</div>

                            (powerline-raw "%l," face1 'l)
                            (powerline-raw "%c" face1 'r)
                            (funcall separator-right face1 'powerline-buffer-position-face)
                            (powerline-raw " %3p" 'powerline-buffer-position-face 'r)
                            (funcall separator-right 'powerline-buffer-position-face face2)
                            (powerline-buffer-size face2 'l)
                            (powerline-raw " " face2)
    
                            (let ((dict (and (featurep 'ispell)
                                             (or
                                              ispell-local-dictionary
                                              ispell-dictionary))))
                              ;; Add 2 spaces after the language indicator
                              ;; (for GNU/Linux).
                              (cond (buffer-read-only
                                     (powerline-raw "%%%%  " default-dictionary-face 'l))
                                    ((null dict)
                                     (powerline-raw "--  " default-dictionary-face 'l))
                                    (t
                                     (powerline-raw (concat (substring dict 0 2) "  ") other-dictionary-face 'l))))
    
                            ;; (powerline-hud face2 face1)
                            )))
            (concat (powerline-render lhs)
                    (powerline-fill mode-line (powerline-width rhs))
                    (powerline-render rhs)))))))
    
    (with-eval-after-load "powerline-autoloads"
      (add-hook 'after-init-hook #'powerline-leuven-theme))

## Displaying the Cursor<a id="sec-16-12" name="sec-16-12"></a>

    ;;** 14.20 The (info "(emacs)Cursor Display")
    
      (leuven--section "14.20 (emacs)The Cursor Display")
    
      ;; Use cursor color and type to indicate some modes (read-only, overwrite
      ;; and normal insert modes).
      (defun leuven--set-cursor-according-to-mode ()
        "Change cursor color according to some minor modes."
        (let ((color (cond (buffer-read-only "purple1")
                           (overwrite-mode   "red")
                           (t                "black"))) ; #21BDFF is less visible.
              (type (if (null overwrite-mode)
                        'bar
                      'box)))
          (set-cursor-color color)
          (setq cursor-type type)))
    
      (add-hook 'post-command-hook #'leuven--set-cursor-according-to-mode)
    
      ;; Cursor to use.
      (setq-default cursor-type 'bar)
    
      ;; Cursor blinks forever.
      (setq blink-cursor-blinks 0)

    ;; Toggle line highlighting in all buffers (Global Hl-Line mode).
    (global-hl-line-mode 1)               ; XXX Perhaps only in prog-modes?
    
    ;; Extensions to hl-line.el.
    (with-eval-after-load "hl-line+-autoloads"
    
      ;; Disable Global Hl-Line mode.
      (global-hl-line-mode -1)
    
      ;; Turn on `global-hl-line-mode' only when Emacs is idle.
      (toggle-hl-line-when-idle))

## Truncation of Lines<a id="sec-16-13" name="sec-16-13"></a>

    ;;** 14.21 (info "(emacs)Line Truncation")
    
      (leuven--section "14.21 (emacs)Line Truncation")
    
      ;; Respect the value of `truncate-lines' in all windows less than the full
      ;; width of the frame.
      (setq truncate-partial-width-windows nil)

## Customization of Display<a id="sec-16-14" name="sec-16-14"></a>

    ;;** 14.23 (info "(emacs)Display Custom")ization
    
      (leuven--section "14.23 (emacs)Display Customization")
    
      ;; Echo what I'm typing *immediately*.
      (setq echo-keystrokes 0.01)

Display pressed keyboard shortcuts on screen during presentations or
screencasts.

    ;; Exhaustive log of interactions with Emacs (display keystrokes, etc.).
    (with-eval-after-load "interaction-log-autoloads"
    
      (autoload 'interaction-log-mode "interaction-log"
        "Global minor mode logging keys, commands, file loads and messages." t)
                                          ; This autoload isn't defined in
                                          ; `interaction-log-autoloads'!
    
      ;; ;; Maximum number of lines to keep in the *Emacs Log* buffer.
      ;; (setq ilog-log-max 10)
    
      (defun leuven-display-interaction-log ()
        "Display the Interaction-Log buffer."
        (interactive)
        (interaction-log-mode 1)
        (display-buffer ilog-buffer-name))
    
      ;; Hotkey for showing the log buffer.
      (global-set-key (kbd "C-h C-l") #'leuven-display-interaction-log))

    )                                       ; Chapter 14 ends here.

# Searching and Replacement<a id="sec-17" name="sec-17"></a>

    ;;* 15 (info "(emacs)Search")ing and Replacement
    
    (leuven--chapter leuven-load-chapter-15-search "15 Searching and Replacement"

## Incremental Search (aka &ldquo;Live Search&rdquo;)<a id="sec-17-1" name="sec-17-1"></a>

Have a look at the &ldquo;Standard Isearch Keys&rdquo; on
<http://www.emacswiki.org/emacs/IncrementalSearch>

Isearch has a binding which deletes all the non-matching characters from the
search string: `C-g` (not a very intuitive binding, because it&rsquo;s usually
associated with aborting operations, not with correcting things).

If you hit `M-e` (to edit the search string), the cursor is moved to the mismatch
position.

Since Emacs 24.3:
-   `M-s _` starts a symbol (identifier) incremental search.
-   `M-s _` in Isearch toggles symbol search mode.
-   `M-s c` in Isearch toggles search case-sensitivity.

    ;;** 15.1 (info "(emacs)Incremental Search")
    
      (leuven--section "15.1 (emacs)Incremental Search")
    
      ;; FIXME Error when selecting search string from kill ring (`M-p')
      ;; ;; Always exit searches at the beginning of the expression found.
      ;; (add-hook 'isearch-mode-end-hook #'isearch-goto-match-beginning)
      ;;
      ;; (defun isearch-goto-match-beginning ()
      ;;   "Use with isearch hook to end search at first char of match."
      ;;   (when isearch-forward (goto-char isearch-other-end)))
    
      ;; ;; Incremental search/query-replace will open the contents.
      ;; (setq search-invisible 'open)         ; XXX
    
      ;; Don't re-hide an invisible match right away.
      (setq isearch-hide-immediately nil)   ; XXX
    
      ;; Scrolling commands are allowed during incremental search (without
      ;; canceling Isearch mode).
      (setq isearch-allow-scroll t)

<div class="note">
When `search-invisible` is `t`, `perform-replace` can replace matches in any invisible
text, but does not display it when prompting the user for confirmation.

When `search-invisible` is `'open`, `perform-replace` can replace matches only in
hidden text (invisible through an overlay), and displays it when prompting the
user.

We lack a way to reveal truly invisible text when performing a search and
replace.  The URL of a bracket link in Org is an example of such truly invisible
text.

Juri&rsquo;s solution to add `'open-all` could also be used in `perform-replace` to let
the user see truly invisible matches before replacing them.

</div>

    ;; Fuzzy matching utilities (a must-have).
    (with-eval-after-load "fuzzy-autoloads"
    
      (autoload 'turn-on-fuzzy-isearch "fuzzy" nil t)
                                          ; This autoload isn't defined in
                                          ; `fuzzy-autoloads'!
    
      (add-hook 'isearch-mode-hook #'turn-on-fuzzy-isearch))

    ;; Show number of matches in mode-line while searching.
    (with-eval-after-load "anzu-autoloads"
    
      ;; Lighter of anzu-mode.
      (setq anzu-mode-lighter "")
    
      ;; Deactive region if you use anzu a replace command with region.
      (setq anzu-deactivate-region t)
    
      ;; Separator of `to' string.
      (setq anzu-replace-to-string-separator " => ")
    
      ;; Function which returns mode-line string.
      (defun leuven--anzu-update-mode-line (here total)
        (when anzu--state
          (let ((status (cl-case anzu--state
                          (search (format (if (> total 1)
                                              " %s of %d%s matches "
                                            " %s of %d%s match ")
                                          (anzu--format-here-position here total)
                                          total (if anzu--overflow-p "+" "")))
                          (replace-query (format " %d replace " total))
                          (replace (format (if (> total 1)
                                               " %d of %d matches "
                                             " %d of %d match ")
                                           here total)))))
            (propertize status 'face 'anzu-mode-line))))
      (setq anzu-mode-line-update-function #'leuven--anzu-update-mode-line)
    
      ;; Enable Global-Anzu mode.
      (global-anzu-mode 1)
    
      ;; Override binding for `query-replace'.
      (global-set-key (kbd "M-%") #'anzu-query-replace)
      (global-set-key (kbd "C-M-%") #'anzu-query-replace-regexp)
    
      ;; (define-key isearch-mode-map (kbd "M-%") #'anzu-query-replace)
      )

## Symbol Search<a id="sec-17-2" name="sec-17-2"></a>

-   **`M-s .` (`isearch-forward-symbol-at-point`):** Start a symbol (identifier) incremental search forward with the symbol
    found near point added to the search string initially.

## Regexp Search<a id="sec-17-3" name="sec-17-3"></a>

Emacs uses basic regular expressions, which means that the extended regexp
operators have to be escaped.  This means you have to use `\(`, `\|`, and `\)`
instead of `(`, `|`, and `)`.

You can build regexps with visual feedback by using:
-   `M-x re-builder` or
-   `M-x regex-tool` (by John Wiegley)

Optimize regexps with `regexp-opt.el`.

    ;;** 15.5 (info "(emacs)Regexp Search")
    
      (leuven--section "15.5 (emacs)Regexp Search")

    ;; Use regexps by default (allows searching across line breaks).
    (global-set-key (kbd "C-s") #'isearch-forward-regexp)
    (global-set-key (kbd "C-r") #'isearch-backward-regexp)
    
    ;; Shift the meaning of `C-M-s/r' with `C-s/r'.
    (global-set-key (kbd "C-M-s") #'isearch-forward)
    (global-set-key (kbd "C-M-r") #'isearch-backward)

## Search Case<a id="sec-17-4" name="sec-17-4"></a>

    ;;** 15.9 (info "(emacs)Search Case")
    
      (leuven--section "15.9 (emacs)Search Case")
    
      ;; Searches should ignore case by default (in all buffers that do not
      ;; override this).
      (setq-default case-fold-search t)

**Case folding** folds together &ldquo;a&rdquo; and &ldquo;A&rdquo;.

**Char folding** allows simple ASCII characters to match their complex Unicode
counterparts.  For instance, if you search for &ldquo;a&rdquo;, you will also match &ldquo;á&rdquo; and
&ldquo;ã&rdquo;; that is, char folding folds together &ldquo;a&rdquo;, &ldquo;á&rdquo; and &ldquo;ã&rdquo;.

## Replacement Commands<a id="sec-17-5" name="sec-17-5"></a>

Pass the string or the regexp directly to `query-replace`, so you can&rsquo;t mangle it
on the way, by pressing `M-%` while at the `I-search:` prompt.

## Other Search-and-Loop Commands<a id="sec-17-6" name="sec-17-6"></a>

`M-x flush-lines` (or `delete-matching-lines`) deletes each line that contains
a match for REGEXP.

    ;;** 15.11 (info "(emacs)Other Repeating Search") Commands
    
      (leuven--section "15.11 (emacs)Other Repeating Search Commands")
    
      ;; ;; Invoke `occur' easily from within `isearch'.
      ;; (define-key isearch-mode-map (kbd "C-o")
      ;;   (lambda ()
      ;;     (interactive)
      ;;     (let ((case-fold-search isearch-case-fold-search))
      ;;       (occur
      ;;        (if isearch-regexp
      ;;            isearch-string
      ;;          (regexp-quote isearch-string))))))
    
      ;; When doing Isearch, hand the word over to `helm-swoop'.
      (define-key isearch-mode-map (kbd "C-o") #'helm-swoop-from-isearch)
    
      (global-unset-key (kbd "M-o")) ; XXX???
    
      ;; "Multi-occur" easily inside Isearch.
      (define-key isearch-mode-map (kbd "M-o") #'helm-multi-swoop-all)
    
      ;; Grep all same extension files from inside Isearch.
      (define-key isearch-mode-map (kbd "C-M-o")
        (lambda ()
          (interactive)
          (grep-compute-defaults)
          (lgrep (if isearch-regexp isearch-string (regexp-quote isearch-string))
                 (if (file-name-extension (buffer-file-name))
                  (format "*.%s" (file-name-extension (buffer-file-name)))
                  "*")
                 default-directory)
          (isearch-abort)))

    )                                       ; Chapter 15 ends here.

# Commands for Fixing Typos<a id="sec-18" name="sec-18"></a>

    ;;* 16 Commands for (info "(emacs)Fixit") Typos
    
    (leuven--chapter leuven-load-chapter-16-fixit "16 Commands for Fixing Typos"

## Checking and Correcting Spelling<a id="sec-18-1" name="sec-18-1"></a>

<div class="note">
On Windows:
-   Install [GNU Aspell 0.50.3](http://ftp.gnu.org/gnu/aspell/w32/Aspell-0-50-3-3-Setup.exe) (more dictionaries than Cygwin Aspell)
-   Install [Aspell English dictionary-0.50-2](http://ftp.gnu.org/gnu/aspell/w32/Aspell-en-0.50-2-3.exe)
-   Add `C:\Program Files (x86)\Aspell\bin` to the System `PATH`

</div>

GNU **Aspell** is better than **Ispell**:

-   It does recognize words with apostrophes (l&rsquo;avion)&#x2026; but it is slower.

-   It can handle UTF-8 documents far better than Ispell can.

When spellchecking a buffer, the `A` command accepts an incorrect word, and adds
it to the **file word list** (`ispell-add-per-file-word-list`).

    ;;** 16.4 Checking and Correcting (info "(emacs)Spelling")
    
      (leuven--section "16.4 (emacs)Checking and Correcting Spelling")
    
      ;; Spelling checker program.
      (setq ispell-program-name             ; Defined in ispell.el.
            (or (executable-find "aspell")
                (executable-find "ispell")
                ;; nil                      ; [Default: "ispell"]
                ))
    
      (defun leuven--executable-ispell-program-name-p ()
        "Ensure that `ispell-program-name' is an executable program name."
        (and (boundp 'ispell-program-name)
             ispell-program-name            ; It can be nil!
             (file-executable-p ispell-program-name)
             ispell-program-name))
    
      (when (leuven--executable-ispell-program-name-p)
    
        (defun leuven-ispell-region-or-buffer ()
          "Interactively check the current region or buffer for spelling errors."
          (interactive)
          (if mark-active
              (if (< (mark) (point))
                  (ispell-region (mark) (point))
                  (ispell-region (point) (mark)))
              (ispell-buffer)))
    
        ;; Key bindings (or `C-c i' prefix key binding?).
        (global-set-key (kbd "C-$") #'leuven-ispell-region-or-buffer)
        (global-set-key (kbd "C-M-$") #'ispell-change-dictionary)
    
        ;; ;; Default dictionary to use (if `ispell-local-dictionary' is nil, that
        ;; ;; is if there is no local dictionary to use in the buffer).
        ;; (setq ispell-dictionary "american") ; see `sentence-end-double-space'

    ;; Enable on-the-fly spell checking.
    (add-hook 'org-mode-hook
              (lambda ()
                (if (or (eq (aref (buffer-name) 0) ?\s)
                                        ; Buffer starting with " *".
                        (and (boundp 'org-babel-exp-reference-buffer)
                             org-babel-exp-reference-buffer))
                                        ; Export buffer.
                    (message "DON'T TURN ON Flyspell mode in `%s'" (buffer-name))
                  (message "Turn on Flyspell mode in `%s'" (buffer-name))
                  (flyspell-mode))))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Enable Flyspell in other modes deriving from Text (HTML, etc.)</b><br  />
nil</div>

    ;; Enable on-the-fly spell checking.
    (add-hook 'text-mode-hook
              (lambda ()
                (message "Turn on Flyspell mode in `%s'" (buffer-name))
                (flyspell-mode)))
    
    (defadvice org-export-as (around leuven-org-export-as activate)
      "Turn off Text mode hooks (Flyspell, etc.) when exporting current Org buffer."
      (let (text-mode-hook)
        ad-do-it))

    ;; Prevent Flyspell from finding mistakes in the code, well in comments and
    ;; strings.
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)

    (with-eval-after-load "ispell"
    
      ;; Save the personal dictionary without confirmation.
      (setq ispell-silently-savep t)
    
      ;; Extensions and extra switches to pass to the `ispell' program.
      (cond

Tell `aspell` to speed up, though this reduces somewhat the quality of its
suggestions.  According to the `aspell` documentation:

-   `ultra` is the fastest suggestion mode, which is still twice as slow as `ispell`.

-   If your machine is fast enough, a better option might be to try `fast` mode,
    which is twice as slow as `ultra`, but more accurate.

-   The `normal` mode, which is the `aspell` default, is even more accurate, but is
    reportedly 10 times slower than `fast` mode.

Use the `-C` option which will ConsiderCamelCaseToBeCorrect.

    ((string-match "aspell" ispell-program-name)
     (setq ispell-extra-args '("--sug-mode=ultra" "-C"))
     (setq ispell-really-aspell t)
     (setq ispell-really-hunspell nil))

    ((string-match "ispell" ispell-program-name)
     (setq ispell-extra-args '())
     (setq ispell-really-aspell nil)
     (setq ispell-really-hunspell nil)))

    ;; ;; Solve the problem of words separated by `-' flagged as
    ;; ;; erroneous by removing the `-' from the value of otherchars.
    ;; (if (fboundp 'ispell-get-decoded-string)
    ;;     (defun ispell-get-otherchars ()
    ;;       (replace-regexp-in-string "-" "" (ispell-get-decoded-string 3))))

Add language indicator.

    ;; (setq-default mode-line-format
    ;;               (cons
    ;;                '(:eval
    ;;                  (let ((dict (and (featurep 'ispell)
    ;;                                   (not buffer-read-only)
    ;;                                   (or ispell-local-dictionary
    ;;                                       ispell-dictionary
    ;;                                       "--" ; default dictionary
    ;;                                       ))))
    ;;                    (and dict
    ;;                         (propertize (concat " " (substring dict 0 2))
    ;;                                     'face 'mode-line-highlight))))
    ;;                (default-value 'mode-line-format)))

    )

<div class="note">
Each time you cycle to another window (`C-x o`), the **Ispell process** gets **killed**
and a **new Ispell process** is **started**, if the language in the next window is
different from the language in the current window&#x2026;

</div>

`flyspell-auto-correct-word` is bound to `C-.`.  Press it one time to correct the
word under the cursor.  If several spellings are possible, they appear in the
minibuffer.  Just keep hitting `C-.` to replace the word with the successive
suggestions.

    (with-eval-after-load "flyspell"
    
      ;; Remove the binding of `flyspell-auto-correct-previous-word', to be used
      ;; by Multiple Cursors.
      (define-key flyspell-mode-map (kbd "C-;") nil))

`flyspell-goto-next-error` is bound to &ldquo;C-,&rdquo;.  Press it to go to the next
previously detected error.

`flyspell-correct-word-before-point` is bound to `C-c $`.  Press it to **pop up a menu**
of possible corrections for misspelled word before point.

    ;; Don't use `M-TAB' to auto-correct the current word (only use `C-.').
    (setq flyspell-use-meta-tab nil)
    ;; FIXME M-TAB is still bound to `flyspell-auto-correct-word' when this
    ;; chunk of code is placed within (with-eval-after-load "flyspell"...)
    
    (with-eval-after-load "flyspell"
    
     ;; Don't consider that a word repeated twice is an error.
     (setq flyspell-mark-duplications-flag nil)
    
     ;; Lower (for performance reasons) the maximum distance for finding
     ;; duplicates of unrecognized words.
     (setq flyspell-duplicate-distance 12000) ; [default: 400000]
    
     ;; Fix the "enabling flyspell mode gave an error" bug.
     (setq flyspell-issue-welcome-flag nil)
    
     ;; ;; Don't print messages for every word (when checking the entire buffer)
     ;; ;; as it causes a (small) slowdown.
     ;; (setq flyspell-issue-message-flag nil)
    
     ;; Dash character (`-') is considered as a word delimiter.
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
           ;; If above is executed, the advised `org-mode-flyspell-verify'
           ;; won't work anymore.
           (flyspell-buffer))))
    
     ;; Key bindings.
     (global-set-key (kbd "C-$") #'flyspell-buffer)
     (global-set-key (kbd "C-M-$") #'leuven-flyspell-toggle-dictionary)
    
     ;; Spell-check your XHTML (by adding `nxml-text-face' to the list of
     ;; faces corresponding to text in programming-mode buffers).
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

I need often bilingual dictionaries like the excellent leo.org and dict.cc;
dict.el with the dict servers is not really an option.  At the moment I&rsquo;m using
mostly my mobile phone as portable dictionary but I&rsquo;d rather want do this within
Emacs.

There seem to be at least 3 solutions:

-   x-dict.el, <http://www.xsteve.at/prg/python/>
-   r-dict.el, <https://github.com/tsdh/rdictcc>
-   eww, but leo.org and dict.cc are barely readable in eww

What is working for you?

I use eedic and dict/dictd running on my own computer.

s-d looks up the word under the cursor and displays the result in a second window.

s-n finds any singleword anagrams of the word under the cursor.

s-w uses Wordplay by Evans Crisswell to list all words that can be
made from the letters in the current word as well as multiword
anagrams.

    ;; Client for rfc2229 dictionary servers.
    (try-require "dictionary-autoloads")
    (with-eval-after-load "dictionary-autoloads"
    
      (global-set-key (kbd "C-c C-d s") #'dictionary-search)
      (global-set-key (kbd "C-c C-d l") #'dictionary-lookup-definition)
      (global-set-key (kbd "C-c C-d m") #'dictionary-match-words))
    
    (with-eval-after-load "dictionary"
    
      ;; Enable/disable the tooltip support for all buffers.
      (if leuven--console-p
          (global-dictionary-tooltip-mode 0)
        (global-dictionary-tooltip-mode 1)))

    )                                       ; Chapter 16 ends here.

# Keyboard Macros<a id="sec-19" name="sec-19"></a>

    ;;* 17 (info "(emacs)Keyboard Macros")
    
    (leuven--chapter leuven-load-chapter-17-keyboard-macros "17 Keyboard Macros"

Shows keyboard macros or latest interactive commands as Emacs Lisp:
<https://github.com/Silex/elmacro>.

## Basic Use<a id="sec-19-1" name="sec-19-1"></a>

-   **`<S-f8>`:** Start recording your keyboard macro.

-   **`<S-f8>` again:** Stop recording it.

-   **`<f8>`:** Call the keyboard macro.

If you want to check the result each time before repeating, type `<f8> e e...`.

If you want to repeat only N times, type `C-u N <f8>`.

If you want to repeat forever or until error, type `C-u 0 <f8>`.

    ;;** 17.1 (info "(emacs)Basic Keyboard Macro") Use
    
      (leuven--section "17.1 (emacs)Basic Keyboard Macro Use")
    
      (defun leuven-kmacro-turn-on-recording ()
        "Start recording a keyboard macro and toggle functionality of key binding."
        (interactive)
        (global-set-key (kbd "<S-f8>") #'leuven-kmacro-turn-off-recording)
        (kmacro-start-macro nil))
    
      (defun leuven-kmacro-turn-off-recording ()
        "Stop recording a keyboard macro and toggle functionality of key binding."
        (interactive)
        (global-set-key (kbd "<S-f8>") #'leuven-kmacro-turn-on-recording)
        (kmacro-end-macro nil))
    
      ;; Start/stop recording a keyboard macro.
      (global-set-key (kbd "<S-f8>") #'leuven-kmacro-turn-on-recording)
    
      ;; Execute the most recent keyboard macro.
      (global-set-key (kbd "<f8>") #'kmacro-call-macro)

## Naming and Saving Keyboard Macros<a id="sec-19-2" name="sec-19-2"></a>

-   **`<C-f8>`:** Give the keyboard macro a name (to invoke it under that name).

Assign the last macro to a free key binding of your choice with
`kmacro-bind-to-key` (bound to `C-x C-k b`).

To **save a named keyboard macro** (with its key binding) for future use, switch to
your `.emacs` file, and run `C-u M-x insert-kbd-macro` (if you leave out the `C-u`,
then it will just insert the macro definition without the `global-set-key`
sequence).

You can add `(interactive)` to make it a command, so that you can use it with
`M-x`.

    ;;** 17.5 Name and (info "(emacs)Save Keyboard Macro")s
    
      (leuven--section "17.5 (emacs)Name and Save Keyboard Macros")
    
      ;; Assign a name to the last keyboard macro defined.
      (global-set-key (kbd "<C-f8>") #'kmacro-name-last-macro)

    )                                       ; Chapter 17 ends here.

# Files Handling<a id="sec-20" name="sec-20"></a>

    ;;* 18 (info "(emacs)Files") Handling
    
    (leuven--chapter leuven-load-chapter-18-files "18 Files Handling"

## Visiting Files<a id="sec-20-1" name="sec-20-1"></a>

    ;;** 18.2 (info "(emacs)Visiting") Files
    
      (leuven--section "18.2 (emacs)Visiting Files")
    
      (defadvice find-file (around leuven-find-file activate)
        "Open the file named FILENAME and report time spent."
        (let ((filename (ad-get-arg 0))
              (find-file-time-start (float-time)))
          (message "INFO- Finding file %s..." filename)
          ad-do-it
          (message "INFO- Found file %s in %.2f s" filename
                   (- (float-time) find-file-time-start))))
    
      ;; Visit a file.
      (global-set-key (kbd "<f3>") #'find-file)

## Saving Files<a id="sec-20-2" name="sec-20-2"></a>

You can define a **shadow file** group to automatically upload local files to
a remote location.

    ;;** 18.3 (info "(emacs)Saving") Files
    
      (leuven--section "18.3 (emacs)Saving Files")
    
      (defadvice save-buffer (around leuven-save-buffer activate)
        "Save the file named FILENAME and report time spent."
        (let ((filename (buffer-file-name))
              (save-buffer-time-start (float-time)))
          (message "INFO- Saving file %s..." filename)
          ad-do-it
          (message "INFO- Saved file %s in %.2f s" filename
                   (- (float-time) save-buffer-time-start))))
    
      ;; Make your changes permanent.
      (global-set-key (kbd "<f2>") #'save-buffer)

Builtin file version control of Emacs.

    ;; Make numbered backups.
    (setq version-control t)
    
    ;; Save backup files (i.e., `foo~' or `foo.~i~') in one central location
    ;; (instead of in the local directory).
    (setq backup-directory-alist
          '((".*" . "~/.emacs.d/backups/")))
                                          ; Filenames matching a regexp are backed
                                          ; up in the corresponding directory.
                                          ; Emacs will `make-directory' it, if
                                          ; necessary.
    
    ;; ;; Number of oldest versions to keep when a new numbeRed backup is made.
    ;; (setq kept-old-versions 0)            ; [Default: 2]
    
    ;; Number of newest versions to keep when a new numbered backup is made.
    (setq kept-new-versions 5)            ; [Default: 2]
    
    ;; Don't ask me about deleting excess backup versions.
    (setq delete-old-versions t)
    
    ;; Always use copying to create backup files (don't clobber symlinks).
    (setq backup-by-copying t)

Customize saving actions.

    ;; Ensure newline at the end of file when it is saved.
    (setq require-final-newline t)
    ;; TODO Do this only for text and Fundamental modes, because I could
    ;; edit binary files (see `mode-require-final-newline')

Update time stamps (and copyright notice) automatically.

    ;; Update time stamps every time you save a buffer.
    (add-hook 'before-save-hook #'time-stamp)
    
    ;; Maintain last change time stamps (`Time-stamp: <>' occurring within
    ;; the first 8 lines) in files edited by Emacs.
    (with-eval-after-load "time-stamp"
    
      ;; Format of the string inserted by `M-x time-stamp':
      ;; `YYYY-MM-DD Day HH:MM' (see `system-time-locale' for non-numeric
      ;; formatted items of time).
      (setq-default time-stamp-format "%:y-%02m-%02d %3a %02H:%02M"))
    
    ;; Update the copyright notice to indicate the current year.
    (add-hook 'before-save-hook
              (lambda ()                  ; Except for ...
                (unless (derived-mode-p 'diff-mode)
                                          ; ... where the patch file can't be
                                          ; changed!
                  (copyright-update))))

## Reverting a Buffer<a id="sec-20-3" name="sec-20-3"></a>

    ;;** 18.4 (info "(emacs)Reverting") a Buffer
    
      (leuven--section "18.4 (emacs)Reverting a Buffer")
    
      ;; Replace current buffer text with the text of the visited file on disk.
      (defun leuven-revert-buffer-without-query ()
        "Unconditionally revert current buffer."
        (interactive)
        (revert-buffer t t)                 ; ignore-auto(-save), noconfirm
        ;; Remove less important overlays
        (dolist (o (overlays-in (window-start) (window-end)))
          (when (or (equal (overlay-get o 'face) 'recover-this-file)
                    (equal (overlay-get o 'face) 'highlight-changes)
                    (equal (overlay-get o 'face) 'highlight-changes-delete)
                    (equal (overlay-get o 'face) 'org-block-executing))
            (delete-overlay o)))            ; Useful when our advice of function
                                            ; `org-babel-execute-src-block' fails to
                                            ; remove the background color.
        (message "Buffer is up to date with file on disk"))
    
      ;; Key binding.
      (global-set-key (kbd "<C-f12>") #'leuven-revert-buffer-without-query)

Turn on auto-revert mode globally.

    (when leuven--cygwin-p                ; Cygwin Emacs uses gfilenotify (based
                                          ; on GLib) and there are performance
                                          ; problems... Emacs bug #20927
    
      ;; Don't use file notification functions.
      (setq auto-revert-use-notify nil))
    
    ;; Enable Global Auto-Revert mode (auto refresh buffers).
    (global-auto-revert-mode 1)           ; Can generate a lot of network traffic
                                          ; if `auto-revert-remote-files' is set
                                          ; to non-nil.

Auto-revert will automatically reload files that have changed outside of Emacs.
It won&rsquo;t revert a buffer that you&rsquo;ve edited inside Emacs, even if the file also
changes outside.

<div class="warning">
`global-auto-revert-mode` blocks Cygwin Emacs for more than 5 seconds at many
points in time. Disabled for now, under Cygwin.

</div>

## Auto Reverting Non-File Buffers<a id="sec-20-4" name="sec-20-4"></a>

Auto-refresh Dired.

    ;; Global Auto-Revert mode operates on all buffers (Dired, etc.)
    (setq global-auto-revert-non-file-buffers t)
    
    ;; Do not generate any messages (be quiet about refreshing Dired).
    (setq auto-revert-verbose nil)        ; Avoid "Reverting buffer `some-dir/'.".

## Auto-Saving: Protection Against Disasters<a id="sec-20-5" name="sec-20-5"></a>

How to get Emacs to auto-save to your local disk (`#file#`).

    ;;** 18.6 (info "(emacs)Auto Save"): Protection Against Disasters
    
      (leuven--section "18.6 (emacs)Auto Save: Protection Against Disasters")
    
      ;; Auto-save every 100 input events.
      (setq auto-save-interval 100)         ; [Default: 300].
    
      ;; Auto-save after 10 seconds idle time.
      (setq auto-save-timeout 10)           ; [Default: 30].

Disable backups and auto-save only in the current buffer with
`M-x sensitive-mode`:

    (define-minor-mode sensitive-mode
      "For sensitive files like password lists.
    It disables backup creation and auto saving in the current buffer.
    
    With no argument, this command toggles the mode.  Non-null prefix argument
    turns on the mode.  Null prefix argument turns off the mode."
      nil                                 ; Initial value.
      " Sensitive"                        ; Indicator for the mode line.
      nil                                 ; Minor mode bindings.
      (if (symbol-value sensitive-mode)
          (progn
            ;; Disable backups.
            (set (make-local-variable 'backup-inhibited) t)
            ;; Disable auto-save.
            (if auto-save-default
                (auto-save-mode -1)))
        ;; Resort to default value of backup-inhibited.
        (kill-local-variable 'backup-inhibited)
        ;; Resort to default auto save setting.
        (if auto-save-default
            (auto-save-mode 1))))

You should enable it for all `.vcf` and `.gpg` files.

Make the message &ldquo;*FILENAME has auto save data*&rdquo; unmissable:

    (defface recover-this-file
      '((t (:weight bold :background "#FF3F3F")))
      "Face for buffers visiting files with auto save data."
      :group 'files)
    
    (defvar leuven--recover-this-file nil
      "If non-nil, an overlay indicating that the visited file has auto save data.")
    
    (defun leuven--recover-this-file ()
      (let ((warn (not buffer-read-only)))
        (when (and warn
                   ;; No need to warn if buffer is auto-saved under the name of
                   ;; the visited file.
                   (not (and buffer-file-name
                             auto-save-visited-file-name))
                   (file-newer-than-file-p (or buffer-auto-save-file-name
                                               (make-auto-save-file-name))
                                           buffer-file-name))
          (set (make-local-variable 'leuven--recover-this-file)
               (make-overlay (point-min) (point-max)))
          (overlay-put leuven--recover-this-file
                       'face 'recover-this-file))))
    
    (add-hook 'find-file-hook #'leuven--recover-this-file)

## Comparing Files<a id="sec-20-6" name="sec-20-6"></a>

    ;;** 18.9 (info "(emacs)Comparing Files")
    
      (leuven--section "18.9 (emacs)Comparing Files")
    
      ;; Default to unified diffs.
      (setq diff-switches "-u")
    
      ;; Compare text in current window with text in next window.
      (global-set-key (kbd "C-=") #'compare-windows)

You can use `M-x smerge-mode` to edit a file with conflict markers (output from
the `diff3` program).  Smerge does not automatically select regions but provides
convenient key bindings to navigate between conflicts and to choose the A or B
variant.

## Diff mode<a id="sec-20-7" name="sec-20-7"></a>

You can review the diff of all file changes, either:

-   **unified** (Diff mode) or
-   **split** (Ediff mode).

In Diff mode, the changes specified in a patch are grouped into &ldquo;hunks&rdquo;, which
are contiguous chunks of text that contain one or more changed lines.

Use `C-x v =` to show what&rsquo;s changed.

You can also decide to revert a change: **select a hunk and apply it (in reverse)**.

    ;;** 18.10 (info "(emacs)Diff Mode")
    
      (leuven--section "18.10 (emacs)Diff Mode")
    
      ;; Mode for viewing/editing context diffs.
      (with-eval-after-load "diff-mode"
    
        ;; Highlight the changes with better granularity.
        (defun leuven-diff-make-fine-diffs ()
          "Enable Diff Auto-Refine mode."
          (interactive)
          (let (diff-auto-refine-mode)      ; Avoid refining the hunks redundantly ...
            (condition-case nil
                (save-excursion
                  (goto-char (point-min))
                  (while (not (eobp))
                    (diff-hunk-next)
                    (diff-refine-hunk)))    ; ... when this does it.
              (error nil))
            (run-at-time 0.0 nil
                         (lambda ()
                           (if (derived-mode-p 'diff-mode)
                               ;; Put back the cursor only if still in a Diff buffer
                               ;; after the delay.
                               (goto-char (point-min)))))))
    
        (defun leuven--diff-make-fine-diffs-if-necessary ()
          "Auto-refine only the regions of 14,000 bytes or less."
          ;; Check for auto-refine limit.
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

To compare 2 text files, you can also use Ediff if the following command
works.

    M-: (executable-find "diff") RET

In Ediff, press:

-   `?` to get help

-   `|` to change from vertical window layout to horizontal window layout, and
    vice versa

-   `a` or `b` key to merge the code from A to B or from B to A

-   `ra` or `rb` to revert your change

    ;; ;; Ediff, a comprehensive visual interface to diff & patch
    ;; ;; setup for Ediff's menus and autoloads
    ;; (try-require 'ediff-hook)
    ;; already loaded (by Emacs?)
    
    (with-eval-after-load "ediff"
    
      ;; Ignore space.
      (setq ediff-diff-options (concat ediff-diff-options " -w"))
                                          ; Add new options after the default ones.
    
      ;; Skip over difference regions that differ only in white space and line
      ;; breaks.
      ;; (setq-default ediff-ignore-similar-regions  t)
      ;; XXX Make another key binding (than `E') with that value in a let-bind
    
      ;; Sometimes grab the mouse and put it in the control frame.
      (setq ediff-grab-mouse 'maybe)
    
      ;; Do everything in one frame.
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    
      ;; Split the window (horizontally or vertically) depending on the frame
      ;; width.
      (setq ediff-split-window-function
            (lambda (&optional arg)
              (if (> (frame-width) split-width-threshold)
                  (split-window-horizontally arg)
                (split-window-vertically arg))))
    
      ;; (setq ediff-merge-split-window-function 'split-window-vertically)
    
      (defun turn-on-visible-mode ()
        "Make all invisible text temporarily visible."
        (visible-mode 1)
        (setq truncate-lines nil)
        (when (derived-mode-p 'org-mode)
          (org-remove-inline-images)))
    
      ;; Force the buffers to unhide (folded) text (in Org files).
      (add-hook 'ediff-prepare-buffer-hook #'turn-on-visible-mode)
    
      (defun turn-off-visible-mode ()
        "Disable Visible mode."
        (visible-mode 0)
        (setq truncate-lines t)
        (when (derived-mode-p 'org-mode)
          (org-display-inline-images)))
    
      (add-hook 'ediff-quit-hook #'turn-off-visible-mode)
    
      )

For **comparing two directories**, you can use `ediff-directories` or [Meld](http://meldmerge.org/).

`ediff-directories` combined with the key sequence `= h x` `= h x` shows you only the
files which differ.

It is not recursive, but it gives you also subdirectory pairs to compare.  Just
hit `RET` on such a pair.

As bonus, it even works for remote directories.

<div class="warning">
`ediff-directories` does not list files in one directory with no corresponding
files in the other directory.

</div>

## Miscellaneous File Operations<a id="sec-20-8" name="sec-20-8"></a>

    ;;** 18.11 (info "(emacs)Misc File Ops")
    
      (leuven--section "18.11 (emacs)Misc File Ops")
    
      ;; Use the system's Trash (when it is available).
      (setq delete-by-moving-to-trash t)

## Accessing Compressed Files<a id="sec-20-9" name="sec-20-9"></a>

-   Display the **contents** of the archive
-   **View or edit** the actual files contained withing the archive

Using the Emacs Dired utility, you can compress or uncompress marked files
(using `gzip`) by pressing `Z`.

If you want to create one **archive** file from marked files, you can use:

    ! tar -cf out.tar *

And if you actually want to **compress** the resulting archive, use one of the two
commands:

    ! tar -cZf out.tar.Z
    ! tar -czf out.tar.gz

## Auto Encryption<a id="sec-20-10" name="sec-20-10"></a>

History:

1.  mailcrypt

2.  PGG

3.  EasyPG (= epg) is a GnuPG interface for Emacs.
    
    It allows you to encrypt/decrypt files within Emacs.  When you use `C-x C-f` to
    access an encrypted file, Emacs prompts you for the passphrase and then
    decrypts the file before displaying it.  When you save the file, Emacs
    automatically encrypts it again with that same key.
    
    It has two aspects:
    
    -   convenient tools which allow to use GnuPG from Emacs (EasyPG Assistant),
        and
    
    -   a fully functional interface library to GnuPG (EasyPG Library).
        
        <div class="inlinetask">
        <b><span class="todo TODO">TODO</span> Fetch the key automatically</b><br  />
        Set the `keyserver` entry in the `~/.gnupg/gpg.conf` file properly.
        </div>

What is PGP/MIME, what is OpenPGP, and how Gnus handles them?
-   PGP/MIME is a standard, which mml2015\* implements using ep[ag]-\*.
-   OpenPGP is a standard, which ep[ag]-\* implements.

    ;; The EasyPG Assistant, transparent file encryption.
    (with-eval-after-load "epa-file"
    
      ;; Stop EasyPG from asking for the recipient used for encrypting files.
      (setq epa-file-encrypt-to "johndoe@example.com")
                                          ; If no one is selected (""), symmetric
                                          ; encryption will always be performed.
    
      ;; Cache passphrase for symmetric encryption (VERY important).
      (setq epa-file-cache-passphrase-for-symmetric-encryption t)
                                          ; Not to sound paranoid.  But if you
                                          ; want caching, it's recommended to use
                                          ; *public-key encryption* instead of
                                          ; symmetric encryption.  `gpg-agent' is
                                          ; the preferred way to do this.
    
      ;; Prompt for the password in the Emacs minibuffer (instead of using a
      ;; graphical password prompt for GPG).
      (setenv "GPG_AGENT_INFO" nil))

## Remote Files<a id="sec-20-11" name="sec-20-11"></a>

    ;;** 18.14 (info "(emacs)Remote Files")
    
      (leuven--section "18.14 (emacs)Remote Files")

### Ange-FTP<a id="sec-20-11-1" name="sec-20-11-1"></a>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Check out why network share aren&rsquo;t accessible</b><br  />
I should be able to open */SERVER/d* via SMB!?
</div>

    ;;*** Ange-FTP
    
      (leuven--section "Ange-FTP")
    
      ;; Transparent FTP support.
      (with-eval-after-load "ange-ftp"
    
        ;; Try to use passive mode in ftp, if the client program supports it.
        (setq ange-ftp-try-passive-mode t)) ; Needed for Ubuntu.

### TRAMP<a id="sec-20-11-2" name="sec-20-11-2"></a>

Open files on a remote machine using [TRAMP](http://www.gnu.org/software/tramp/) (other protocols than just FTP) from
your machine, edit and save them!

`/method:user@host:/path/to/some/file`

Examples:
-   `C-x C-f /ssh:user@host:/etc/motd RET` (`root` to modify it!)
-   `C-x C-f /plink:user@host:/etc/motd RET` (from Windows Emacs)
-   `C-x C-f /sudo:root@localhost:/etc/motd RET`
-   `C-x C-f /su::/etc/motd RET` (shortened syntax for the `root` account on the
    local host)

Also `M-x find-dired`, `rgrep`, `lgrep`, `compile -` properly work with TRAMP (run on
remote server and show result in your Emacs).

<div class="note">
`sshfs` can give you the same functionality as TRAMP: it is like a personal NFS
(another mounted file system) over SSH.  If you can SSH to a server, you can
probably do `sshfs`.

</div>

    ;;*** TRAMP - Transparent Remote Access, Multiple Protocols
    
      (leuven--section "TRAMP")
    
      (with-eval-after-load "tramp"         ; The autoloads are predefined.

#### TODO Configuring TRAMP for use<a id="sec-20-11-2-1" name="sec-20-11-2-1"></a>

-   Selecting a default method

    <div class="note">
    As there are **issues with Cygwin `ssh`** (which only works with Cygwinized Emacs),
    **Windows users** should use the PuTTY implementation of SSH (`plink` method).
    
    </div>
    
        ;; Default transfer method.
        (setq tramp-default-method          ; [Default: "scp"]
              (cond (leuven--win32-p "plink")
                    (t "ssh")))
    
    <div class="tip">
    You might try out the `rsync` *external* method, which saves the remote files quite
    a bit faster than SSH.  It&rsquo;s based on SSH, so it works the same, just saves
    faster.
    
    </div>

-   TODO Find file as root

    You can just do `C-x C-f /sudo:user@localhost:/etc/motd RET`.
    
    Because I&rsquo;m lazy, I have a hack (from Tassilo Horn): if I try to open a file
    for which I don&rsquo;t have permissions for, I&rsquo;m queried if I want to open it as
    root using the `sudo` tramp method.
    
        (defun leuven--find-file-sudo-header-warning ()
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
          (leuven--find-file-sudo-header-warning))
        
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

-   Connecting to a remote host using multiple hops

    New proxy system (introduced in 2004, instead of the old &ldquo;multi-hop&rdquo; methods) to
    edit files on a remote server by going via another server.
    
        ;; Route to be followed for specific host/user pairs.
        (add-to-list 'tramp-default-proxies-alist
                     ;;  "final host"    "user"    "proxy in the middle"
                     '("10.10.13.123" "\\`root\\'" "/ssh:%h:"))
    
    Opening `/sudo:10.10.13.123:` would connect first `10.10.13.123` via `ssh` under your
    account name, and perform `sudo -u root` on that host afterwards.  It is important
    to know that the given method is applied on the host which has been reached so
    far.  The trick is to think from the end.

-   Reusing passwords for several connections

        ;; How many seconds passwords are cached.
        (setq password-cache-expiry 60)     ; [Default: 16]

-   Remote shell setup hints

    It is worth noting that you can customize the prompt that tramp expects
    (`tramp-login-prompt-regexp`) but my advice would rather be to make your prompt
    TRAMP-compatible and not the other way around.
    
    **The author of TRAMP doesn&rsquo;t think this ever needs to be changed, so please tell
    him about it if you need to change this.**
    
        ;; String used for end of line in rsh connections.
        (setq tramp-rsh-end-of-line         ; [Default: "\n"]
              (cond (leuven--win32-p "\n")
                    (t "\r")))

-   Auto-save and Backup configuration

        ;; "Turn off" the effect of `backup-directory-alist' for TRAMP files.
        (add-to-list 'backup-directory-alist
                     (cons tramp-file-name-regexp nil))
        
        ;; Faster auto saves.
        (setq tramp-auto-save-directory temporary-file-directory)

#### Frequently Asked Questions<a id="sec-20-11-2-2" name="sec-20-11-2-2"></a>

Be notified when TRAMP file transfers are complete by making Emacs beep after
reading from or writing to the remote host.

    (defadvice tramp-handle-write-region
      (after leuven-tramp-write-beep-advice activate)
      "Make TRAMP beep after writing a file."
      (interactive)
      (beep))
    
    (defadvice tramp-handle-do-copy-or-rename-file
      (after leuven-tramp-copy-beep-advice activate)
      "Make TRAMP beep after copying a file."
      (interactive)
      (beep))
    
    (defadvice tramp-handle-insert-file-contents
      (after leuven-tramp-insert-beep-advice activate)
      "Make TRAMP beep after inserting contents of a file."
      (interactive)
      (beep))

#### How to Customize Traces<a id="sec-20-11-2-3" name="sec-20-11-2-3"></a>

The usual way to debug TRAMP is to set `tramp-verbose` to 6.  This will produce a
debug buffer, which you can show at `tramp-devel@gnu.org`.

    ;; Debugging TRAMP.
    (setq tramp-verbose 6))             ; [Maximum: 10]

The best way to report a TRAMP bug is to call `M-x tramp-bug`; this prepares an
email, including several trace information for analysis.

## Convenience Features for Finding Files<a id="sec-20-12" name="sec-20-12"></a>

    ;;** 18.17 (info "(emacs)File Conveniences")
    
      (leuven--section "18.17 (emacs)File Conveniences")
    
      ;; Filenames excluded from the recent list.
      (setq recentf-exclude                 ; Has to be set before you require
                                            ; `recentf'!
            '(
              ".recentf"
              "~$"                          ; Emacs (and others) backup.
              "\\.aux$" "\\.log$" "\\.toc$" ; LaTeX.
              "/tmp/"
              ))
    
      ;; Setup a menu of recently opened files.
      (idle-require 'recentf)
    
      (with-eval-after-load "recentf"
    
        ;; Maximum number of items that will be saved.
        (setq recentf-max-saved-items 100)  ; Just 20 is too recent.
    
        ;; File to save the recent list into.
        (setq recentf-save-file (concat user-emacs-directory ".recentf"))
    
        ;; (When using TRAMP) turn off the cleanup feature of `recentf'.
        (setq recentf-auto-cleanup 'never)  ; Disable before we start recentf!
    
        ;; Save file names relative to my current home directory.
        (setq recentf-filename-handlers '(abbreviate-file-name))
    
        ;; Enable `recentf' mode.
        (recentf-mode 1))

### Helm<a id="sec-20-12-1" name="sec-20-12-1"></a>



#### What is Helm?<a id="sec-20-12-1-1" name="sec-20-12-1-1"></a>

Helm (successor of Anything) is a incremental completion and selection narrowing
framework for Emacs.

It will help **steer** you in the right direction when you&rsquo;re looking for stuff in
Emacs (like buffers, files, etc).  You don&rsquo;t have anymore to remember the
structure of your working directory; you can focus on more important things.

#### How does it work?<a id="sec-20-12-1-2" name="sec-20-12-1-2"></a>

The *default* prefix for Helm commands is `C-x c`.  In Emacs Leuven, we changed it
to `C-c h`.

In Helm, generally, look at the **mode line**: you will see `C-c ?:Help`, it is your
friend; hit it for more info.

-   Narrow the list by typing some patterns (use the **space as separator** for
    multiple patterns),

-   Select an element with `<Up>` / `<Down>` / `<PageUp>` / `<PageDown>` / `C-p` / `C-n` /
    `C-v` / `M-v`,

-   Choose with `RET`.

-   `C-z` executes an action without quitting the Helm session (persistent action).
    
    <div class="note">
    For `helm-find-files`:
    
    -   On a regular file,
        1.  First hit on `C-z` expands the file name
        2.  Second hit opens the file and displays its contents in the other window
        3.  Third hit kills the buffer (unless it was already open before starting
            helm session).
    
    -   On image files (`.jpg`, etc..)
        1.  Second hit (or `C-u C-z`) displays the image in the other window
        2.  If you then turn on `helm-follow-mode` (`C-c C-f`), you turn on Helm in
            **image browser** (i.e., use `C-n/p` or arrows `down/up`).
        3.  You can rotate image with `M-l/r` (these are persistent actions too).
    
    </div>

Kill your buffers with `C-u C-z` one by one.

To go back to the **previous element** of the minibuffer history, use `M-p`.

To mark all candidates in a Helm buffer, use `M-a` (`helm-mark-all`) or `M-m`
(`helm-toggle-all-marks`).

Switch to `*Helm Log*` buffer with `M-x helm-open-last-log`.

#### Changes in key bindings<a id="sec-20-12-1-3" name="sec-20-12-1-3"></a>

In `helm-find-file`, you may want to try using the left arrow to go to the parent
directory!

You can reassign `TAB` only during \`helm-find-file&rsquo;.  The
`helm-find-file` almost looks like Dired and I assign `^` for going
up.  Minor side effect is you cannot type in `^` without using `C-q`.

Sometimes you want to hit `TAB` for \`helm-selection-action&rsquo;; however,
you chose not to happen.  Now look for substitute for `TAB`.
Something not `TAB` can be `C-z`, `C-TAB`, and `SPC`.

    (define-key helm-find-files-map (kbd "<C-tab>") #'helm-select-action)
    (define-key helm-find-files-map (kbd "<SPC>") #'helm-select-action)
    (define-key helm-find-files-map (kbd "<RET>") #'helm-confirm-and-exit-minibuffer)
    (define-key helm-find-files-map (kbd "^") #'helm-find-files-up-one-level)

    (define-key helm-grep-mode-map (kbd "<RET>") #'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n") #'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p") #'helm-grep-mode-jump-other-window-backward)

#### Helm documentation<a id="sec-20-12-1-4" name="sec-20-12-1-4"></a>

-   **Advanced usage** on [Emacs-helm](https://github.com/emacs-helm/helm#advanced-usage) page
-   [Emacs-helm wiki](https://github.com/emacs-helm/helm/wiki)
-   [Helm configuration file of Thierry Volpiatto](https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el)!
-   <http://tuhdo.github.io/helm-intro.html>

Helm allows you to do interesting things with its interactive interface. For
example:

-   interactive grep: ![img](//tuhdo.github.io/static/live_grep.gif)
-   interactive occur: ![img](//tuhdo.github.io/static/part3/helm-occur.gif)

#### Vocabulary<a id="sec-20-12-1-5" name="sec-20-12-1-5"></a>

-   `volatile` means your candidates are recomputed each time you enter a character
    in pattern (cached candidates are not reused).  Async sources are by essence
    volatile.

-   `delayed` means your candidates are computed after `helm-input-idle-delay`
      seconds.

-   `no-matchplugin` means to not use the multi regexp matching provided by
    `helm-match-plugin.el`.

#### Features<a id="sec-20-12-1-6" name="sec-20-12-1-6"></a>

Finally, the strings in Helm are not just strings; they are **regexps** and I think
they are more powerful then normal fuzzy matcher.

Grep in Helm is interactive, unlike the stock grep/rgrep. Demo:
![img](//tuhdo.github.io/static/live_grep.gif)

No package does any of this:

-   Live grep: ![img](//tuhdo.github.io/static/live_grep.gif)
-   A live outline tree for jumping to function/variable in current file:

-   Demo 1: ![img](//tuhdo.github.io/static/part3/helm-semantic-or-imenu.gif)
-   Demo 2:

![img](//cloud.githubusercontent.com/assets/4818719/4102208/cda8f392-311e-11e4-9c83-e68df38ef68e.gif)

-   Interactively select color and faces: ![img](//tuhdo.github.io/static/part3/helm-color.gif)

-   Interactive Eshell history, certainly superior to the stock C-c C-l:

![img](//tuhdo.github.io/static/part3/helm-eshell-history.gif) . I store a large
amount of eshell/shell history, so I want a quick way to narrow down.

XXX Helm is also very useful to look up Elisp variable/function documentation
and for finding stuff in the Info Manuals (e.g. helm-info-elisp or
helm-info-emacs).

XXX Look at helm-dictionary.

    (leuven--section "Helm")
    
    ;; Change `helm-command-prefix-key'.
    (global-set-key (kbd "C-c h") #'helm-command-prefix)
    
    ;; Open Helm (QuickSilver-like candidate-selection framework).
    (when (try-require 'helm-config)      ; [default `helm-command-prefix-key']
                                          ; Explicitly loads `helm-autoloads'!
                                          ; CAUTION for recursive loads...
    
      (global-unset-key (kbd "C-x c"))
    
      (global-set-key (kbd "C-M-z") #'helm-resume)

    ;; Via: http://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
    (setq helm-echo-input-in-header-line t)
    ;; (defun helm-hide-minibuffer-maybe ()
    ;;   (when (with-helm-buffer helm-echo-input-in-header-line)
    ;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
    ;;       (overlay-put ov 'window (selected-window))
    ;;       (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
    ;;                               `(:background ,bg-color :foreground ,bg-color)))
    ;;       (setq-local cursor-type nil))))
    ;;
    ;; (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    ;; Better version of `occur'.
    (global-set-key (kbd "C-o") #'helm-occur) ; helm-regexp.el
    (global-set-key (kbd "C-c o") #'helm-occur) ; helm-regexp.el

    (global-set-key (kbd "M-x") #'helm-M-x)

    ;; Speedy file opening.
    (global-set-key (kbd "<f3>") #'helm-for-files)
    
    ;; (global-set-key (kbd "C-x C-f") #'helm-find-files) ; OK.

    ;; Buffer list.
    (global-set-key (kbd "C-x b") #'helm-mini) ; OK.
                                        ; = `helm-buffers-list' + recents.
    
    (global-set-key (kbd "C-x C-b") #'helm-buffers-list) ; OK.

    ;; `dabbrev-expand' (M-/) =>`helm-dabbrev'

    (global-set-key (kbd "C-x r l") #'helm-bookmarks) ; Instead of `bookmark-jump'.
    (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks) ; XXX?

    ;; Install from https://github.com/thierryvolpiatto/emacs-bmk-ext.
    (global-set-key (kbd "C-x r b") #'helm-bookmark-ext)

    (defun leuven-helm-org-prog-menu (arg)
      "Jump to a place in the buffer using an Index menu.
    For Org mode buffers, show Org headlines.
    For programming mode buffers, show functions, variables, etc."
      (interactive "P")
      (cond ((derived-mode-p 'org-mode) (helm-org-in-buffer-headings))
            ((derived-mode-p 'tex-mode) (helm-imenu))
            (t (helm-semantic-or-imenu arg)))) ; More generic than `helm-imenu'.
    
    (global-set-key (kbd "<f4>") #'leuven-helm-org-prog-menu) ; Awesome.
                                        ; And `C-c =' (like in RefTeX)?
    
    (global-set-key (kbd "C-c o") #'helm-org-agenda-files-headings)

    (global-set-key (kbd "M-y") #'helm-show-kill-ring) ; OK.
    ;; (global-set-key (kbd "C-h SPC") #'helm-all-mark-rings)
    (global-set-key (kbd "C-c m") #'helm-all-mark-rings)

    ;; (global-set-key (kbd "M-5") #'helm-etags-select)

    (global-set-key (kbd "C-h a") #'helm-apropos) ; OK!

    (global-set-key (kbd "C-h i") #'helm-info-emacs) ; OK.
    ;; (global-set-key (kbd "C-h d") #'helm-info-at-point)
    ;; (global-set-key (kbd "C-h 4") #'helm-info-elisp)

    ;; (global-set-key (kbd "C-S-h C-c") #'helm-wikipedia-suggest)

    (global-set-key (kbd "C-h b") #'helm-descbinds) ; OK.

See helm-flycheck!

    )                                     ; require 'helm-config ends here.

    (with-eval-after-load "helm"
    
      ;; Rebind TAB to do persistent action.
      (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
                                          ; Make TAB works in terminal.
      ;; List actions using C-z.
      (define-key helm-map (kbd "C-z") #'helm-select-action)
    
      (define-key helm-map (kbd "C-M-n") #'helm-next-source)
      (define-key helm-map (kbd "C-M-p") #'helm-previous-source)
    
      ;; Ace-Jump to a candidate line in Helm window.
      (define-key helm-map (kbd "@") 'ace-jump-helm-line)
    
      ;; Various functions for Helm (Shell history, etc.).
      (require 'helm-misc)
      ;; For multi-line items in e.g. minibuffer history, match entire items,
      ;; not individual lines within items.
    
      ;; (try-require 'helm-dictionary)
    
      ;; Use the *current window* (no popup) to show the candidates.
      (setq helm-full-frame nil)
    
      ;; Open `helm-buffer' in another window.
      (setq helm-split-window-default-side 'other)
    
      ;; Default function used for splitting window.
      (setq helm-split-window-preferred-function
            (lambda (window)
              (split-window-sensibly)))
    
      ;; ;; Move to end or beginning of source when reaching top or bottom of
      ;; ;; source.
      ;; (setq helm-move-to-line-cycle-in-source t)
    
      ;; Candidates separator of `multiline' source (such as
      ;; `helm-show-kill-ring').
      (setq helm-candidate-separator
            "--8<-----------------------separator------------------------>8---")
    
      ;; Suppress displaying sources which are out of screen at first.
      (setq helm-quick-update t)
    
      ;; ;; Time that the user has to be idle for, before candidates from
      ;; ;; DELAYED sources are collected.
      ;; (setq helm-idle-delay 0.01)
    
      ;; Time that the user has to be idle for, before ALL candidates are
      ;; collected (>= `helm-idle-delay') -- also effective for NON-DELAYED
      ;; sources.
      (setq helm-input-idle-delay 0.1)    ; 0.06 OK
    
      ;; ;; Enable adaptive sorting in all sources.
      ;; (helm-adaptive-mode 1)
    
      ;; ;; Enable generic Helm completion (for all functions in Emacs that use
      ;; ;; `completing-read' or `read-file-name' and friends).
      ;; (helm-mode 1)
      )

#### Helm find files<a id="sec-20-12-1-7" name="sec-20-12-1-7"></a>

`helm-find-files` is powerful because:

-   It has **fuzzy matching** by default (well, not enabled by default).

-   It has highlighting on candidates depend on types: directory has a color,
    plain file has a color, executable has a color, shell script has a color.

-   Depends on what file type your highlighting candidate is, Helm opens it
    appropriately. For example, if you RET on a directory, Helm opens dired; if
    you RET on a file, Helm opens that file. You can also customize the
    application to open the files. Stock find-file at least can open directory;
    Ido simply can&rsquo;t.

-   You can open multiple files with helm-find-files by marking with C-SPC and
    press RET. If you want to everything at current directory, you can use M-a to
    mark all then RET. You can even use wildcard expression like stock find-file

-   Helm has a list of actions to apply on your files: checksum file, open file
    externally, switch to eshell, grep, diff&#x2026;

In helm-find-files, you can alway invoke helm-ff-do-grep with C-s or C-u C-s for
recursive searching. Probably helm-projectile can replace helm-find-files more
if it has this feature? I think you can do this by reusing projectile-grep.

helm-find-files can even find file at point. That is, you don&rsquo;t need to remember
ffap as well. If you write Emacs lisp, move point over the required file and
execute helm-find-files, it will prompt the file location and you simply RET to
enter. All in once nice package.


-   **`C-l`:** Delete the last segment of a file name (in the minibuffer).

    (with-eval-after-load "helm-files"
    
      ;; Don't show only basename of candidates in `helm-find-files'.
      (setq helm-ff-transformer-show-only-basename nil)
    
      ;; Search for library in `require' and `declare-function' sexp.
      (setq helm-ff-search-library-in-sexp t)
    
      ;; ;; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
      ;; (setq helm-ff-file-name-history-use-recentf t)
      )

    ;; This set Helm to open files using designated programs.
    (setq helm-external-programs-associations
          '(("rmvb" . "smplayer")
            ("mp4" . "smplayer")))
    
    ;; Set the warning threshold to 500 MB, which will get ride of "File abc.mp4 is
    ;; large (330.2M), really open? (y or n)" annoying message.
    (setq large-file-warning-threshold 500000000)

See [Sublime Text 2&rsquo;s &ldquo;Goto Anything&rdquo; (or instant search) for Emacs?](http://stackoverflow.com/questions/14726601/sublime-text-2s-goto-anything-or-instant-search-for-emacs)
-   <https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-completion.org>

#### Descbinds<a id="sec-20-12-1-8" name="sec-20-12-1-8"></a>

Use the separate Helm Descbinds to get a quick **key bindings** overview.  It will
make your life much easier with million Emacs keys.

    ;; A convenient `describe-bindings' with `helm'.
    (with-eval-after-load "helm-descbinds"
    
      ;; Window splitting style.
      (setq helm-descbinds-window-style 'split-window))

#### Grep<a id="sec-20-12-1-9" name="sec-20-12-1-9"></a>

Launchable from `current-buffer` or from `helm-find-files`.

    ;;
    (with-eval-after-load "helm-grep-autoloads"
    
        (global-set-key (kbd "M-g ,") #'helm-do-grep)
    
        (global-set-key (kbd "M-g a") #'helm-do-grep-ag) ; Thierry Volpiatto
                                          ; Or `C-c p s s' (Helm-projectile ag?)
        )

Keymap:

`helm-ag-map` and `helm-do-ag-map` are inherited by `helm-map`.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Action</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-c o</td>
<td class="left">Open other window</td>
</tr>


<tr>
<td class="left">C-l</td>
<td class="left">Search in parent directory</td>
</tr>


<tr>
<td class="left">C-c C-e</td>
<td class="left">Switch to edit mode</td>
</tr>


<tr>
<td class="left">C-x C-s</td>
<td class="left">Save ag results to buffer(Ask save buffer name if prefix key is specified)</td>
</tr>


<tr>
<td class="left">C-c C-f</td>
<td class="left">Enable helm-follow-mode</td>
</tr>


<tr>
<td class="left">C-c > , right</td>
<td class="left">Move to next file</td>
</tr>


<tr>
<td class="left">C-c < , left</td>
<td class="left">Move to previous file</td>
</tr>


<tr>
<td class="left">C-c ?</td>
<td class="left">Show help message</td>
</tr>
</tbody>
</table>

    ;; the_silver_searcher.
    (when (executable-find "ag")
    
      ;; The silver searcher with Helm interface.
      (with-eval-after-load "helm-ag-autoloads"
    
        (global-set-key (kbd "C-c s") #'helm-ag)
        (global-set-key (kbd "M-s s") #'helm-ag)
    
        ;; Search with Ag from project root.
        (global-set-key (kbd "C-S-r") #'helm-do-ag-project-root)
    
        ;; Search with Ag.  Ask for directory first.
        (global-set-key (kbd "C-S-d") 'helm-do-ag)
    
        ;; Search with Ag this file (like Swoop).
        (global-set-key (kbd "C-S-f") #'helm-ag-this-file)
        (global-set-key (kbd "M-g >") #'helm-ag-this-file)
    
        ;; Search with Ag in current projectile project.
        (global-set-key (kbd "C-S-a") 'helm-projectile-ag)
    
        (global-set-key (kbd "M-g ,") #'helm-ag-pop-stack)
        ))
    
    (with-eval-after-load "helm-ag"
    
      ;; Base command of `ag'.
      (setq helm-ag-base-command (concat helm-ag-base-command " --ignore-case"))
    
      ;; Command line option of `ag'
      (setq helm-ag-command-option "--all-text")
    
      ;; Insert thing at point as search pattern.
      (setq helm-ag-insert-at-point 'symbol))

See <https://github.com/dingmingxin/dotfiles/blob/master/config_emacs/config/custom-init-helm.el>

#### Helm M-x (search for a command)<a id="sec-20-12-1-10" name="sec-20-12-1-10"></a>

There&rsquo;s no denying it: it&rsquo;s really difficult to remember all of these various
commands. Rather than falling back to using the mouse and menu selections,
instead type `M-x`.

This will bring up a Helm menu, where you can then search for your desired
command.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Get gray for common text of candidates</b><br  />
See <http://code.tutsplus.com/tutorials/essential-textmate-shortcuts-tips-and-techniques--net-21168>
</div>

<div class="note">
With `helm-M-x`, to pass prefix arguments to the command you want to run, you have
to type `C-u` **after** typing `M-x` and selecting the command &#x2013; that is, **before**
pressing `RET`.

</div>

    (with-eval-after-load "helm-command"
    
      ;; Save command even when it fails (on errors).
      (setq helm-M-x-always-save-history t))

#### Helm locate<a id="sec-20-12-1-11" name="sec-20-12-1-11"></a>

Use Helm to **quickly navigate between files**, even in the largest projects.

<div class="note">
Locate for **Windows** users: you have to install [Everything](http://www.voidtools.com/download.php) with its command line
interface `es.exe`.

</div>

    ;; (with-eval-after-load "helm-autoloads"
    ;;   (global-set-key [remap locate] #'helm-locate))
    
    (with-eval-after-load "helm-locate"
    
      (when (and (or leuven--win32-p leuven--cygwin-p)
                 (executable-find "es"))
    
        ;; Sort locate results by full path.
        (setq helm-locate-command "es -s %s %s")))

#### Helm for buffers<a id="sec-20-12-1-12" name="sec-20-12-1-12"></a>

As a starting point for all **searches in buffers and files**, use `helm-buffers-list`
and `helm-find-files`: you can launch `helm-multi-occur` and `helm-do-grep` from
there.

You can also use the `@` prefix to **search buffer contents**.

    (with-eval-after-load "helm-buffers"
    
      ;; Don't truncate buffer names.
      (setq helm-buffer-max-length nil)
    
      ;; Never show details in buffer list.
      (setq helm-buffer-details-flag nil)
    
      ;; String to display at end of truncated buffer names.
      (setq helm-buffers-end-truncated-string "…"))

    ;; (with-eval-after-load "helm-adaptive"
    ;;
    ;;   ;; don't save history information to file
    ;;   (remove-hook 'kill-emacs-hook 'helm-adaptive-save-history))

#### Helm ring<a id="sec-20-12-1-13" name="sec-20-12-1-13"></a>

By pressing `M-y`, you will see the **clipboard history** (list of all previous
clippings) and can pick the one you want to paste using arrow keys.  Use `RET` to
insert it.

`helm-all-mark-rings` allows you to view the content of the both the **local and
global mark rings** in a friendly interface, so you can always jump back to where
you were.

    ;; kill-ring, mark-ring, and register browsers for Helm.
    (with-eval-after-load "helm-ring"
    
      ;; Max number of lines displayed per candidate in kill-ring browser.
      (setq helm-kill-ring-max-lines-number 20))

    ;; (with-eval-after-load "helm-utils"
    ;;   (setq helm-yank-symbol-first t)

#### Helm-ls-git<a id="sec-20-12-1-14" name="sec-20-12-1-14"></a>

    ;; List Git files.
    (with-eval-after-load "helm-ls-git-autoloads"
    
      ;; (global-set-key (kbd "C-c C-f") #'helm-ls-git-ls) ; used by Org!
      (global-set-key (kbd "M-+") #'helm-ls-git-ls)
      (global-set-key (kbd "<S-f3>") #'helm-ls-git-ls)
    
      ;; Browse files and see status of project with its VCS.
      (global-set-key (kbd "C-x C-d") #'helm-browse-project))

See <http://edvorg.com/edvorg/emacs-configs/blob/master/init.d/init-helm.el> for
much more configs with Git&#x2026;

#### Imenu<a id="sec-20-12-1-15" name="sec-20-12-1-15"></a>

Imenu is like an outline tree in regular IDE, but Helm makes it interactive, and
fast.  You can ask questions such as &ldquo;Is there a `function` whose name contains
`memory`?&rdquo;.

#### Google<a id="sec-20-12-1-16" name="sec-20-12-1-16"></a>

`helm-google-suggest` is very efficient when you get completions from Google (so
you can type only a few characters to get relevant completions) and then you can
send this completion to various services (e.g. wiki lookup), so you don&rsquo;t have
to start a separate Wikipedia lookup, Youtube lookup, etc. command, because you
can use the same command for any of them.

<div class="note">
You can easily add actions for other sites you usually search for info on.

</div>

    ;; Emacs Helm Interface for quick Google searches
    (with-eval-after-load "helm-google-autoloads"
      (global-set-key (kbd "C-c h g") #'helm-google)
      (global-set-key (kbd "C-c h s") #'helm-google-suggest))
    
    ;; (with-eval-after-load "helm-google"
    ;;
    ;;   ;; (when (executable-find "curl")
    ;;   ;;   (setq helm-google-suggest-use-curl-p t))
    ;;   )

#### Helm projectile<a id="sec-20-12-1-17" name="sec-20-12-1-17"></a>

Exploring large projects with Projectile and Helm Projectile:
<http://tuhdo.github.io/helm-projectile.html>

The new Helm Projectile is not just a single command \`helm-projectile\` anymore;
it can now replace many Projectile &ldquo;core commands&rdquo;, such as
\`projectile-find-file\`.

A few demos:

-   Select and open multiple files,
    ![img](//tuhdo.github.io/static/helm-projectile/helm-projectile-find-files-1.gif) :
    Now we can open multiple files at once with \`helm-projectile\`. It outweighs Ido
    -   flx for finding files, since you can only open one file in Ido. Yes, you can
    
    open one file faster in Helm, but when you need to open two or more files,
    Helm is more suitable for the large.

-   Jump to any file depends on context, even if the file path is in a text file,
    ![img](//tuhdo.github.io/static/helm-projectile/helm-projectile-find-files-dwim-1.gif)

-   Switch between other files with same names but different extensions,
    ![img](//tuhdo.github.io/static/helm-projectile/helm-projectile-find-other-file.gif). Work
    not only for C/C++ but other languages, and is customizable. You don&rsquo;t have to
    configure anything, like adding include paths for the command to
    search. Everything is automatic. Just use it as it is.

-   Ediff two selected files from helm-projectile-find-file,
    ![img](//tuhdo.github.io/static/helm-projectile/helm-projectile-find-file-ediff.gif).

-   Invoke Etags while in helm-projectile-find-file session; when in Etags
    session, you can switch back later to helm-projectile-find-file,
    ![img](//tuhdo.github.io/static/helm-projectile/helm-projectile-etags.gif).

-   You can invoke \`helm-do-grep\` on any project directory when using
    a \`helm-projectile\` command.

    ;; Always ignore .class files.
    (add-to-list 'projectile-globally-ignored-file-suffixes ".class")

#### Lisp completion<a id="sec-20-12-1-18" name="sec-20-12-1-18"></a>

<div class="warning">
This (from Thierry Volpiatto) seems good, except that it changes the behavior of
`TAB` when done in a word.  Instead of indenting, it completes the word at point,
changing the buffer&rsquo;s contents!

</div>

    ;; Lisp complete or indent.
    (define-key lisp-interaction-mode-map
      [remap indent-for-tab-command] #'helm-lisp-completion-at-point-or-indent)
    (define-key emacs-lisp-mode-map
      [remap indent-for-tab-command] #'helm-lisp-completion-at-point-or-indent)

`M-TAB`?

    ;; Lisp complete.
    (define-key lisp-interaction-mode-map
      [remap completion-at-point] #'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map
      [remap completion-at-point] #'helm-lisp-completion-at-point)

#### Helm swoop<a id="sec-20-12-1-19" name="sec-20-12-1-19"></a>


Pressing twice C-o goes from Helm Swoop to Helm Multi Swoop&#x2026;

    ;; Efficiently hopping squeezed lines powered by Helm interface
    ;; (= Helm occur + Follow mode!).
    (with-eval-after-load "helm-swoop-autoloads"
    
      ;; Better version of `(helm-)occur'.
      (global-set-key (kbd "C-o") #'helm-swoop)
      (global-set-key (kbd "M-s o") #'helm-swoop)
      ;; (global-set-key (kbd "M-i") #'helm-swoop)
      ;; (global-set-key (kbd "M-I") #'helm-swoop-back-to-last-point)
    
      (global-set-key (kbd "M-s O") #'helm-multi-swoop)
      (global-set-key (kbd "M-s /") #'helm-multi-swoop)
      ;; (global-set-key (kbd "C-c M-i") #'helm-multi-swoop)
    
      ;; (global-set-key (kbd "C-x M-i") #'helm-multi-swoop-all)
    
      ;; When doing Isearch, hand the word over to `helm-swoop'.
      (define-key isearch-mode-map (kbd "C-o") #'helm-swoop-from-isearch)
      ;; (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch)
    
      (with-eval-after-load "dired"
        (define-key dired-mode-map (kbd "C-o") #'helm-swoop)
        ;; (define-key dired-mode-map (kbd "M-i") #'helm-swoop)
        ))
    
    (with-eval-after-load "helm-swoop"
    
      ;; From `helm-swoop' to `helm-multi-swoop-all'.
      (define-key helm-swoop-map (kbd "C-o") #'helm-multi-swoop-all-from-helm-swoop)
      ;; (define-key helm-swoop-map (kbd "M-i") #'helm-multi-swoop-all-from-helm-swoop)
    
      ;; Don't slightly boost invoke speed in exchange for text color.
      (setq helm-swoop-speed-or-color t)
    
      ;; Split direction.
      ;; (setq helm-swoop-split-direction 'split-window-horizontally)
      (setq helm-swoop-split-direction 'split-window-sensibly)
    
      ;; Don't save each buffer you edit when editing is complete.
      (setq helm-multi-swoop-edit-save nil))

### Image mode<a id="sec-20-12-2" name="sec-20-12-2"></a>

AFAICT you need to eval something like

(setq image-type-header-regexps '((&ldquo;.\*&rdquo; . imagemagick)))

to be able to resize images in image-mode with imagemagick (for example, from
the &ldquo;Image&rdquo; menu).  If resizing works for you when visiting an image file,
I guess your imagemagick support is ok.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> auto-image-file-mode in find-file-hook for performance reasons!?</b><br  />
nil</div>

New commands since Emacs 24.4:

-   `v` in Dired works just as you request: it shows the image,
-   `n` shows the next image in the same directory (`image-next-file`),
-   `p` shows the previous image (`image-previous-file`), and
-   `q` quits.

Get EXIF data with `image-dired-get-exif-data`.

    (leuven--section "Image mode")
    
    ;; Show image files as images (not as semi-random bits).
    (add-hook 'find-file-hook #'auto-image-file-mode)

    )                                       ; Chapter 18 ends here.

# Using Multiple Buffers<a id="sec-21" name="sec-21"></a>

    ;;* 19 Using Multiple (info "(emacs)Buffers")
    
    (leuven--chapter leuven-load-chapter-19-buffers "19 Using Multiple Buffers"

## Listing Existing Buffers<a id="sec-21-1" name="sec-21-1"></a>

-   The `C` (current) column has a `.` for the buffer from which you came.
-   The `R` (read-only) column has a `%` if the buffer is read-only.
-   The `M` (modified) column has a `*` if it is modified.

    ;;** 19.2 (info "(emacs)List Buffers")
    
      (leuven--section "19.2 (emacs)List Buffers")
    
      (unless (locate-library "helm-autoloads")
    
        ;; Operate on buffers like Dired.
        (global-set-key (kbd "C-x C-b") #'ibuffer))
    
      (with-eval-after-load "ibuffer"
    
        ;; Completely replaces `list-buffer'.
        (defalias 'ibuffer-list-buffers 'list-buffer)
    
        ;; Don't show the names of filter groups which are empty.
        (setq ibuffer-show-empty-filter-groups nil)
    
        ;; Filtering groups.
        (setq ibuffer-saved-filter-groups
              '(("default"
                 ("Chat"
                  (mode . circe-mode))
                 ("Org"
                  (or (mode . diary-mode)
                      (mode . org-mode)
                      (mode . org-agenda-mode)))
                 ("LaTeX"
                  (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
                 ("Gnus & News"
                  (or (mode . message-mode)
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
                  (or (mode . svn-status-mode)
                      (mode . svn-log-edit-mode)
                      (name . "^\\*svn-")
                      (name . "^\\*vc\\*$")
                      (name . "^\\*Annotate")
                      (name . "^\\*git-")
                      (name . "^\\*vc-")))
                 ("Emacs"
                  (or (name . "^\\*scratch\\*$")
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
                  (or (mode . Info-mode)
                      (mode . apropos-mode)
                      (mode . woman-mode)
                      (mode . help-mode)
                      (mode . Man-mode))))))
    
        (add-hook 'ibuffer-mode-hook
                  (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "default")))
    
        ;; Order the groups so the order is: [Default], [agenda], [emacs].
        (defadvice ibuffer-generate-filter-groups
          (after leuven-reverse-ibuffer-groups activate)
          (setq ad-return-value (nreverse ad-return-value))))

## Killing Buffers<a id="sec-21-2" name="sec-21-2"></a>

    ;;** 19.4 (info "(emacs)Kill Buffer")
    
      (leuven--section "19.4 (emacs)Kill Buffer")
    
      ;; kill this buffer without confirmation (if not modified)
      (defun leuven-kill-this-buffer-without-query ()
        "Kill the current buffer without confirmation (if not modified)."
        (interactive)
        (kill-buffer nil))
    
      ;; key binding
      (global-set-key (kbd "<S-f12>") #'leuven-kill-this-buffer-without-query)

## Operating on Several Buffers<a id="sec-21-3" name="sec-21-3"></a>

    ;;** 19.5 (info "(emacs)Several Buffers")
    
      (leuven--section "19.5 (emacs)Several Buffers")
    
      ;; put the current buffer at the end of the list of all buffers
      (global-set-key (kbd "<f12>") #'bury-buffer)
                                            ; conflict when GDB'ing Emacs under
                                            ; Win32

## Convenience Features and Customization of Buffer Handling<a id="sec-21-4" name="sec-21-4"></a>

    ;;** 19.7 (info "(emacs)Buffer Convenience") and Customization of Buffer Handling
    
      (leuven--section "19.7 (emacs)Buffer Convenience and Customization of Buffer Handling")
    
      ;; unique buffer names dependent on file name
      (try-require 'uniquify)
    
      (with-eval-after-load "uniquify"
    
        ;; distinguish directories by adding extra separator
        (setq uniquify-trailing-separator-p t))

    )                                       ; Chapter 19 ends here.

# Multiple Windows<a id="sec-22" name="sec-22"></a>

    ;;* 20 Multiple (info "(emacs)Windows")
    
    (leuven--chapter leuven-load-chapter-20-windows "20 Multiple Windows"

## Concepts of Emacs Windows<a id="sec-22-1" name="sec-22-1"></a>

    ;;** 20.1 (info "(emacs)Basic Window")
    
      (leuven--section "20.1 (emacs)Basic Window")

## Using Other Windows<a id="sec-22-2" name="sec-22-2"></a>

    ;;** 20.3 (info "(emacs)Other Window")
    
      (leuven--section "20.3 (emacs)Other Window")
    
      ;; Cycle through all windows on current frame.
      (global-set-key (kbd "<f6>") #'other-window)
    
      ;; Reverse operation of `C-x o' (or `f6').
      (global-set-key (kbd "<S-f6>") #'previous-multiframe-window)

## Deleting and Rearranging Windows<a id="sec-22-3" name="sec-22-3"></a>

    ;;** 20.5 (info "(emacs)Change Window")
    
      (leuven--section "20.5 (emacs)Change Window")
    
      (defun leuven-delete-or-split-window ()
        "Cycle between 1 window and 2 windows.
    
      When splitting the window, the new window is selected, as it
      makes more sense to do something there first.
    
      The window's contents is unchanged by default.
    
      Do you want to see another part of the same file?  You've
      nothing to do.
    
      Do you want to see the last file you were visiting?  Simply
      bury the current buffer (f12).
    
      Do you want to go back to the first window?  Switch to
      it (f6)."
        (interactive)
        (cond ((= (count-windows) 1)
               (select-window
                (if (> (frame-width) split-width-threshold)
                    (split-window-horizontally)
                  (split-window-vertically))))
              (t
               (delete-other-windows))))
    
      ;; Delete all windows in the selected frame except the selected window.
      (global-set-key (kbd "<f5>") #'leuven-delete-or-split-window)

The following code flips a two-window frame, so that left is right, or up is
down.

    ;; Swap 2 windows.
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
    
    (global-set-key (kbd "C-c ~") #'leuven-swap-windows)

The following code toggles between horizontal and vertical layout of two
windows.  Useful when Emacs opens the window below instead at the side.

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
    
    (global-set-key (kbd "C-c |") #'leuven-toggle-window-split)

## Dedicated windows<a id="sec-22-4" name="sec-22-4"></a>

How to avoid displaying another buffer in a specific window, making the current
window always display this buffer (&ldquo;sticky window&rdquo;).

    (defun toggle-current-window-dedication ()
      "Toggle whether the current active window is dedicated or not."
      (interactive)
      (let* ((window (selected-window))
             (dedicated (window-dedicated-p window)))
        (set-window-dedicated-p window (not dedicated))
        (message "Window %sdedicated to %s"
                 (if dedicated "no longer " "")
                 (buffer-name))))
    
    ;; Press [pause] key in each window you want to "freeze".
    (global-set-key (kbd "<pause>") #'toggle-current-window-dedication)

## Displaying a Buffer in a Window<a id="sec-22-5" name="sec-22-5"></a>

Splits screen vertically if the width of the window is large enough.

See
<http://stackoverflow.com/questions/1381794/too-many-split-screens-opening-in-emacs>
for alternative code&#x2026;

See as well `split-window-preferred-function`.

    ;;** 20.6 (info "(emacs)Displaying Buffers")
    
      (leuven--section "20.6 (emacs)Pop Up Window")
    
      ;; Don't allow splitting windows vertically.
      (setq split-height-threshold nil)
    
      ;; ;; Minimum width for splitting windows horizontally.
      ;; (setq split-width-threshold 160)      ; See `split-window-sensibly'.

## Window Handling Convenience Features and Customization<a id="sec-22-6" name="sec-22-6"></a>

Use `M-x scroll-all-mode` to scroll all visible windows together in parallel.

    )                                       ; Chapter 20 ends here.

# Frames and Graphical Displays<a id="sec-23" name="sec-23"></a>

    ;;* 21 (info "(emacs)Frames") and Graphical Displays
    
    (leuven--chapter leuven-load-chapter-21-frames "21 Frames and Graphical Displays"

## Mouse Commands for Editing<a id="sec-23-1" name="sec-23-1"></a>

    ;;** 21.1 (info "(emacs)Mouse Commands")
    
      (leuven--section "21.1 (emacs)Mouse Commands")
    
      ;; Scroll one line at a time.
      (setq mouse-wheel-scroll-amount
            '(1
              ((shift) . 1)))

## Creating Frames<a id="sec-23-2" name="sec-23-2"></a>

    ;;** 21.6 (info "(emacs)Creating Frames")
    
      (leuven--section "21.6 (emacs)Creating Frames")

1.  Resize the frame to the size you want.
2.  Enter `(frame-parameters)` in the `*scratch*` buffer.
3.  Evaluate the form: place the cursor after the closing parenthesis, and type
    `C-j`, so that the output goes right into the `*scratch*` buffer.

By putting customizations of `default-frame-alist` in your init file, you can
control the appearance of **all the frames** Emacs creates, including the initial
one.

<div class="note">
`initial-frame-alist` is just for overriding properties for the first frame.

</div>

    (when (display-graphic-p)
    
      ;; Put Emacs exactly where you want it, every time it starts up.
      (setq initial-frame-alist
            '((top . 0)
              (left . 0)))
    
      ;; Auto-detect the screen dimensions and compute the height of Emacs.
      (add-to-list 'default-frame-alist
                   (cons 'height
                         (/ (-
                             ;; Height of Display 1.
                             (nth 4
                                  (assq 'geometry
                                        (car (display-monitor-attributes-list)))) ; XXX Emacs 24.4 needed!
                             106)       ; Allow for Emacs' title bar and taskbar
                                        ; (from the OS).
                            (frame-char-height)))))

    ;; Title bar display of visible frames.
    (setq frame-title-format
          (format "%s Emacs%s %s%s of %s - PID: %d"
                  (replace-regexp-in-string "-.*$" ""
                                            (capitalize (symbol-name system-type)))
                  (if (string-match "^x86_64-.*" system-configuration)
                      "-w64"
                    "-w32")
                  emacs-version
                  (if (and (boundp 'emacs-repository-version)
                           emacs-repository-version)
                      (concat " (" (substring
                                    (replace-regexp-in-string
                                     " .*" "" emacs-repository-version) 0 7) ")")
                    "")
                  (format-time-string "%Y-%m-%d" emacs-build-time)
                  (emacs-pid)))

    (defun leuven-detach-window ()
      "Close current window and re-open it in new frame."
      (interactive)
      (let ((current-buffer (window-buffer)))
        (delete-window)
        (select-frame (make-frame))
        (set-window-buffer (selected-window) current-buffer)))

## Frame Commands<a id="sec-23-3" name="sec-23-3"></a>

-   **`toggle-frame-fullscreen` (f11 by default):** Toggle fullscreen mode of the selected frame.  **Ignore window manager
    screen decorations.**

-   **`toggle-frame-maximized` (M-f10 by default):** Toggle maximization state of the selected frame.  Respect window manager
    screen decorations.

    ;;** 21.7 (info "(emacs)Frame Commands")
    
      (leuven--section "21.7 (emacs)Frame Commands")
    
      (defun leuven-maximize-frame ()
        "Maximize the current frame."
        (interactive)
        (cond ((or leuven--win32-p leuven--cygwin-p)
               (w32-send-sys-command 61488))
              (leuven--x-window-p
               (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                      '(2 "_NET_WM_STATE_FULLSCREEN" 0))))
        (global-set-key (kbd "C-c z") #'leuven-restore-frame))
    
      (defun leuven-restore-frame ()
        "Restore a minimized frame."
        (interactive)
        (cond ((or leuven--win32-p leuven--cygwin-p)
               (w32-send-sys-command 61728))
              (leuven--x-window-p
               (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                      '(2 "_NET_WM_STATE_FULLSCREEN" 0))))
        (global-set-key (kbd "C-c z") #'leuven-maximize-frame))
    
      (global-set-key (kbd "C-c z") #'leuven-maximize-frame)
    
      ;; Maximize Emacs frame by default.
      (modify-all-frames-parameters '((fullscreen . maximized)))

## Speedbar Frames<a id="sec-23-4" name="sec-23-4"></a>

    ;;** 21.9 (info "(emacs)Speedbar")
    
      (leuven--section "21.9 (emacs)Speedbar Frames")
    
      (unless (featurep 'helm-config)       ; Helm is better than speedbar!
    
        ;; Jump to speedbar frame.
        (global-set-key (kbd "<f4>") #'speedbar-get-focus))
    
      ;; Everything browser (into individual source files), or Dired on steroids.
      (with-eval-after-load "speedbar"
    
        ;; Number of spaces used for indentation.
        (setq speedbar-indentation-width 2)
    
        ;; Add new extensions for speedbar tagging (allow to expand/collapse
        ;; sections, etc.) -- do this BEFORE firing up speedbar?
        (speedbar-add-supported-extension
         '(".bib" ".css" ".jpg" ".js" ".nw" ".org" ".php" ".png" ".R" ".tex" ".txt"
           ".w" "README"))
    
        ;; Bind the arrow keys in the speedbar tree.
        (define-key speedbar-mode-map (kbd "<right>") #'speedbar-expand-line)
        (define-key speedbar-mode-map (kbd "<left>") #'speedbar-contract-line)
    
        ;; Parameters to use when creating the speedbar frame in Emacs.
        (setq speedbar-frame-parameters '((width . 30)
                                          (height . 45)
                                          (foreground-color . "blue")
                                          (background-color . "white")))
    
        ;; Speedbar in the current frame (vs in a new frame).
        (when (and (not (locate-library "helm-config"))
                                            ; helm is better than speedbar!
                   (locate-library "sr-speedbar"))
    
          (autoload 'sr-speedbar-toggle "sr-speedbar" nil t)
          (global-set-key (kbd "<f4>") #'sr-speedbar-toggle)))

## Scroll Bars<a id="sec-23-5" name="sec-23-5"></a>

    ;;** 21.12 (info "(emacs)Scroll Bars")
    
      (leuven--section "21.12 (emacs)Scroll Bars")
    
      (if (and (display-graphic-p)
               ;; (featurep 'powerline)
               )
    
          ;; Turn scroll bar off.
          (scroll-bar-mode -1)
    
        ;; Position of the vertical scroll bar.
        (setq-default vertical-scroll-bar 'right))

## Tool Bars<a id="sec-23-6" name="sec-23-6"></a>

    ;;** 21.15 (info "(emacs)Tool Bars")
    
      (leuven--section "21.15 (emacs)Tool Bars")
    
      ;; Turn tool bar off.
      (when (display-graphic-p)
        (tool-bar-mode -1))

## Using Dialog Boxes<a id="sec-23-7" name="sec-23-7"></a>

    ;;** 21.16 Using (info "(emacs)Dialog Boxes")
    
      (leuven--section "21.16 (emacs)Using Dialog Boxes")
    
      ;; Don't use dialog boxes to ask questions.
      (setq use-dialog-box nil)
    
      ;; Don't use a file dialog to ask for files.
      (setq use-file-dialog nil)

## Tooltips<a id="sec-23-8" name="sec-23-8"></a>

    ;;** 21.17 (info "(emacs)Tooltips")
    
      (leuven--section "21.17 (emacs)Tooltips")
    
      ;; Disable Tooltip mode (use the echo area for help and GUD tooltips).
      (unless leuven--console-p (tooltip-mode -1))

    )                                       ; Chapter 21 ends here.

# International Character Set Support<a id="sec-24" name="sec-24"></a>

    ;;* 22 (info "(emacs)International") Character Set Support
    
    (leuven--chapter leuven-load-chapter-22-international "22 International Character Set Support"

For any user who needs symbols that are not in the 7-bit ASCII set, our
recommendation is to move to Unicode UTF-8.  That is the only encoding that is
the same across all platforms and operating systems that support it.

To check your locale settings, you can have a look to what Emacs produce (in a
mail buffer) under &ldquo;Important settings&rdquo; when you type
`M-x report-emacs-bug RET foo RET`:

**Important settings**:
-   value of `$LC_ALL`: `nil`
-   value of `$LC_COLLATE`: `nil`
-   value of `$LC_CTYPE`: `nil`
-   value of `$LC_MESSAGES`: `nil`
-   value of `$LC_MONETARY`: `nil`
-   value of `$LC_NUMERIC`: `nil`
-   value of `$LC_TIME`: `nil`
-   value of `$LANG`: `en_US.UTF-8`
-   value of `$XMODIFIERS`: `nil`
-   `locale-coding-system`: `utf-8-unix`
-   `default-enable-multibyte-characters`: `t`

## Introduction to International Character Sets<a id="sec-24-1" name="sec-24-1"></a>

<div class="tip">
To open (or save) a file in UTF-8, you can press `C-x RET c utf-8 RET`
(`universal-coding-system-argument`) before the `C-x C-f` (or `C-x C-s`).

</div>

To help you find all the chars you need to replace by escape sequences, you can
use `C-u C-s [^[:ascii:]]`.

`M-x describe-coding-system RET RET`

To see all the non-ASCII characters you can type with the `C-x 8` prefix, type
`C-x 8 C-h`.

    ;;** 22.1 (info "(emacs)International Chars")
    
      (leuven--section "22.1 (emacs)International Chars")
    
      ;; Keyboard input definitions for ISO 8859-1.
      (with-eval-after-load "iso-transl"
    
        ;; Add binding for "zero width space".
        (define-key iso-transl-ctl-x-8-map (kbd "0") [?​]))

## Language Environments<a id="sec-24-2" name="sec-24-2"></a>

    ;;** 22.2 (info "(emacs)Language Environments")
    
      (leuven--section "22.2 (emacs)Language Environments")
    
      ;; Specify your character-set locale.
      (setenv "LANG" "en_US.utf8")          ; For `svn' not to report warnings.
    
      ;; System locale to use for formatting time values.
      (setq system-time-locale "C")         ; Make sure that the weekdays in the
                                            ; time stamps of your Org mode files and
                                            ; in the agenda appear in English.
    
      ;; (setq system-time-locale (getenv "LANG"))
                                            ; For weekdays in your locale settings.

One could use:
-   `LC_ALL`,
-   `LC_COLLATE=C` (or `LC_COLLATE=POSIX`),
-   `LC_CTYPE`,
-   `LC_ALL` or
-   `LANG`

but this won&rsquo;t work for a Windows binary (well for the Cygwin version) of Emacs.

## Input Methods<a id="sec-24-3" name="sec-24-3"></a>

    ;;** 22.3 (info "(emacs)Input Methods")
    
      (leuven--section "22.3 (emacs)Input Methods")
    
      ;; Get 8-bit characters in terminal mode (Cygwin Emacs).
      (set-input-mode (car (current-input-mode))
                      (nth 1 (current-input-mode))
                      0)

    (defun leuven-list-unicode-display (&optional regexp)
      "Display a list of unicode characters and their names in a buffer."
      (interactive "sRegexp (default \".*\"): ")
      (let* ((regexp (or regexp ".*"))
             (case-fold-search t)
             (cmp (lambda (x y) (< (cdr x) (cdr y))))
             ;; alist like ("name" . code-point).
             (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
                                                 (ucs-names))
                               cmp)))
        (with-help-window "*Unicode characters*"
          (with-current-buffer standard-output
            (dolist (c char-alist)
              (insert (format "0x%06X\t" (cdr c)))
              (insert (cdr c))
              (insert (format "\t%s\n" (car c))))))))

## Recognizing Coding Systems<a id="sec-24-4" name="sec-24-4"></a>

In GNU Emacs, when you specify the coding explicitly in the file via an explicit
`coding:` cookie, that overrides `file-coding-system-alist`.

XXX Declare PDF files as binary

    ;;** 22.6 (info "(emacs)Recognize Coding") Systems
    
      (leuven--section "22.6 (emacs)Recognize Coding Systems")
    
      ;; Default coding system (for new files), also moved to the front of the
      ;; priority list for automatic detection.
      (prefer-coding-system 'utf-8-unix)    ; Unix flavor for code blocks executed
                                            ; via Org-Babel.

## Specifying a File&rsquo;s Coding System<a id="sec-24-5" name="sec-24-5"></a>

    ;;** 22.7 (info "(emacs)Specify Coding") System of a File
    
      (leuven--section "22.7 (emacs)Specify Coding System of a File")
    
      ;; To copy and paste to and from Emacs through the clipboard (with coding
      ;; system conversion).
      (cond (leuven--win32-p
             (set-selection-coding-system 'compound-text-with-extensions))
            (t
             (set-selection-coding-system 'utf-8)))

## Bidirectional Editing<a id="sec-24-6" name="sec-24-6"></a>

    ;;** 22.19 (info "(emacs)Bidirectional Editing")
    
      (leuven--section "22.19 (emacs)Bidirectional Editing")
    
      ;; Faster scrolling (if you never expect to have to display bidirectional
      ;; scripts, such as Arabic and Hebrew).
      (setq-default bidi-paragraph-direction 'left-to-right)

    )                                       ; Chapter 22 ends here.

# Major and Minor Modes<a id="sec-25" name="sec-25"></a>

    ;;* 23 (info "(emacs)Modes")
    
    (leuven--chapter leuven-load-chapter-23-major-and-minor-modes "23 Major and Minor Modes"

## How Major Modes are Chosen<a id="sec-25-1" name="sec-25-1"></a>

See &ldquo;Syntax of Regexps&rdquo;:
-   `\'` matches end of string
-   `$` matches end of line

    ;;** 23.3 (info "(emacs)Choosing Modes")
    
      (leuven--section "23.3 (emacs)Choosing File Modes")

### Choice based on the file name<a id="sec-25-1-1" name="sec-25-1-1"></a>

Instead of superseding the binding in `auto-mode-alist`, you can replace it
(brute force) with `(setcdr (rassq 'old-mode auto-mode-alist) 'new-mode)`.

    ;; List of filename patterns.
    (setq auto-mode-alist
          (append '(("\\.log\\'"       . text-mode)
                    ;; ("\\.[tT]e[xX]\\'" . latex-mode)
                    ;; ("\\.cls\\'"       . LaTeX-mode)
                    ("\\.cgi\\'"       . perl-mode)
                    ;; ("[mM]akefile"     . makefile-mode)
                    (".ssh/config\\'"  . ssh-config-mode)
                    ("sshd?_config\\'" . ssh-config-mode)
                    ) auto-mode-alist))
    
    ;; Major mode for fontifiying ssh config files.
    (autoload 'ssh-config-mode "ssh-config-mode"
      "Major mode for fontifiying ssh config files." t)

    ;; Helper code for use with the "ledger" command-line tool.
    (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
    (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))
    (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
    
    (with-eval-after-load "ledger-commodities"
    
      ;; Default commodity for use in target calculations in ledger reconcile.
      (setq ledger-reconcile-default-commodity "EUR")) ; "€"
    
    ;; Provide custom fontification for ledger-mode.
    (with-eval-after-load "ledger-fontify"
    
      ;; If t, the highlight entire xact with state.
      (setq ledger-fontify-xact-state-overrides nil))
                                          ; Don't override the highlighting of
                                          ; each posted item in a xact if it is
                                          ; cleared/pending. XXX
    
    (with-eval-after-load "ledger-init"
    
      ;; (setq ledger-default-date-format "%Y-%m-%d")
      (setq ledger-default-date-format "%Y/%m/%d")
      )
    
    (with-eval-after-load "flycheck"
    
      ;; Flycheck integration for ledger files.
      (try-require 'flycheck-ledger))

    ;; major mode for editing comma-separated value files
    (with-eval-after-load "csv-mode-autoloads"
    
      (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode)))
    
    (with-eval-after-load "csv-mode"
    
      ;; field separators: a list of *single-character* strings
      (setq csv-separators '("," ";")))

### Choice based on the interpreter name<a id="sec-25-1-2" name="sec-25-1-2"></a>

    ;; list of interpreters specified in the first line (starts with `#!')
    (push '("expect" . tcl-mode) interpreter-mode-alist)

### Choice based on the text at the start of the buffer<a id="sec-25-1-3" name="sec-25-1-3"></a>

For a list of buffer beginnings, see `magic-mode-alist`.

    ;; ;; load generic modes which support e.g. batch files
    ;; (try-require 'generic-x)

    )                                       ; Chapter 23 ends here.

# Indentation<a id="sec-26" name="sec-26"></a>

    ;;* 24 (info "(emacs)Indentation")
    
    (leuven--chapter leuven-load-chapter-24-indentation "24 Indentation"

## Indentation Commands<a id="sec-26-1" name="sec-26-1"></a>

-   **`M-m`:** `back-to-indentation`.

-   **`C-M-\`:** Run the command `indent-region` (which does the job of the imaginary command
    `unsuck-html-layout` in `html-mode`).

    ;;** 24.1 (info "(emacs)Indentation Commands") and Techniques
    
      (leuven--section "24.1 (emacs)Indentation Commands and Techniques")
    
      (defun leuven-indent-buffer ()
        "Indent each non-blank line in the buffer."
        (interactive)
        (save-excursion
          (indent-region (point-min) (point-max) nil)))

    (global-set-key (kbd "C-x \\") #'align-regexp)

    (defun leuven-align-to-equals (begin end)
      "Align region to equal signs."
      (interactive "r")
      (align-regexp begin end "\\(\\s-*\\)=" 1 1))
    
    (global-set-key (kbd "C-c =") #'leuven-align-to-equals)

    ;; Minor mode to aggressively keep your code always indented.
    (with-eval-after-load "aggressive-indent-autoloads"
    
      ;; Enable aggressive indent mode everywhere.
      (aggressive-indent-global-mode))

<div class="warning">
If we enable `aggressive-indent-mode`, then the tangled Emacs Lisp code (from this
file) is beginning at column 0.

</div>

    ;; Show vertical lines to guide indentation.
    (with-eval-after-load "indent-guide-autoloads-XXX" ; Display problems with CrossMapIntegration.java
    
      ;; Enable indent-guide-mode automatically.
      (add-hook 'prog-mode-hook #'indent-guide-mode))
    
    (with-eval-after-load "indent-guide"
    
      ;; Character used as vertical line.
      (setq indent-guide-char
            (cond ((char-displayable-p ?\u254E) "╎")
                  ((char-displayable-p ?\u2502) "│")
                  (t ":")))
    
      (diminish 'indent-guide-mode))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Look at the \`dtrt-indent&rsquo; package</b><br  />
nil</div>

## Tabs vs. Spaces<a id="sec-26-2" name="sec-26-2"></a>

Don&rsquo;t tabify.  Instead, <span class="underline">reindent</span> everything, e.g. with `C-M-\`.

    ;;** 24.3 TABs vs. (info "(emacs)Just Spaces")
    
      (leuven--section "24.3 TABs vs. (emacs)Just Spaces")
    
      ;; Indentation can't insert TABs.
      (setq-default indent-tabs-mode nil)

    )                                       ; Chapter 24 ends here.

# Commands for Human Languages<a id="sec-27" name="sec-27"></a>

    ;;* 25 Commands for (info "(emacs)Text") Human Languages
    
    (leuven--chapter leuven-load-chapter-25-text "25 Commands for Human Languages"

## Words<a id="sec-27-1" name="sec-27-1"></a>

    ;;** 25.1 (info "(emacs)Words")
    
      (leuven--section "25.1 (emacs)Words")

## Sentences<a id="sec-27-2" name="sec-27-2"></a>

    ;;** 25.2 (info "(emacs)Sentences")
    
      (leuven--section "25.2 (emacs)Sentences")
    
      ;; ;; A single space does end a sentence.
      ;; (setq-default sentence-end-double-space nil) ; see `ispell-dictionary'

The default value of `sentence-end-double-space` is good for French as well, as
we put two spaces after a interrogation/question mark. That way, those two
spaces won&rsquo;t be transformed into one&#x2026;

## Filling Text<a id="sec-27-3" name="sec-27-3"></a>

There are several **no-break space** characters required for French punctuation:

-   **No-break thin space** (`202F`), known in Unicode as &ldquo;narrow no-break space&rdquo;,
    required before `?`, `!` and `;`.

-   **No-break space** (`00A0`), required before `:` and `»`, and required after `«`.
    
    It can be inserted with `S-SPC`:
    
        (defun leuven-nbsp-command ()
          "Insert the no-break space character 00A0."
          (interactive)
          (insert-char ?\u00A0))
        
        (global-set-key (kbd "S-SPC") #'leuven-nbsp-command)

They could be used to avoid breaking at certain bad places.

Another solution to avoid that is to add `fill-french-nobreak-p` to
`fill-nobreak-predicate`.

    ;;** 25.5 (info "(emacs)Filling") Text
    
      (leuven--section "25.5 (emacs)Filling Text")
    
      ;; Line-wrapping beyond that column (when pressing `M-q').
      (setq-default fill-column 80)
    
      ;; (Un-)fill paragraph.
      (defun leuven-fill-paragraph (&optional arg)
        "`M-q' runs the command `fill-paragraph'.
      `C-u M-q' runs \"unfill-paragraph\": it takes a multi-line paragraph and
      converts it into a single line of text."
        (interactive "P")
        (let ((fill-column (if arg
                               (point-max)
                             fill-column)))
          (fill-paragraph nil)))
    
      (global-set-key (kbd "M-q") #'leuven-fill-paragraph)
    
      ;; Prevent breaking lines just before a punctuation mark such as `?' or `:'.
      (add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)
    
      ;; Activate Auto Fill for all text mode buffers.
      (add-hook 'text-mode-hook #'auto-fill-mode)

    ;; Graphically indicate the fill column.
    (try-require 'fill-column-indicator)
    (with-eval-after-load "fill-column-indicator"
    
      ;; Color used to draw the fill-column rule.
      (setq fci-rule-color "#FFE0E0")
    
      ;; Show the fill-column rule as dashes.
      (setq fci-rule-use-dashes t)
    
      ;; Ratio of dash length to line height.
      (setq fci-dash-pattern 0.5)
    
      ;; Enable fci-mode in programming, message and Org modes.
      (add-hook 'prog-mode-hook #'fci-mode) ; 3 special chars at the end of every line when exporting code blocks to HTML!!!
      (add-hook 'message-mode-hook #'fci-mode)
      (add-hook 'org-mode-hook #'fci-mode)

<div class="warning">
Enabling `fci-mode` as a global minor mode slows down the tangling of this file by
a factor 2!?

</div>

    ;; Avoid `fci-mode' and `auto-complete' popups.
    (defvar sanityinc/fci-mode-suppressed nil)
    (defadvice popup-create (before suppress-fci-mode activate)
      "Suspend fci-mode while popups are visible"
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
      (when fci-mode
        (turn-off-fci-mode)))
    (defadvice popup-delete (after restore-fci-mode activate)
      "Restore fci-mode when all popups have closed"
      (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
        (setq sanityinc/fci-mode-suppressed nil)
        (turn-on-fci-mode))))

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

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Replace ' by \`&rsquo; in Emacs Lisp and by \`\` in other modes</b><br  />
nil</div>

      (defun leuven-smart-punctuation-apostrophe ()
        "Replace second apostrophe by backquote in front of symbol."
        (interactive)
        (cond
         ((or (bolp) (not (looking-back "'")))
          ;; Insert just one '.
          (self-insert-command 1))
         ((save-excursion
            (backward-char)
            ;; Skip symbol backwards.
            (and (not (zerop (skip-syntax-backward "w_.")))
                 (not (looking-back "`"))
                 (or (insert-and-inherit "`") t))))
         (t
          ;; Insert `' around following symbol.
          (delete-char -1)
          (unless (looking-back "`") (insert-and-inherit "`"))
          (save-excursion
            (skip-syntax-forward "w_.")
            (unless (looking-at "'") (insert-and-inherit "'"))))))
    
      (defun leuven-smart-punctuation-quotation-mark ()
        "Replace two following double quotes by French quotes."
        (interactive)
        (let ((dict (or (when (boundp 'ispell-local-dictionary)
                          ispell-local-dictionary)
                        (when (boundp 'ispell-dictionary)
                          ispell-dictionary))))
    (message ">>> %s" major-mode)
          (cond
           ((and (string= dict "francais")
                 (eq (char-before) ?\")
                 (or (not (equal mode-name "Org"))
                     (not (member (org-element-type (org-element-at-point))
                                  '(src-block keyword table dynamic-block)))))
            (backward-delete-char 1)
            (insert "«  »")
            (backward-char 2))
           ((and (eq (char-before) ?\")
                 (derived-mode-p 'latex-mode))
            (backward-delete-char 1)
            (insert "\\enquote{}")
            (backward-char 1))
           (t
            (insert "\"")))))
    
      (defun leuven--smart-punctuation ()
        "Replace second apostrophe or quotation mark."
        (local-set-key [39] #'leuven-smart-punctuation-apostrophe)
        (local-set-key "\"" #'leuven-smart-punctuation-quotation-mark))
    
      (add-hook 'text-mode-hook #'leuven--smart-punctuation)
      (add-hook 'message-mode-hook #'leuven--smart-punctuation)

    (with-eval-after-load "key-chord-autoloads"
      (key-chord-mode 1))
    
    ;; Map pairs of simultaneously pressed keys to commands.
    (with-eval-after-load "key-chord"
    
      (with-eval-after-load "hideshow"    ; Package.
        ;; (key-chord-define hs-minor-mode-map "--" #'hs-hide-block) ; Not autoloaded. That's SQL comment marker!
        (key-chord-define hs-minor-mode-map "++" #'hs-show-block) ; Not autoloaded.
        ;; (key-chord-define hs-minor-mode-map "//" #'hs-hide-all) ; Not autoloaded. That's Java comment marker!
        (key-chord-define hs-minor-mode-map "**" #'hs-show-all)) ; Not autoloaded.
    
      (key-chord-define-global "<<" (lambda () (interactive) (insert "«")))
      (key-chord-define-global ">>" (lambda () (interactive) (insert "»")))
    
      (with-eval-after-load "diff-hl"    ; Package.
        (key-chord-define diff-hl-mode-map ">>" #'diff-hl-next-hunk)
        (key-chord-define diff-hl-mode-map "<<" #'diff-hl-previous-hunk))
    
      (key-chord-define-global "hb" #'describe-bindings)
      (key-chord-define-global "hf" #'describe-function)
      (key-chord-define-global "hv" #'describe-variable)
    
      (with-eval-after-load "expand-region-autoloads" ; Autoloads file.
        (key-chord-define-global "hh" #'er/expand-region)) ; Autoloaded.
    
      (with-eval-after-load "ace-window-autoloads" ; Autoloads file.
        (key-chord-define-global "jj" #'ace-jump-word-mode) ; Autoloaded.
        ;; (key-chord-define-global "jk" #'ace-jump-mode-pop-mark) ; Autoloaded. lijkt.
        ;; (key-chord-define-global "jl" #'ace-jump-line-mode) ; Autoloaded. pijl.
        )
    
      (with-eval-after-load "ace-window"
        (key-chord-define-global "jw" #'ace-window))
    
      (with-eval-after-load "dired-x"
        (key-chord-define-global "xj" #'dired-jump)) ; Autoloaded?
    
      (key-chord-define-global "vb" #'eval-buffer)
      ;; (key-chord-define-global "vg" #'eval-region) ; 2015-02-17 Crash Gnus `C-u g'
    
      ;; (key-chord-define-global "x0" #'delete-window) ; 2015-02-09 Crash Gnus `C-u 3'
      ;; (key-chord-define-global "x1" #'delete-other-windows) ; 2015-02-05 Crash Gnus `C-u 1'
      (key-chord-define-global "xh" #'mark-whole-buffer)
      (key-chord-define-global "xk" #'kill-buffer)
      (key-chord-define-global "xo" #'other-window)
      (key-chord-define-global "xs" #'save-buffer)
    
      (key-chord-define-global "yy" #'browse-kill-ring)
      (key-chord-define-global "zk" #'zap-to-char)
    
      (key-chord-define-global ";s" #'set-mark-command)
    
      ;; (with-eval-after-load "org-loaddefs" ; Autoloads file?
        ;; (key-chord-define-global ",a" #'org-agenda) ; Autoloaded. ; 2015-02-18 Crash Gnus `C-u a'
        ;; (key-chord-define-global ",c" #'org-capture)) ; Autoloaded. ; "Donc," is problematic...
    
      (with-eval-after-load "org"         ; Package.
        (key-chord-define org-mode-map ",u" #'outline-up-heading)
        (key-chord-define org-mode-map ",w" #'org-refile) ; Not autoloaded.
        ;; (key-chord-define org-mode-map ",," #'org-mark-ring-goto)           ;; Return to previous location before link.
        ;; (key-chord-define org-mode-map ",." #'org-time-stamp)               ;; Create new timestamp.
        ;; (key-chord-define org-mode-map ",b" #'org-tree-to-indirect-buffer)  ;; Show complete tree in dedicated buffer.
        ;; (key-chord-define org-mode-map ",d" #'org-todo)                     ;; Toggle todo for headline.
        ;; (key-chord-define org-mode-map ",e" #'org-insert-link)              ;; Edit current link.
        ;; (key-chord-define org-mode-map ",f" #'org-footnote-action)          ;; Create new footnote link.
        ;; (key-chord-define org-mode-map ",g" #'er/open-org-calendar)         ;; Open calendar integration. :Functions.el:
        ;; (key-chord-define org-mode-map ",h" #'org-toggle-heading)           ;; Toggle heading for current line/list item.
        ;; (key-chord-define org-mode-map ",k" #'org-cut-subtree)              ;; Kill subtree.
        ;; (key-chord-define org-mode-map ",n" #'er/org-narrow-and-reveal)     ;; Narrow region and reveal. :Functions.el:
        ;; (key-chord-define org-mode-map ",o" #'org-open-at-point)            ;; Open link at point.
        ;; (key-chord-define org-mode-map ",p" #'org-priority)                 ;; Toggle priority.
        ;; (key-chord-define org-mode-map ",t" #'org-set-tags-command)         ;; Choose tags.
        ;; (key-chord-define org-mode-map ",v" #'org-paste-subtree)            ;; Paste subtree.
        ;; (key-chord-define org-mode-map ",w" #'er/org-widen-and-outline)     ;; Widen and outline. :Functions.el:
        ;; (key-chord-define org-mode-map ",y" #'org-copy-subtree)             ;; Copy subtree.
        ;; (key-chord-define org-mode-map "<H" #'org-list-make-subtree)        ;; Toggle headings for all list items in subtree.
        )
    
      ;; (key-chord-define-global "ac" #'align-current)
      ;; (key-chord-define-global "fc" #'flycheck-mode)
      ;; (global-set-key (kbd "M-2") #'highlight-symbol-occur)
      ;; (global-set-key (kbd "M-3") (lambda () (interactive) (highlight-symbol-jump -1)))
      ;; (global-set-key (kbd "M-4") (lambda () (interactive) (highlight-symbol-jump 1)))
      ;; (key-chord-define-global "vg" #'vc-git-grep)
    
      ;; (key-chord-define-global "''" "`'\C-b")
      ;; (key-chord-define-global "dq" "\"\"\C-b")
      ;; (key-chord-define-global ";d" #'dired-jump-other-window)
      ;; (key-chord-define-global "jk" #'dabbrev-expand)
      ;; (key-chord-define-global "JJ" #'find-tag)
      ;; (key-chord-define-global ",." "<>\C-b")
      ;; (key-chord-define-global "''" "`'\C-b")
      ;; (key-chord-define-global ",," #'indent-for-comment)
      )

## Case Conversion Commands<a id="sec-27-4" name="sec-27-4"></a>

    ;;** 25.6 (info "(emacs)Case") Conversion Commands
    
      (leuven--section "25.6 (emacs)Case Conversion Commands")
    
      ;; Enable the use of the commands `downcase-region' and `upcase-region'
      ;; without confirmation.
      (put 'downcase-region 'disabled nil)
      (put 'upcase-region 'disabled nil)

## Outline Mode<a id="sec-27-5" name="sec-27-5"></a>


Outline is line-oriented and does not distinguish end-of-block.

    ;;** 25.8 (info "(emacs)Outline Mode")
    
      (leuven--section "25.8 (emacs)Outline Mode")

### TODO outline-minor-mode (inside plain latex-mode as well)<a id="sec-27-5-1" name="sec-27-5-1"></a>

Though Outline minor mode has NOTHING to do with folding of code as such, some
people have the following code in their `ruby-mode-hook` to enable code folding
using `outline-minor-mode` + `outline-magic`.

    (outline-minor-mode 1)
    (set (make-local-variable 'outline-regexp) "^[ \t]*\\(?:def\\|class\\|module\\)\\|^[ \t]*###==")

Outline minor mode is also used to collapse Lisp code (i.e., to see in the
buffer just the definition of a function instead of the whole body).

See also the library hs-minor-mode (See section 31.7).

You can use `org-cycle` in other modes, with `outline-minor-mode`.

See <https://github.com/zk-phi/dotfiles/blob/master/emacs/init.el>.

    ;; Outline mode commands for Emacs.
    (with-eval-after-load "outline"
    
      ;; Bind the outline minor mode functions to an easy to remember prefix
      ;; key (more accessible than the horrible prefix `C-c @').
      (setq outline-minor-mode-prefix (kbd "C-c C-o")) ; like in nXML mode
    
      ;; ;; Make other `outline-minor-mode' files (LaTeX, etc.) feel the Org
      ;; ;; mode outline navigation (written by Carsten Dominik).
      ;; (try-require 'outline-magic)
      ;; (with-eval-after-load "outline-magic"
      ;;   (add-hook 'outline-minor-mode-hook
      ;;             (lambda ()
      ;;               (define-key outline-minor-mode-map
      ;;                 (kbd "<S-tab>") #'outline-cycle)
      ;;               (define-key outline-minor-mode-map
      ;;                 (kbd "<M-left>") #'outline-promote)
      ;;               (define-key outline-minor-mode-map
      ;;                 (kbd "<M-right>") #'outline-demote)
      ;;               (define-key outline-minor-mode-map
      ;;                 (kbd "<M-up>") #'outline-move-subtree-up)
      ;;               (define-key outline-minor-mode-map
      ;;                 (kbd "<M-down>") #'outline-move-subtree-down))))
    
      ;; ;; Extra support for outline minor mode.
      ;; (try-require 'out-xtra)
    
    
      ;; Org-style folding for a `.emacs' (and much more).
    
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
    
      ;; Fontify the whole line for headings (with a background color).
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
    
      (add-hook 'outline-minor-mode-hook #'leuven--outline-minor-mode-hook)
    
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

For Emacs Lisp, Stefan Monnier additionally uses:

    (add-hook 'outline-minor-mode-hook
              (lambda ()
                (when (and outline-minor-mode (derived-mode-p 'emacs-lisp-mode))
                  (hide-sublevels 1000))))

which starts outline-minor-mode by hiding all the bodies of functions.

He also uses `reveal-mode` which automatically unhides the bodies when you try to
move the cursor into them (so you don&rsquo;t need to remember the key sequences to
use for opening/closing elements).

### org-global-cycle + outline-magic<a id="sec-27-5-2" name="sec-27-5-2"></a>

The visibility-cycling features are written in a way that they are independent
of the outline setup.  The following setup provides standard Org mode
functionality (headline folding and unfolding) in `outline-minor-mode` on
`C-TAB` and `S-TAB`.  We use `C-TAB` instead of `TAB`, because `TAB` usually
has mode-specific tasks.

Note that for `C-TAB` to work, the cursor needs to be on a headline (the line
where the ellipsis shows).

    ;; (add-hook 'outline-minor-mode-hook
    ;;   (lambda ()
    ;;     (define-key outline-minor-mode-map (kbd "<C-tab>") #'org-cycle)
    ;;     (define-key outline-minor-mode-map (kbd "<S-tab>") #'org-global-cycle))) ; backtab?

Now doing

M-x find-library RET ox.el RET

S-TAB (one or more times)

will give you a give quick overview of all the function names.

Or check out `outline-magic.el`, which does this and also provides **promotion and
demotion** functionality.

### org<a id="sec-27-5-3" name="sec-27-5-3"></a>

    (global-set-key (kbd "<S-tab>") #'org-cycle) ; that works (but on level 1+)
    ;; TODO Look at org-cycle-global and local below, they work better, but
    ;; still on level 1+
    ;; TODO Replace it by a function which alternatively does `hide-body' and
    ;; `show-all'

### org-struct-mode<a id="sec-27-5-4" name="sec-27-5-4"></a>

> Experienced users use outline-minor-mode - It takes time to getting used to
> it.

You can also use `orgstruct-mode`, a minor mode that comes with `org-mode`.

`org-global-cycle` (!= `org-cycle-global`) now works fine in buffers using
`orgstruct-mode`.

As for handling &ldquo;large&rdquo; files, I use this:

      ;; from Bastien
    
      ;; ;; XXX 2010-06-21 Conflicts with outline-minor-mode bindings
      ;; ;; add a hook to use `orgstruct-mode' in Emacs Lisp buffers
      ;; (add-hook 'emacs-lisp-mode-hook #'orgstruct-mode)
    
      (defun org-cycle-global ()
        (interactive)
        (org-cycle t))
    
      (global-set-key (kbd "C-M-]") #'org-cycle-global)
                                            ; XXX ok on Emacs Lisp, not on LaTeX
                                            ; S-TAB?
    
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
    
      (global-set-key (kbd "M-]") #'org-cycle-local)
                                            ; XXX ok on Emacs Lisp, not on LaTeX
    
    ;; C-M-] and M-] fold the whole buffer or the current defun.

I made a video to demonstrate it quickly: [navigating-emacs](https://vimeo.com/55570133).

This helps me survive in files like `org.el` and `org-agenda.el`.

### fold-dwim<a id="sec-27-5-5" name="sec-27-5-5"></a>

    ;; (global-set-key (kbd "C-c +") #'fold-dwim-toggle)
    ;; (global-set-key (kbd "C-c C-+") #'fold-dwim-show-all)
    ;; (global-set-key (kbd "C-c C--") #'fold-dwim-hide-all)

### fold-dwim-org<a id="sec-27-5-6" name="sec-27-5-6"></a>

    ;; ;; Unified user interface for Emacs folding modes, bound to Org key-strokes.
    ;; (try-require 'fold-dwim-org)

    ;; 25.8.2
    (global-set-key (kbd "<M-f6>") #'visible-mode)

## Boxquote<a id="sec-27-6" name="sec-27-6"></a>

Use Unicode characters.

    ;;** (info "(emacs-goodies-el)boxquote")
    
      (leuven--section "(emacs-goodies-el)boxquote")
    
      (with-eval-after-load "boxquote-autoloads"
        (global-set-key (kbd "C-c q") #'boxquote-region))
    
      (with-eval-after-load "boxquote"
        (setq boxquote-top-and-tail  "────")
        (setq boxquote-title-format  " %s")
        (setq boxquote-top-corner    "  ┌")
        (setq boxquote-side          "  │ ")
        (setq boxquote-bottom-corner "  └"))

In Gnus, you can mark some region with enclosing tags by pressing `C-c M-m`
(`message-mark-inserted-region`) or by clicking on `<menu-bar> <Message> <Insert
Region Marked>`.

    --8<---------------cut here---------------start------------->8---
    ...
    ...
    ...
    --8<---------------cut here---------------end--------------->8---

## Phonetic<a id="sec-27-7" name="sec-27-7"></a>

    ;;** (info "phonetic")
    
      (leuven--section "phonetic")
    
      ;; Phonetic spelling.
      (when (locate-library "phonetic")
        (autoload 'phonetize-region "phonetic"
          "Translate the region according to the phonetic alphabet." t))

    )                                       ; Chapter 25 ends here.

# Org Mode (Getting Things Done)<a id="sec-28" name="sec-28"></a>


-   40 variables most frequently customized in Org
    <http://orgmode.org/worg/org-configs/org-customization-survey.html>

-   Org config examples
    <http://repo.or.cz/w/Worg.git/tree/HEAD:/org-configs>

    ;;* 25.10 Org Mode
    
    ;; (info "(org)Top") outline-based notes management and organizer
    
    (leuven--chapter leuven-load-chapter-25.10-org-mode "25.10 Getting Things Done (with Org mode)"

After all the configuration has been done, you can easily manage your daily
work and tasks with Org mode.

    ;;* 1 (info "(org)Introduction")
    
    ;;** 1.2 (info "(org)Installation")
    
      ;; Autoloads.
      (try-require 'org-loaddefs)
    
      ;; Getting started.
      (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
      (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
    
      (global-set-key (kbd "C-c l") #'org-store-link)
      (global-set-key (kbd "C-c c") #'org-capture)
      (global-set-key (kbd "C-c a") #'org-agenda)
    
      ;; Using links outside Org.
      (global-set-key (kbd "C-c L") #'org-insert-link-global)
      (global-set-key (kbd "C-c O") #'org-open-at-point-global)
    
      (when (or (not (boundp 'org-agenda-files))
                (null org-agenda-files))
        (message "WARN- Found no entries in `org-agenda-files'")
        (sit-for 1.5))
    
      (with-eval-after-load "org"
        ;; Display the Org mode manual in Info mode.
        (global-set-key (kbd "C-h o") #'org-info)
                                            ; XXX Not autoloaded.
    
        ;; Unbind `C-j' and `C-''.
        (define-key org-mode-map (kbd "C-j") nil)
        (define-key org-mode-map (kbd "C-'") nil)) ; `org-cycle-agenda-files'.

These variables need to be set before Org mode is loaded.

    ;; These variables need to be set before org.el is loaded...
    
    ;; ;; Face to be used by `font-lock' for highlighting in Org mode Emacs
    ;; ;; buffers, and tags to be used to convert emphasis fontifiers for HTML
    ;; ;; export. XXX Format changed! XXX
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

`org-emphasis-alist` has a `:set` handler `org-set-emph-re` which will do the job of
setting up the regexps. I don&rsquo;t want to call `org-set-emph-re` directly, instead
I set `org-emphasis-alist` to itself and let the customize interface call the
handler for me.

    (with-eval-after-load "org"
      ;; Allow both single and double quotes in the border.
      (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
      (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)))

    ;; Single character alphabetical bullets (a, b, c, ..., X, Y, Z) are allowed.
    (setq org-list-allow-alphabetical t)

If you use `org-emphasis-alist` to do simple highlighting, you should better use
[font-lock-add-keywords](http://www.emacswiki.org/emacs/AddKeywords).

    ;; Libraries that should (always) be loaded along with `org.el'
    ;; (loaded when opening the first Org file).
    (setq org-modules nil)

Original value was:
-   `org-bbdb`
-   `org-bibtex`
-   `org-docview`
-   `org-gnus`
-   `org-info`
-   `org-jsinfo`
-   `org-irc`
-   `org-mew`
-   `org-mhe`
-   `org-rmail`
-   `org-vm`
-   `org-w3m`
-   `org-wl`

When you have `org-id` in `org-modules`, unique **ID&rsquo;s** are generated when you link
to a task (using `org-id-get-create`) &#x2014; instead of links containing **headline
text**.

ID&rsquo;s are saved in `~/.emacs.d/.org-id-locations` on my system (see
`org-id-locations-file`).

    ;; Check that org-checklist is found before adding it!
    ;;
    ;;   ;; Set the RESET_CHECK_BOXES and LIST_EXPORT_BASENAME properties in items as
    ;;   ;; needed.
    ;;   (add-to-list 'org-modules 'org-checklist) ; From org-contrib.

    ;; Globally unique ID for Org mode entries (see `org-store-link')
    ;; (takes care of automatically creating unique targets for internal
    ;; links, see `C-h v org-id-link-to-org-use-id RET').
    (add-to-list 'org-modules 'org-id)

<div class="note">
If you want to insert a **link to a file** in an Org mode document, you can also
call `org-insert-link` (`C-c C-l`) with a `C-u` prefix (to get the regular `find-file`
interface).

</div>

    ;; Support for links to Gnus groups and messages from within Org mode.
    (add-to-list 'org-modules 'org-gnus)

    ;; Habit tracking code for Org mode.
    (add-to-list 'org-modules 'org-habit)

    ;; Make sure to turn `org-info' on in order to link to info nodes.
    (add-to-list 'org-modules 'org-info)

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Look at other modules</b><br  />
-   `org-eval`
-   `org-eval-light`
-   `org-exp-bibtex`
-   `org-exp-blocks` (supported by the new exporter out of the box)
-   `org-man`
-   `org-mouse`
-   `org-mtags`
-   `org-panel`
-   `org-R`
</div>

    (add-hook 'org-mode-hook
              (lambda ()
                ;; ;; Create a binding for `org-show-subtree'.
                ;; (local-set-key (kbd "C-c C-S-s") #'org-show-subtree)
                ;; (local-set-key (kbd "C-c s") #'org-show-subtree)
    
                ;; (local-set-key (kbd "C-c h") #'hide-other) ; XXX Helm
    
                ;; Table.
                (local-set-key (kbd "C-M-w") #'org-table-copy-region)
                (local-set-key (kbd "C-M-y") #'org-table-paste-rectangle)
    
                ;; Remove some bindings.
                (local-unset-key (kbd "C-c SPC")) ; Used by Ace Jump.
                (local-unset-key (kbd "C-c C-<")) ; Used by Multiple Cursors.
                ;; (local-unset-key (kbd "C-c %")) ; XXX
                ;; (local-unset-key (kbd "C-c &")) ; XXX
    
                ))

<div class="note">
See `(define-key flyspell-mode-map (kbd "C-;") nil))` for an alternative manner of
unsetting local keys?

</div>

### Activation<a id="sec-28-0-1" name="sec-28-0-1"></a>

      (with-eval-after-load "org"
        (message "... Org Introduction")
    
    ;;** 1.3 (info "(org)Activation")
    
        (leuven--section "1.3 (org)Activation")
    
        ;; Insert the first line setting Org mode in empty files.
        (setq org-insert-mode-line-in-empty-file t))

## Document Structure<a id="sec-28-1" name="sec-28-1"></a>

    ;;* 2 (info "(org)Document Structure")
    
      (with-eval-after-load "org"
        (message "... Org Document Structure")
    
        ;; Improve display of the ellipsis.
        (set-face-attribute 'org-ellipsis nil
                            :box '(:line-width 1 :color "#999999")
                            :foreground "#999999" :background "#FFF8C0"
                            :underline nil)
    
        ;; Ellipsis to use in the Org mode outline.
        (setq org-ellipsis
              (if (char-displayable-p ?\u25BA) ; This test takes ~ 0.40s hence,
                                               ; wrapped in `with-eval-after-load'.
                  " \u25BA"                 ; String (black right-pointing pointer) XXX #929490
                'org-ellipsis)))            ; Face.
    
      ;; RET follows links (except in tables, where you must use `C-c C-o').
      (setq org-return-follows-link t)
    
      ;; Blank lines.
      (setq org-blank-before-new-entry
            '(;; Insert  a blank line before new heading.
              (heading . t)
    
              ;; Try to make an intelligent decision whether to insert a
              ;; blank line or not before a new item.
              (plain-list-item . auto)))

### Headlines<a id="sec-28-1-1" name="sec-28-1-1"></a>

`C-e` goes **right before** the end of the invisible region and `TAB` will unfold as
expected, both in a folded subtree or in a folded drawer.

    ;;** (info "(org)Headlines")
    
      (leuven--section "2.2 (org)Headlines")
    
      ;; ;; `C-a' and `C-e' behave specially in headlines and items.
      (setq org-special-ctrl-a/e 'reversed)

The package `org-inlinetask.el` (for **tasks independent of outline hierarchy**)
installs the key binding `C-c C-x t` to insert a new **inline task**.

    (with-eval-after-load "org"
      (message "... Org Headlines")
    
      ;; Insert an inline task (independent of outline hierarchy).
      (try-require 'org-inlinetask))      ; Needed.
    
    (with-eval-after-load "org-inlinetask"
    
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

    (defun org-latex-format-inlinetask-default-function
      (todo _todo-type priority title tags contents info)
      "Default format function for a inlinetasks.
    See `org-latex-format-inlinetask-function' for details."
      (let ((full-title
             (concat (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
                     (when priority (format "\\framebox{\\#%c} " priority))
                     title
                     (when tags
                       (format "\\hfill{}\\textsc{:%s:}"
                               (mapconcat
                                (lambda (tag) (org-latex-plain-text tag info))
                                tags ":"))))))
        (concat "\\begin{center}\n"
                "\\fcolorbox{black}{yellow}{\n"
                "\\begin{minipage}[c]{\\textwidth}\n"
                full-title "\n\n"
                (and (org-string-nw-p contents)
                     (concat "\\rule[.8em]{\\textwidth}{2pt}\n\n" contents))
                "\\end{minipage}\n"
                "}\n"
                "\\end{center}")))

    (setq org-inlinetask-export-templates
          '((latex "%s\\footnote{%s\\\\ %s}\\marginpar{\\fbox{\\thefootnote}}"
                   '((unless
                         (eq todo "")
                       (format "\\fbox{\\textsc{%s%s}}" todo priority))
                     heading content))))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Use todonotes</b><br  />
Why use footnotes when you can use `todonotes`
(<https://www.ctan.org/pkg/todonotes>)?

It can even make a `list-of-todo-notes`.  See also
<http://tex.stackexchange.com/questions/9796/how-to-add-todo-notes> for more
options.
</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Have a solution for base article, report, book (w/o todonotes)</b><br  />
nil</div>

### Visibility cycling<a id="sec-28-1-2" name="sec-28-1-2"></a>

    ;;** (info "(org)Visibility cycling")
    
      (leuven--section "2.3 (org)Visibility cycling")
    
      ;; Do not switch to OVERVIEW at startup.
      (setq org-startup-folded nil)         ; Emacs hangs when editing a 5-line Org
                                            ; file (with Company auto-starting after
                                            ; 2 characters and 0 s delay)
    
      ;; Inhibit startup when preparing agenda buffers -- agenda optimization.
      (setq org-agenda-inhibit-startup t)

    (setq w32-pass-apps-to-system nil)
    (setq w32-apps-modifier 'hyper)       ; Apps key.
    
    (with-eval-after-load "org"
      ;; Create indirect buffer and narrow it to current subtree.
      (define-key org-mode-map (kbd "<H-RET>") #'org-tree-to-indirect-buffer))

Note that **Imenu** does display the current **Org subtree** in the mode line, not the
full outline path.

### 2.4 Motion<a id="sec-28-1-3" name="sec-28-1-3"></a>

    ;;** (info "(org)Motion")
    
      (leuven--section "2.4 (org)Motion")
    
      ;; Outline-node based navigation similar to the behavior of paredit-mode in
      ;; Lisp files.
      (add-hook 'org-mode-hook
                (lambda ()
                  ;; (local-set-key (kbd "M-n") #'outline-next-visible-heading)
                  ;; (local-set-key (kbd "M-p") #'outline-previous-visible-heading)
                  ;;
                  (local-set-key (kbd "C-M-n") #'outline-next-visible-heading)
                  (local-set-key (kbd "C-M-p") #'outline-previous-visible-heading)
                  (local-set-key (kbd "C-M-u") #'outline-up-heading)))

Move through an Org file like `cd` on a file system with `org-goto`:
`C-c C-j /foo/bar/baz RET`

vs `C-u C-c C-j`

    ;; Headlines in the current buffer are offered via completion
    ;; (interface also used by the `refile' command).
    (setq org-goto-interface 'outline-path-completion)

    (with-eval-after-load "org"
    
      (defun leuven-org-reveal (&optional all-siblings)
        "Show all siblings of current level.
      `C-u C-c C-r' does the same as default Org mode: show all hidden siblings."
        (interactive "P")
        (if all-siblings
            (org-reveal t)
          (org-show-siblings)))
    
      (define-key org-mode-map (kbd "C-c C-r") #'leuven-org-reveal))

See also Sparse trees (See section 28.1.5) for behavior when revealing a location.

### Structure editing<a id="sec-28-1-4" name="sec-28-1-4"></a>

    ;;** (info "(org)Structure editing")
    
      (leuven--section "2.5 (org)Structure editing")
    
      ;; Don't adapt indentation to outline node level.
      (setq org-adapt-indentation nil)
    
      ;; ;; FIXME Choose the right value!
      ;; (setq org-M-RET-may-split-line nil)

### Sparse trees<a id="sec-28-1-5" name="sec-28-1-5"></a>


    ;;** (info "(org)Sparse trees")
    
      (leuven--section "2.6 (org)Sparse trees")
    
      (with-eval-after-load "org"

To control visibility when revealing a location, see `org-show-context-detail`:

    (when (boundp 'org-show-context-detail)
      (add-to-list 'org-show-context-detail '(tags-tree . minimal))
      (add-to-list 'org-show-context-detail '(occur-tree . minimal))))

### Plain lists<a id="sec-28-1-6" name="sec-28-1-6"></a>

An empty line does not end all plain list levels: if you need to separate
consecutive lists with blank lines, always use two of them.

    ;;** (info "(org)Plain lists")
    
      (leuven--section "2.7 (org)Plain lists")
    
      ;; Maximum indentation for the second line of a description list.
      (setq org-description-max-indent 3)
    
      ;; Don't make tab cycle visibility on plain list items.
      (setq org-cycle-include-plain-lists nil) ;; 'integrate?
    
      ;; (setq org-cycle-separator-lines -2)

### Footnotes <a id="sec-28-1-7" name="sec-28-1-7"></a>

    ;;** (info "(org)Footnotes")
    
      (leuven--section "2.10 (org)Footnotes")
    
      ;; Use `C-c C-x f' to add a footnote, to go back to the message
      ;; *and* to go to a footnote.
      (global-set-key (kbd "C-c C-x f") #'org-footnote-action)

## Tables<a id="sec-28-2" name="sec-28-2"></a>

See [Org as a spreadsheet system: a short introduction](http://orgmode.org/worg/org-tutorials/org-spreadsheet-intro.html).

    ;;* 3 (info "(org)Tables")
    
      (setq org-table-use-standard-references 'from)
    
    ;;** 3.1 The (info "(org)Built-in table editor")
    
      (leuven--section "3.1 The (org)Built-in table editor")
    
      ;; Default export parameters for `org-table-export'.
      (setq org-table-export-default-format "orgtbl-to-csv")
    
    ;;** 3.5 (info "(org)The spreadsheet")
    
      (leuven--section "3.5 (org)The spreadsheet")
    
      (with-eval-after-load "org-table"
        ;; Some Calc mode settings for use in `calc-eval' for table formulas.
        (setcar (cdr (memq 'calc-float-format org-calc-default-modes))
                '(float 12)))               ; [Default: 8]

To **update all tables** in a file, use `org-table-recalculate-buffer-tables` if the
dependencies are only backwards.

If you have dependencies in both directions, to iterate all tables in a file,
in order to converge table-to-table dependencies, use
`org-table-iterate-buffer-tables`.

## Hyperlinks<a id="sec-28-3" name="sec-28-3"></a>

    ;;* 4 (info "(org)Hyperlinks")

If you insert a <http://> link containing equal signs (such as
<http://test/test?name=me>), the verbatim link being inserted in the Org document
has the equal signs escaped, unless you have:

    ;; Don't hexify URL when creating a link.
    (setq org-url-hexify-p nil)

### External links<a id="sec-28-3-1" name="sec-28-3-1"></a>

    (with-eval-after-load "org"
      (message "... Hyperlinks")
    
      ;; Open non-existing files.
      (setq org-open-non-existing-files t)
    
      ;; Function and arguments to call for following `mailto' links.
      (setq org-link-mailto-program '(compose-mail "%a" "%s")))

    ;; Support for links to Gnus groups and messages from within Org mode.
    (with-eval-after-load "org-gnus"
    
      ;; Create web links to Google groups or Gmane (instead of Gnus messages).
      (setq org-gnus-prefer-web-links t))

### Handling links<a id="sec-28-3-2" name="sec-28-3-2"></a>

    ;; Global identifiers for Org mode entries.
    (with-eval-after-load "org-id"
    
      ;; Storing a link to an Org file will use entry IDs.
      (setq org-id-link-to-org-use-id
            'create-if-interactive-and-no-custom-id))

PDF viewer?  See `org-file-apps`.  Just change to your favorite viewer.  And make
sure that you&rsquo;re calling `org-return` to open (it&rsquo;s bound to `C-m`).

    ;; PDFs visited in Org mode are opened in Evince (and not in the default choice).
    (add-hook 'org-mode-hook
              (lambda ()
                (delete '("\\.pdf\\'" . default) org-file-apps)
                (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

    (add-hook 'org-mode-hook
              (lambda ()
                (delete '("\\.x?html?\\'" . default) org-file-apps)
                (add-to-list 'org-file-apps '("\\.x?html?\\'" . system))))
    
    ;; (setcdr (assq 'org-file-apps (assoc "\\.x?html?\\'" org-file-apps))
    ;;         'system)

If I inline an image in an Org file, I can toggle its display with `C-c C-x C-v`
and resize it to a specific width.

    (with-eval-after-load "org"
      (message "... Handling links")
    
      ;; 4.4 Show inline images when loading a new Org file.
      (setq org-startup-with-inline-images t) ; Invokes org-display-inline-images.
    
      ;; 4.4 Try to get the width from an #+ATTR.* keyword and fall back on 320px
      ;; width if none is found.
      (setq org-image-actual-width '(320))

Search for backlinks:

    (defun leuven-org-search-backlinks ()
      "Show all entries that point to the current node.  Also show the current
    node itself.
    
    This makes ID links quasi-bidirectional."
      (interactive)
      (let ((org-agenda-files
             (add-to-list 'org-agenda-files (buffer-file-name))))
        (org-search-view nil (org-entry-get nil "ID" t))))

### Link abbreviations<a id="sec-28-3-3" name="sec-28-3-3"></a>

Link abbreviations provide a quick way of linking to resources that are
frequently referenced, so that you can type just the key term instead of the
complete URL.

Here is an example: every time you need to use
<http://www.google.com/search?q=searchterms>, you can just type
`[[google:searchterms]]` or `[[google:searchterms][description]]` instead.

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

## &ldquo;TODO&rdquo; Items<a id="sec-28-4" name="sec-28-4"></a>

    ;;* 5 (info "(org)TODO Items")

### Basic TODO functionality<a id="sec-28-4-1" name="sec-28-4-1"></a>

    ;;** 5.1 (info "(org)TODO basics") functionality
    
      (leuven--section "5.1 (org)TODO basics functionality")
    
      ;; 5.1 Select a TODO state and bypass any logging associated with that.
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    
      ;; Some commands act upon headlines in the active region.
      (setq org-loop-over-headlines-in-active-region 'start-level)

### Extended use of TODO keywords<a id="sec-28-4-2" name="sec-28-4-2"></a>

We use statuses to **create a workflow** for tasks.

The statuses we use are a simple as possible to work well in the context of
work.

We use the following 4-letter status keywords:

-   **`TODO`:** Approved task, to be done.

-   **`STRT`:** Some progress has already been done on the task.

-   **`WAIT`:** Task delegated to someone else.  Or waiting for some external event to be
    able to work on the task.

-   **`SDAY`:** Task to be approved (and eventually done) in the future.

-   **`DONE`:** Task is completed.

-   **`CANX`:** Task is declined (but kept documented, instead of being deleted).

There is just 1 exception (made on purpose) to the 4-letter scheme:

-   **`NEW`:** Proposed task, idea or wish, **not approved yet**.  Draft.

One could add a `CLSD` or `VRFD` state for bug fixes, for example, which need to be
verified before really closing the task.

Extra status for invoices:

-   **`DRAFT`:** Invoice created, but you have **not sent** it to your client.

-   **`UNPAID` (or `SENT`?):** Your client has been notified.

-   **`PAID`:** Your client has **paid** this invoice &#x2013; you have received their payment.

(See FreshBooks, LessAccounting and BlinkSale)

    ;;** 5.2 Use of (info "(org)TODO extensions")
    
      (leuven--section "5.2 Use of (org)TODO extensions")
    
      ;; List of TODO entry keyword sequences (+ fast access keys and specifiers
      ;; for state change logging).
      (setq org-todo-keywords
            '((sequence "NEW(n!)"           ; Proposal, idea (under review), to be
                                            ; prioritized.
                        "TODO(t!)"          ; Open, not (yet) started.
                        "STRT(s!)"          ; In progress, working on, doing.
                        "WAIT(w!)"          ; On hold, to be discussed, assigned,
                                            ; feedback.
                        "SDAY(y!)"          ; Someday, maybe, perhaps, wish.
                        "|"
                        "DONE(d!)"          ; Completed, closed, fixed, resolved,
                                            ; verified.
                        "CANX(x!)")         ; Wontfix, rejected, ignored.
    
              (sequence "QTE(q!)"           ; Planning.
                        "QTD(Q!)"           ; Awaiting approval.
                        "|"
                        "APP(A!)"           ; Approved.
                        "REJ(R!)")          ; Rejected.
    
              (sequence "OPENPO(O!)"
                        "|"
                        "CLSDPO(C!)")))

`SDAY` is **not** a completion state (in order not to be struck through).

    (with-eval-after-load "org-faces"
    
      ;; Faces for specific TODO keywords.
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
    
      ;; Org standard faces.
      (set-face-attribute 'org-todo nil
                          :weight 'bold :box '(:line-width 1 :color "#D8ABA7")
                          :foreground "#D8ABA7" :background "#FFE6E4")
    
      (set-face-attribute 'org-done nil
                          :weight 'bold :box '(:line-width 1 :color "#BBBBBB")
                          :foreground "#BBBBBB" :background "#F0F0F0")
    
      ;; Org non-standard faces.
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

    ;; Block switching entries to DONE if
    ;; 1) there are undone child entries, or
    ;; 2) the parent has an `:ORDERED:' property and there are prior
    ;;    siblings not yet done.
    (setq org-enforce-todo-dependencies t)

    ;; 5.2.7 Don't dim blocked tasks in the agenda display -- agenda optimization.
    (setq org-agenda-dim-blocked-tasks nil) ; XXX not sure about this one

    ;; Block switching the parent to DONE if there are unchecked checkboxes.
    (setq org-enforce-todo-checkbox-dependencies t)

### Progress logging<a id="sec-28-4-3" name="sec-28-4-3"></a>

    ;;** 5.3 (info "(org)Progress logging")
    
      (leuven--section "5.3 (org)Progress logging")
    
      ;; ;; 5.3.1 Don't insert a CLOSED time stamp each time a TODO entry is marked DONE.
      ;; (setq org-log-done nil)
    
      ;; 5.3.2 The notes will be ordered according to time.
      (setq org-log-states-order-reversed nil)
    
      ;; 5.3.2 Insert state change notes and time stamps into a LOGBOOK drawer.
      (setq org-log-into-drawer t)          ; should be the DEFAULT!
    
      ;; ~5.3.2 Heading for state change added to entries.
      (with-eval-after-load "org"
        (message "... Progress logging")
    
        (setcdr (assq 'state org-log-note-headings)
                "State %-12S  ->  %-12s %t")) ; "State old -> new + timestamp".

Warning! The docstring of `org-log-note-headings` mentions that &ldquo;in fact, it is
**not a good idea to change the \`state&rsquo; entry**, because agenda log mode depends
on the format of these entries.&rdquo;

    (with-eval-after-load "org-habit"
    
      ;; Show habits for future days.
      (setq org-habit-show-habits-only-for-today nil)
    
      ;; Use character "heavy check mark" to show completed days on which a task
      ;; was done.
      (setq org-habit-completed-glyph ?\u2714)
    
      ;; Use character "heavy quadruple dash vertical" to identify today.
      (setq org-habit-today-glyph ?\u250B))

### Breaking down tasks<a id="sec-28-4-4" name="sec-28-4-4"></a>

    ;;** 5.5 (info "(org)Breaking down tasks")
    
      (leuven--section "5.5 (org)Breaking down tasks")
    
      ;; Automatically change a TODO entry to DONE when all children are done.
      (defun leuven--org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states)  ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    
      (add-hook 'org-after-todo-statistics-hook #'leuven--org-summary-todo)

## Tags<a id="sec-28-5" name="sec-28-5"></a>

Context (place, time or particular resources for doing a task) and people are
something best implemented with tags.

By convention, **user-defined tags are written in lowercase**; built-in tags with
special meaning are written with all capitals.

`org-use-tag-inheritance` can be setup to a regular expression, for example so
that all tags starting with `@` or `#` will be excluded from inheritance with:

    (setq org-use-tag-inheritance "^[^@#]")

    ;;* 6 (info "(org)Tags")
    
      ;; Column to which tags should be indented in a headline.
      (setq org-tags-column -80)
    
      ;; 6.2 List of tags ("contexts") allowed in Org mode files.
      (setq org-tag-alist '((:startgroup . nil)
                             ("personal"  . ?p)
                             ("work"      . ?w)
                            (:endgroup . nil)
                            ("call"        . ?c)
                            ("errands"     . ?e)
                            ("finance"     . ?f)
                            ("mail"        . ?m)
    
                            ("notbillable" . ?B)
                            ;; ("reading" . ?r)
                            ;; ("proj" . ?P)
    
                            ("ARCHIVE"     . ?a) ; speed command + action in task list
                            ("crypt"       . ?C)
                            ("FLAGGED"     . ??) ; = ASAP
                            ))
    
      ;; Faces for specific tags.
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
               (:weight bold :slant italic
                :foreground "white" :background "#DB2D27")) ; :background "#EDC6C8"
              ("now"
               (:slant italic
                :foreground "#000000"))     ; :background "#FFEA80"
              ("notbillable"
               (:slant italic
                :foreground "#8774AF"))     ; :background "#DED0EA"
              ))
    
      ;; 6.2 Exit fast tag selection after first change (toggle this with `C-c').
      (setq org-fast-tag-selection-single-key t)

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Extend the following function to take `filetags` into account</b><br  />
nil</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Check whether this function removes tags of DIRECT parent</b><br  />
nil</div>

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

## Properties and Columns<a id="sec-28-6" name="sec-28-6"></a>

By convention, **user-defined properties are capitalized**; built-in properties with
special meaning are written with all capitals.

    ;;* 7 (info "(org)Properties and Columns")
    
    ;;** 7.1 (info "(org)Property syntax")
    
      (leuven--section "7.1 (org)Property syntax")
    
      ;; List of property/value pairs that can be inherited by any entry.
      (setq org-global-properties
            '(("Effort_ALL" .
               "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"
               ;; "0d 1d 2d 3d 4d 5d 6d 7d 8d 10d"
               ;; "0 1:00 4:00 1d 2d 1w 2w"
               )))

## Dates and Times<a id="sec-28-7" name="sec-28-7"></a>

    ;;* 8 (info "(org)Dates and Times")
    
      (leuven--section "8 (org)Dates and Times")
    
      ;; Insinuate appt if Org mode is loaded.
      (with-eval-after-load "org"
        (message "... Org Dates and Times")
    
        (try-require 'appt))

### Creating time stamps<a id="sec-28-7-1" name="sec-28-7-1"></a>

    ;;** 8.2 (info "(org)Creating timestamps")
    
      (leuven--section "8.2 (org)Creating time stamps")
    
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
    
      ;; Number of minutes to round time stamps to.
      (setq org-time-stamp-rounding-minutes '(1 1))

### Deadlines and scheduling<a id="sec-28-7-2" name="sec-28-7-2"></a>

    ;;** 8.3 (info "(org)Deadlines and scheduling")
    
      (leuven--section "8.3 (org)Deadlines and scheduling")

    ;; Information to record when the scheduling date is modified.
    (setq org-log-reschedule nil)
    
    ;; Information to record when the deadline date is modified.
    (setq org-log-redeadline 'time)

#### Inserting deadline/schedule<a id="sec-28-7-2-1" name="sec-28-7-2-1"></a>

Schedule it with `C-c C-s`:

-   **`RET`:** Today.

-   **`+1d RET`:** Tomorrow.

-   **`+1w RET`:** Next week.

-   **`+4w RET`:** Next &ldquo;month&rdquo;.

Remove scheduling date with `C-u C-c C-s`.

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

#### Repeated tasks<a id="sec-28-7-2-2" name="sec-28-7-2-2"></a>

    ;; TODO state to which a repeater should return the repeating task.
    (setq org-todo-repeat-to-state "TODO")

### Clocking work time<a id="sec-28-7-3" name="sec-28-7-3"></a>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Resume clocks when opening the first Org file</b><br  />
Alternative to requiring org-clock in every Emacs session.
</div>

    ;;** 8.4 (info "(org)Clocking work time")
    
      (leuven--section "8.4 (org)Clocking work time")
    
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
      ;; (require 'org-clock)               ;! needed for trying to automatically
                                            ;! re-clock at Emacs startup
    
      ;; XXX Under test!
      (add-hook 'org-mode-hook
                (lambda ()
                  (require 'org-clock)
                  (setq org-clock-persist t)
                  (org-clock-persistence-insinuate)))
    
      (with-eval-after-load "org-clock"
    
        ;; ;; 8.4 Save both the running clock and the entire clock history when Emacs
        ;; ;; is closed, and resume it next time Emacs is started up.
        ;; (setq org-clock-persist t)
        ;;
        ;; ;; 8.4 Set up hooks for clock persistence.
        ;; (org-clock-persistence-insinuate)
    
        ;; Resume clocking task on clock-in if the clock is open.
        (setq org-clock-in-resume t)
    
        ;; Number of clock tasks to remember in history.
        (setq org-clock-history-length 35)  ; 1-9A-Z
    
        ;; 8.4.2 Include the current clocking task time in clock reports.
        (setq org-clock-report-include-clocking-task t)
    
        ;; 8.4.2 Format string used when creating CLOCKSUM lines and when generating
        ;; a time duration (avoid showing days).
        (setq org-time-clocksum-format
              '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
                                            ; Some clocktable functions cannot
                                            ; digest day formats (e.g.,
                                            ; org-clock-time%).
    
        ;; ;; 8.4.2 Use fractional times.
        ;; (setq org-time-clocksum-use-fractional t)
    
        ;; Format string for the total time cells.
        (setq org-clock-total-time-cell-format "%s")
    
        ;; Format string for the file time cells.
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
    
        (global-set-key (kbd "C-c C-x C-q") #'leuven-org-clock-in-interrupted-task)
    
        ;; 8.4.3 Resolve open clocks if the user is idle more than X minutes.
        (setq org-clock-idle-time 240)
    
        (defun leuven--org-switch-to-started (kwd)
          "Switch task state to STRT.
        Skip normal headlines and capture tasks."
          (if (and kwd
                   (not (string-equal kwd "STRT"))
                   (not (and (boundp 'org-capture-mode) org-capture-mode)))
              "STRT"
            nil))
    
        ;; 8.4.3 Set task to todo state STRT while clocking it.
        (setq org-clock-in-switch-to-state 'leuven--org-switch-to-started)
    
        ;; Clock won't be stopped when the clocked entry is marked DONE.
        (setq org-clock-out-when-done nil)
    
        ;; Time included for the mode line clock is all time clocked into this task
        ;; today.
        (setq org-clock-mode-line-total 'today)
        (setq org-clock-mode-line-total 'all)
    
        ;; Get an alert (notification) when your planned time is over.
        (setq org-clock-sound "~/Public/Music/Sounds/alarm.wav")
        ;;! Use start-process to have an external program play the sound to
        ;;! avoid ignored keystrokes until after the sound plays (start-process
        ;;! "ding" nil "play" "~/Public/Music/Sounds/alarm.wav")
    
        ;; Default range when displaying clocks with `org-clock-display'.
        (setq org-clock-display-default-range 'untilnow)
    
        ;; Remove the clock line when the resulting time is 0:00.
        (setq org-clock-out-remove-zero-time-clocks t)
    
        ;; ;; When clocking into a task with a clock entry which has not been
        ;; ;; closed, resume the clock from that point.
        ;; (setq org-clock-in-resume t)
    
        ;; Ask the user if they wish to clock out before killing Emacs.
        (defun leuven--org-query-clock-out ()
          "Ask the user before clocking out.
        This is a useful function for adding to `kill-emacs-query-functions'."
          (if (and (featurep 'org-clock)
                   (funcall 'org-clocking-p)
                   (y-or-n-p "You are currently clocking time, clock out? "))
              (org-clock-out)
            t))                             ; Only fails on keyboard quit or error.
    
        (add-hook 'kill-emacs-query-functions #'leuven--org-query-clock-out)
    
        )                                   ; with-eval-after-load "org-clock" ends here.

### Effort estimates<a id="sec-28-7-4" name="sec-28-7-4"></a>

If you have an `Effort` property defined, the **estimated time** is also shown in
the mode line, against the **actual time** spent (reported through **time clocking**).

When clocking in, ask for a time estimate if the property is not yet defined.

    ;;** 8.5 (info "(org)Effort estimates")
    
      (leuven--section "8.5 (org)Effort estimates")
    
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

XXX We could forbid an empty answer to the effort estimate question! XXX

XXX We could ask for a ranged estimate (best case - worst case).

XXX When clocking out, show the remaining effort (allows to re-estimate for
a better schedule).

## Capture - Refile - Archive<a id="sec-28-8" name="sec-28-8"></a>

### Capture<a id="sec-28-8-1" name="sec-28-8-1"></a>

The ultimate capture tool to **quickly add** tasks or notes.

Structure:
-   Contacts
-   Calendar
-   Tasks
-   Notes
-   Journal

    ;;* 9 (info "(org)Capture - Refile - Archive")
    
      (leuven--section "9.1 (org)Capture")
    
      ;; 9.1.2 Directory with Org files.
      (setq org-directory
            (directory-file-name            ; This function removes the final slash.
             (cond ((file-directory-p "~/org/") "~/org/")
                   (t "~/"))))
    
      ;; 9.1.2 Default target for storing notes.
      (setq org-default-notes-file          ; Inbox for collecting
                                            ; [Default: "~/.notes"].
            (concat org-directory "/refile.org"))

`%`-escapes:

-   **%a:** Annotation (**link**).

-   **%i:** Initial content (**selected text**).

-   **%?:** Cursor position.

-   **%^T:** Prompt for a date and time.

-   **%^G:** Prompt for tags with completion on tags in all agenda files.

-   **%t:** Time stamp (date only).

-   **%<sup>prompt</sup>:** Prompt the user for a string.

-   **%[file]:** Insert the contents of the file.

-   **%U:** Inactive time stamp with date and time.

Looking at the info manual I found the documentation for &ldquo;immediate-finish&rdquo;, but
not for &ldquo;jump-to-captured&rdquo;. I found it in the code and it seems to be exactly
what I want.

Use `:jump-to-captured` to tell capture to jump to the note after storing it.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Look at `:kill-buffer`</b><br  />
nil</div>

      ;; 9.1.2 templates for the creation of capture buffers
    
      ;; ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")
    
      ;; Fast note taking in Org mode (the ultimate capture tool).
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
                       (file+headline ,org-default-notes-file "Events")
                       "* %^{Appointment}%?
    %^T
    
    %i"
                       :empty-lines 1) t)
                       ;; TODO Prompt only for date, not time...

#### TODO Ask for refile location<a id="sec-28-8-1-1" name="sec-28-8-1-1"></a>

Find a location for refiling on capture.

    (add-to-list 'org-capture-templates
                 `("Z" "Refile me!" entry
                   (function leuven--find-location)
                   "** TODO Put this in some other file\n\n"
                   :prepend t) t)
    
    (defun leuven--find-location ()
      "Find my CollectBox file and some headline in the current buffer."
      (find-file org-default-notes-file)
      (goto-char (point-min))
      (helm-org-in-buffer-headings)
      (org-forward-heading-same-level 1))

#### TODO Email-to-task<a id="sec-28-8-1-2" name="sec-28-8-1-2"></a>

Instantly **create tasks** and notes **from emails**, with a **link** back **to the emails**.

The following rules will apply to interpret an email to a task:

-   **Email subject:** Task name.

-   **Person in the `From:` field:** Task creator.

-   **Person(s) in the `To:` field:** Task assignee(s).

-   **Email body (or selected region):** Inserted into the task&rsquo;s description.

-   **Email attachment(s):** Inserted as task&rsquo;s attachment(s).
    
    XXX Wouldn&rsquo;t it be great if files attached to email can be quickly added
    to the tasks or projects you create?

Additional parameters can be included via the commands you use:

-   **Create a `TODO` Action:** Create a new action.

-   **Create a `TODO` Action Remind 3:** Create a new action.  Set reminder in 3 days.

-   **Create a `TODO` Action Due Date 7:** Create a new action.  Set due date in 7 days.

-   **Create a `TODO` Action of an existing Project:** Attach it to something that we&rsquo;re already working on.

-   **Create a `WAIT` Item:** Create a new &ldquo;Waiting For&rdquo; item, eventually with an &ldquo;Assignee&rdquo; property.

-   **Create a `SDAY` Item:** Create a new &ldquo;Someday&rdquo; item.

-   **Create a Reference Item:** Create a new &ldquo;Reference&rdquo; item.

        (add-to-list 'org-capture-templates
                     `("m" "Email processing...") t)
    
        (add-to-list 'org-capture-templates
                     `("mT" "Create a TODO Action + edit" entry
                       (file+headline ,org-default-notes-file "Messages") ; #+FILETAGS: :mail:
                       "* TODO %^{Creating action}%? (from %:fromname)
       %:date-timestamp-inactive
    
    #+begin_verse
    %i
    #+end_verse
    
    From %a"
                       :empty-lines 1) t)
    
        (add-to-list 'org-capture-templates
                     `("mt" "Create a TODO Action" entry
                       (file+headline ,org-default-notes-file "Messages") ; #+FILETAGS: :mail:
                       "* TODO %:subject%? (from %:fromname)
       %:date-timestamp-inactive
    
    #+begin_verse
    %i
    #+end_verse
    
    From %a"
                       :empty-lines 1
                       :immediate-finish t) t)

The property `:immediate-finish`, when set, immediately files the item without
further prompt (skipping `C-c C-c`); very handy for quick storing of emails.

With the clock options, `org-capture` automatically clocks in.  When pressing
`C-c C-c`, it clocks out.

    (add-to-list 'org-capture-templates
                 `("p" "Phone call" entry
                   (file+headline ,org-default-notes-file "Phone calls")
                   "* %?"
                   :clock-in t
                   :clock-resume t
                   :empty-lines 1) t)
    
    (add-to-list 'org-capture-templates
                 `("i" "interruption" entry
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
    ;;           :clock-in t
    ;;           :clock-resume t
    ;;           :immediate-finish t)
    ;;
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
                       :empty-lines 1
                       :immediate-finish t) t)
    
        ;; Default `org-capture-templates' key to use.
        (setq org-protocol-default-template-key "w")

    )                                   ; with-eval-after-load "org-capture" ends here.

### Attachments<a id="sec-28-8-2" name="sec-28-8-2"></a>

Use the following for conveniently accessing your data.

    ;; bug when C-c C-l
      ;; ;; 4.6 Shortcut links.
      ;; (add-to-list 'org-link-abbrev-alist '(("att" . org-attach-expand-link)))

Instead of modifying `org-link-abbrev-alist`, you can also use

    #+LINK: att %(org-attach-expand-link)

in your Org buffers.

### Protocols for external access<a id="sec-28-8-3" name="sec-28-8-3"></a>

Open Org source (and Capture) from Firefox.

Have a look at [Store links in Emacs Org using org-protocol.el](http://vimeo.com/5662410).

    (leuven--section "9.4 (org)Protocols")
    
    ;; 9.4 Capture from Firefox (to store links and text).
    (with-eval-after-load "org-protocol"
    
      ;; Map online URL to an existing working file.
      (add-to-list 'org-protocol-project-alist
                   '("Worg at http://orgmode.org/worg/"
                     :online-suffix ".html"
                     :working-suffix ".org"
                     :base-url "http://orgmode.org/worg/"
                     :working-directory "~/Public/Repositories/worg/") t))

### Refile and copy<a id="sec-28-8-4" name="sec-28-8-4"></a>

Once a date has been scheduled, **move the task to the appropriate project**, by
using the **refile** command `C-c C-w`.  This lets me select (with completion) the
header under which the entry will be placed.

    (with-eval-after-load "org"
      (message "... Org Refile")
    
      (defvar leuven-org-refile-extra-files
        (if (file-exists-p "~/org/notes/")
            (directory-files "~/org/notes/" t "^[^\\.#].*\\.\\(txt\\|org\\)$")
          nil)
        "List of extra files to be used as targets for refile commands.")
    
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

### Archiving<a id="sec-28-8-5" name="sec-28-8-5"></a>

    (leuven--section "9.6 (org)Archiving")
    
    ;; 9.6.1 Subtrees should be archived in the current file.
    (setq org-archive-location "::* Archive")
    
    )

## Agenda Views<a id="sec-28-9" name="sec-28-9"></a>

      (leuven--section "10 (org)Agenda Views")
    
    ;;* 10 (info "(org)Agenda Views")
    
      (with-eval-after-load "org-agenda"
    
        ;; Multiple same-day time stamps in entry make multiple agenda lines.
        (setq org-agenda-skip-additional-timestamps-same-entry nil)
    
        ;; Show outline path in echo area after line motion (though, may bring some
        ;; slowness).
        (setq org-agenda-show-outline-path t)
    
        ;; 10.0 Restore the window configuration when exiting the agenda.
        (setq org-agenda-restore-windows-after-quit t)

    ;; ;; Speed up agenda by avoiding to update some text properties.
    ;; (setq org-agenda-ignore-drawer-properties '(effort category)) ; org.el

### Agenda files<a id="sec-28-9-1" name="sec-28-9-1"></a>

    ;;** 10.1 (info "(org)Agenda files")
    
        (leuven--section "10.1 (org)Agenda files")
    
        (when (boundp 'org-agenda-files)
          (message "INFO- Found %s entries in `org-agenda-files'"
                   (length org-agenda-files))
          ;; (sit-for 0.5)
          )

Avoid any possibility of getting a customized version of `org-agenda-files` added
at the end of your `.emacs` file.

    (add-hook 'org-mode-hook
              (lambda ()
                (local-unset-key (kbd "C-c ["))
                (local-unset-key (kbd "C-c ]"))))

### The agenda dispatcher<a id="sec-28-9-2" name="sec-28-9-2"></a>

    ;;** 10.2 (info "(org)Agenda dispatcher")
    
        (leuven--section "10.2 (org)Agenda dispatcher")
    
        ;; Enable sticky agenda: `q' key will bury agenda buffers (instead of
        ;; killing).
        (setq org-agenda-sticky t)

### The Built-in agenda views<a id="sec-28-9-3" name="sec-28-9-3"></a>

    ;;** 10.3 The (info "(org)Built-in agenda views")
    
        (leuven--section "10.3 (org)Built-in agenda views")

Press `C-c a a` to jump you to this week&rsquo;s task page from anywhere.

    ;; Default duration for appointments that only have a starting time.
    (setq org-agenda-default-appointment-duration nil)
    
    ;; ;; Duration of an appointment will add to day effort.
    ;; (setq org-agenda-columns-add-appointments-to-effort-sum t)

`C-c a t` should show **all** the `TODO` items&#x2026; but&#x2026;

    ;; Show dated entries in the global `todo' list.
    (setq org-agenda-todo-ignore-with-date nil)
                                        ;!! tricky setting

    ;; Show entries with a time stamp in the global `todo' list.
    (setq org-agenda-todo-ignore-timestamp nil)

Don&rsquo;t tell me about stuff that isn&rsquo;t due yet: tasks scheduled in the future
should no longer show up in global todo or tags searches until that date
arrives &#x2013; at which point it will be in all lists.

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

#### Advanced Search<a id="sec-28-9-3-1" name="sec-28-9-3-1"></a>

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

### Presentation and sorting<a id="sec-28-9-4" name="sec-28-9-4"></a>

See
<http://www.dgtale.ch/index.php?option=com_content&view=article&id=52&Itemid=61>

    ;;** 10.4 (info "(org)Presentation and sorting")
    
        (leuven--section "10.4 (org)Presentation and sorting")

You can modify `org-agenda-prefix-format` to show extra information in the lists.

<div class="note">
If none of the built-in format specifiers are sufficient, you can use
`%(my-function-here)` as a specifier and `my-function-here` will be called on each
headline (to return any kind of string you want).

</div>

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
    
    (with-eval-after-load "org-faces"
    
      ;; Faces for showing deadlines in the agenda.
      (setq org-agenda-deadline-faces
            '((1.0001 . leuven-org-deadline-overdue)
              (0.9999 . leuven-org-deadline-today)
              (0.8571 . leuven-org-deadline-tomorrow) ; = 6/7, see `org-deadline-warning-days'
              (0.0000 . leuven-org-deadline-future)))
    
      ;; See http://www.dgtale.ch/index.php?option=com_content&view=article&id=52&Itemid=61.
    
      ;; Org non-standard faces.
      (defface leuven-org-deadline-overdue
        '((t (:foreground "#F22659")))
        "Face used to highlight tasks whose due date is in the past.")
    
      (defface leuven-org-deadline-today
        '((t (:weight bold :foreground "#4F4A3D" :background "#FFFFCC")))
        "Face used to highlight tasks whose due date is today.")
    
      (defface leuven-org-deadline-tomorrow
        '((t (:foreground "#40A80B")))
        "Face used to highlight tasks whose due date is tomorrow.")
    
      (defface leuven-org-deadline-future
        '((t (:foreground "#40A80B")))
        "Face used to highlight tasks whose due date is for later."))

    (with-eval-after-load "org-agenda"
    
      ;; ;; 10.4 Column to shift tags to (in agenda items).
      ;; (setq org-agenda-tags-column -132)
    
      ;; Right-justify tags in the agenda buffer.
      (defun leuven--org-agenda-right-justify-tags ()
        "Justify the tags to the right border of the agenda window."
        (let ((org-agenda-tags-column (- 2 (window-width))))
          (org-agenda-align-tags)))
      (add-hook 'org-agenda-finalize-hook #'leuven--org-agenda-right-justify-tags))

Show the time grid in the daily agenda but not in the weekly agenda, except
for today&rsquo;s date:

    ;; 10.4.2 Settings for time grid for agenda display.
    (setq org-agenda-time-grid '((daily remove-match)
                                 ""
                                 (0800 1000 1200 1400 1600 1800 2000)))
    
    ;; String for the current time marker in the agenda.
    (setq org-agenda-current-time-string "Right now")

`time-up` will only be applied to those items have a time stamp for the day in
question, so that other sorting parameters will not be outranked by `time-up` for
the rest of the entries.

<div class="note">
The file `refile.org` should come first.  My current workaround is to add
`:CATEGORY: @refile` in its subtrees, as the `@` symbol is alphabetically before the
letters.

</div>

Agenda view: put related tasks together, then by priority.

<div class="note">
Not sure about that anymore: the agenda view is to work on tasks, hence prior
tasks (then grouped by project) should arrive ahead; on the opposite, when doing
a review, it&rsquo;s good to have all tasks grouped by project.

</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Sort agenda by prior, then categ, then date</b><br  />
Inverting `category-up` and `priority-down` in the agenda does unexpected
sorting: first by priority (OK), but then by scheduled time (while it should be
grouped by category)!
</div>

    ;; 10.4.3 Sorting structure for the agenda items of a single day.
    (setq org-agenda-sorting-strategy   ; custom value
          '((agenda time-up category-up priority-down effort-down)
            ;; (agenda priority-down time-up category-up effort-down)
            (todo category-up priority-down effort-down)
            (tags category-up priority-down effort-down)
            (search category-up)))
    
    ;; Show agenda in the current window, keeping all other windows.
    (setq org-agenda-window-setup 'current-window)

With this function, the new sort order is temporary.  Pressing `r` for refresh
will restore the default sort order for the buffer:

    (defun my-org-agenda-change-sorting-strategy (strategy)
      "Change the sorting strategy."
      (interactive (list
                    (completing-read "Choose a strategy: "
                                     (mapcar 'cdr (cdr org-sorting-choice))
                                     nil t)))
      ;; adjust the following types as needed - e.g., add 'agenda, etc.
      (org-agenda-check-type t 'todo 'tags 'search)
      (let ((org-agenda-sorting-strategy (list (intern strategy))))
    (org-agenda-redo)))

### Agenda commands<a id="sec-28-9-5" name="sec-28-9-5"></a>

    ;;** 10.5 (info "(org)Agenda commands")
    
      (leuven--section "10.5 (org)Agenda commands")
    
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
            (concat                         ; "\n"
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
                :box (:line-width 1 :color "#AAEE77")
                :foreground "black" :background "#BFFA9E"))
              :gap-face
              ((:weight bold
                :box (:line-width 1 :color "#BBDDFF")
                :foreground "black" :background "#D0EDFF"))))

If you press `E`, it will **show** the **first notes** about the tasks.

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

Contextual auto-exclusion for tags in the Agenda view.  For example, I use the
following tags for `TODO`:

-   **Phone:** Needs a phone.

-   **Errands:** Done in town.

-   **Home:** Done at home.

Now, it&rsquo;s quite easy for my computer to figure out which of these are
possible, based on my location:

-   **Phone:** Am I outside of normal calling hours?

-   **Errands:** Am I outside of business hours?

-   **Home:** Does my IP address begin with 192.168?

I can now define the function `leuven--org-auto-exclude-function` to auto-exclude
based on this type of context information.

All I have to do is type `/ RET` in the agenda view now, and it excludes based
on my machine&rsquo;s current temporal and physical context.

    ;; 10.5 Commands in the agenda buffer.
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
    
    ;;! Ensure that `:refile:' tags never will be excluded!
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

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> We should ensure that appointments don&rsquo;t disappear when filtering late at nite</b><br  />
-   Look at appointments tagged `errands`
-   Filter at 23:00 with `/ RET`
-   Appointments such as &ldquo;going to the doctor&rdquo; will be removed from the agenda
    view&#x2026;
</div>

### Custom agenda views<a id="sec-28-9-6" name="sec-28-9-6"></a>

    ;; Make the block agenda more compact (no agenda span name, no week number, no
    ;; separator line).
    (setq org-agenda-compact-blocks t)
    (setq org-agenda-compact-blocks nil)
    
    (setq org-agenda-block-separator
          (propertize (make-string 132 (string-to-char "_"))
                      'face '(:foreground "#59ACE2"))) ; lighter version with #C0E2F4

Links to common agenda views:
-   <(org-agenda nil "a")>
-   [Show Waiting Tasks]((org-agenda nil "w"))
-   [Show Projects]((org-agenda nil "p"))

or even:

    [[org-search:+work-boss-TODO="DONE"]]

if you define a new type of link.  See [Use Org-Mode Links for Absolutely Anything](http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html)
for more.

You can as well use your agenda and use `/` to limit the view to what you want
(`C-c a a / TAG`).

Commands:
-   <ls -l>
-   <pwd>

    ;;** 10.6 (info "(org)Custom agenda views")
    
      (leuven--section "10.6 (org)Custom agenda views")
    
      (with-eval-after-load "org-agenda"
        (let ((leuven-org-agenda-views
               (concat leuven--directory "org-leuven-agenda-views.el")))
          (when (file-exists-p leuven-org-agenda-views)
            (load-file leuven-org-agenda-views))))
                                            ; with-eval-after-load "org-agenda" ends here.

### Display all active tasks from the current directory<a id="sec-28-9-7" name="sec-28-9-7"></a>

The function `leuven-org-todo-list-current-dir` will display your tasks from the
current directory (including from local files `TODO.org` and `BUGS.org`, for
example).

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Add recursive option to show all tasks from subdir</b><br  />
nil</div>

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
    
    ;; "TODO list" without asking for a directory.
    (global-set-key (kbd "<C-f3>") #'leuven-org-todo-list-current-dir)

### Exporting Agenda Views<a id="sec-28-9-8" name="sec-28-9-8"></a>

    ;;** 10.7 (info "(org)Exporting Agenda Views")
    
      (leuven--section "10.7 (org)Exporting Agenda Views")
    
      ;; 10.7 Alist of variable/value pairs that should be active during agenda
      ;; export.
      (setq org-agenda-exporter-settings
            '((ps-number-of-columns 1)      ; 2?
              (ps-landscape-mode t)
              ;; (org-agenda-add-entry-text-maxlines 5)
              (htmlize-output-type 'css)))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> htmlize-output-type is duplicated lots of times!</b><br  />
nil</div>

### Agenda column view<a id="sec-28-9-9" name="sec-28-9-9"></a>

    ;;** 10.8 (info "(org)Agenda column view")
    
      (leuven--section "10.8 (org)Agenda column view")
    
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

### Not sorted<a id="sec-28-9-10" name="sec-28-9-10"></a>

    (with-eval-after-load "org-agenda"

    (defadvice org-agenda-switch-to
      (after leuven-org-agenda-switch-to activate)
      "Recenter after jumping to the file which contains the item at point."
      (recenter))

#### Mouse cursor doesn&rsquo;t highlight agenda lines<a id="sec-28-9-10-1" name="sec-28-9-10-1"></a>

XXX Does this work?

    (add-hook 'org-agenda-finalize-hook
              (lambda ()
                (remove-text-properties (point-min) (point-max)
                                        '(mouse-face t))))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Adding &ldquo;:tangle yes&rdquo; on the above code block results in partial tangling!</b><br  />
nil</div>

#### Emphasize items in agenda<a id="sec-28-9-10-2" name="sec-28-9-10-2"></a>

    (add-hook 'org-agenda-finalize-hook
              (lambda ()
                (let ((inhibit-read-only t))
                  (goto-char (point-min))
                  (org-do-emphasis-faces (point-max)))))

#### Mark a task as DONE and create a follow-up task<a id="sec-28-9-10-3" name="sec-28-9-10-3"></a>

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

Source: Sacha Chua.

#### Capture something based on the agenda<a id="sec-28-9-10-4" name="sec-28-9-10-4"></a>

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

Source: Sacha Chua.

## Markup for rich export<a id="sec-28-10" name="sec-28-10"></a>

    ;;* 11 (info "(org)Markup")
    
      (leuven--section "11 (org)Markup")
    
      (with-eval-after-load "org-faces"
    
        ;; Add a face to #+begin_quote and #+begin_verse blocks.
        (setq org-fontify-quote-and-verse-blocks t))
    
      (with-eval-after-load "org"
        (message "... Org Markup")
    
        ;;??? Change the face of a headline (as an additional information) if it is
        ;; marked DONE (to face `org-headline-done').
        (setq org-fontify-done-headline t)
    
        ;; 11.1 Hide the emphasis marker characters.
        (setq org-hide-emphasis-markers t)  ; Impact on table alignment!

### Images and Tables<a id="sec-28-10-1" name="sec-28-10-1"></a>

Automatic screenshot insertion.

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

### Macro replacement<a id="sec-28-10-2" name="sec-28-10-2"></a>

    ;; Hide the brackets marking macro calls.
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

### Embedded LaTeX<a id="sec-28-10-3" name="sec-28-10-3"></a>

To get one entity in the middle of a word, use `{}`, i.e. some `w\entity{}rd` (new
exporter only).

    ;; 11.7.1 Define user entities to produce special characters.
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

To list all available entities, run `M-x org-entities-help`.

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

## Exporting<a id="sec-28-11" name="sec-28-11"></a>

    ;;* 12 (info "(org)Exporting")
    
      ;; Bind the exporter dispatcher to a key sequence.
      (with-eval-after-load "org"
        (message "... Org Exporting")
    
        ;; Libraries in this list will be loaded once the export framework is needed.
        (setq org-export-backends '(ascii html icalendar latex odt md))
    
        (define-key org-mode-map (kbd "C-c C-e") #'org-export-dispatch))

For Org buffers, add an enhanced version of `save-buffer` which does:

1.  Save buffer (updating dynamic blocks and tables)
2.  Execute the code blocks, and save once again
3.  Tangle code blocks, if any to be tangled
4.  Export to HTML and PDF, if such files already exist
5.  Call `send-patch`?

Notes:
-   Use the asynchronous export (if possible, so that I don&rsquo;t loose time, or let
    it open to the value of that var)?
-   Different behaviors can be obtained with `C-u` (or `C-u C-u`)

Why not adding a recursive functionality as well (in Emacs Lisp too), to be
launched from Dired?

      (with-eval-after-load "org"
    
        (defun org-save-buffer-and-do-related ()
          "Save buffer, execute/tangle code blocks, and export to HTML/PDF."
          (interactive)
          (let* ((orgfile (buffer-file-name))
                 (base-name (file-name-base orgfile))
                 (mdfile (concat base-name ".md"))
                 (htmlfile (concat base-name ".html"))
                 (texfile (concat base-name ".tex"))
                 (pdffile (concat base-name ".pdf")))
            (save-buffer)                   ; See other commands in
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
                                    (message "You should run `org-lint'!!!")
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
                  (message "Markdown is up to date with Org file")))
              (when (file-exists-p htmlfile)
                (if (file-newer-than-file-p orgfile htmlfile)
                    (measure-time "Buffer exported to HTML"
                     (org-html-export-to-html))
                  (message "HTML is up to date with Org file")))
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
                  (message "PDF is up to date with Org file")))
              (beep))))
    
        (define-key org-mode-map (kbd "<f9>") #'org-save-buffer-and-do-related))

<div class="note">
When called non-interactively, `org-lint` returns the reports, as an alist or nil,
so it can be used as a predicate.

</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Execute all code blocks in buffer</b><br  />
Ce qui serait mieux :

-   executer tout dans le buffer (sauf les never, évidemment)
-   exporter en let-bind&rsquo;ant eval à never&#x2026; SAUF POUR DES BLOCKS DONT ON NE
    VOULAIT PAS LE RÉSULTAT (TROP LONG, PAR EXEMPLE) DANS LE BUFFER MAIS QUE L&rsquo;ON
    VEUT À L&rsquo;EXPORT !!

Ansi, on aurait les valeurs à jour dans le buffer Org, et les deux exports
(HTML et PDF) seraient identiques entre eux, et avec le buffer Org.
</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Check that SETUPFILES (like for Bigblow) or INCLUDE files exist</b><br  />
Before exporting&#x2026;
</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Map through inline code blocks to remove inline results</b><br  />
`(org-babel-map-inline-src-blocks nil (org-babel-remove-inline-result))`
</div>

### Export options<a id="sec-28-11-1" name="sec-28-11-1"></a>

    ;;** 12.2 (info "(org)Export options")
    
      (leuven--section "12.2 (org)Export options")
    
      ;; Org generic export engine.
      (with-eval-after-load "ox"
    
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

Eric Schulte has proposed the `by-backend` function (updated by Nicolas Goaziou)
for **backend-dependent execution** (for example, conditionally export TikZ to an
SVG image on HTML export and insert TikZ &ldquo;as is&rdquo; on PDF export):

    (defmacro by-backend (&rest body)
      `(case org-export-current-backend ,@body))

See [Backend dependent execution &#x2013; conditionally export tikz to SVG on HTML
export](http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html#sec-4-3) for more information.

### Export settings<a id="sec-28-11-2" name="sec-28-11-2"></a>

    ;;** 12.3 Export settings
    
      (setq org-export-exclude-tags '("noexport" "crypt"))

### HTML export<a id="sec-28-11-3" name="sec-28-11-3"></a>

To get a start for your CSS file, use the command
`M-x org-export-htmlize-generate-css` to extract class definitions.

    ;;** 12.5 (info "(org)HTML export")
    
      ;; Org HTML export engine.
      (with-eval-after-load "ox-html"
    
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
    
        ;; 13.1.5 Don't include the JavaScript snippets in exported HTML files.
        (setq org-html-head-include-scripts nil)
    
        ;; ;; 12.5.9 Turn inclusion of the default CSS style off.
        ;; (setq org-html-head-include-default-style nil)

[HTML Tidy](http://tidy.sourceforge.net/) is a HTML linter.  Please see [List of the Tidy errors](http://www.htmlpedia.org/wiki/HTML_Tidy).

    ;; Check that `tidy' is in PATH, and that configuration file exists.
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

      ;; HTML checkbox output.
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
    
    )                                   ; with-eval-after-load "ox-html" ends here.

Emacs 23+ users: in order to avoid &ldquo;Invalid face&rdquo; errors, you need to use the
version of `htmlize` made available by Carsten Dominik in `org-mode/contrib/lisp`
directory.

    ;;** (info "(emacs-goodies-el)htmlize")
    
      (leuven--section "(emacs-goodies-el)htmlize")
    
      ;; HTML-ize font-lock buffers.
      (autoload 'htmlize-buffer "htmlize"
        "Convert BUFFER to HTML, preserving colors and decorations." t)
      (autoload 'htmlize-region "htmlize"
        "Convert the region to HTML, preserving colors and decorations." t)
      (autoload 'htmlize-file "htmlize"
        "Load FILE, fontify it, convert it to HTML, and save the result." t)
    
      (with-eval-after-load "htmlize"
    
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

To get around PostScript problems, you can write the buffer to your browser
and then print from there, and in color.

This adds a Quick Print option to your menu bar (under File).

    ;; Quick print preview (to Web browser) with `htmlize-view-buffer'.
    (autoload 'htmlize-view-buffer "htmlize-view"
      "Convert buffer to html preserving faces and view in web browser." t)
    
    ;; Same key binding as Org export to HTML (open in browser).
    (global-set-key (kbd "C-c C-e h o") #'htmlize-view-buffer)
    
    ;; View current buffer as html in web browser.
    (with-eval-after-load "htmlize-view"
    
      ;; Add "Quick Print" entry to file menu.
      (htmlize-view-add-to-files-menu))

Now, you can print from the browser in (complete) Unicode, using your system&rsquo;s
capabilities.

### LaTeX and PDF export<a id="sec-28-11-4" name="sec-28-11-4"></a>

    ;;** 12.6 (info "(org)LaTeX and PDF export")

    (leuven--section "12.6 (org)LaTeX and PDF export")
    
    ;; LaTeX back-end.
    (with-eval-after-load "ox-latex"
    
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

Look at the variable `org-latex-remove-logfiles`, which controls whether some
of the files produced by LaTeX (`org-latex-logfiles-extensions`) are removed.

<div class="warning">
Under Windows Emacs, `executable-find` searches for `latexmk.exe` only; it **never
will find the Cygwin symlink** to `latexmk.pl`, though Cygwin bin directory is first
in the PATH.

OTOH, when commands will be launched by the Org export process, as that will run
in the Bash/Zsh shell, Org will call the first match for `latexmk`, hence the one
from Cygwin&#x2026;

Not very coherent!

</div>

    (defun leuven--change-pdflatex-program (backend)
      "Automatically run XeLaTeX, if asked, when exporting to LaTeX."
    
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
    
        (message "INFO- LaTeX engine: %s" org-latex-pdf-engine-full-path)
        (message "INFO- LaTeX command: %s" org-latex-pdf-command)
    
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
        ;; (message "Export command: %S" org-latex-pdf-process)
        ))
    
    ;; Hook run before parsing an export buffer.
    (add-hook 'org-export-before-parsing-hook #'leuven--change-pdflatex-program)

<div class="warning">
As of 2015-09-29, it seems that `LATEX_COMPILER` is the keyword to use the above
functionality in standard Org mode!

</div>

<div class="note">
If LaTeXmk succeeds, the `.tex` file should be removed as well (as we can produce
it independently anyway); but adding `rm $(cygpath %f)` did not do the trick (in
Windows Emacs, well in Cygwin Emacs!?)&#x2026;

</div>

<div class="tip">
You can clear out this individual function by calling `remove-hook`:

    (remove-hook 'org-export-before-parsing-hook #'leuven--change-pdflatex-program)

</div>

You can easily customize this variable on a per file basis.  If you seldom use
`bibtex`, have the default be to run `pdflatex` just once or twice.  Then, for any
Org file that needs `bibtex`, simply put in the following line (or a variation
thereof):

    #+BIND: org-latex-pdf-process ("pdflatex %b" "bibtex %b" "pdflatex %b" "pdflatex %b")

Also, you can use `latexmk` (in TeX Live) which can save you some time when it
is not necessary to rebuild index and/or bibliography.

    ;; Export source code using `listings' (instead of `verbatim').
    (setq org-latex-listings t)
    
    ;; 12.6.2 Default packages to be inserted in the header.
    ;; Include the `listings' package for fontified source code.
    (add-to-list 'org-latex-packages-alist '("" "listings") t)
    
    ;; Include the `xcolor' package for colored source code.
    (add-to-list 'org-latex-packages-alist '("" "xcolor") t)

When using the `utf8` option to `inputenc` (and not `utf8x` which should be avoided,
as it uses `ucs` which is no longer maintained), we have 2 solutions to support
the UTF-8 **no-break space**:

-   Convert it in Org mode, when exporting (via a filter), or
    
        ;; Filter for no-break spaces.
        (defun leuven--latex-filter-nbsp (text backend info)
          "Convert no-break spaces when exporting to LaTeX/Beamer."
          (when (memq backend '(latex beamer))
            (replace-regexp-in-string " " "~" text)))
        
        (add-to-list 'org-export-filter-plain-text-functions
                     'leuven--latex-filter-nbsp)

-   Convert it in LaTeX:
    
        ;; Convert `nbsp' to its LaTeX equivalent.
        (add-to-list 'org-latex-packages-alist
                     (concat "\\ifdefined\\DeclareUnicodeCharacter{"
                             "\\DeclareUnicodeCharacter{00A0}{~}"
                             "}\\fi") t)

XXX The first one is better because &#x2026; (see mail of Daniel Flipo)

The exporter will **add** a language option to the LaTeX `babel` package according to
the `#+LANGUAGE:` keyword (only) if:

-   The `babel` package is explicitly loaded (in preamble), <span class="underline">and</span>
-   The language is different from the one set by the user.

    ;; Include the `babel' package for language-specific hyphenation and
    ;; typography.
    (add-to-list 'org-latex-packages-alist '("french" "babel") t)

What about `apacite` and `tikz` in `org-latex-packages-alist`?

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

### &ldquo;Ignore heading + promote children&rdquo; tag<a id="sec-28-11-5" name="sec-28-11-5"></a>

The actual contents of an abstract can be rather large.

To be able to put it into a collapsable headline **without exporting the
headline**, but **well its contents**, you can add a `ignore` tag to remove the
headline:

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

In addition, it promotes all headlines under the removed `ignore`-tagged
headline.  This is useful to support structures like the following:

    # Wrapping an abstract in a headline
    
    * Abstract                                                             :ignore:
    #+LaTeX: \begin{abstract}
    #+HTML: <div id="abstract">
    ...
    #+HTML: </div>
    #+LaTeX: \end{abstract}
    
    # Placing References under a headline (using ox-bibtex in contrib)
    
    * References                                                           :ignore:
    #+BIBLIOGRAPHY: dissertation plain
    
    # Inserting an appendix for LaTeX using the appendix package.
    
    * Appendix                                                             :ignore:
    #+LaTeX: \begin{appendices}
    ** Reproduction
    ** Data Sets
    ** Tooling
    ** Definitions
    #+LaTeX: \end{appendices}

    )                                   ; with-eval-after-load "ox-latex" ends here.

### LaTeX Beamer and PDF export<a id="sec-28-11-6" name="sec-28-11-6"></a>

    ;; 12.6.6 Beamer class export.
    ;; (require 'ox-beamer)
    (with-eval-after-load "ox-beamer"
    
      ;; Default title of a frame containing an outline.
      (setq org-beamer-outline-frame-title "Plan")) ; [default: "Outline"]

### OpenDocument Text (ODT)<a id="sec-28-11-7" name="sec-28-11-7"></a>

ODT export is not loaded by default.  You will have to load it explicitly or to
add it to `org-export-backends`.

    (with-eval-after-load "ox-odt"
    
      ;; Convert "odt" format to "doc" format.
      (setq org-odt-preferred-output-format "doc")
    
      (when leuven--cygwin-p
        (setcdr (assoc "LibreOffice" org-odt-convert-processes)
                "soffice --headless --convert-to %f%x --outdir \"$(cygpath -m %d)\" \"$(cygpath -m %i)\"")))

<div class="tip">
With the amazing `ox-pandoc`, you can **export to `.docx` directly**.  No need to go
through LaTeX!

</div>

## Publishing<a id="sec-28-12" name="sec-28-12"></a>

Publish related Org mode files as a website.

<div class="warning">
It can be used for generating different PDF files from the same source file&#x2026;
See [9.2 The Multiple Export Case](http://orgmode.org/worg/org-tutorials/org-latex-export.html#sec-9-2).

</div>

    ;;* 13 (info "(org)Publishing")
    
      (leuven--section "13 (org)Publishing")
    
      (with-eval-after-load "ox-publish"
    
        ;; Show message about files *not* published.
        (setq org-publish-list-skipped-files nil)

### Uploading files<a id="sec-28-12-1" name="sec-28-12-1"></a>

    ;; ;; 13.2 Always publish all files.
    ;; ;; (do not use time stamp checking for skipping unmodified files)
    ;; (setq org-publish-use-timestamps-flag nil)

### Triggering publication<a id="sec-28-12-2" name="sec-28-12-2"></a>

    ;; 13.4 Force publishing all files.
    (defun leuven-org-publish-all-force ()
      (interactive)
      (org-publish-all t)))

## Working With Source Code<a id="sec-28-13" name="sec-28-13"></a>

Literate programming and reproducible research.

    ;;* 14 (info "(org)Working With Source Code")

Automate the redisplay of inline images, so that the images are always
up-to-date.

    (with-eval-after-load "ob-core"
    
      ;; Make the images in the Emacs buffer automatically refresh after
      ;; execution.
    
      ;; (add-hook 'org-babel-after-execute-hook
      ;;           (lambda ()
      ;;             (org-display-inline-images nil t))) ; DOESN'T WORK!
      ;;                                     ; More efficient with refresh == t.
    
      (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

### Editing source code<a id="sec-28-13-1" name="sec-28-13-1"></a>

    ;;** 14.2 (info "(org)Editing source code")
    
      (leuven--section "14.2 (org)Editing source code")

Mapping between languages (listings in LaTeX) and their major mode (in Emacs).

    (with-eval-after-load "org-src"
    
      ;; Mapping languages to their major mode (for editing the source code block
      ;; with `C-c '').
      (add-to-list 'org-src-lang-modes    ; Add new languages.
                   '("dot" . graphviz-dot)))

    ;; Display the source code edit buffer in the current window, keeping all
    ;; other windows.
    (setq org-src-window-setup 'current-window)
    
    ;; FIXME Bind this to the correct keys.
    (defun leuven-org-babel-expand-src-block ()
      (interactive)
      (let ((org-src-window-setup 'reorganize-frame))
        (org-babel-expand-src-block)))
    
    ;; Indent the content of a source code block.
    (setq org-edit-src-content-indentation 2)
    
    ;; Fontify code in code blocks (highlight syntax in the Org buffer).
    (setq org-src-fontify-natively t)     ;! Create overlay
                                          ;! `org-block-background' and remove
                                          ;! text property `org-block'.
    
    ;; Preserve spaces and `TAB' characters in source code blocks.
    (setq org-src-preserve-indentation t) ; Or add a `-i' flag to you source block.
    
    ;; Same effect for `TAB' as in the language major mode buffer (indenting
    ;; properly when hitting the `TAB' key).
    (setq org-src-tab-acts-natively t)
    
    
    ;; (with-eval-after-load "org"
    ;;   (message "... Org Editing source code")
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

C-c TAB
Toggle the visibility of existing tags in the buffer.  This can be used as a
cheap preview (sgml-tags-invisible).

View just the source-code blocks within the current Org-babel file (something
logically equivalent to &ldquo;tangle&rdquo;, but without creating a separate file).

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

### Evaluating code blocks<a id="sec-28-13-2" name="sec-28-13-2"></a>

    ;;** 14.5 (info "(org)Evaluating code blocks")
    
      (leuven--section "14.5 (org)Evaluating code blocks")
    
      ;; I don't want to execute code blocks with `C-c C-c' (evaluate code
      ;; block only with `C-c C-v e').
      (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)
    
      ;; Languages for which Org-babel will raise literate programming errors when
      ;; noweb references can not be resolved.
    
      (with-eval-after-load "ob-core"
        (add-to-list 'org-babel-noweb-error-langs "emacs-lisp"))

Write error messages to the Messages buffer for invalid `org-sbe` calls.

    ;; Debugging org-sbe calls.
    (defadvice org-sbe (around get-err-msg activate)
      "Issue messages at errors."
      (condition-case err
          (progn
        ad-do-it)
        (error
         (message "Error in org-sbe: %S" err)
         (signal (car err) (cdr err)))))

### Languages<a id="sec-28-13-3" name="sec-28-13-3"></a>

    (with-eval-after-load "ob-exp"
      ;; Template used to export the body of code blocks.
      (setq org-babel-exp-code-template
            ;; (concat "\n==:\n"
                    org-babel-exp-code-template)
            ;; )
      )
    
    ;; Keep lower-case.
    (setq org-babel-results-keyword "results")

Customize the `org-babel-load-languages` variable to enable support for
languages which can be evaluated in Org mode buffers.

    ;;** 14.7 (info "(org)Languages")
    
      (leuven--section "14.7 (org)Languages")
    
      ;; FIXME Test executable-find (of Rterm, gnuplot, ruby, etc.) before
      ;; setting language to yes...
    
      (with-eval-after-load "org"
        (message "... Org Languages")
    
        ;; Configure Babel to support most languages.
        (add-to-list 'org-babel-load-languages '(R . t)) ; Requires R and ess-mode.
        (add-to-list 'org-babel-load-languages '(awk . t))
        (add-to-list 'org-babel-load-languages '(ditaa . t)) ; Sudo aptitude install openjdk-6-jre.
        (add-to-list 'org-babel-load-languages '(dot . t))
        (add-to-list 'org-babel-load-languages '(java . t))
        (add-to-list 'org-babel-load-languages '(latex . t)) ; Shouldn't you use #+begin/end_latex blocks instead?
        (add-to-list 'org-babel-load-languages '(ledger . t)) ; Requires ledger.
        (add-to-list 'org-babel-load-languages '(makefile . t))
        (add-to-list 'org-babel-load-languages '(org . t))
        (add-to-list 'org-babel-load-languages '(python . t))
        (if (locate-library "ob-shell")     ; ob-sh renamed on 2013-12-13
            (add-to-list 'org-babel-load-languages '(shell . t))
          (add-to-list 'org-babel-load-languages '(sh . t)))
        (add-to-list 'org-babel-load-languages '(sql . t))
    
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
    
          ;; (message "Source blocks checked in %s."
          ;;          (buffer-name (buffer-base-buffer)))
          )
    
        (add-hook 'org-mode-hook #'org-src-block-check t))
                                            ; Place this at the end to ensure that
                                            ; errors do not stop applying other
                                            ; functions in the `org-mode-hook' (such
                                            ; as switching the dictionary).

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Why isn&rsquo;t diff-mode.el found?</b><br  />
<file:///cygdrive/d/Users/fni/org/personal/Org-for-dummies.md>
</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Check src blocks in  **temp** buffers?</b><br  />
nil</div>

`screen` offers support for interactive terminals.  Mostly shell scripts.  Heavily
inspired by `eev`.

Eric Schulte believes `screen` has more of a focus on sustained interaction with
an interactive terminal.

### Prettier (or at least fancier) code block delimiters<a id="sec-28-13-4" name="sec-28-13-4"></a>

    (defun prettier-org-code-blocks ()
      (interactive)
      (font-lock-add-keywords nil
        '(("\\(\+begin_src\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                     nil)))
          ("\\(\+end_src\\)"
           (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                     nil))))))
    
    (add-hook 'org-mode-hook #'prettier-org-code-blocks)

### Library of Babel<a id="sec-28-13-5" name="sec-28-13-5"></a>

<div class="note">
As the LOB contains code blocks of different languages, and as we now **check that
languages are known**, this section 14.6 has been moved **after** the section 14.7
where we **load languages** such as R, etc.

</div>

    ;;** 14.6 (info "(org)Library of Babel")
    
      (leuven--section "14.6 (org)Library of Babel")
    
      (with-eval-after-load "org"
    
        ;; Load the NAMED code blocks defined in Org mode files into the library of
        ;; Babel (global `org-babel-library-of-babel' variable).
        (let ((lob-file (concat (file-name-directory (locate-library "org"))
                                "../doc/library-of-babel.org")))
          (when (file-exists-p lob-file)
            (org-babel-lob-ingest lob-file))))

### Key bindings and useful functions<a id="sec-28-13-6" name="sec-28-13-6"></a>

    (leuven--section "14.11 (org)Key bindings and useful functions")
    
    (with-eval-after-load "ob-core"
    
      (defadvice org-babel-next-src-block
        (after leuven-org-babel-next-src-block activate)
        "Recenter after jumping to the next source block."
        (recenter))
    
      (defadvice org-babel-previous-src-block
        (after leuven-org-babel-previous-src-block activate)
        "Recenter after jumping to the previous source block."
        (recenter)))

## Miscellaneous<a id="sec-28-14" name="sec-28-14"></a>

    ;;* 15 (info "(org)Miscellaneous")

    ;; From Dan Davison.
    (defun leuven-switch-to-org-scratch ()
      "Switch to a temp Org buffer.  If the region is active, insert it."
      (interactive)
      (let ((contents (and (use-region-p)
                           (buffer-substring (region-beginning)
                                             (region-end)))))
        (find-file "/tmp/org-scratch.org")
        (if contents (insert contents))))

### Change PROPERTY drawer location<a id="sec-28-14-1" name="sec-28-14-1"></a>

<span class="timestamp-wrapper"><span class="timestamp">[2014-10-28 Tue]</span></span>

Find all entries where :PROPERTIES: is not the first item listed.

    (defun org-check-property-drawers ()
      (interactive)
      (org-element-map (org-element-parse-buffer 'element) 'headline
        (lambda (h)
          (and (org-element-map h 'drawer
                 (lambda (d) (equal (org-element-property :name d) "PROPERTIES"))
                 nil t 'headline)
               (let ((begin (org-element-property :begin h)))
                 (message "Entry with erroneous properties drawer at %d" begin)
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
              (unless (and inline-re (org-looking-at-p inline-re))
                (save-excursion
                  (let ((end (save-excursion (outline-next-heading) (point))))
                    (forward-line)
                    (when (org-looking-at-p org-planning-line-re) ; Org-8.3.
                      (forward-line))
                    (when (and (< (point) end)
                               (not (org-looking-at-p org-property-drawer-re))
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

### Ispell<a id="sec-28-14-2" name="sec-28-14-2"></a>

<div class="note">
The `leuven--org-switch-dictionary` function is called as well on each code block
(at least, in emails that are being read), in buffers called `*fontification*`.

</div>

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
    
    ;; Guess dictionary.
    (add-hook 'org-mode-hook #'leuven--org-switch-dictionary)

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Avoid (error &ldquo;Undefined dictionary: american&rdquo;)</b><br  />
when dictionaries are not installed. IN FACT, ISPELL IS NOT AT ALL INSTALLED!

Check out for valid dictionaries:
(append ispell-local-dictionary-alist ispell-dictionary-alist)
</div>

### Easy templates<a id="sec-28-14-3" name="sec-28-14-3"></a>

Lower-case in `BEGIN_SRC` and other keywords: do not stand out!

    ;;** 15.2 (info "(org)Easy Templates")
    
      (leuven--section "15.2 (org)Easy Templates")
    
      (with-eval-after-load "org"
        (message "... Org Easy Templates")
    
        ;; Modify `org-structure-template-alist' to keep lower-case easy templates.
        (mapc (lambda (asc)
                (let ((org-sce-dc (downcase (nth 1 asc))))
                  (setf (nth 1 asc) org-sce-dc)))
              org-structure-template-alist)
    
        (add-to-list 'org-structure-template-alist
                     '("n" "#+begin_note\n?\n#+end_note"))
    
        (add-to-list 'org-structure-template-alist
                     '("w" "#+begin_warning\n?\n#+end_warning"))
    
        (add-to-list 'org-structure-template-alist
                     '("t" "#+begin_tip\n?\n#+end_tip"))
    
        (add-to-list 'org-structure-template-alist
                     '("C" "#+begin_comment\n?\n#+end_comment"))
    
        (add-to-list 'org-structure-template-alist
                     '("E" "\\begin\{equation\}\n?\n\\end\{equation\}" "")))

### Speed keys<a id="sec-28-14-4" name="sec-28-14-4"></a>

Activate single letter commands (for example, outline navigation with `f`, `b`, `n`,
and `p`) at beginning of a headline:

-   **`f`:** `org-forward-same-level`.

-   **`b`:** `org-backward-same-level`.

-   **`n`:** `outline-next-visible-heading`.

-   **`p`:** `outline-previous-visible-heading`.

For navigating blocks:

-   **F:** `org-next-block`.

-   **B:** `org-previous-block`.

    ;;** 15.3 (info "(org)Speed keys")
    
      (leuven--section "15.3 (org)Speed keys")
    
      (with-eval-after-load "org"
        (message "... Org Speek keys")
    
        ;; Activate single letter commands at beginning of a headline.
        (setq org-use-speed-commands t)
    
        (add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))
        (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))

#### Convert Windows Apps key to Hyper<a id="sec-28-14-4-1" name="sec-28-14-4-1"></a>

Here are more obvious key bindings to **execute** (than `C-c C-v C-e` and company)
using &ldquo;hyper&rdquo;:

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

To **tangle**:

    (define-key org-mode-map (kbd "H-t") #'org-babel-tangle)

    )

### Code evaluation and security issues<a id="sec-28-14-5" name="sec-28-14-5"></a>

For security resons, evaluation is not turned on by default.

    ;;** 15.4 (info "(org)Code evaluation security") issues
    
      (leuven--section "15.4 (org)Code evaluation security issues")
    
      (with-eval-after-load "ob-core"
    
        ;;!! Don't be prompted on every code block evaluation.
        (setq org-confirm-babel-evaluate nil))

    (with-eval-after-load "ob-core"
    
      (defface org-block-executing
        '((t (:background "#FFE0FF")))
        "Face used for the source block background when executed.")
    
      ;; Change the color of code blocks while they are being executed.
      (defadvice org-babel-execute-src-block (around progress nil activate)
        "Create an overlay indicating when code block is running."
        (let ((o (make-overlay (org-element-property :begin (org-element-at-point))
                               (1- (org-element-property :end (org-element-at-point))))))
          (recenter)
          (overlay-put o 'face 'org-block-executing)
          (measure-time "Executed code block" ad-do-it)
          (delete-overlay o))))

<div class="note">
We&rsquo;ve disabled the advice `ad-Advice-org-babel-execute-src-block` because of the
error message `` `recenter'ing a window that does not display current-buffer `` when
exporting `gnus-leuven.txt` to HTML.

</div>

### A cleaner outline view<a id="sec-28-14-6" name="sec-28-14-6"></a>

    ;;** 15.8 A (info "(org)Clean view")
    
      (with-eval-after-load "org"
        (message "... Org Clean view")
    
        ;; 15.8 Don't skip even levels for the outline.
        (setq org-odd-levels-only nil))

### Interaction with other packages<a id="sec-28-14-7" name="sec-28-14-7"></a>

    ;;** 15.10 (info "(org)Interaction")
    
      (leuven--section "15.10 (org)Interaction")

<div class="note">
On <span class="timestamp-wrapper"><span class="timestamp">[2015-05-01 Fri]</span></span>, in commit `49a656a`, Nicolas Goaziou has removed
`org-babel-src-name-w-name-regexp`, and replaced it by the function call
`(org-babel-named-src-block-regexp-for-name)`.

Until modification of the following code, it has been disabled, as it breaks
Org&#x2026;

</div>

    (with-eval-after-load "org"
    
      ;; Support shift-selection for making and enlarging regions when the cursor
      ;; is not in a special context.
      (setq org-support-shift-select t)
    
      ;; Maximum level for Imenu access to Org mode headlines.
      (setq org-imenu-depth 3)
    
      ;; Extension of Imenu.
      (when (and (featurep 'ob-core)      ; `org-babel' has been loaded.
                 (featurep 'imenu))       ; Imenu has been loaded.
    
        (setq org-src-blocks-imenu-generic-expression
              `(("Snippets" ,org-babel-src-name-w-name-regexp 2)))
    
        (add-hook 'org-mode-hook
                  (lambda ()
                    (setq imenu-generic-expression
                          org-src-blocks-imenu-generic-expression))))
    
      ;; Alternative to Imenu.
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

### Org-crypt<a id="sec-28-14-8" name="sec-28-14-8"></a>

Org-crypt provides for encrypting individual entries in an otherwise
non-encrypted file.

To later decrypt an entry that&rsquo;s encrypted, use `M-x org-decrypt-entry` or `C-c
C-r` (fits nicely with the meaning of &ldquo;reveal&rdquo;).

    ;; Keep my encrypted data (like account passwords) in my Org mode files with
    ;; a special tag instead.
    (with-eval-after-load "org"
      (message "... Org Crypt")
    
      (try-require 'org-crypt))           ; Loads org, gnus-sum, etc...
    
    (with-eval-after-load "org-crypt"
    
      ;; Encrypt all entries before saving.
      (org-crypt-use-before-save-magic)
    
      ;; Which tag is used to mark headings to be encrypted.
      (setq org-tags-exclude-from-inheritance '("crypt")))

## Other<a id="sec-28-15" name="sec-28-15"></a>

Anonymize Org contents (sanitize the sensitive data).

    (defun leuven-org-scramble-contents ()
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

Promote ignoreheading. Works in ASCII backend too:

    (defun yz/org-export-ignore-headline (backend)
      "Ignore headlines with tag `ignoreheading'."
      (save-excursion
        (let ((org-allow-promoting-top-level-subtree t))
          (org-map-entries
           (lambda ()
             (when (member "ignoreheading" (org-get-tags-at nil 'local))
               (org-promote-subtree)
               (delete-region (line-beginning-position) (line-end-position))))))))
    
    (add-hook 'org-export-before-parsing-hook #'yz/org-export-ignore-headline)

    (defun insert-one-equal-or-two ()
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

Export the current subtree into an email body, using **properties** to populate
the mail message:

-   `MAIL_SUBJECT` or subtree heading -> &ldquo;Subject&rdquo;
-   `MAIL_TO` -> &ldquo;To&rdquo;
-   `MAIL_CC` -> &ldquo;Cc
-   `MAIL_BCC` -> &ldquo;BCc&rdquo;
-   `MAIL_FMT` -> determines the format of the email (e.g., `org` by default, `ascii`
      or `html`)

    (with-eval-after-load "org"
      (message "... Org Mime")
    
      ;; Using Org mode to send buffer/subtree per mail.
      (try-require 'org-mime))
    
    (with-eval-after-load "org-mime"
    
      (add-hook 'org-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c m") #'org-mime-subtree)))
    
      (defun leuven-mail-subtree ()
        (interactive)
        (org-agenda-goto)
        (org-mime-subtree))
    
      (add-hook 'org-agenda-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c m") #'leuven-mail-subtree)))
    
      ;; Add a `mail_composed' property with the current time when
      ;; `org-mime-subtree' is called.
      (add-hook 'org-mime-send-subtree-hook
                (lambda ()
                  (org-entry-put (point) "mail_composed" (current-time-string)))))

## A.3 Adding hyperlink types<a id="sec-28-16" name="sec-28-16"></a>

For more flexibility, you can use a URL-like syntax which could then export
conditionally on the output format.

See <http://orgmode.org/worg/org-tutorials/org-latex-export.html#sec-10-3>

    ;;** A.3 (info "(org)Adding hyperlink types")
    
      (with-eval-after-load "org"
        (message "... Org Adding hyperlink types")
    
        ;; Define a new link type (`latex') whose path argument can hold the name of
        ;; any LaTeX command.
        (org-add-link-type
         "latex" nil
         (lambda (path desc format)
           (cond
            ((eq format 'html)
             (format "<span class=\"%s\">%s</span>" path desc))
            ((eq format 'latex)
             (format "\\%s{%s}" path desc)))))
    
        ;; Add background color by using custom links like [[bgcolor:red][Warning!]].
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

## A.5 Tables and lists in arbitrary syntax<a id="sec-28-17" name="sec-28-17"></a>

    (defun leuven-org-send-all-buffer-tables ()
      "Export all Org tables of the LaTeX document to their corresponding LaTeX tables."
       (interactive)
       (org-table-map-tables
          (lambda () (orgtbl-send-table 'maybe))))

## A.6 Dynamic blocks<a id="sec-28-18" name="sec-28-18"></a>

<div class="note">
For some (yet) unknown reason, `leuven--org-update-buffer-before-save` gets added
to a local copy of the global variable `before-save-hook`.  We now decided to add
it to the hook outside of Org (not anymore in an `with-eval-after-load` form).

</div>

    ;;** A.6 (info "(org)Dynamic blocks")
    
      (defun leuven--org-update-buffer-before-save ()
        "Update all dynamic blocks and all tables in the buffer before save."
        (when (derived-mode-p 'org-mode)
          (message "INFO- Update Org buffer %s"
                   (file-name-nondirectory (buffer-file-name)))
          ;; (sit-for 1.5)
          (let ((cache-long-scans nil)      ; Make `forward-line' much faster and
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

## Org-contrib<a id="sec-28-19" name="sec-28-19"></a>

### Org-effectiveness<a id="sec-28-19-1" name="sec-28-19-1"></a>

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

### Org-notmuch<a id="sec-28-19-2" name="sec-28-19-2"></a>

Link emails from Org files.

### Google Weather<a id="sec-28-19-3" name="sec-28-19-3"></a>


    * Weather
    
    %%(org-google-weather "Lille" "en-gb")


    ;; Add weather forecast in your Org agenda.
    (autoload 'org-google-weather "org-google-weather"
      "Return Org entry with the weather for LOCATION in LANGUAGE." t)
    
    (with-eval-after-load "org-google-weather"
      ;; (try-require 'url)
    
      ;; Add the city.
      (setq org-google-weather-format "%C %i %c, %l°-%h°"))

    )                                       ; Chapter 25.10-org-mode ends here.

# TeX<a id="sec-29" name="sec-29"></a>

    ;;** 25.11 (info "(emacs)TeX Mode")
    
    (leuven--chapter leuven-load-chapter-25.11-tex-mode "25.11 TeX Mode"

<div class="warning">
Emacs ships with `tex-mode.el` but that&rsquo;s a separate thing from AUCTeX.  All
AUCTeX functions start with `TeX-*` or `LaTeX-*` (case matters) or `font-latex-*`.

</div>

## Native TeX Mode<a id="sec-29-1" name="sec-29-1"></a>

-   **`tex-mode` or `latex-mode`:** Default Emacs built-in (La)TeX mode (menu `TeX`; text `LaTeX` displayed in the
    mode line).

    (leuven--section "25.11 (emacs)TeX Mode")

Under Windows, in the **native** TeX mode, `shell-file-name` must be `cmdproxy.exe` to:

-   avoid the error &ldquo;comint-send-string: writing to process: invalid argument,
    tex-shell&rdquo;, and

-   properly quote `tex-start-commands`.

    ;; Get colored PDFLaTeX output.
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

## AUCTeX<a id="sec-29-2" name="sec-29-2"></a>

-   **`TeX-mode` or `LaTeX-mode`:** AUCTeX (menus `Preview`, `LaTeX`, `Command` and `Ref`; text `LaTeX/P` displayed in
    the mode line).

AUCTeX supports TeX and extensions such as LaTeX.  For LaTeX, there is a
(major) `LaTeX-mode` in AUCTeX which runs (in this order):

1.  `text-mode-hook`, then
2.  `TeX-mode-hook`, and then
3.  a hook `LaTeX-mode-hook` special to the LaTeX mode.

If you need to make a customization via a hook which is only relevant for the
LaTeX mode, put it into the `LaTeX-mode-hook`; if it is relevant for any AUCTeX
mode, add it to `TeX-mode-hook` and if it is relevant for all text modes, append
it to `text-mode-hook`.

AUCTeX defines the `tex-mode.el` mode names as alias of its own modes: it
aliases `(la)tex-mode` to `(La)TeX-mode`.

      (leuven--section "25.11 (emacs)AUCTeX Mode")
    
    ;;** 1.2 (info "(auctex)Installation") of AUCTeX
    
      (try-require 'tex-site)
    
      ;; Support for LaTeX documents.
      (with-eval-after-load "latex"

You can detect the **successful activation of AUCTeX**: after loading a LaTeX
file, AUCTeX gives you a `Command` menu.

-   Press `C-c C-c File RET RET` to run `dvips` (note that the command is `File` and
    not `Dvips` as one might expect)

-   Press `C-c C-c Print RET RET` to run `GSview` (also somewhat misleading name)

-   If you want to print the document, do it from `GSview`.

    ;; ;; LaTeX-sensitive spell checking
    ;; (add-hook 'tex-mode-hook
    ;;           (lambda ()
    ;;             (make-local-variable 'ispell-parser)
    ;;             (setq ispell-parser 'tex)))

    ;;** 2.1 (info "(auctex)Quotes")
    
        (leuven--section "2.1 (auctex)Quotes")
    
        ;; Insert right brace with suitable macro after typing left brace.
        (setq LaTeX-electric-left-right-brace t)
    
    ;;** 2.6 (info "(auctex)Completion")
    
        (leuven--section "2.6 (auctex)Completion")
    
        ;; If this is non-nil when AUCTeX is loaded, the TeX escape character `\'
        ;; will be bound to `TeX-electric-macro'.
        (setq TeX-electric-escape t)
    
    ;;** 2.8 (info "(auctex)Indenting")
    
        (leuven--section "2.8 (auctex)Indenting")
    
        ;; Leave the `tikzpicture' code unfilled when doing `M-q'.
        (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
    
        ;; Auto-indentation (suggested by the AUCTeX manual -- instead of adding
        ;; a local key binding to `RET' in the `LaTeX-mode-hook').
        (setq TeX-newline-function 'newline-and-indent)

    ;;* 3 Controlling Screen (info "(auctex)Display")
    
    ;;** 3.1 (info "(auctex)Font Locking")
    
        (leuven--section "3.1 (auctex)Font Locking")
    
        ;; (for Org mode) Add the `comment' environment to the variable
        ;; `LaTeX-verbatim-environments' so that, if the `#+TBLFM' line contains
        ;; an odd number of dollar characters, this does not cause problems with
        ;; font-lock in LaTeX-mode.
        (add-to-list 'LaTeX-verbatim-environments "comment")

    ;;** 4.1 Executing (info "(auctex)Commands")
    
        (leuven--section "4.1 Executing (auctex)Commands")
    
        ;; Add a command to execute on the LaTeX document.
        (add-to-list 'TeX-command-list
                     '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    
        (defun leuven--LaTeX-mode-hook ()
          ;; Default command to run in the LaTeX buffer.
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
    
        (add-hook 'LaTeX-mode-hook #'leuven--LaTeX-mode-hook)
    
        ;; Don't ask user for permission to save files before starting TeX.
        (setq TeX-save-query nil)
    
        (defun TeX-default ()
          "Choose the default command from `C-c C-c'."
          (interactive)
          (TeX-save-document "")          ; or just use `TeX-save-query'
          (execute-kbd-macro (kbd "C-c C-c RET")))
    
        ;; Rebind the "compile command" to default command from `C-c C-c' (in LaTeX
        ;; mode only).
        (define-key LaTeX-mode-map (kbd "<f9>") #'TeX-default)
    
        ;; Use PDF mode by default (instead of DVI).
        (setq-default TeX-PDF-mode t)

A decent viewer reloads the PDF automatically when the file has changed while
staying on the same page (no need to close & reopen).

Support for forward search with PDF files means that the viewer jumps to the
page in the output file corresponding to the position in the source file.
Currently, this only works if you use the pdfsync LaTeX package and xpdf or
SumatraPDF as your PDF viewer.

    ;;** 4.2 (info "(auctex)Viewing") the formatted output
    
        (leuven--section "4.2 (auctex)Viewing the formatted output")
    
        (defvar leuven--sumatrapdf-command
          (concat leuven--windows-program-files-dir "SumatraPDF/SumatraPDF.exe")
          "Path to the SumatraPDF executable.")
    
        ;; Use a saner PDF viewer (evince, SumatraPDF).
        (setcdr (assoc "^pdf$" TeX-output-view-style)
                (cond ((or leuven--win32-p leuven--cygwin-p)
                       `("." (concat "\"" ,leuven--sumatrapdf-command "\" %o")))
                      (t
                       '("." "evince %o"))))
    
        ;; For AUCTeX 11.86+.
        (when (or leuven--win32-p leuven--cygwin-p)
          (when (boundp 'TeX-view-program-list)
            (add-to-list 'TeX-view-program-list
                         `("SumatraPDF"
                           (concat "\"" ,leuven--sumatrapdf-command "\" %o")))))
    
        (when (or leuven--win32-p leuven--cygwin-p)
          (setcdr (assoc 'output-pdf TeX-view-program-selection)
                  '("SumatraPDF")))

<div class="tip">
Under Windows, we could open the PDF file with `start "" xxx.pdf` (in a command
prompt).

</div>

    ;;** 4.7 (info "(auctex)Documentation")

`C-c ?` (or `M-x TeX-doc`) displays documentation for a package.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> TeX-doc does not provide a list of available keywords for completion</b><br  />
Nor a default when point is on a command name&#x2026;
</div>

    ;;** 5.2 (info "(auctex)Multifile") Documents
    
        ;; ;; Assume that the file is a master file itself.
        ;; (setq-default TeX-master t)
    
    ;;** 5.3 Automatic (info "(auctex)Parsing Files")
    
        ;; Enable parse on load (if no style hook is found for the file).
        (setq TeX-parse-self t)
    
        ;; Enable automatic save of parsed style information when saving the buffer.
        (setq TeX-auto-save t)
    
    ;;** 5.4 (info "(auctex)Internationalization")
    
        ;; ;; XXX Insert a literal hyphen.
        ;; (setq LaTeX-babel-insert-hyphen nil)
    
    ;;** 5.5 (info "(auctex)Automatic") Customization
    
        ;; TODO Add beamer.el to TeX-style-path
    
    ;;*** 5.5.1 (info "(auctex)Automatic Global") Customization for the Site
    
        (leuven--section "5.5.1 (auctex)Automatic Global Customization for the Site")
    
        ;; Directory containing automatically generated TeX information.
        (setq TeX-auto-global
              (concat user-emacs-directory "auctex-auto-generated-info/"))
                                            ; Must end with a slash.
    
    ;;*** 5.5.3 (info "(auctex)Automatic Local") Customization for a Directory
    
        (leuven--section "5.5.3 (auctex)Automatic Local Customization for a Directory")
    
        ;; Directory containing automatically generated TeX information.
        (setq TeX-auto-local (concat user-emacs-directory "auctex-auto-generated-info/"))
                                            ; Must end with a slash.

## Preview-LaTeX<a id="sec-29-3" name="sec-29-3"></a>

You can detect the successful activation of `preview-latex`: after loading a
LaTeX file, `preview-latex` gives you a `Preview` menu.

    ;;** (info "(preview-latex)Top")
    
        (leuven--section "(preview-latex)Top")
    
        (with-eval-after-load "preview"
    
          ;; Path to `gs' command (for format conversions).
          (setq preview-gs-command
            (cond (leuven--win32-p
                   (or (executable-find "gswin32c.exe")
                       "C:/texlive/2015/tlpkg/tlgs/bin/gswin32c.exe"))
                                            ; Default value.
                  (t
                   (or (executable-find "rungs") ; For Cygwin Emacs.
                       "/usr/bin/gs"))))
          (leuven--file-exists-and-executable-p preview-gs-command)
    
          ;; Scale factor for included previews.
          (setq preview-scale-function 1.2))

<div class="note">
This is also called when exporting from Org to PDF.  Why?

To show it, just edit the path to `gswin32c` and you&rsquo;ll see a message &ldquo;Can&rsquo;t find
executable&rdquo; in the echo area.

</div>

## RefTeX<a id="sec-29-4" name="sec-29-4"></a>

AUCTeX is fantastic, and RefTeX just makes things better.

To fill in `\ref{}` and `\cite{}` commands, you can use `C-c &`
(`reftex-view-crossref`).

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Conflict with YASnippet prefix key and `org-mark-ring`-push</b><br  />
nil</div>

A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `C-c =`.  Hitting `l` there will show all the labels
and cites.

Labels can be created with `C-c (` and referenced with `C-c )`.  When referencing,
you get a menu with all labels of a given type and context of the label
definition.  The selected label is inserted as a `\ref` macro.

Citations can be made with `C-c [` which will use a regular expression to pull out
a **formatted** list of articles from your BibTeX database.  The selected citation
is inserted as a `\cite` macro.

Index entries can be made with `C-c /` which indexes the word at point or the
current selection.  More general index entries are created with `C-c <`.
`C-c >` displays the compiled index.

    (add-hook 'LaTeX-mode-hook #'reftex-mode) ; with AUCTeX LaTeX mode
    
    ;; Minor mode with distinct support for `\label', `\ref', `\cite' and
    ;; `\index' in LaTeX.
    (with-eval-after-load "reftex"
    
      ;; Turn all plug-ins on.
      (setq reftex-plug-into-AUCTeX t)
    
      ;; Use a separate selection buffer for each label type -- so the menu
      ;; generally comes up faster.
      (setq reftex-use-multiple-selection-buffers t))

## BibTeX<a id="sec-29-5" name="sec-29-5"></a>

    ;; BibTeX mode.
    (with-eval-after-load "bibtex"
    
      ;; Current BibTeX dialect.
      (setq bibtex-dialect 'biblatex))
    
    )                                   ; with-eval-after-load "latex" ends here.

    )                                       ; Chapter 25.11-tex-mode ends here.

# SGML and HTML Modes<a id="sec-30" name="sec-30"></a>

    (leuven--chapter leuven-load-chapter-25-text "25 Commands for Human Languages"
    
    ;;** 25.12 (info "(emacs)HTML Mode")
    
      (leuven--section "25.12 (emacs)HTML Mode")

## HTML<a id="sec-30-1" name="sec-30-1"></a>

### html-mode<a id="sec-30-1-1" name="sec-30-1-1"></a>

The default HTML mode, derived from SGML mode (see the HTML and SGML menus),
works quite well for editing HTML4 documents (whose tags don&rsquo;t have to close).

-   **`C-c C-v`:** **View** your file **in your browser** (`browse-url-of-buffer`).

-   **`C-c C-b` (or `C-c <left>`):** Jump to the opening HTML tag (`sgml-skip-tag-backward`).

-   **`C-c C-f` (or `C-c <right>`):** Jump to the closing HTML tag (`sgml-skip-tag-forward`).

-   **`C-c TAB`:** Hide all of the angle-bracketed tags in the buffer (`sgml-tags-invisible`).
    This can be used as a cheap **preview** of **just the bare text**.

-   **`C-c /`:** `sgml-close-tag`.

-   **`C-M-i`:** `ispell-complete-word`.

Plug [HTML Tidy](http://tidy.sourceforge.net/) with `tidy.el`.

    (with-eval-after-load "tidy-autoloads"
      (when (executable-find "tidy")
    
        (defun leuven--html-mode-hook ()
          "Customize html(-helper)-mode."
    
          ;; Set up a "tidy" menu in the menu bar.
          (when (boundp 'html-mode-map)
            (tidy-build-menu html-mode-map))
          (when (boundp 'html-helper-mode-map)
            (tidy-build-menu html-helper-mode-map))
    
          ;; Bind the key sequence `C-c C-c' to `tidy-buffer'.
          (local-set-key (kbd "C-c C-c") #'tidy-buffer)
    
          (setq sgml-validate-command "tidy"))
    
        ;; Also run from `html-helper-mode'.
        (add-hook 'html-mode-hook #'leuven--html-mode-hook)))

### html-helper-mode<a id="sec-30-1-2" name="sec-30-1-2"></a>

You might also want to consider `html-helper-mode`, which has a lot more
features than plain `html-mode`:

-   autocompletion,
-   auto-insertion of closing tags&#x2026; and
-   a lot more.

When you open a buffer for HTML editing, a didactic HTML menu appears, so you
can use your mouse to invoke commands.

Using the prefix-arg (`C-u`) to relevant HTML tags, `html-helper-mode` will put
tags around a region you&rsquo;ve specified.

    (when (locate-library "html-helper-mode")
    
      (autoload 'html-helper-mode "html-helper-mode"
        "Mode for editing HTML documents." t)
    
      ;; Invoke html-helper-mode automatically on .html files.
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-helper-mode))
    
      ;; Invoke html-helper-mode automatically on .asp files.
      (add-to-list 'auto-mode-alist '("\\.asp\\'" . html-helper-mode))
    
      ;; Invoke html-helper-mode automatically on .jsp files.
      (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-helper-mode)))

## XHTML<a id="sec-30-2" name="sec-30-2"></a>

nXML (See section 30.4) seems to be the most recommended mode to view and edit well-formed **XHTML**
(reformulation of HTML as an XML application).

    (add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . xml-mode))
                                          ; Alias for `nxml-mode'.
    
    ;; (add-to-list 'auto-mode-alist '("\\.axvw\\'" . xml-mode)) ; ARCHIBUS view

Alternatively, you can use [nXhtml](http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html) mode, derived from nXML mode.  It adds a lot
of things useful for **XHTML** files with mixed content:

-   handling **multiple major modes** (Mumamo) in one buffer (for CSS, JS, PHP and
    similar things): get the correct syntax highlighting and indentation for
    each of them,
-   folding,
-   improved interface to Tidy (?),
-   etc.

Though, it takes a while to load.

## web-mode<a id="sec-30-3" name="sec-30-3"></a>

[web-mode](http://web-mode.org/) could be another alternative for editing HTML documents embedding
CSS/JS and blocks (client/server side).

    (with-eval-after-load "web-mode-autoloads"
      (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.aspx\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.axvw\\'" . web-mode))) ; ARCHIBUS view
    
    (with-eval-after-load "web-mode"
    
      (setq web-mode-enable-current-element-highlight t)
    
      (setq web-mode-enable-auto-pairing t)
    
      ;; Enable block face (useful for setting background of <style>).
      (setq web-mode-enable-block-face t)
    
      ;; Enable part face (useful for setting background of <script>).
      (setq web-mode-enable-part-face t))

## XML<a id="sec-30-4" name="sec-30-4"></a>


nXML mode (default for editing XML files, since GNU Emacs 23.2) does:

-   real-time **validation against a schema in RELAX NG** (actually RNC, i.e. the
    &ldquo;Compact&rdquo; Syntax): `C-c C-n` (assuming RNG validation is on),

-   **code completion** against the RNG schema.

Some key bindings:

-   **`C-M-p`:** Move backward over one element (`nxml-backward-element`).

-   **`C-M-n`:** Move forward over one element (`nxml-forward-element`).

-   **`C-M-u`:** Move up the element structure (`nxml-backward-up-element`).

-   **`C-M-d`:** Move down the element structure (`nxml-down-element`).

    (with-eval-after-load "nxml-mode"
    
      ;; Indent 4 spaces (for the children of an element relative to the start-tag).
      (setq nxml-child-indent 4)
    
      ;; Remove the binding of `C-c C-x' (`nxml-insert-xml-declaration'), used by
      ;; Org timeclocking commands.
      (define-key nxml-mode-map (kbd "C-c C-x") nil)
    
      ;; View the buffer contents in a browser.
      (define-key nxml-mode-map (kbd "C-c C-v") #'browse-url-of-buffer)
                                          ; XXX Normally bound to
                                          ; `rng-validate-mode'.

    ;; Fix XML folding.
    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   nxml-forward-element
                   nil))
    
    (add-hook 'nxml-mode-hook 'hs-minor-mode))

## Highlight the closing tag<a id="sec-30-5" name="sec-30-5"></a>

      ;; Highlight the current SGML tag context.
      (try-require 'hl-tags-mode)
      (with-eval-after-load "hl-tags-mode"
    
        (add-hook 'html-mode-hook
                  (lambda ()
                    (require 'sgml-mode)
                    ;; When `html-mode-hook' is called from `html-helper-mode'.
                    (hl-tags-mode 1)))      ; XXX Can't we simplify this form?
    
        (add-hook 'nxml-mode-hook #'hl-tags-mode)
    
        (add-hook 'web-mode-hook #'hl-tags-mode)
    )

## CSS<a id="sec-30-6" name="sec-30-6"></a>

    ;; TODO: Handle media queries
    ;; TODO: Handle wrapped lines
    ;; TODO: Ignore vendor prefixes
    (defun sort-css-properties ()
      "Sort CSS properties alphabetically."
      (interactive)
      (let ((start (search-forward "{"))
            (end (search-forward "}")))
        (when (and start end)
          (sort-lines nil start end)
          (sort-declarations))))

## Skewer: live web development with Emacs<a id="sec-30-7" name="sec-30-7"></a>

This lets you send HTML, CSS, and Javascript fragments to Google Chrome. You may
need to start Chrome with `chrome --allow-running-insecure-content`, if you&rsquo;re
using the user script with HTTPS sites.

## JS2-mode + JS2-refactor<a id="sec-30-8" name="sec-30-8"></a>

    (with-eval-after-load "js2-mode-autoloads"
    
      (add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode)))
    
    (with-eval-after-load "js2-mode"
    
      (add-hook 'js2-mode-hook #'tern-mode)
    
      (define-key js2-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
      (define-key js2-mode-map (kbd "C-M-x") 'js-send-last-sexp-and-go)
      (define-key js2-mode-map (kbd "C-c b") 'js-send-buffer)
      (define-key js2-mode-map (kbd "C-c d") 'my/insert-or-flush-debug)
      (define-key js2-mode-map (kbd "C-c C-b") 'js-send-buffer-and-go)
    
      (js2-imenu-extras-setup)
    
      ;; Add highlighting of many ECMA built-in functions.
      (setq js2-highlight-level 3))

    (defvar my/debug-counter 1)
    (defun my/insert-or-flush-debug (&optional reset beg end)
      (interactive "pr")
      (cond
       ((= reset 4)
        (save-excursion
          (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
          (setq my/debug-counter 1)))
       ((region-active-p)
        (save-excursion
          (goto-char end)
          (insert ");\n")
          (goto-char beg)
          (insert (format "console.log('DEBUG: %d', " my/debug-counter))
          (setq my/debug-counter (1+ my/debug-counter))
          (js2-indent-line)))
       (t
        ;; Wrap the region in the debug
        (insert (format "console.log('DEBUG: %d');\n" my/debug-counter))
        (setq my/debug-counter (1+ my/debug-counter))
        (backward-char 3)
        (js2-indent-line))))

    (defun js2-imenu-record-object-clone-extend ()
      (let* ((node (js2-node-at-point (1- (point)))))
      (when (js2-call-node-p node)
        (let* ((args (js2-call-node-args node))
               (methods (second args))
               (super-class (first args))
               (parent (js2-node-parent node)))
          (when (js2-object-node-p methods)
            (let ((subject (cond ((js2-var-init-node-p parent)
                                  (js2-var-init-node-target parent))
                                 ((js2-assign-node-p parent)
                                  (js2-assign-node-left parent)))))
              (when subject
                (js2-record-object-literal methods
                                           (js2-compute-nested-prop-get subject)
                                           (js2-node-abs-pos methods)))))))))

<http://emacs.stackexchange.com/questions/2658/how-to-display-the-list-of-functions-in-imenu-and-collapse-uncollapse-functions>

    )                                       ; Chapter 25 ends here.

# Editing Programs<a id="sec-31" name="sec-31"></a>

    ;;* 26 Editing (info "(emacs)Programs")
    
    (leuven--chapter leuven-load-chapter-26-programs "26 Editing Programs"

Move the current line up or down.

    ;; Swap the current and previous line.
    (defun leuven-move-line-up ()
      "Move the current line up."
      (interactive)
      (transpose-lines 1)
      (forward-line -2))
    
    ;; Swap the current and next line.
    (defun leuven-move-line-down ()
      "Move the current line down."
      (interactive)
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1))
    
    (add-hook 'prog-mode-hook
              (lambda ()
                (local-set-key (kbd "<C-S-up>") #'leuven-move-line-up)
                (local-set-key (kbd "<C-S-down>") #'leuven-move-line-down)))
                                          ; Sublime Text

    (defun leuven-scroll-up-one-line ()
      "Scroll text of current window upward 1 line."
      (interactive)
      (scroll-up 1))
    
    (defun leuven-scroll-down-one-line ()
      "Scroll text of current window downward 1 line."
      (interactive)
      (scroll-down 1))
    
    (add-hook 'prog-mode-hook
              (lambda ()
                (local-set-key (kbd "<C-up>") #'leuven-scroll-up-one-line)
                (local-set-key (kbd "<C-down>") #'leuven-scroll-down-one-line)))
                                          ; Sublime Text + SQL Management Studio

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Add &ldquo;move view&rdquo; up/down on C-up/down</b><br  />
See an example in SQL Server Management Studio
</div>

## Major Modes for Programming Languages<a id="sec-31-1" name="sec-31-1"></a>

Have a look at:
-   [CEDET](http://cedet.sourceforge.net) for development in AWK, C/C++, C#, Calc, Erlang, Java, Javascript, Make,
    PHP, Python, Ruby and Scheme,
-   [SLIME](http://common-lisp.net/project/slime) for Common Lisp development,
-   JDEE for Java programs.

Emacs tool for Emacs Lisp code analysis (to keep overview of the function calls
and dependecies between functions/variables): byte-compile-generate-call-tree

Also `who-calls.el`

    ;;** 26.1 Major Modes for (info "(emacs)Program Modes")
    
      (leuven--section "26.1 Major Modes for (emacs)Program Modes")

## Top-Level Definitions, or Defuns<a id="sec-31-2" name="sec-31-2"></a>


Imenu is a great tool allowing you to **go to a function definition** (by name), but
only if the definition is **in the buffer you are currently editing** (it won&rsquo;t jump
and open another file).

XXX Autoloaded?

    ;;** 26.2 Top-Level Definitions, or (info "(emacs)Defuns")
    
      (leuven--section "26.2 Top-Level Definitions, or (emacs)Defuns")

`font-lock-mode-hook` is run after entering a major mode. You can make use of this
to add an Imenu index to the menu bar in any mode that supports Imenu.

    ;; Making buffer indexes as menus.
    (try-require 'imenu)                  ; Awesome!
    (with-eval-after-load "imenu"
    
      ;; Add Imenu to the menu bar in any mode that supports it.
      (defun try-to-add-imenu ()
        (condition-case nil
            (imenu-add-to-menubar "Outline") ;; Imenu index.
          (error nil)))
      (add-hook 'font-lock-mode-hook #'try-to-add-imenu)
    
      ;; Bind Imenu from the mouse.
      (global-set-key [S-mouse-3] #'imenu)

    ;; String to display in the mode line when current function is unknown.
    (setq which-func-unknown "(Top Level)")
    
    ;; Show current function in mode line (based on Imenu).
    (which-function-mode 1)             ; ~ Stickyfunc mode (in header line)

Truncate the current function name (for the mode line):

        (defun my-which-func-current ()
          (let ((current (gethash (selected-window) which-func-table)))
            (if current
                (truncate-string-to-width current 30 nil nil "...")
              which-func-unknown)))
    
        (setq which-func-format
              `("[" (:propertize (:eval (my-which-func-current))
                                 local-map ,which-func-keymap
                                 face which-func
                                 mouse-face mode-line-highlight
                                 help-echo "mouse-1: go to beginning\n\
    mouse-2: toggle rest visibility\n\
    mouse-3: go to end") "]")))

`helm-imenu-in-all-buffers` command pops an Helm interface with all the **Imenu tags**
**across all buffers with the same mode as the current one**.

<div class="note">
In a sense, it is similar to `etag` selection, but works only for the *open*
buffers &#x2013; you don&rsquo;t get thousands of symbols from `etags` to deal with.  And this
is often more convenient as you don&rsquo;t have to explicitly build the `etags` table.

</div>

    (with-eval-after-load "helm-autoloads"
    
      ;; Keybinding to quickly jump to a symbol in buffer.
      (global-set-key [remap imenu] #'helm-imenu)
    
      ;; Helm Imenu tag selection across all buffers (with the same mode).
      (global-set-key (kbd "C-c i") #'helm-imenu-in-all-buffers))
    
    (with-eval-after-load "helm-imenu"
    
      ;; Do not directly jump to the definition even if there is just on candidate.
      (setq helm-imenu-execute-action-at-once-if-one nil))

## Indentation for Programs<a id="sec-31-3" name="sec-31-3"></a>

Changing the &ldquo;hanginess&rdquo; of a brace and then reindenting, will not move the
brace to a different line.  For this, you&rsquo;re better off getting an external
program like GNU `indent`, which will rearrange brace location, among other
things.

    ;;** 26.3 (info "(emacs)Program Indent")ation
    
        (leuven--section "26.3 (emacs)Program Indentation")
    
        ;; Turn on auto-fill mode in Lisp modes.
        (add-hook 'lisp-mode-hook #'auto-fill-mode)
        (add-hook 'emacs-lisp-mode-hook #'auto-fill-mode)
    
        ;; Auto-indentation: automatically jump to the "correct" column when
        ;; the RET key is pressed while editing a program (act as if you
        ;; pressed `C-j').
        (add-hook 'prog-mode-hook
                  (lambda ()
                    (local-set-key (kbd "<RET>") #'newline-and-indent)))
    
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
    
          ;; Use the SMIE code for navigation and indentation.
          (setq sh-use-smie t))

## Commands for Editing with Parentheses<a id="sec-31-4" name="sec-31-4"></a>

    ;;** 26.4 Commands for Editing with (info "(emacs)Parentheses")
    
      (leuven--section "26.4 Commands for Editing with (emacs)Parentheses")
    
      ;; Move cursor to offscreen open-paren when close-paren is inserted.
      (setq blink-matching-paren 'jump-offscreen) ; Doesn't work when
                                                  ; ‘show-paren-mode’ is enabled.
    
      ;; Highlight matching paren.
      (show-paren-mode 1)
      (setq show-paren-style 'mixed)
      (setq show-paren-ring-bell-on-mismatch t)

XXX Is the following still necessary?  Paren are highlighted by smartparens?

    ;; Highlight (nearest) surrounding parentheses (abd brackets).
    (with-eval-after-load "highlight-parentheses"
    
      (define-globalized-minor-mode global-highlight-parentheses-mode
        highlight-parentheses-mode
        (lambda ()
          (highlight-parentheses-mode t)))
    
      (setq hl-paren-colors '("red"))
      (setq hl-paren-background-colors '("white"))
    
      (global-highlight-parentheses-mode t))

    ;; Highlight nested parens, brackets, braces a different color at each depth.
    (with-eval-after-load "rainbow-delimiters-autoloads"
    
      ;; Enable rainbow-delimiters in programming modes.
      (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

To customize it, see
<http://yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial/>.

### Moving in the Parenthesis Structure<a id="sec-31-4-1" name="sec-31-4-1"></a>

    ;; Jump to matching parenthesis.
    (defun leuven-match-paren (arg)
      "Go to the matching parenthesis, if on a parenthesis."
      (interactive "p")
      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))
    
    (global-set-key (kbd "C-)") #'leuven-match-paren)

### Matching Parentheses<a id="sec-31-4-2" name="sec-31-4-2"></a>

Balanced editing: inserting two matching parentheses at once, etc.

-   electric-pair-mode (in Emacs, since 24.1)
    
    Here&rsquo;s a quick summary (`|` marks point):
    
    -   typing `((((` makes            `((((|))))`
    -   typing `))))` afterwards makes `(((())))|`
    -   if the buffer has too many closers, an opener before them will **not** autopair
    -   if the buffer has too many openers, a closer after them will **not** autoskip
    -   in a mixed parenthesis situation with `[]`&rsquo;s and `()`&rsquo;s, it tries to do
        sensible things

    ;; Enable automatic parens pairing (Electric Pair mode).
    (electric-pair-mode 1)
    
    (defvar org-electric-pairs
      '(
        ;; (?\* . ?\*)
        ;; (?/ . ?/)
        ;; (?_ . ?_)
        ;; (?= . ?=)                      ; Too much used in code blocks.
        (?~ . ?~))
      "Electric pairs for Org mode.")     ; See `org-emphasis-alist'.
    
    (defun leuven--org-add-electric-pairs ()
      (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
      ;; (setq-local electric-pair-text-pairs electric-pair-pairs) ; In comments.
    )
    
    (add-hook 'org-mode-hook #'leuven--org-add-electric-pairs)

Deleting one bracket doesn&rsquo;t delete the other.  If you want that, install
`autopair` package.

-   **smartparens**
    
    Pair can be simple as parentheses or brackets, or can be programming tokens
    such as `if` &#x2026; `fi` or `if` &#x2026; `end` in many languages.
    
    If you get into any serious lisp coding, check out `paredit` – it forces you to
    always have balanced brackets in the file, thus avoiding a whole class of
    silly but hard-to-spot mistakes on lines ending with and avalanche of ))))).
    
    If you are less into bondage&submission, `smartparens.el` is another option, it
    **helps** to have balanced brackets w/o breaking your will ;-)
    
    It is more about ease-of-coding them B&D.
    
    <div class="note">
    If we believe Xiao Hanyu, smartparens is the future, it is the ultimate
    solution for paren pairs management in Emacs world. It is flexible, uniform
    and highly customizable. It is also bundled with a comprehensive
    documentation, besides the aforementioned wiki, you can also M-x
    sp-cheat-sheet to get live examples, which, I think, is really a innovative
    feature.
    
    </div>
    
    **Bug 18785: Emacs hangs with Org mode when point is in LOGBOOK.**

    ;; Automatic insertion, wrapping and paredit-like navigation with user defined
    ;; pairs.
    (with-eval-after-load "smartparens-autoloads-XXX"
    
      ;; Default configuration for smartparens package.
      (require 'smartparens-config)
    
      ;; Toggle Smartparens mode in all buffers.
      (smartparens-global-mode 1)
    
      ;; Toggle Show-Smartparens mode in all buffers.
      (show-smartparens-global-mode 1)
    
      ;; Add local pairs in Org mode.
      (sp-with-modes 'org-mode
        ;; (sp-local-pair "'" nil :actions nil)
        (sp-local-tag "*" "*" "*" :actions '(wrap)) ; Bold.
        (sp-local-tag "/" "/" "/" :actions '(wrap)) ; Italic.
        (sp-local-tag "_" "_" "_" :actions '(wrap)) ; Underline.
        (sp-local-tag "=" "=" "=" :actions '(wrap)) ; Verbatim.
        (sp-local-tag "~" "~" "~" :actions '(wrap))) ; Code.
    
      ;; Remove local pairs in Text mode.
      (sp-local-pair 'text-mode "'" nil :actions nil)
      (sp-local-pair 'text-mode "\"" nil :actions nil)
    
      (push 'latex-mode sp-ignore-modes-list)

`turn-on-smartparens-strict-mode` add years to one&rsquo;s life to allow for more
hacking.  Seriously.

    ;; ;; Enable smartparens-strict-mode in all Lisp modes.
    ;; (mapc (lambda (mode)
    ;;         (add-hook (intern (format "%s-hook" (symbol-name mode)))
    ;;                   #'smartparens-strict-mode))
    ;;       sp--lisp-modes)
    )

-   **paredit** is (was?) the thing to use for Emacs Lisp!
    
    Paredit-kill kills text until next paren or double quote!  Also exists now in
    smartparens strict mode&#x2026;

    ;; Minor mode for editing parentheses.
    (with-eval-after-load "paredit-autoloads"
    
      (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    
      ;; Common Lisp editing extensions.
      (with-eval-after-load "redshank"    ; Requires `paredit'.
    
        (add-hook 'emacs-lisp-mode-hook #'redshank-mode)))

Use it in ELisp; use smartparens in other modes (NOT globally!)

Keep looking at the [cheatsheet](http://www.emacswiki.org/emacs/PareditCheatsheet) until you&rsquo;ve got the hang of it.

-   autopair

    (with-eval-after-load "autopair-autoloads"
    
      ;; Attempts to wrap the selected region.
      (setq autopair-autowrap t)
    
      ;; Enable Autopair-Global mode.
      (autopair-global-mode 1))

## Manipulating Comments<a id="sec-31-5" name="sec-31-5"></a>

    ;;** 26.5 (info "(emacs)Comments")
    
      (leuven--section "26.5 (emacs)Comments")
    
      ;; Always comments out empty lines.
      (setq comment-empty-lines t)

    (defadvice comment-dwim (around leuven-comment activate)
      "When called interactively with no active region, comment a single line instead."
      (if (or (use-region-p) (not (called-interactively-p 'any)))
          ad-do-it
        (comment-or-uncomment-region (line-beginning-position)
                                     (line-end-position))
        (message "Commented line")))

## Documentation Lookup<a id="sec-31-6" name="sec-31-6"></a>

Function argument hint.

    ;;** 26.6 (info "(emacs)Documentation") Lookup
    
      (leuven--section "26.6 (emacs)Documentation Lookup")
    
      ;; Idle time to wait before printing documentation.
      (setq eldoc-idle-delay 0.2)
    
      ;; Resize echo area to fit documentation.
      (setq eldoc-echo-area-use-multiline-p t)
    
      ;; Show the function arglist or the variable docstring in the echo area.
      (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
      (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; (global-eldoc-mode)                 ; In Emacs 25.
    
      ;; Highlight the arguments in `font-lock-variable-name-face'.
      (defun leuven--frob-eldoc-argument-list (string)
        "Upcase and fontify STRING for use with `eldoc-mode'."
        (propertize (upcase string)
                    'face 'font-lock-variable-name-face))
      (setq eldoc-argument-case 'leuven--frob-eldoc-argument-list)

## Hideshow minor mode<a id="sec-31-7" name="sec-31-7"></a>


XXX See <http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs>

Hideshow can **fold** and unfold (i.e. hide and un-hide) logical blocks of **code** in
many programming modes.

`hs-minor-mode.el` collapses code for a lot of languages, not only Lisp.  See
outline-minor-mode (See section 27.5) as well.

    ;;** 26.7 (info "(emacs)Hideshow") minor mode
    
      (leuven--section "26.7 (emacs)Hideshow minor mode")
    
      ;; Enable Hideshow (code folding) for programming modes.
      (add-hook 'prog-mode-hook #'hs-minor-mode)
    
      (with-eval-after-load "hideshow"
    
        ;; Change those really awkward key bindings with `@' in the middle.
        (define-key hs-minor-mode-map (kbd "H--") #'hs-hide-block)
        (define-key hs-minor-mode-map (kbd "<H-left>") #'hs-hide-block)
                                            ; `C-c @ C-h' (collapse current fold)
        (define-key hs-minor-mode-map (kbd "H-+") #'hs-show-block)
        (define-key hs-minor-mode-map (kbd "<H-right>") #'hs-show-block)
                                            ; `C-c @ C-s' (expand current fold)
        (define-key hs-minor-mode-map (kbd "H-/") #'hs-hide-all)
        (define-key hs-minor-mode-map (kbd "<H-up>") #'hs-hide-all)
                                            ; `C-c @ C-M-h' (collapse all folds)
        (define-key hs-minor-mode-map (kbd "H-*") #'hs-show-all)
        (define-key hs-minor-mode-map (kbd "<H-down>") #'hs-show-all)
                                            ; `C-c @ C-M-s' (expand all folds)
    
        (defcustom hs-face 'hs-face
          "*Specify the face to to use for the hidden region indicator"
          :type 'face
          :group 'hideshow)
    
        (defface hs-face
          '((t (:box (:line-width 1 :color "#777777") :foreground "#9A9A6A" :background "#F3F349")))
          "Face to hightlight the \"...\" area of hidden regions"
          :group 'hideshow)
    
        (defun hs-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display (propertize "..." 'face 'hs-face))))
    
        (setq hs-set-up-overlay 'hs-display-code-line-counts))

    ;; If hideshowvis is not installed, do not attempt to configure it, as this
    ;; will prevent packages (including hideshowvis itself) from compiling.
    (when display-graphic-p
      (with-eval-after-load "hideshowvis-autoloads"
    
        ;; Enable hideshowvis for programming modes.
        (add-hook 'prog-mode-hook
                  (lambda ()
                    (require 'fold-dwim)  ; More syntax definitions.
                    (hideshowvis-enable)))
    
        ;; (defface hs-fringe-face
        ;;   '((t (:box (:line-width 2 :color "#808080" :style released-button)
        ;;         :foreground "#999999")))
        ;;   "Face used to highlight the fringe on folded regions"
        ;;   :group 'hideshow)
    
        (defun hs-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (let* ((marker-string "*fringe-dummy*")
                   (marker-length (length marker-string))
                   (display-string "..."))
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
    
        (setq hs-set-up-overlay 'hs-display-code-line-counts)))

## Completion for Symbol Names<a id="sec-31-8" name="sec-31-8"></a>


    ;;** 26.8 (info "(emacs)Symbol Completion")
    
      (leuven--section "26.8 (emacs)Symbol Completion")

It&rsquo;s more or less a convention that each language mode binds its symbol
completion command (`completion-at-point`) to `<M-tab>`.

Note that `<M-tab>` is used by many window managers themselves (typically for
switching between windows) and is not passed to applications.

In that case, you should:

-   type `ESC TAB` or `C-M-i` for completion, or

-   bind the command normally bound to `<M-tab>` to a key that is convenient for
    you to hit, such as `<C-tab>`.

    ;; When you hit `<C-tab>', call the command normally bound to `<M-tab>'.
    (global-set-key (kbd "<C-tab>")
      (lambda ()
        (interactive)
        (call-interactively (key-binding (kbd "<M-tab>")))))

See also Dabbrev (See section 34.2), Emacs&rsquo; standard autocompletion (on by default).

## Glasses minor mode<a id="sec-31-9" name="sec-31-9"></a>

Add overlays to use a different face for the capital letters in symbols like
`CamelCaseUnreadableSymbol`, to make them easier to read.

    ;;** 26.9 (info "(emacs)Glasses") minor mode
    
      (leuven--section "26.9 (emacs)Glasses minor mode")
    
      (add-hook 'ess-mode-hook #'glasses-mode)
      (add-hook 'inferior-ess-mode-hook #'glasses-mode)
      (add-hook 'java-mode-hook #'glasses-mode)
    
      (with-eval-after-load "glasses"
    
        ;; String to be displayed as a visual separator in unreadable identifiers.
        (setq glasses-separator "")
    
        ;; No display change.
        (setq glasses-original-separator "")
    
        ;; Face to be put on capitals of an identifier looked through glasses.
        (make-face 'leuven-glasses-face)
        (set-face-attribute 'leuven-glasses-face nil :weight 'bold)
        (setq glasses-face 'leuven-glasses-face)
                                            ; Avoid the black foreground set in
                                            ; `emacs-leuven-theme' to face `bold'.
    
        ;; Set properties of glasses overlays.
        (glasses-set-overlay-properties)
    
        ;; No space between an identifier and an opening parenthesis.
        (setq glasses-separate-parentheses-p nil))

subword-mode : M-f/M-b in `CamelCaseUnreadableSymbol`.

## C and related modes<a id="sec-31-10" name="sec-31-10"></a>

### Eclim<a id="sec-31-10-1" name="sec-31-10-1"></a>

[Eclim](http://eclim.org/) provides the ability to **bring [Eclipse](http://www.eclipse.org/) code editing features** (to Vim, but
also to other editors thanks to third party clients):

-   &ldquo;true&rdquo; **code completion** (i.e., only context-sensitive completions),
-   **code validation** (report any validation errors found),
-   searching for declarations or references, and
-   [many more](http://eclim.org/features.html).

The initial goal was to provide Eclipse **Java** functionality, but support for
various other languages (**C/C++**, HTML/CSS, Groovy, JavaScript, **PHP**, **Python**, **Ruby**,
XML/DTD/XSD, etc.) has been added.

#### Download / install<a id="sec-31-10-1-1" name="sec-31-10-1-1"></a>

1.  Install [Eclipse](http://www.eclipse.org/downloads/).
2.  Install [Eclim](http://eclim.org/install.html) (see `vim.skip` and `vim.skip.hint`).
3.  Install [Emacs-eclim](https://github.com/senny/emacs-eclim) from MELPA.

#### Getting started<a id="sec-31-10-1-2" name="sec-31-10-1-2"></a>

You should **create an Eclipse project first**, and then you can create a Java file
and open it in Emacs.  Open plain Java file without creating an Eclipse project
will not work.

<div class="note">
&ldquo;Could not find eclipse project for <file>.java&rdquo;?  Are you using **Cygwin Emacs**
with a Windows Java installation?  `/cygdrive` paths won&rsquo;t be understood&#x2026;

</div>

#### The Eclim daemon<a id="sec-31-10-1-3" name="sec-31-10-1-3"></a>

Eclimd (Eclim&rsquo;s daemon) can run without a graphical Eclipse, but you can also
run it from plain Eclipse.  So whenever you need some eclipse&rsquo;s functionalities
that aren&rsquo;t (yet) supported by either `emacs-eclim` or `eclim`, you can switch to it
(e.g. the debugger or the profiler).

#### [FAQ / Troubleshooting](http://eclim.org/faq.html)<a id="sec-31-10-1-4" name="sec-31-10-1-4"></a>

To tell eclim **which Eclipse workspace to use**, you can start eclimd like so:

    eclimd -Dosgi.instance.area.default=@user.home/another_workspace

#### Core functionality<a id="sec-31-10-1-5" name="sec-31-10-1-5"></a>

-   **`C-c C-e p c` (or `M-x eclim-project-create`):** Not documented.

-   **`M-x eclim-manage-projects`:** **Manage all your eclim projects** in one buffer.

-   **`M-x eclim-project-build`:** Triggers a **build** of **the current project**.

-   **`C-c C-e b` (or `M-x eclim-problems`):** **Show current compilation problems** in a separate window.

-   **`C-c C-e o` (or `M-x eclim-problems-open`):** Opens a new window inside the current frame showing the current project
    **compilation problems**.

-   **`C-c C-e f d` (or `M-x eclim-java-find-declaration` or `M-.` in Emacs Leuven):** **Find** and display the **declaration** of the **Java identifier at point**.

-   **`C-c C-e f r` (or `M-x eclim-java-find-references`):** **Find** and display **references** for the **Java identifier at point**
         = list callers.

-   **M-x eclim-run-class:** **Run the current class**.

-   **`C-c C-e t` (or `M-x eclim-run-junit`):** **Run** the current **JUnit** class or method at point.

    ;; An interface to the Eclipse IDE.
    (with-eval-after-load "emacs-eclim-autoloads-XXX"
    
      ;; Enable Eclim mode in Java.
      (add-hook 'java-mode-hook #'eclim-mode))
    
    (with-eval-after-load "eclim"
    
      ;; Find Eclim installation.
      (setq eclim-executable
            (or (executable-find "eclim")
                (concat leuven--windows-program-files-dir "eclipse/eclim.bat")))
      ;; (setq eclim-executable "C:/PROGRA~2/eclipse/eclim.bat")
      ;; (setq eclim-executable "C:/Users/Fabrice/Downloads/eclipse/eclim.bat")
    
      ;; (add-to-list 'eclim-eclipse-dirs
      ;;              (concat leuven--windows-program-files-dir "eclipse/eclim"))
    
      ;; Print debug messages.
      (setq eclim-print-debug-messages t)
    
      ;; Add key binding.
      (define-key eclim-mode-map (kbd "M-.") #'eclim-java-find-declaration)

    ;; Display compilation error messages in the echo area.
    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)

     ;; Add the emacs-eclim source.
     (require 'ac-emacs-eclim-source)
    
     ;;! Limit `ac-sources' to Eclim source.
     (defun ac-emacs-eclim-java-setup ()
       (setq ac-sources '(ac-source-emacs-eclim)))
    ;; (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
    
     (ac-emacs-eclim-config)

<div class="note">
There are 2 Eclim Company-mode backends:
-   `company-eclim`, the built-in one.
-   `company-emacs-eclim`, which is a bit more feature-rich (it uses YASnippet, for
    instance), and also a bit buggier.

</div>

See <http://java-coders.com/p/tsdh/emacs-eclim> for Company.

    ;; Configure company-mode.
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)

    ;; Control the Eclim daemon from Emacs.
    (require 'eclimd)
    
    )

    )                                       ; Chapter 26 ends here.

# Compiling and Testing Programs<a id="sec-32" name="sec-32"></a>

    ;;* 27 (info "(emacs)Building") Compiling and Testing Programs
    
    (leuven--chapter leuven-load-chapter-27-building "27 Compiling and Testing Programs"

## Running Compilations under Emacs<a id="sec-32-1" name="sec-32-1"></a>

My build command: `cd /path/to/Makefile && make -f Makefile`.

You don&rsquo;t need a `Makefile` to perform simple tasks, because `make` knows a lot of
built in rules out of the box.  For example, to compile a `.c` source file `foo.c`
into a program `foo`, all you need is say `make -k foo`, and `make` will do it even
without a `Makefile`.

XXX Compiling with `ant`?

    ;;** 27.1 Running (info "(emacs)Compilation")s under Emacs
    
      (leuven--section "27.1 Running (emacs)Compilations under Emacs")
    
      ;; Invoke a compiler with the same command as in the last invocation of
      ;; `compile'.
      (autoload 'recompile "compile"
        "Re-compile the program including the current buffer." t)
    
      (global-set-key (kbd "<f9>") #'recompile)
    
      ;; Scroll the `*compilation*' buffer window to follow output as it appears.
      (setq compilation-scroll-output t)
    
      ;; Number of lines in a compilation window.
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
    
      (defun cc-goto-first-error( buffer exit-condition )
        (with-current-buffer buffer
          (goto-char (point-min))
          (compilation-next-error 1)
          (beep)))
    
      (add-to-list 'compilation-finish-functions 'cc-goto-first-error)
    
      (defvar make-clean-command "make clean all"
        "*Command used by the `make-clean' function.")
    
      (defun make-clean (&optional arg)
        "Run a make clean."
        (interactive "P")
        (require 'compile)                  ; Needed for compile-internal.
        (if arg
            (setq make-clean-command
                  (read-string "Command: " make-clean-command)))
        (save-some-buffers (not compilation-ask-about-save) nil)
        (compile-internal make-clean-command "No more errors"))
    
      (global-set-key (kbd "<S-f9>") #'make-clean)

You&rsquo;re debugging Bash code?  I normally use `mode-compile.el` for this.
Basically, it runs Bash with lots of debug output.

### Java<a id="sec-32-1-1" name="sec-32-1-1"></a>

Compile within Emacs using `ant`.

    (defvar leuven--ant-command-history nil
      "Ant command history variable")
    
    (defun leuven-ant(&optional args)
      "Runs ant in the current project. Starting at the directory
       where the file being visited resides, a search is made for
       build.xml recursively. A maven command is made from the first
       directory where the build.xml file is found is then displayed in
       the minibuffer. The command can be edited as needed and then
       executed. Errors are navigate to as in any other compile mode"
      (interactive)
      (let ((fn (buffer-file-name)))
        (let ((dir (file-name-directory fn)))
          (while (and (not (file-exists-p (concat dir "/build.xml")))
                      (not (equal dir (file-truename (concat dir "/..")))))
            (setf dir (file-truename (concat dir "/.."))))
          (if (not (file-exists-p (concat dir "/build.xml")))
              (message "No build.xml found")
            (compile (read-from-minibuffer "Command: "
                                           (concat "ant -emacs -f "
                                           dir "/build.xml compile") nil
                                           nil
                                           'leuven--ant-command-history))))))
    
    (add-hook 'java-mode-hook
              (lambda () (local-set-key "<f9>" 'leuven-ant)))

## Compilation Mode<a id="sec-32-2" name="sec-32-2"></a>

When Emacs visits the locus of an error message, it momentarily highlights the
relevant source line.  The duration of this highlight is determined by the
variable `next-error-highlight`.

    ;;** 27.2 (info "(emacs)Compilation Mode")
    
      (leuven--section "27.2 (emacs)Compilation Mode")
    
      ;; Automatically jump to the first error during compilation.
      (setq compilation-auto-jump-to-first-error t)
    
      ;; Display the next compiler error message.
      (global-set-key (kbd "<f10>") #'next-error)
                                            ; Also on `C-x `' and `M-g n'.
    
      ;; Display the previous compiler error message.
      (global-set-key (kbd "<S-f10>") #'previous-error)
                                            ; Also on `M-g p'.
    
      ;; Display the first compiler error message.
      (global-set-key (kbd "<C-f10>") #'first-error)
    
      ;; ;; Prefer fringe.
      ;; (setq next-error-highlight 'fringe-arrow)
    
      ;; Highlight and parse the whole compilation output as soon as it arrives.
      (setq compile-auto-highlight t)

## Searching with Grep under Emacs<a id="sec-32-3" name="sec-32-3"></a>

Find files matching some regexp, and click on the hyperlinks to the files to
locate your match.

The commands

-   `lgrep` (local search, with `grep-template`) and
-   `rgrep` (**recursive** search, with `grep-find-template`)

are somehow more user-friendly than the `M-x grep` command.

The doc string of `grep-use-null-device` which explains that, with its default
value `auto-detect`, Emacs invokes Grep once with `/dev/null` appended (causing the
search to return no results) for the purposes of detecting how to invoke it
thereafter.

    ;;** 27.4 (info "(emacs)Grep Searching") under Emacs
    
      (leuven--section "27.4 (emacs)Grep Searching under Emacs")
    
      (with-eval-after-load "grep"
    
        ;; Ignore case distinctions in the default `grep' command.
        (grep-apply-setting 'grep-command "grep -i -H -n -e ")
    
        ;; Do not append `null-device' (`/dev/null' or `NUL') to `grep' commands.
        (grep-apply-setting 'grep-use-null-device nil)
                                            ; Not necessary if the `grep' program
                                            ; used supports the `-H' option.
    
        ;; For Windows.
        (when leuven--win32-p
          ;; Default find command for `M-x grep-find'.
          (grep-apply-setting 'grep-find-command '("findstr /sn *" . 13)))

Ag, the silver searcher (in Cygwin!):
-   <https://github.com/howardabrams/dot-files/blob/master/emacs.org>
-   <https://www.reddit.com/r/emacs/comments/3fr4ro/how_do_you_grep/>

Ag output options:

-   **`--[no]color`:** Print **color** codes in results.

-   **`--column`:** Print **column numbers** in results.

-   **`--[no]heading`:** 

-   **`--[no]group`:** Same as `--[no]break --[no]heading`.

-   **`--line-numbers` (or `--numbers`):** Print **line numbers** even for streams.

Ag search options:

-   **`--depth`:** Search up to **`NUM` directories deep** (Default: `25`).

-   **`--ignore-case`:** Match **case insensitively**.

-   **`--all-text`:** Search **all text files** (doesn&rsquo;t include hidden files).

    ag %s -l --nocolor --hidden -g ""

    (when (executable-find "agXXX") ; Need to fix base dir and file extensions
    
      ;; Default grep command for `M-x grep'.
      ;; (grep-apply-setting 'grep-command "ag --nogroup --numbers ")
    
      ;; Default command to run for `M-x lgrep'.
      ;; (grep-apply-setting 'grep-template "ag --depth 0 <R> <F>")
    
      ;; Default find command for `M-x grep-find'.
      ;; (grep-apply-setting 'grep-find-command '("ag --noheading --column " . 25))
    
      ;; Default command to run for `M-x rgrep' (`C-c 3').
      (grep-apply-setting 'grep-find-template
                          "ag --color --nogroup --line-numbers <R> ."))
                                        ; `<D>' for the base directory.
                                        ; `<X>' for the find options to restrict
                                        ;       directory list.
                                        ; `<F>' for the find options to limit
                                        ;       the files matched.
                                        ; ------------------------------------
                                        ; `<C>' for the place to put `-i' if the
                                        ;       search is case-insensitive.
                                        ; `<R>' for the regular expression to
                                        ;       search for.

    ;; (setq-default grep-first-column 1)
    
    ;; Use `find -print0' and `xargs -0'.
    (setq grep-find-use-xargs 'gnu))    ; with-eval-after-load "grep" ends here.

    ;; Run `grep' via `find', with user-friendly interface.
    (global-set-key (kbd "C-c 3") #'rgrep)

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

## Finding Syntax Errors On The Fly<a id="sec-32-4" name="sec-32-4"></a>

Flycheck (aka &ldquo;Flymake done right&rdquo;) can perform static code analysis **on-the-fly**,
and **highlight warnings and errors** in the code immediately as you are editing it,
if you made a mistake.

To **list all errors in the current buffer**, you can popup an error list with
`C-c ! l` (`M-x flycheck-list-errors`).

In the error list window the following keybindings are available:

-   **`n`:** Move to the next error.

-   **`p`:** Move to the previous error.

-   **`q`:** Hide the error list window.

-   **`RET`:** Jump to the location of the error at point.

-   **`g`:** Refresh the error list, by triggering a new syntax check in the associated
    buffer.

-   **`S`:** Sort the error list by the column at point. Press repeatedly to inverse the
    sorting order.

    ;;** 27.5 (info "(emacs)Flymake")
    
      (leuven--section "27.5 (emacs)Flymake")
    
      ;; Modern on-the-fly syntax checking.
      (with-eval-after-load "flycheck-autoloads"
    
        ;; Enable Flycheck mode in all programming modes.
        (add-hook 'prog-mode-hook #'flycheck-mode))
    
      (with-eval-after-load "flycheck"
    
        ;; Delay in seconds before displaying errors at point.
        (setq flycheck-display-errors-delay 0.3)
    
        ;; ;; Indicate errors and warnings via icons in the right fringe.
        ;; (setq flycheck-indication-mode 'right-fringe)
    
        ;; Remove newline checks, since they would trigger an immediate check when
        ;; we want the `flycheck-idle-change-delay' to be in effect while editing.
        (setq flycheck-check-syntax-automatically
              '(save
                idle-change
                ;; new-line
                mode-enabled))
    
        ;; Each buffer get its local `flycheck-idle-change-delay' because of the
        ;; buffer-sensitive adjustment above.
        (make-variable-buffer-local 'flycheck-idle-change-delay)
    
        (defun leuven--adjust-flycheck-automatic-syntax-eagerness ()
          "Adjust how often we check for errors based on if there are any.
    
    This lets us fix any errors as quickly as possible, but in
    a clean buffer we're an order of magnitude laxer about checking."
          (setq flycheck-idle-change-delay
                (if (assq 'error (flycheck-count-errors flycheck-current-errors))
                    ; only check for REAL errors (original source: Magnar Sveen)
                    1
                  20)))
    
        ;; Functions to run after each syntax check.
        (add-hook 'flycheck-after-syntax-check-hook
                  #'leuven--adjust-flycheck-automatic-syntax-eagerness)
    
        ;; Change mode line color with Flycheck status.
        (when (locate-library "flycheck-color-mode-line")
          (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

    (global-set-key (kbd "C-x C-S-e") #'elint-current-buffer)
    
    (with-eval-after-load "elint"
      (add-to-list 'elint-standard-variables 'current-prefix-arg)
      (add-to-list 'elint-standard-variables 'command-line-args-left)
      (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
      (add-to-list 'elint-standard-variables 'emacs-major-version)
      (add-to-list 'elint-standard-variables 'window-system))

## Running Debuggers Under Emacs<a id="sec-32-5" name="sec-32-5"></a>

    ;;** 27.6 Running (info "(emacs)Debuggers") Under Emacs
    
      (leuven--section "27.6 Running (emacs)Debuggers Under Emacs")
    
      (with-eval-after-load "gdb-mi"
    
        ;; Enable Gdb-Many-Windows mode.
        (setq gdb-many-windows t))          ; The only important parameter for GDB.

## Debugging Lisp programs<a id="sec-32-6" name="sec-32-6"></a>

Emacs has the basic debugger/stack trace, but it also has the Edebug facility,
which is very powerful, for the more complex situation.

With that source-level debugger for Emacs Lisp, you can:
-   step through evaluation, stopping before and after each expression,
-   set conditional or unconditional breakpoints.

    ;;** Debugging Lisp programs
    
      ;; Source-level debugger for Emacs Lisp.
      (with-eval-after-load "edebug"
    
        ;; ;; Display a trace of function entry and exit.
        ;; (setq edebug-trace t)

While `edebug`&rsquo;ging, use `T` to view a trace buffer (`*edebug-trace*`).  Emacs will
quickly execute the rest of your code, printing out the arguments and return
values for each expression it evaluates.

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
    
    (define-key edebug-mode-map [remap top-level] #'leuven-edebug-quit))

## Executing Lisp Expressions<a id="sec-32-7" name="sec-32-7"></a>

Just as in C, C++, Java, Perl, Python, etc, Lisp code is kept in files.  All
the normal editing operations are performed on files.  In this respect, hacking
in Lisp is like hacking in any other language that you are used to.  What&rsquo;s
different is that what you are hacking is a running Lisp program.  When you
edit a function definition or add a new one, you compile it into a running
program.  There is no compile, link, run, debug cycle as you know it from C or
Java.

Ponder that for a minute.

When you fix a bug in a C function, you have to recompile, relink, and reload
your program before you can test the fix.  You don&rsquo;t do that in Lisp.  You make
the fix and then go straight to testing it.  This process can be even faster
than fixing a bug in a scripting language like Perl.

## Libraries of Lisp Code for Emacs<a id="sec-32-8" name="sec-32-8"></a>

    ;;** 27.8 (info "(emacs)Lisp Libraries") for Emacs
    
      (leuven--section "27.8 (emacs)Lisp Libraries")
    
      ;; Remove *.elc when save.
      (defun remove-elc-on-save ()
        "If you're saving an elisp file, likely the .elc is no longer valid."
        (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook
                  (lambda ()
                    (if (file-exists-p (concat buffer-file-name "c"))
                        (delete-file (concat buffer-file-name "c"))))))
    
      (add-hook 'emacs-lisp-mode-hook #'remove-elc-on-save)
    
      ;; Force load of `.el' files when they are newer than the `.elc' files.
      (setq load-prefer-newer t)            ; From Emacs 24.4.

## Evaluating Emacs Lisp Expressions<a id="sec-32-9" name="sec-32-9"></a>

-   **`C-x C-e`:** Evaluate the Emacs Lisp expression before point, and print the value in
    the echo area (`eval-last-sexp`).

-   **`C-M-x` (in Emacs Lisp mode):** Evaluate the defun containing or after point, and print the value in the
    echo area (`eval-defun`).

To (attempt to) cleanly reevaluate a buffer of Emacs Lisp code, use
`nuke-and-eval`.

    ;;** 27.9 (info "(emacs)Lisp Eval") Expressions
    
      (leuven--section "27.9 (emacs)Lisp Eval Expressions")
    
      ;; Enable the use of the command `eval-expression' without confirmation.
      (put 'eval-expression 'disabled nil)
    
      ;; Maximum depth of lists to print in the result of the evaluation commands
      ;; before abbreviating them.
      (setq eval-expression-print-level nil) ; No limit.
    
      ;; Maximum length of lists to print in the result of the evaluation commands
      ;; before abbreviating them.
      (setq eval-expression-print-length nil) ; No limit.

    ;; ;; Limit serving to catch infinite recursions for you before they
    ;; ;; cause actual stack overflow in C, which would be fatal for Emacs.
    ;; (setq max-lisp-eval-depth 600)        ; 1000?

    (defun eval-and-replace ()
      "Replace the preceding sexp with its value."
      (interactive)
      (let ((value (eval (preceding-sexp))))
        (kill-sexp -1)
        (insert (format "%S" value))))
    
    (global-set-key (kbd "C-c e") #'eval-and-replace)

For on-the-fly evaluation/substitution of Emacs Lisp code, see
<https://github.com/Fuco1/litable>.

See demo of it: <http://www.youtube.com/watch?v=TMoPuv-xXMM>.

    ;; Dynamic evaluation replacement with Emacs.
    (with-eval-after-load "litable-autoloads"
    
      (add-hook 'lisp-interaction-mode-hook #'litable-mode))

## Lisp Interaction Buffers (`*scratch*`)<a id="sec-32-10" name="sec-32-10"></a>

    ;;** 27.10 (info "(emacs)Lisp Interaction") Buffers
    
      (leuven--section "27.10 (emacs)Lisp Interaction Buffers")
    
      ;; Don't display the "Welcome to GNU Emacs" buffer on startup.
      (setq inhibit-startup-screen t)
    
      ;; Don't insert instructions in the `*scratch*' buffer at startup.
      (setq initial-scratch-message nil)

Don&rsquo;t use any specific mode for the initial buffer.  That ensure that no Lisp
or Org specific stuff gets loaded at each startup time.

    ;; Major mode command symbol to use for the initial `*scratch*' buffer.
    (setq initial-major-mode 'fundamental-mode)

**That code must be used in the `.emacs` file (or libraries loaded from it).**
Otherwise, `*scratch*` will already have been created, in the default major
mode.

To evaluate a non-interactive command, simply use `IELM`.

    )                                       ; Chapter 27 ends here.

# Maintaining Programs<a id="sec-33" name="sec-33"></a>

    ;;* 28 (info "(emacs)Maintaining") Programs
    
    (leuven--chapter leuven-load-chapter-28-maintaining "28 Maintaining Programs"

## Version Control<a id="sec-33-1" name="sec-33-1"></a>

Version control systems that interact with Emacs in a generic way.

-   **`C-x v d`:** Open Dired buffer in VC-mode.

-   **`C-x v v`:** Take appropriate &ldquo;next&rdquo; action (add, commit).

-   **`C-x v r` (or `M-!` ?):** Switch Git branches.

    ;;** 28.1 (info "(emacs)Version Control")
    
      (leuven--section "28.1 (emacs)Version Control")

### Version Control and the Mode Line<a id="sec-33-1-1" name="sec-33-1-1"></a>

&ldquo;Unmodified-according-to-VC&rdquo; buffers use `-` as a separator in their VC
indicator, and modified buffers have `:` (e.g., `CVS-1.2` vs. `CVS:1.2`).  The
tooltip over the VC indicator also says more explicitly.

<div class="inlinetask">
<b>Lisp expression in mode-line-format</b><br  />
The Lisp expressions supported in `mode-line-format` are those documented in the
manual, and nothing else.  In particular, the `:eval` part must appear
explicitly, and any symbol stands for its value (which is not eval&rsquo;ed).  IOW, a
full-blown Lisp evaluation there is not implemented&#x2026;
</div>

### Features of the Log Entry Buffer<a id="sec-33-1-2" name="sec-33-1-2"></a>

-   **`C-c C-d`:** Show the diff we are about to commit.

-   **`D` (`vc-git-previous-revision`):** Get the previous revision.

Using Emacs VC, you only have to write the ChangeLog, then use `C-c C-a` to insert
it into the commit buffer. So there is no need to &ldquo;write the same thing twice&rdquo;.

    ;;*** 28.1.4 (info "(emacs)Log Buffer")
    
      (defun leuven--vc-log-mode-setup ()
        (when (leuven--executable-ispell-program-name-p)
          (setq ispell-local-dictionary "american")
          (flyspell-mode)))
    
      (add-hook 'vc-log-mode-hook #'leuven--vc-log-mode-setup)

    (with-eval-after-load "vc-git"
    
      ;; Major mode for editing git commit messages.
      (try-require 'git-commit))
    
    (with-eval-after-load "git-commit"
    
      ;; Turn on on-the-fly spell-checking.
      (add-hook 'git-commit-setup-hook #'flyspell-mode)
    
      ;; Turn off save-place.
      (add-hook 'git-commit-setup-hook
                (lambda ()
                  (toggle-save-place 0))))

See <http://whattheemacsd.com/setup-magit.el-01.html>.

Or use Tig?  See <http://blogs.atlassian.com/2013/05/git-tig/>.

### Examining And Comparing Old Revisions<a id="sec-33-1-3" name="sec-33-1-3"></a>

    ;;*** 28.1.6 (info "(emacs)Old Revisions")
    
      (leuven--section "28.1.6 Examining And Comparing Old Revisions")
    
      ;; Switches for diff under VC.
      (setq vc-diff-switches diff-switches)

### VC Change Log<a id="sec-33-1-4" name="sec-33-1-4"></a>

    ;;*** 28.1.7 (info "(emacs)VC Change Log")
    
      (leuven--section "28.1.7 VC Change Log")

-   **`C-x v l`:** Display the change history for the current **file** (`vc-print-log`).

-   **`C-x v L`:** Open the **log buffer** for the current **repository** (List the change log for the
    current VC controlled tree in a window).
    
    When in it:
    
    -   `C-m` to read the commit message,
    -   `d` to see the corresponding diff.

We have at least two pairs of commands:

-   `C-x v l` vs `C-x v L` *and*
-   `C-x v =` vs `C-x v D`,

of which one operates on the current **file** vs   
the other on the **repository** as a whole (entire project).

-   `C-x v l` to view the file&rsquo;s history.
-   `n` and `p` to move between commits.
-   `f` to visit the file as of the commit at point.

`git-timemachine` makes the process of **navigating between different versions** of
a file almost completely seamless.

You call `M-x git-timemachine RET` from a buffer visiting a tracked file, then you
can use the following keys to navigate historic versions of the file:

-   **`p`:** Visit previous historic version.

-   **`n`:** Visit next historic version.

-   **`w`:** Copy the abbreviated hash of the current historic version.

-   **`W`:** Copy the full hash of the current historic version.

-   **`q`:** Exit the time machine.

    ;; Walk through Git revisions of a file.
    (with-eval-after-load "git-timemachine-autoloads"
    
      ;; Number of chars from the full SHA1 hash to use for abbreviation.
      (setq git-timemachine-abbreviation-length 7)
    
      (global-set-key (kbd "C-x v t") #'git-timemachine))

    ;; Pop up last commit information of current line.
    (with-eval-after-load "git-messenger-autoloads"
    
      (global-set-key (kbd "C-x v p") #'git-messenger:popup-message) ; `C-h g'.
    
      ;; Pop up commit ID and author name too.
      (setq git-messenger:show-detail t))

### VC Directory Mode<a id="sec-33-1-5" name="sec-33-1-5"></a>

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
    
      ;; VC status without asking for a directory.
      (global-set-key (kbd "<C-f9>") #'leuven-vc-jump)
    
      (add-hook  'vc-dir-mode-hook
                 (lambda ()
                   ;; Hide up-to-date and unregistered files.
                   (define-key vc-dir-mode-map
                     (kbd "x") #'leuven-vc-dir-hide-up-to-date-and-unregistered)
                   (define-key vc-dir-mode-map
                     (kbd "E") #'vc-ediff)
                   (define-key vc-dir-mode-map
                     (kbd "#") #'vc-ediff-ignore-whitespace)
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
          ;; Go over from the last item to the first and remove the unregistered
          ;; files and directories with no child files.
          (while (not (eq crt first))
            (let* ((data (ewoc-data crt))
                   (dir (vc-dir-fileinfo->directory data))
                   (next (ewoc-next vc-ewoc crt))
                   (prev (ewoc-prev vc-ewoc crt))
                   ;; ewoc-delete does not work without this...
                   (inhibit-read-only t))
              (when (or
                     ;; Remove directories with no child files.
                     (and dir
                          (or
                           ;; Nothing follows this directory.
                           (not next)
                           ;; Next item is a directory.
                           (vc-dir-fileinfo->directory (ewoc-data next))))
                     ;; Remove files in the unregistered state.
                     (eq (vc-dir-fileinfo->state data) 'unregistered))
                (ewoc-delete vc-ewoc crt))
              (setq crt prev)))))
    
      (defun vc-ediff-ignore-whitespace (historic &optional not-urgent)
        "Ignore regions that differ in white space & line breaks only."
        (interactive (list current-prefix-arg t))
        (require 'ediff)
        (let ((ediff-ignore-similar-regions t))
          (call-interactively 'vc-ediff)))  ; XXX does not work yet!

### Customizing VC<a id="sec-33-1-6" name="sec-33-1-6"></a>

    ;;*** 28.1.13 (info "(emacs)Customizing VC")
    
      (leuven--section "28.1.13 Customizing VC")
    
      ;; Files covered by VC get backups (as with other files).
      (setq vc-make-backup-files t)
    
      ;; http://www.emacswiki.org/emacs/VcTopDirectory
      ;; For Git.
      (defadvice vc-dir-prepare-status-buffer
                 (before leuven-vcs-goto-top-directory activate compile)
        (let* ((backend (ad-get-arg 2))
               (vcs-dir (ad-get-arg 1))
               (vcs-top-dir (vc-call-backend backend 'responsible-p vcs-dir)))
          (when (stringp vcs-top-dir)
            (ad-set-arg 1 vcs-top-dir))))
    
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
    
      (define-key vc-prefix-map (kbd "=") #'leuven-vc-diff)

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Customize \`vc-annotate-background&rsquo; (and future \`vc-annotate-foreground&rsquo;)</b><br  />
nil</div>

## Change Logs<a id="sec-33-2" name="sec-33-2"></a>

    ;;** 28.2 (info "(emacs)Change Log")
    
      (leuven--section "28.2 (emacs)Change Logs")
    
      (with-eval-after-load "add-log"
    
        ;; Don't make a new entry, when the last entry was made by you and on the
        ;; same date.
        (setq add-log-always-start-new-record nil)
    
        ;; Add the file's version number to the change log entry.
        (setq change-log-version-info-enabled t)
    
        (add-hook 'change-log-mode-hook
                  (add-to-list
                   'change-log-font-lock-keywords
                   '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                     (0 'change-log-date-face)
                     ("\\([^<(]+?\\)[   ]*[(<]\\([[:alnum:]_.+-]+@[[:alnum:]_.-]+\\)[>)]" nil nil
                      (1 'change-log-name)
                      (2 'change-log-email))))))

## Tags Tables (navigating code)<a id="sec-33-3" name="sec-33-3"></a>

Using tags tables is the most generic approach to setup **code navigation**.  A
&ldquo;tag&rdquo; signifies a language object for which an index entry is available.

<div class="note">
There are many situations when you cannot or might not want to use `tags`.  You
would like to use Imenu (See section 31.2) because:

-   Imenu is meaningful in non-programming modes and for non-visiting buffers.

-   Virtually all modes already define Imenu, so it works out of the box
    everywhere, even if you read a language in which you will never write.

-   And finally, Imenu operates on what you work on &#x2013; open files &#x2013; you don&rsquo;t get
    thousands of symbols from `etags` to deal with.

Using `helm-imenu-in-all-buffers` offers a much better, more uniform and more
consistent experience than anything else.

</div>

Always run `make TAGS` right after you build some project.

Once you&rsquo;ve built a `TAGS` file (`ctags` / `etags`), you can find all commands that
match a regexp.

Support for tags has been in Emacs for a long time: any installation of Emacs
should comes with the `etags` program, which supports many different languages.

Comparison of tools:

-   **etags (Emacs-style TAGS file):** Ada, Asm, **C**, Objective C, **C++**, C#, Cobol, Erlang, Forth, Fortran, HTML,
    **Java**, **Lisp**, Lua, Makefile, Pascal, Perl, **PHP**, Postscript, Prolog, **Python**,
    Scheme, TeX, TeXinfo, and YACC.
    
    On Windows, Emacs comes with `etags` and a similar executable file called
    `ctags`.  In the Emacs distribution, `etags.exe` = `ctags.exe` in the `bin`
    directory.

-   **Exuberant Ctags:** Ant, Asm, Asp, Awk, Basic, BETA, **C**, **C++**, C#, Cobol, DosBatch, Eiffel,
    Erlang, Flex, Fortran, HTML, **Java**, JavaScript, **Lisp**, Lua, Makefile,
    MatLab, OCaml, Pascal, Perl, **PHP**, **Python**, REXX, **Ruby**, Scheme, Sh, SLang,
    SML, SQL, Tcl, TeX, Vera, Verilog, VHDL, Vim, and YACC.
    
    You should download **Exuberant Ctags** from <http://ctags.sourceforge.net/> or
    from Cygwin.  Only **Exuberant** `ctags.exe` recognizes `-e` option (*etags mode*,
    which will create a tag file for use with the Emacs editor).
    
    Exuberant Ctags can be extended to look for other things via regular
    expression.  It is a wonderful thing!
    
    For example, I can now look for tags in my shell code to by running:
    
        etags --langdef=shell --regex-shell='/function [_[:alnum:]]+/[_[:alnum:]]+/' --language-force=shell

-   **Cscope:** C, but flexible enough to support C++ and Java, and to be used as a
    generalized &ldquo;grep database&rdquo;.

-   **ebrowse:** For C++, you will be better off with ebrowse, a program that also comes
    with Emacs.  It understands C++ much better than etags does, and provides
    similar commands (so you don&rsquo;t lose anything).  Check out the Ebrowse
    manual, which is part of the Emacs distribution, for details.

-   **GNU Idutils:** 

-   **GNU Global (gtags):** **C**, **C++**, Yacc, **Java**, and **PHP4**.
    
    Global is a lot smarter about finding tags tables, and is fairly fast to
    update.

**Emacs TAGS** and **Exuberant Ctags** don&rsquo;t do fancy stuff, e.g. keeping an index of
function references.  They record only positions where a function (or variable
etc.) is **defined**.

Recording positions where a function is **called** (finding all **symbol references**)
is the kind of thing external tools such as **GNU Global**, **GNU Idutils** and **Cscope**
can do for you, if you&rsquo;re working with a language that they support.  **Grep**
would be the fallback method.

Please also see [Comparison with Similar Tools](http://hub.opensolaris.org/bin/view/Project+opengrok/).

Once you have a good tagging system in place, many tools become enabled for your
language, such as:

-   tag decoration mode (like drawing lines over the top of function tags),
-   stickyfunc mode (which shows the current function in the header line),
-   Emacs Code Browser (See section 33.4.2), and
-   tag jumping.

    ;;** 28.3 (info "(emacs)Tags")
    
      (leuven--section "28.3 (emacs)Tags Tables")

### Creating Tags Tables<a id="sec-33-3-1" name="sec-33-3-1"></a>

First of all, you must build a `TAGS` file (which keeps the symbols from your
project, by scanning all the source and header files with the `etags` command).

For example, you can have a `TAGS` Makefile target to do this for you:

    TAGS:
            rm -f TAGS
            find $$(pwd) \( -name \*.el \
                         -o -name \*.[chCH] \
                         \) -print | /usr/bin/ctags -e -

You can create a `TAGS` file by using `M-x compile RET tags RET`.  Every time
you changes your source file, you need to regenerate the tag file.

Alternatively,

    (with-eval-after-load "etags"
    
      ;; Tags enhancements.
      (try-require 'sure-tags))

will make sure that `TAGS` file exists (and builds it if it doesn&rsquo;t), allowing
you to

-   first rebuild the `TAGS` file or
-   specify a new one

when the search fails.

### Selecting a Tags Table<a id="sec-33-3-2" name="sec-33-3-2"></a>

After this, you can select the tag file to use with the command
`M-x visit-tags-table RET`.

    ;; List of file names of tags tables to search.
    (setq tags-table-list
          '(
            "~/TAGS"
            ;; "/usr/local/lib/emacs/src/TAGS"
            ;; "/usr/share/texmf-texlive/tex/latex/TAGS"
            ))

### Finding a Tag (go to function definition)<a id="sec-33-3-3" name="sec-33-3-3"></a>

You can **go to** the first **definition** of a tag (matching your regexp) &#x2013; **according**
**to TAGS file** &#x2013; by using `M-x find-tag` (bound to `M-.`).  The default tag is the
identifier under the cursor.

To continue searching for **next definition**, use `C-u M-.`.

    (defun find-next-tag ()
      (interactive)
      (find-tag nil t))

Use `M-*` (`M-x pop-tag-mark`) to **jump back** to where you last invoked `M-.`.

XXX See <http://marmalade-repo.org/packages/jump-dls> to jump to the source of the
symbol at point using a number of possible methods, such as semantic, tags, etc.

### Select from multiple TAGS files<a id="sec-33-3-4" name="sec-33-3-4"></a>

You can search for **occurrences** of tags that match you regexp on all files in
the TAGS table, by using `M-x tags-search RET`.

To continue searching for next match (or to **pop back** to where `M-.` was last
invoked??), use &ldquo;M-,&rdquo;.

    (with-eval-after-load "etags"
    
      ;; Select from multiple tags.
      (try-require 'etags-select))
    
    (with-eval-after-load "etags-select"
    
      ;; Do a `find-tag-at-point', and display all exact matches.
      (global-set-key (kbd "M-?") #'etags-select-find-tag-at-point))

However, the default Emacs&rsquo; etag feature is not quite good. You should use etags
provided by Helm.  Instead of activating `etags-select-goto-tag`, try
`helm-etags-select`.  If the project is big, it take some time to load tag file.
But when it is done, the next search will be very fast.

See <http://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring/>.

### Find Emacs Lisp symbol definition<a id="sec-33-3-5" name="sec-33-3-5"></a>

Find the definition of the Emacs Lisp symbol near point:

-   **`C-x F`:** Find **function**.

-   **`C-x K`:** Find function **bound to key** sequence.

-   **`C-x V`:** Find **variable**.

-   **`M-x find-face-definition`:** Find **face**.

Use `C-x 4` for displaying in another window, and `C-x 5` for displaying in
another frame.

    ;; Find the definition of the Emacs Lisp function or variable near point.
    (find-function-setup-keys)

Go to source code for symbol at point:

    (with-eval-after-load "lisp-mode"
    
      (defun leuven-goto-lisp-symbol-at-point ()
        "Go to the definition of the Emacs Lisp symbol at point."
        (interactive)
        (require 'thingatpt)              ; XXX use find-tag instead?
        (let ((sym (symbol-at-point)))    ; or (find-tag-default) or (current-word)?
          (funcall (pcase sym
                     ((pred facep)           'find-face)
                     ((pred symbol-function) 'find-function)
                     (_                      'find-variable))
                   sym)))
    
      (define-key emacs-lisp-mode-map (kbd "M-.") #'leuven-goto-lisp-symbol-at-point))

## Emacs Development Environment<a id="sec-33-4" name="sec-33-4"></a>

### Collection of Emacs Development Environment Tools<a id="sec-33-4-1" name="sec-33-4-1"></a>

    ;;** 28.4 (info "(emacs)EDE")
    
      (leuven--section "28.4 Emacs Development Environment")

CEDET (integrated into Emacs 23.2) brings improvements over a plain TAGS file.
It provides:

-   **[Semantic](http://www.gnu.org/software/emacs/manual/semantic.html):** A **parser** and code analyzer which provides **smart completion**
         (&ldquo;Intellisense&rdquo;).

-   **EDE:** A **project management** system which can **generate Makefiles** to compile your
    code for you.
    
    (Look as well at Projectile!)

-   **SRecode:** A template / **code generation** system which can convert tags from semantic
    back into code.

-   **COGRE (COnnected GRaph Editor):** A **UML diagram editor** which can generate code from a class diagram that you
    draw in Emacs.

See [supported languages](http://cedet.sourceforge.net/languagesupport.shtml).

The commands to display **symbol references** are:

-   **`C-c , g`:** `semantic-symref-symbol`.

-   **`C-c , G`:** `semantic-symref` (current tag).

    (unless (string< emacs-version "23.2")
      ;; ;; Enable global EDE (project management) features.
      ;; (global-ede-mode 1)
    
      (setq semantic-default-submodes
            '(
              ;; Turn Semantic DB mode on (Semantic parsers store the results of
              ;; parsing source code in a database file, which can be saved for
              ;; future Emacs sessions).
              global-semanticdb-minor-mode
    
              ;; The idle scheduler will automatically reparse buffers in idle
              ;; time.
              global-semantic-idle-scheduler-mode ; [minimum-features]
    
              ;; Display a summary of the symbol at point in the echo area
              ;; (~ ElDoc).
              global-semantic-idle-summary-mode ; [code-helpers]
    
              ;; Display a tooltip with a list of possible completions near the
              ;; cursor.
              global-semantic-idle-completions-mode ; [gaudy-code-helpers]
    
              ;; Turn Semantic MRU Bookmarks on (keep track of the Most
              ;; Recently Used tags).
              global-semantic-mru-bookmark-mode
    
              ;; Enable Semantic-Stickyfunc mode (display a header line that shows
              ;; the declaration line of the function or tag).
              global-semantic-stickyfunc-mode ; [gaudy-code-helpers]
    
              ;; Enable Semantic-Highlight-Func mode.
              global-semantic-highlight-func-mode ; [excessive-code-helpers]
    
              ;; Turn on all active decorations.
              global-semantic-decoration-mode ; [gaudy-code-helpers]
              ))
    
      ;; XXX If prog-mode, then Semantic will be launched after Emacs init, as
      ;; the scratch buffer is in Emacs Lisp...
      (add-hook 'java-mode-hook #'semantic-mode)
                                          ; Enable parser features (Semantic mode)
                                          ; and install a `Development' menu on
                                          ; the menu-bar.
    
      ;; ;; Smart completion, and display of information for tags & classes.
      ;; (require 'semantic/ia)
      ;;
      ;; (require 'semantic/db)
    
      (with-eval-after-load "semantic"
    
        (defun leuven--semantic ()
          ;; Automatically complete whatever symbol you are typing.
          (local-set-key
            (kbd "C-c ?") #'semantic-ia-complete-symbol) ; Better binding: `M-/'?
    
          ;; Jump to the definition of the symbol under cursor.
          (local-set-key
            (kbd "C-c j") #'semantic-ia-fast-jump) ; Where a symbol is declared.
    
          ;; Show the documentation of the symbol under cursor.
          (local-set-key
            (kbd "C-c q") #'semantic-ia-show-doc) ; Show javadoc of the right method.
    
          ;; Show a summary about the symbol under cursor.
          (local-set-key
            (kbd "C-c s") #'semantic-ia-show-summary)
    
    
          ;; Show possible public member functions or data members (when at `.'
          ;; or `->' after an object name).
          (local-set-key
            (kbd "C-c >") #'semantic-complete-analyze-inline)
    
          ;; Toggle between the implementation and a prototype of symbol under
          ;; cursor.
          (local-set-key
            (kbd "C-c p") #'semantic-analyze-proto-impl-toggle)
    
          ;; Visit the header file under cursor.
          (local-set-key
            (kbd "C-c =") #'semantic-decoration-include-visit)
    
    
          ;; Unfold the block under cursor.
          (local-set-key
            (kbd "C-c +") #'semantic-tag-folding-show-block)
    
          ;; Fold the block under cursor.
          (local-set-key
            (kbd "C-c -") #'semantic-tag-folding-fold-block)
    
          ;; C-c C-c is not a prefix key!
          ;; ;; Unfold all.
          ;; (local-set-key
          ;;   (kbd "C-c C-c +") #'semantic-tag-folding-show-all)
          ;;
          ;; ;; Fold all.
          ;; (local-set-key
          ;;   (kbd "C-c C-c -") #'semantic-tag-folding-fold-all)
          )
    
        (add-hook 'prog-mode-hook #'leuven--semantic)
    
        (defun leuven--c-mode-semantic ()
          "Completion on `.' or `->'."
          (local-set-key (kbd ".") #'semantic-complete-self-insert)
          (local-set-key (kbd ">") #'semantic-complete-self-insert)
          (local-set-key (kbd "C-c C-r") #'semantic-symref))
    
        (add-hook 'c-mode-common-hook #'leuven--c-mode-semantic))
                                          ; Note that this will apply to all
                                          ; cc-modes, e.g. c-mode, c++-mode,
                                          ; php-mode, csharp-mode, awk-mode.
    
      ;; Hooks, specific for Semantic.
      (defun leuven--semantic-imenu ()
        (imenu-add-to-menubar "TAGS"))
    
      (add-hook 'semantic-init-hooks #'leuven--semantic-imenu)
    
      )

### Advanced Code Browsing<a id="sec-33-4-2" name="sec-33-4-2"></a>


Some people use CEDET in combination with [ECB (the Emacs Code Browser)](http://www.xemacs.org/Documentation/packages/html/ecb_2.html), which
provides views of directories and files.

Keyboard navigation:

-   **`C-c . g d`:** Go to directories.

-   **`C-c . g s`:** Go to sources.

-   **`C-c . g m`:** Go to methods.

-   **`C-c . g h`:** Go to history.

-   **`C-c . g 1`:** Go to edit buffer.

    ;; Emacs Code Browser.
    (custom-set-variables '(ecb-options-version "2.40"))
    (try-require 'ecb-autoloads)
    (with-eval-after-load "ecb-autoloads"
    
      ;; Trick for starting ECB 2.40 (with CEDET merged in Emacs since 23.2).
      (require 'semantic/analyze)
      (provide 'semantic-analyze)
      (provide 'semantic-ctxt)
      (provide 'semanticdb)
      (provide 'semanticdb-find)
      (provide 'semanticdb-mode)
      (provide 'semantic-load)
    
      (setq stack-trace-on-error t)
    
      ;; Don't show tip of the day at start time of ECB.
      (setq ecb-tip-of-the-day nil)
    
      ;; ;; Toggle activation of ECB (between `ecb-activate' and `ecb-deactivate').
      ;; (global-set-key (kbd "C-c e") #'ecb-minor-mode)
    
      ;; (global-set-key (kbd "<M-left>") #'ecb-goto-window-methods)
      ;; (global-set-key (kbd "<M-right>") #'ecb-goto-window-edit1)
      )

If you&rsquo;ve installed CEDET and ECB, EmacsAssist is worth trying out:
<http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00292.html>

It uses CEDET to provide a handy symbols browser for the current file, that
narrows down the list as you type substrings.  Tastes differ, but I for one
really like this.

    (try-require 'eassist)
    (with-eval-after-load "eassist"
    
      (defun leuven--c-mode-eassist ()
        (local-set-key (kbd "C-c t") #'eassist-switch-h-cpp)
        (local-set-key (kbd "C-x t") #'eassist-switch-h-cpp)
        ;; (local-set-key (kbd "C-c e") #'eassist-list-methods)
        )
    
      (add-hook 'c-mode-common-hook #'leuven--c-mode-eassist))
                                         ; Note that this will apply to all
                                         ; cc-modes, e.g. c-mode, c++-mode,
                                         ; php-mode, csharp-mode, awk-mode.

    )                                       ; Chapter 28 ends here.

### Refactoring<a id="sec-33-4-3" name="sec-33-4-3"></a>

    (req-package emr
      :config
      (progn (emr-initialize)
                     (define-key emacs-lisp-mode-map
                       (kbd "M-RET")
                       #'emr-show-refactor-menu)))

# Abbrevs<a id="sec-34" name="sec-34"></a>

An &ldquo;abbrev&rdquo; is a **word** which has been defined (in a fixed list) to &ldquo;expand&rdquo;
into a specified **expansion**.

    ;;* 29 (info "(emacs)Abbrevs")
    
    (leuven--chapter leuven-load-chapter-29-abbrevs "29 Abbrevs"
    
      ;; See (info "(autotype)") as well

## Controlling Abbrev Expansion<a id="sec-34-1" name="sec-34-1"></a>

Inspired by [TextMate](http://macromates.com/).  In fact, many editors support snippets: Eclipse,
Notepad++, etc.

For complex templates, I use `YASnippet`.  For example, new file templates (where
it&rsquo;s Lisp evaluation is handy), class and function templates with docblocks,
etc. I use it to reduce the repetitious parts of programming, and let me focus
on getting things done.

`YASnippet` does support mirror fields and transformations.  It also does support
multiple snippets with same name.  It will also expand snippets containing
non-word-constituent characters, which `abbrev` can&rsquo;t.  So I can&rsquo;t have `@p` expand
to `@param` with `abbrev`, but I can with `YASnippet`.

XXX Look at <https://github.com/vderyagin/dotemacs/blob/master/conf/yasnippet-configuration.el>

    ;;** 29.3 Controlling (info "(emacs)Expanding Abbrevs")
    
      (leuven--section "29.3 Controlling Expanding Abbrevs")

### Installation<a id="sec-34-1-1" name="sec-34-1-1"></a>

    ;; Yet Another Snippet extension for Emacs
    (with-eval-after-load "yasnippet-autoloads"
      (idle-require 'yasnippet))
    
    (with-eval-after-load "yasnippet"

#### Use YASnippet as a global minor mode<a id="sec-34-1-1-1" name="sec-34-1-1-1"></a>

    ;; Enable YASnippet in all buffers.
    (yas-global-mode 1)

#### Use `yas-minor-mode` on a per-buffer basis<a id="sec-34-1-1-2" name="sec-34-1-1-2"></a>

    ;; Enable YASnippet in programming modes only.
    (add-hook 'prog-mode-hook #'yas-minor-mode)

    (with-eval-after-load "diminish-autoloads"
      (diminish 'yas-minor-mode " y"))
    
    ;; (setq yas-verbosity 1)
    
    ;; Load the snippet tables.
    (yas-reload-all)

### Organizing snippets<a id="sec-34-1-2" name="sec-34-1-2"></a>

Where are the snippets?  `yas-snippet-dirs` is assumed to contain **directories of
modes** with snippets.  By default, put **your own snippets** in the directory
`~/.emacs.d/snippets`.

Load additional snippets.

    ;; Add root directories that store the snippets.
    (let ((leuven-snippets              ; Additional YASnippets.
           (concat leuven--directory "snippets"))
          (org-snippets
           (concat leuven--local-repos-directory "yasnippet-org-mode")))
    
      (when (file-directory-p org-snippets)
        (add-to-list 'yas-snippet-dirs org-snippets))
    
      (when (file-directory-p leuven-snippets)
        (add-to-list 'yas-snippet-dirs leuven-snippets)))
                                        ; The first element (inserted last) is
                                        ; always the user-created snippets
                                        ; directory.
    
    ;; Use Snippet mode for files with a `yasnippet' extension.
    (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

### Expanding snippets<a id="sec-34-1-3" name="sec-34-1-3"></a>

-   **`C-c & C-s`:** Insert snippet at point.

    ;; Bind `yas-expand' to SPC.
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "SPC") #'yas-expand)
    
    ;; Don't expand when you are typing in a string or comment.
    (add-hook 'prog-mode-hook
              (lambda ()
                (setq yas-buffer-local-condition
                      '(if (nth 8 (syntax-ppss))
                                        ; Non-nil if in a string or comment.
                           '(require-snippet-condition . force-in-comment)
                         t))))
    
    ;; UI for selecting snippet when there are multiple candidates.
    (setq yas-prompt-functions '(yas-dropdown-prompt))

### Writing snippets<a id="sec-34-1-4" name="sec-34-1-4"></a>

See <http://cupfullofcode.com/snippet-expansion-with-yasnippet/>.

-   **`C-c & C-n` (`yas-new-snippet`):** Creates a new snippet, pre-filled out with the basic structure.

-   **`C-c & C-v`:** `yas-visit-snippet-file`.

### YASnippet menu<a id="sec-34-1-5" name="sec-34-1-5"></a>

After editing your snippets:

-   `M-x yas-recompile-all` to compile all snippets,

-   `M-x yas-reload-all` to reload them all.

    (global-set-key (kbd "C-c & C-r") #'yas-reload-all)

Automatically reload snippets after saving.

    ;; Automatically reload snippets after saving.
    (defun recompile-and-reload-all-snippets ()
      (interactive)
      (when (derived-mode-p 'snippet-mode)
        (yas-recompile-all)
        (yas-reload-all)
        (message "Reloaded all snippets")))
    
    (add-hook 'after-save-hook #'recompile-and-reload-all-snippets)

Display all available snippets with `M-x yas-describe-tables` (or `C-c & C-l` in
Emacs Leuven):

    (global-set-key (kbd "C-c & C-l") #'yas-describe-tables)

See <http://stackoverflow.com/questions/10155181/display-all-snippets-of-yasnippet>.

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
    
    (global-set-key [mouse-3] #'lawlist-popup-context-menu)

### Frequently Asked Questions<a id="sec-34-1-6" name="sec-34-1-6"></a>

#### Why is there an extra newline?<a id="sec-34-1-6-1" name="sec-34-1-6-1"></a>

    (add-hook 'snippet-mode-hook
              (lambda ()
                (setq require-final-newline nil)))

#### Why doesn&rsquo;t TAB expand a snippet?<a id="sec-34-1-6-2" name="sec-34-1-6-2"></a>

1.  Check the mode line to see if there&rsquo;s `yas`. If not, then try `M-x
       yas-minor-mode` to manually turn on the minor mode and try to expand the
    snippet again.
    
    If it works, then, you can add the following code to your `.emacs` before
    loading YASnippet:
    
        (add-hook 'the-major-mode-hook #'yas-minor-mode-on)
    
    where `the-major-mode` is the major mode in which `yas-minor-mode` isn&rsquo;t enabled
    by default.
    
    You can also use the command `M-x yas-global-mode` to turn on YASnippet
    automatically for all major modes.

2.  If `yas-minor-mode` is on but the snippet still not expanded, then try to see
    what command is bound to the `TAB` key: press `C-h k` and then press `TAB`. Emacs
    will show you the result.
    
    You&rsquo;ll see a buffer prompted by Emacs saying that `TAB` runs the command &ldquo;&#x2026;&rdquo;.
    Alternatively, you might see `TAB` runs the command &ldquo;&#x2026;&rdquo;. Note the
    difference between `TAB` and `TAB` where the latter has priority.
    
    If you see `TAB` bound to a command other than `yas-expand` (e.g. in org-mode),
    you can try a work around. See
    <https://github.com/capitaomorte/yasnippet/blob/master/doc/faq.org>.

#### yas-key-syntaxes<a id="sec-34-1-6-3" name="sec-34-1-6-3"></a>

`yas-key-syntaxes` is the variable that allows you control the behavior &ldquo;don&rsquo;t
expand words finishing with the snippet key&rdquo;, even if:

-   the default syntax table for the major mode you are working on says that &ldquo;C-s&rdquo;
    (for example) is two separate words, and

-   assuming you don&rsquo;t want to fix the major mode&rsquo;s behavior.

Find it at <http://capitaomorte.github.io/yasnippet/snippet-reference.html#yas-key-syntaxes>

    ;; ;; Make the "yas/minor-mode"'s expansion behavior to take input word
    ;; ;; including hyphen.
    ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
                                        ; [default:
                                        ; '("w" "w_" "w_." "w_.()"
                                        ;   yas-try-key-from-whitespace)]
    
    )

### Reference<a id="sec-34-1-7" name="sec-34-1-7"></a>

    ;; Log level for `yas--message'.
    (setq yas-verbosity 2)              ; Warning.

## Dynamic Abbrev Expansion<a id="sec-34-2" name="sec-34-2"></a>


&ldquo;Dynamic abbrevs&rdquo; allow the meanings of abbreviations (expansions) to be
**determined automatically from the contents of the buffer**.  It is completely
independent of Abbrev mode.

-   `M-/` runs the command `dabbrev-expand` by default.  Expand previous word
    &ldquo;dynamically&rdquo;.  **Expands to the most recent, preceding word** for which this is
    a prefix.

-   `C-M-/` runs the command `dabbrev-completion`.  Completion on current word.  Like
    `M-/` but finds all expansions in the current buffer and **presents suggestions
    for completion**.

See completion for symbol names (See section 31.8).

    ;;** 29.7 (info "(emacs)Dabbrev Customization")
    
      (leuven--section "29.7 Dabbrev Customization")
    
      ;; (with-eval-after-load "dabbrev"
      ;;
      ;;   ;; Preserve case when expanding the abbreviation.
      ;;   (setq dabbrev-case-replace nil))

However, there are 3 amazing &ldquo;intelligent guessing completion frameworks&rdquo; in
Emacs:

-   &ldquo;Hippie&rdquo; expansion,
-   Auto-Complete and Company.

Hippe-expand is great (can complete whole code blocks for you, making it possible
to e.g. cycle through all if-tests in your projects).

But Auto-Complete and Company mode are even better. They&rsquo;ll give you a nice
IntelliSense style menu, will &ldquo;learn&rdquo; as you use it, and allows you to define
a file with the completions you&rsquo;d like to be available in the various languages.

I prefer Company over Auto-Complete but YMMV.

### &ldquo;Hippie&rdquo; expansion (Autotype)<a id="sec-34-2-1" name="sec-34-2-1"></a>

Typically, **we bind `hippie-expand` to `M-/`**, with `dabbrev-expand` (the standard
binding of `M-/`) providing one of the expansion possibilities.

    ;; Expand text trying various ways to find its expansion.
    (global-set-key (kbd "M-/") #'hippie-expand) ; Built-in.

<div class="note">
Would we want to bind `hippie-expand` on TAB, we should have to write a `smart-tab`
command which would look if the major mode is Org and then act differently&#x2026;

</div>


    (with-eval-after-load "hippie-exp"
    
      ;; List of expansion functions tried (in order) by `hippie-expand'
      ;; (completion strategy).
      (setq hippie-expand-try-functions-list
            '(;; Searching the current buffer.
              try-expand-dabbrev
    
              ;; Searching visible window parts.
              try-expand-dabbrev-visible
    
              ;; ;; Searching (almost) all other buffers (see
              ;; ;; `hippie-expand-ignore-buffers').
              ;; try-expand-dabbrev-all-buffers
    
              ;; Emacs Lisp symbol, as many characters as unique.
              try-complete-lisp-symbol-partially
    
              ;; Emacs Lisp symbol.
              try-complete-lisp-symbol
    
              ;; File name, as many characters as unique.
              try-complete-file-name-partially
    
              ;; File name.
              try-complete-file-name))

Configure Hippie-expand to also expand YASnippets (on the key used by
`hippie-expand`, then).

    ;; Integrate YASnippet with `hippie-expand'.
    (with-eval-after-load "yasnippet"
    
      (add-to-list 'hippie-expand-try-functions-list
                   'yas-hippie-try-expand)))
                                        ; Makes more sense when placed at the
                                        ; top of the list.

### Auto-Complete (&ldquo;IntelliSensy&rdquo; completion when writing code)<a id="sec-34-2-2" name="sec-34-2-2"></a>

Auto-Complete shows completions as you type, so you can fill in long words by
typing only a few characters &#x2013; really brilliant!

Auto-complete uses the variable `ac-modes` to decide whether to enable
auto-completion in a particular mode; by default, `org-mode` (which interprets the
`TAB` key) is not present in this list. So, to enable auto-completion in Org
mode, simply add it to the `ac-modes`:

    (add-to-list 'ac-modes 'org-mode)

Popups:

-   **Completion menu**
    
    Candidates suffix:
    
    -   (nothing) &#x2013; means it is buffer string cache complete.
    -   `v` &#x2013; Variable
    -   `f` &#x2013; Function
    -   `s` &#x2013; Symbol
    -   `c` &#x2013; Constant
    -   `a` &#x2013; Abbrev, yasnippet
    -   `d` &#x2013; Dictionary, environment variable (e.g. LC\_CTYPE)
    -   `m` &#x2013; Module

-   **Quick help** (tooltip appearing at the side of the completion menu)

According to the default settings, completion is done by `TAB` or `RET`:

-   **`ac-expand` (`TAB`):** Select candidates in cycle.

-   **`ac-complete` (`RET`):** Complete a selected candidate immediately.

There are other ways to select candidates:
-   `M-1` to select candidate 1,
-   `M-2` to select candidate 2,
-   and so on (until 9).

Other operations are enabled temporarily **once the completion is started**:

-   **`C-n` or `C-p`:** Proceed to the **next match** or to the **previous match**.

-   **`C-s` (`ac-isearch`):** **Filter** completion candidates.

-   **`<C-down>` or `C-M-n` (`ac-quick-help-scroll-down`):** Scroll down quick help.

-   **`<C-up>` or `C-M-p` (`ac-quick-help-scroll-up`):** Scroll up quick help.

-   **`<f1>` (or `C-?`, `ac-help`):** Show additional **help** (for functions, arguments and objects) in **temporary
    buffer**.
    
    Use `C-M-v` (or `C-M-S-v`) to scroll down (or up).

-   **`<M-f1>` (or `C-M-?`, `ac-persist-help`):** Show additional **help** in **persistent buffer**.

Eventually, see [How to make auto-complete work with yasnippet and abbrev?](http://stackoverflow.com/questions/19900949/how-to-make-auto-complete-work-with-yasnippet-and-abbrev)

    ;; Auto Completion.
    (with-eval-after-load "auto-complete-autoloads-XXX"
      (idle-require 'auto-complete-config)
    
      (global-set-key (kbd "C-/") #'auto-complete))
    
    (with-eval-after-load "auto-complete-config"
    
      ;; 6.1 Set a list of sources to use (by default + for some major modes)
      (ac-config-default))                ; ... and enable Auto-Complete mode in
                                          ; all buffers.
    
    (with-eval-after-load "auto-complete"
                                          ; Required by ESS.
    
      ;; ;; 5.4 Completion will be started automatically by inserting 2 characters.
      ;; (setq ac-auto-start 2)              ; Also applies on arguments after opening
      ;;                                     ; parenthesis in ESS.
    
      ;; 7.5 Use `C-n/C-p' to select candidates (only when completion menu is
      ;; displayed).
      (setq ac-use-menu-map t)
      (define-key ac-menu-map (kbd "C-n") #'ac-next)
      (define-key ac-menu-map (kbd "C-p") #'ac-previous)
    
      ;; Unbind some keys (inconvenient in Comint buffers).
      (define-key ac-completing-map (kbd "M-n") nil)
      (define-key ac-completing-map (kbd "M-p") nil)
    
      ;; Add other modes into `ac-modes'.
      (setq ac-modes
            (append ac-modes
                    '(change-log-mode
                      latex-mode
                      org-mode
                      prog-mode           ; Programming modes.
                      snippet-mode
                      sql-mode
                      text-mode)))
    
      ;; 7.9 Just ignore case.
      (setq ac-ignore-case t)
    
      ;; 8.1 Delay to completions will be available.
      (setq ac-delay 0)                   ; Faster than default 0.1.
    
      ;; 8.2 Completion menu will be automatically shown.
      (setq ac-auto-show-menu 0.2)        ; [Default: 0.8].
    
      ;; 8.13 Delay to show quick help.
      (setq ac-quick-help-delay 0.5)
    
      ;; 8.15 Max height of quick help.
      (setq ac-quick-help-height 10)      ; Same as `ac-menu-height'.
    
      ;; 8.16 Limit on number of candidates.
      (setq ac-candidate-limit 100)
    
      ;; (setq ac-disable-inline t)
      ;; (setq ac-candidate-menu-min 0)
    
      ;; Completion by TAB.
      (define-key ac-completing-map (kbd "<tab>") #'ac-complete)
    
      ;; Completion by RET.
      (define-key ac-completing-map (kbd "<RET>") #'ac-complete)
    
      ;; Abort.
      (define-key ac-completing-map (kbd "C-g") #'ac-stop)
      (define-key ac-completing-map (kbd "<left>") #'ac-stop)
    
      ;; 11.1 Avoid Flyspell processes when auto completion is being started.
      (ac-flyspell-workaround))

XXX Test the following.

    ;; Enable the drop down auto complete inside comment lines.
    (setq ac-disable-faces nil)

### Company mode (&ldquo;Complete anything&rdquo;)<a id="sec-34-2-3" name="sec-34-2-3"></a>

The best alternative to Auto-Complete seems to be Company mode.

Besides that the maintainer of company-mode is incredibly competent and
friendly,

-   company-mode is super easy to extend (whereas you often have to write 3-4
    times as much code to get the equivalent functionality out of AC backend);

-   company &ldquo;just works&rdquo;, most of the time.  It&rsquo;s incredibly simple to use.

See <https://github.com/company-mode/company-mode/issues/68> for a **feature
comparison** between both.

Completion will start automatically after you type a few letters. Use `M-n` and
`M-p` to select, `RET` to complete or `TAB` to complete the common
part. **Search through the completions** with `C-s`, `C-r` and `C-o`. Press `M-(digit)` to
quickly complete with one of the first 10 candidates.

Type `M-x company-complete` to initiate completion manually. Bind this command to
a key combination of your choice.

When the completion candidates are shown, press `<f1>` to **display the full
documentation** for the selected symbol, or `C-w` to **show its definition**. Not all
back-ends support this.

company-mode has a unique take on auto-completion where it will reject
keystrokes if it doesn&rsquo;t match any possibilities. It can also search
available completions and filter as well.

    ;; Modular text completion framework.
    (with-eval-after-load "company-autoloads"
    
      ;; Enable Company mode in all buffers ....
      (global-company-mode 1)
    
      (global-set-key (kbd "<C-tab>") #'company-complete)
      (global-set-key (kbd "C-/") #'company-complete)
    
      (global-set-key (kbd "C-/") #'helm-company) ;; ?
    
      (global-set-key (kbd "C-c y") #'company-yasnippet)
                                          ; Better than `helm-yas-complete' as
                                          ; `company-yasnippet' shows both the key
                                          ; and the replacement.
      )

-   <https://github.com/proofit404/company-tern>
-   <http://stackoverflow.com/questions/18102833/could-not-start-tern-server-in-emacs?rq=1>
-   <https://github.com/syl20bnr/spacemacs/issues/5733>
-   <https://github.com/ternjs/tern/issues/256>
-   <https://github.com/angelozerr/tern.java/wiki/Tern-Eclipse-IDE>
-   <https://github.com/angelozerr/tern.java/wiki/Getting-Started>
-   <http://emmet.io/blog/sublime-tern/>
-   <http://emacs.stackexchange.com/questions/3093/how-to-navigate-a-javascript-project>
-   <https://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring>

Installing <https://github.com/angelozerr/tern.java/wiki/Installation-Update-Site>.

<https://nodejs.org/download/release/latest/win-x64/>

    (with-eval-after-load "company-tern-autoloads"
    
      (add-to-list 'company-backends 'company-tern))

    (with-eval-after-load "company"
    
      (global-set-key (kbd "C-/") #'company-complete-common)
    
      ;; ... Except in some modes.
      (setq company-global-modes
            '(not ess-mode                ; In (i)ESS buffers, Auto-Complete is
                  inferior-ess-mode       ; enabled by default.
                  magit-status-mode
                  help-mode))

By default, the list of candidates is sorted alphabetically, unless the backend
(or we) chooses to sort them in a different way.

    ;; Sort candidates according to their occurrences.
    (setq company-transformers '(company-sort-by-occurrence))
    (setq company-transformers '(;; company-sort-by-statistics ;; unknown
                                 company-sort-by-backend-importance))
    
    ;; Minimum prefix length for idle completion.
    (setq company-minimum-prefix-length 2)
    
    ;; Start completion immediately.
    (setq company-idle-delay 0)
    
    ;; Show quick-access numbers for the first ten candidates.
    (setq company-show-numbers t)
    
    ;; Selecting item before first or after last wraps around.
    (setq company-selection-wrap-around t)
    
    ;; Use `C-n/C-p' to select candidates (only when completion menu is
    ;; displayed).
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    
    ;; Unbind some keys (inconvenient in Comint buffers).
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    
    ;; Completion by TAB.
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection) ; Complete with the selected candidate
                                        ; XXX `company-complete'???
    
    ;; Temporarily show the documentation buffer for the selection.
    (define-key company-active-map (kbd "<f1>") #'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-?") #'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-c C-d") #'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-?") #'company-show-doc-buffer)
    
    ;;! Temporarily display a buffer showing the selected candidate in context.
    (define-key company-active-map (kbd "M-.") #'company-show-location)
    
    ;; Abort.
    (define-key company-active-map (kbd "C-g") #'company-abort)
    (define-key company-active-map (kbd "<left>") #'company-abort)

    ;; Add support for keypad events (`<kp-numbers>' without the modifier).
    (eval-after-load 'company
      '(dotimes (i 10)
         (define-key company-active-map
           (read-kbd-macro (format "<kp-%d>" i))
           #'company-complete-number)))
    
    ;; Do nothing if the indicated candidate contains digits (actually, it will
    ;; try to insert the digit you type).
    (advice-add
     'company-complete-number :around
     (lambda (fun n)
       (let ((cand (nth (+ (1- n) company-tooltip-offset)
                        company-candidates)))
         (if (string-match-p "[0-9]" cand)
             (let ((last-command-event (+ ?0 n)))
               (self-insert-command 1))
           (funcall fun n))))
     '((name . "Don't complete numbers")))

If you press `<right>` when the menu is displayed, it will cancel completion.

**First** run the **indent** function, then try **completion** only if it didn&rsquo;t change
anything.

XXX The following does not work in Org files (TAB does not expand nodes anymore)!

    ;; See `tab-always-indent'.
    
    (defun leuven-indent-or-complete ()
      "Indent the current line; if point doesn't move, then try to complete."
      (interactive)
      (let ((p (point)))
        ;; (if (minibufferp)
        ;;     (minibuffer-complete)
        (call-interactively 'indent-for-tab-command)
        (when (and (= p (point))
                   (not (bolp))
                   (looking-at "\\_>"))
          (call-interactively 'company-complete-selection))))
    
    (define-key company-mode-map (kbd "<tab>") #'leuven-indent-or-complete)
    
    ;; (defun leuven--tab-fix ()
    ;;   (local-set-key (kbd "<tab>") #'leuven-indent-or-complete))
    ;;
    ;; (add-hook 'prog-mode-hook #'leuven--tab-fix)

Only trigger the tooltip on manual invocation, but still have the preview (of
the first candidate) show up immediately:

    (defadvice company-pseudo-tooltip-unless-just-one-frontend
               (around only-show-tooltip-when-invoked activate)
      (when (company-explicit-action-p)
        ad-do-it))

From <https://github.com/company-mode/company-mode/issues/87>.

See also <https://github.com/company-mode/company-mode/issues/123>.

    )

With the following text (where &ldquo;|&rdquo; stands for the cursor):

    GOODBYE
    good|

-   the prefix &ldquo;good&rdquo; will match no word (no completion to &ldquo;goodBYE&rdquo;!) while
-   the prefix &ldquo;GOOD&rdquo; will match &ldquo;GOODBYE&rdquo; only.

In &ldquo;code&rdquo; modes (see `company-dabbrev-code-modes`) and their derivatives, Company
will &#x2013; by default &#x2013; complete only symbols, not text in comments or strings.

    ;; Dabbrev-like company-mode back-end for code.
    (with-eval-after-load "company-dabbrev-code"
    
      ;; ;; Search all other buffers
      ;; (setq company-dabbrev-code-other-buffers 'all)
    
      ;; Offer completions in comments and strings.
      (setq company-dabbrev-code-everywhere t)
    
      ;; ;; Ignore case when collecting completion candidates.
      ;; (setq company-dabbrev-code-ignore-case t)
      )

In the other modes, Company will pass control to other back-ends
(e.g. `company-dabbrev`).

    ;; Dabbrev-like company-mode completion back-end.
    (with-eval-after-load "company-dabbrev"
    
      ;; Only search in the current buffer
      (setq company-dabbrev-other-buffers nil) ; Prevent Company completing
                                               ; numbers coming from other files
    
      ;; Don't ignore case when collecting completion candidates.
      (setq company-dabbrev-ignore-case nil)
    
      ;; Don't downcase the returned candidates.
      (setq company-dabbrev-downcase nil)
    
      ;; Skip invisible text (Org drawers, etc.).
      (setq company-dabbrev-ignore-invisible t))

XXX What about `company-search-candidates` vs `company-filter-candidates`?

#### Company-quickhelp<a id="sec-34-2-3-1" name="sec-34-2-3-1"></a>

      (with-eval-after-load "company-quickhelp-autoloads-XXX"
    
        ;; Enable `company-quickhelp-mode'.
        (company-quickhelp-mode 1)
    
        ;; ;; Delay to show quick help.
        ;; (setq company-quickhelp-delay 0.5)
    
        ;; Maximum number of lines to show in the popup.
        (setq company-quickhelp-max-lines 10))
    
    )                                       ; Chapter 29 ends here.

# Dired, the Directory Editor<a id="sec-35" name="sec-35"></a>

    ;;* 30 (info "(emacs)Dired"), the Directory Editor
    
    (leuven--chapter leuven-load-chapter-30-dired "30 Dired, the Directory Editor"

## Entering Dired<a id="sec-35-1" name="sec-35-1"></a>

    ;;** (info "(emacs)Dired Enter")
    
      ;; Directory-browsing commands.
      (with-eval-after-load "dired"
    
        (leuven--section "30.1 (emacs)Dired Enter")

<div class="note">
ls-lisp (See section 35.2), Emacs&rsquo;s own **emulation of `ls`** is used by **default on MS Windows** (which
does not have an `ls` program).  Though, it does not support as many options as
GNU `ls`.

</div>

    ;; Switches passed to `ls' for Dired.
    (setq dired-listing-switches "-alF")

## Emulation of `ls` on MS-Windows<a id="sec-35-2" name="sec-35-2"></a>


Use Emacs&rsquo;s own emulation of `ls` in all versions of Emacs.

    ;;** (info "(emacs)ls in Lisp")
    
        (leuven--section "G.4 (emacs)ls in Lisp")
    
        ;; Emulate insert-directory completely in Emacs Lisp.
        (when (require 'ls-lisp)
    
          ;; Disable the case sensitive sort of file names.
          (setq ls-lisp-ignore-case t)
    
          ;; Sort directories first.
          (setq ls-lisp-dirs-first t)
    
          ;; Use `ls-lisp' in all versions of Emacs (for Dired sorting to work OK!).
          (setq ls-lisp-use-insert-directory-program nil)
                                            ; [Default: nil for Windows, t otherwise]
    
          ;; Use ISO 8601 dates.
          (setq ls-lisp-format-time-list
                '("%Y-%m-%d %H:%M"
                  "%Y-%m-%d %H:%M"))
    
          ;; Use localized date/time format.
          (setq ls-lisp-use-localized-time-format t))

## Navigation in the Dired Buffer<a id="sec-35-3" name="sec-35-3"></a>

In Dired, `M->` and `M-<` never took me where I wanted to go.  Now, they do.

    ;;** (info "(emacs)Dired Navigation")
    
        (leuven--section "30.2 (emacs)Dired Navigation")
    
        (defun dired-back-to-top ()
          (interactive)
          (beginning-of-buffer)
          (dired-next-line 4))
    
        (define-key dired-mode-map
          (vector 'remap 'beginning-of-buffer) #'dired-back-to-top)
    
        (defun dired-jump-to-bottom ()
          (interactive)
          (end-of-buffer)
          (dired-next-line -1))
    
        (define-key dired-mode-map
          (vector 'remap 'end-of-buffer) #'dired-jump-to-bottom)

## Deleting Files with Dired<a id="sec-35-4" name="sec-35-4"></a>

    ;;** (info "(emacs)Dired Deletion")
    
        (leuven--section "30.3 (emacs)Dired Deletion")
    
        ;; Recursive deletes allowed, after asking for each directory at top level.
        (setq dired-recursive-deletes 'top)

## Visiting Files in Dired<a id="sec-35-5" name="sec-35-5"></a>

-   **`^`:** `dired-up-directory`.

-   **`o`:** Open the file or directory in **another window**.

-   **`C-o`:** Open the file but stay on Dired buffer.

    ;;** (info "(emacs)Dired Visiting")
    
        (leuven--section "30.5 (emacs)Dired Visiting")

### Open file with default tool (open directory with Windows Explorer)<a id="sec-35-5-1" name="sec-35-5-1"></a>

On top of the traditional ways, there are also these add-ons which open files
using external programs (such as PDF viewers), based on their extension.

One advantage is that using the traditional `!` (or `X`) switch with Dired locks up
Emacs until you close the other program.  These add-ons do not and leave Emacs
free for continued used.

<div class="note">
Alternative

Nowadays, there&rsquo;s at least the built-in `dired-do-async-shell-command`, bound to
`&`.

In addition to what others have mentioned, if you are on MS Windows then, after
loading `w32-browser.el` and `dired+.el`, you can use item `Open Associated Windows
App` in the `Single` menu to open a file with the associated program.

Same thing for the marked files, using `Oepn Associated Windows Apps` in menus
`Multiple` and `Multiple > Marked Here and Below`.

And you can use item `Single > ~Open in Windows Explorer` to open a file or folder
in Windows Explorer.

You can also right-click on a file and pick `Open Associated Windows App` or `Open
in Windows Explorer`.

There are key bindings for these as wel:

-   **`M-RET` or `mouse-2`:** open with associated proram
-   **`C-RET`:** open in Explorer

</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Read <http://www.piprime.fr/1302/plus-qu-un-explorateur-de-fichiers-le-mode-dired-d-emacs/></b><br  />
nil</div>

These days, there are mature programs for launching an arbitrary file with an
appropriate viewer:

-   `xdg-open` on GNU systems,

-   `open` on Mac OS X,
    
    If `xdg-open` is not available, one can replace it with `gnome-open` or `kde-open`.

-   `start` on MS Windows.
    
    Though, it is a bad idea to use `start` on Windows: it has too many quirks
    regarding quoting of its command line.  Instead, just invoke the existing
    primitive `w32-shell-execute` with the `open` verb.  As a nice side effect, this
    will support Unicode file names, something that is currently impossible when
    you invoke an external command.

    (defun helm-open-file-with-default-tool (file)
      "Open FILE with the default tool on this platform."
      (let ((program (cond (leuven--linux-p "xdg-open")
                           (leuven--mac-p "open")))
            process-connect-type)
        (if leuven--win32-p
            (helm-w32-shell-execute-open-file file)
          (start-process "helm-open-file-with-default-tool" nil program file))))

    (defun dired-open ()
      (interactive)
      (let ((program (cond (leuven--linux-p "xdg-open")
                           (leuven--mac-p "open"))))
        (dired-do-shell-command program nil
                                (dired-get-marked-files t current-prefix-arg))))
    
    (define-key dired-mode-map (kbd "F") #'dired-open)

#### Ask a WWW browser to display file<a id="sec-35-5-1-1" name="sec-35-5-1-1"></a>

Just ask a WWW browser to display the current file.

    ;; In Dired, ask a WWW browser to display the file named on this line.
    (define-key dired-mode-map (kbd "e") #'browse-url-of-dired-file) ; <C-RET>?

It works for files as well as for directories.

It opens the Windows Explorer for:

-   the **current directory** when invoked on `.`, and
-   the **parent directory** when invoked on `..`.
    
    <div class="inlinetask">
    <b><span class="todo TODO">TODO</span> Mapcar on the browse-url function</b><br  />
    See code of `w32-dired-open-files-externally`.
    </div>
    
    <div class="inlinetask">
    <b><span class="todo TODO">TODO</span> `C-u RET` opens dired on Org link</b><br  />
    We should probably have the same type of key binding for opening Explorer from
    Dired.
    </div>

#### MANY FILES at once (for Windows)<a id="sec-35-5-1-2" name="sec-35-5-1-2"></a>

For those under Windows:

    ;; Open files using Windows associations.
    (when (or leuven--win32-p
              leuven--cygwin-p)
      (defun w32-dired-open-files-externally (&optional arg)
        "In Dired, open the marked files (or directories) with the default
      Windows tool."
        (interactive "P")
        (mapcar
         (lambda (file)
           (w32-shell-execute "open" (convert-standard-filename file)))
         (dired-get-marked-files nil arg)))
    
      ;; Bind it to `E' in Dired mode.
      (define-key dired-mode-map (kbd "E") #'w32-dired-open-files-externally))

### Open file with `eww`<a id="sec-35-5-2" name="sec-35-5-2"></a>

    ;; Open current file with eww.
    (defun dired-open-with-eww ()
      "In Dired, visit (with eww) the file named on this line."
      (interactive)
      (eww-open-file (file-name-sans-versions (dired-get-filename) t)))
    
    ;; Add a binding "W" -> `dired-open-with-eww' to Dired.
    (define-key dired-mode-map (kbd "W") 'dired-open-with-eww)

## Operating on Files<a id="sec-35-6" name="sec-35-6"></a>

    ;;** (info "(emacs)Operating on Files")
    
        (leuven--section "30.7 (emacs)Operating on Files")
    
        ;; Try to guess a default target directory (if there is a Dired buffer
        ;; displayed in the next window).
        (setq dired-dwim-target t)
    
        ;; Copy recursively without asking.
        (setq dired-recursive-copies 'always)

## Updating the Dired Buffer<a id="sec-35-7" name="sec-35-7"></a>

<div class="warning">
The following code block is currently disabled, because it causes problems with
Org and PDF LaTeX: when we&rsquo;ve opened the TeX file, to compile it from LaTeX
because an error has been reported (with no details), then the TeX file blocks
the next automatic exports (`C-u C-c C-e`), asking for confirmation because the
TeX buffer is open&#x2026;

    Reverting buffer `prestations/'.
    prestations-2013-06.tex changed on disk; really edit the buffer? (y, n, r or C-h) n

</div>

    (add-hook 'dired-mode-hook
              (lambda ()
    
                ;; Auto-refresh Dired on file change.
                (auto-revert-mode)
                (setq-default auto-revert-interval 1)))

Maybe the following fixes this above problem?

    ;;** (info "(emacs)Dired Updating")
    
        (leuven--section "30.15 (emacs)Dired Updating")
    
        ;; Automatically revert Dired buffer *on revisiting*.
        (setq dired-auto-revert-buffer t)

Press `s` then `s`, `x`, `t`, `n` or `d` to sort by Size, eXtension, **Time**, **Name** or name
grouping Dirs first:

    ;; Dired sort.
    (try-require 'dired-sort-map)

## Dired and `find`<a id="sec-35-8" name="sec-35-8"></a>

For searches in Dired, see `dired-do-search` (`A`).
-   Search through all **marked files** for a match for regexp
-   Stops when a match is found
-   To continue searching for next match, use command &ldquo;M-,&rdquo;

    ;;** (info "(emacs)Dired and Find")
    
        (leuven--section "30.16 (emacs)Dired and Find")

    ;; ;; What to use in place of `-ls' as the final argument.
    ;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
    ;; ;; Quicker to collate the matches and then use `xargs' to run the command
    ;; ;; (variable defined in `find-dired.el').

    ;; Search for files with names matching a wild card pattern and Dired the
    ;; output.
    (global-set-key (kbd "C-c 1") #'find-name-dired)
                                        ; Case insensitive if
                                        ; `read-file-name-completion-ignore-case'
                                        ; is non-nil.
    
    ;; `find-grep-dired' case insensitivity.
    (setq find-grep-options "-i -q")
    
    ;; Search for files with contents matching a wild card pattern and Dired the
    ;; output.
    (global-set-key (kbd "C-c 2") #'find-grep-dired)

## Editing the Dired Buffer<a id="sec-35-9" name="sec-35-9"></a>

`Wdired` mode is **great for renaming (a lot of) files** in a directory, as it allows
**editing the Dired buffer** like a text file, using all the power of Emacs.  That
is, one can use keyboard macros, search and replace, rectangle mode (great for
adding prefixes to file names), flip mode bits with the mouse, etc.!

To enter it, type `C-x C-q`.

    ;;** (info "(emacs)Wdired")
    
        (leuven--section "30.17 Editing the (emacs)Wdired Buffer")
    
        ;; Put a Dired buffer in a mode in which filenames are editable.
        (with-eval-after-load "wdired"
    
          ;; Permissions bits of the files are editable.
          (setq wdired-allow-to-change-permissions t))

## Viewing Image Thumbnails in Dired<a id="sec-35-10" name="sec-35-10"></a>

`M-x image-dired` and choose a directory with images.

Prerequisites:
-   [ImageMagick](http://www.imagemagick.org.) package
-   JpegTRAN program
-   [exiftool](http://www.sno.phy.queensu.ca/~phil/exiftool/) to **edit comments and tags** for each image.

Key bindings:

-   **C-t d:** `image-dired-display-thumbs` (on marked files).

-   **C-t e:** `image-dired-dired-edit-comment-and-tags`.

For more information, see:
-   <http://www.emacswiki.org/Tumme>
-   <http://wikemacs.org/wiki/Image-dired>

    ;;** (info "(emacs)Image-Dired")
    
        (leuven--section "30.18 Viewing Image Thumbnails in Dired")
    
        ;; Use Dired to browse and manipulate your images.
        (with-eval-after-load "image-dired"
    
          ;; Maximum number of files to show before warning the user.
          (setq image-dired-show-all-from-dir-max-files 100)
    
          ;; Size of button-like border around thumbnails.
          (setq image-dired-thumb-relief 0)
    
          ;; Size of the margin around thumbnails.
          (setq image-dired-thumb-margin 4))

## Other Dired features<a id="sec-35-11" name="sec-35-11"></a>

To copy the **name** of the file at point (or the folder you&rsquo;re looking at in Dired)
into the kill ring (in order to make use of it elsewhere), use
`dired-copy-filename-as-kill`, which is bound to `w` in Dired.

<div class="tip">
Use `0 w` to put the **full path**.

</div>

## Dired &ldquo;extra&rdquo; features<a id="sec-35-12" name="sec-35-12"></a>

Extra Dired functionality:

-   You can **jump to the Dired buffer** corresponding to the current buffer by
    pressing **`C-x C-j`** (`dired-jump`).  If in Dired already, pop up a level and goto
    old directory&rsquo;s line.
    
    **Standard** alternative: `C-x d RET`, though it doesn&rsquo;t set point on the right
    file.

-   `dired-x` also has a feature to &ldquo;guess&rdquo; the right shell command and the right
    external viewer for documents (see `dired-guess-shell-alist-user`)

    ;;** Dired Extra
    
        (leuven--section "30.XX (dired-x)Top")
    
        (require 'dired-x))                 ; with-eval-after-load "dired" ends here.

## Dired+<a id="sec-35-13" name="sec-35-13"></a>

Extensions to Dired:

-   Provide fancy **highlighting**.

-   Let you act on sets of marked files in subdirs, recursively, for example.

There are global key bindings that Dired+ makes:

-   **`C-x D`:** `diredp-dired-union`
-   **`C-x E`:** `diredp-add-to-dired-buffer`
-   **`C-x C-M-f`:** `diredp-fileset`
-   **`C-x R`:** `diredp-dired-recent-dirs`

These are not bindings for Dired mode.  They are global bindings that set up
a Dired buffer.  They are thus similar to the default global bindings `C-x d` and
`C-x C-j`.

    ;;** Dired+
    
      (leuven--section "30.XX Dired+")
    
      (when (try-require 'dired+)
    
        ;; Don't hide details in Dired.
        (setq diredp-hide-details-initially-flag nil)
    
        ;; Don't display the next Dired buffer the same way as the last.
        (setq diredp-hide-details-propagate-flag nil)
    
        ;; Don't wrap "next" command around to buffer beginning.
        (setq diredp-wrap-around-flag nil)
    
        ;; Dired `find-file' commands reuse directories.
        (diredp-toggle-find-file-reuse-dir 1)
    
        ;; Up, reusing Dired buffers.
        (define-key dired-mode-map (kbd "C-x C-j")
          #'diredp-up-directory-reuse-dir-buffer))

## VC diff highlighting<a id="sec-35-14" name="sec-35-14"></a>

    ;;** Diff-hl
    
      (leuven--section "30.XX Diff-hl")
    
      ;; Enable VC diff highlighting on the side of a Dired window.
      (with-eval-after-load "diff-hl-autoloads"
        (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

    )                                       ; Chapter 30 ends here.

# The Calendar and the Diary<a id="sec-36" name="sec-36"></a>

    ;;* 31 The (info "(emacs)Calendar/Diary")
    
    (leuven--chapter leuven-load-chapter-31-calendar-diary "31 The Calendar and the Diary"

## Calendar Motion<a id="sec-36-1" name="sec-36-1"></a>

    ;;** 31.1 (info "(emacs)Calendar Motion")
    
      (leuven--section "31.1 (emacs)Calendar Motion")
    
      ;; Years must be written in full.
      (setq diary-abbreviated-year-flag nil)
    
      ;; Set the style of calendar and diary dates to ISO (how to interpret the
      ;; dates).
      (setq calendar-date-style 'iso)
    
      ;; Week in the calendar begins on Monday.
      (setq calendar-week-start-day 1)
    
      ;; Mark all visible dates that have diary entries.
      (when (file-exists-p "~/diary")
        (setq calendar-mark-diary-entries-flag t))
    
      ;; Mark the current date (by changing its face) after generating a calendar,
      ;; if today's date is visible.
      (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

## Scroll Calendar<a id="sec-36-2" name="sec-36-2"></a>

    ;;** 31.2 (info "(emacs)Scroll Calendar")
    
      (leuven--section "31.2 (emacs)Scroll Calendar")
    
      ;; Fix foolish calendar-mode scrolling after loading `calendar.el'.
      (add-hook 'calendar-load-hook
                (lambda ()
                  (define-key calendar-mode-map (kbd ">") #'calendar-scroll-left)
                  (define-key calendar-mode-map (kbd "<") #'calendar-scroll-right)))

## Times of Sunrise/Sunset<a id="sec-36-3" name="sec-36-3"></a>

    ;;** 31.7 Times of (info "(emacs)Sunrise/Sunset")
    
      (leuven--section "31.7 Times of (emacs)Sunrise/Sunset")
    
      ;; ;; Calendar functions for solar events.
      ;; (with-eval-after-load "solar"
      ;;
      ;;   ;; Name of the calendar location.
      ;;   (setq calendar-location-name "Leuven, BE")
      ;;
      ;;   ;; Latitude of `calendar-location-name'.
      ;;   (setq calendar-latitude 50.88)
      ;;
      ;;   ;; Longitude of `calendar-location-name'.
      ;;   (setq calendar-longitude 4.70))

## Diary<a id="sec-36-4" name="sec-36-4"></a>

The Emacs diary keeps track of appointments or other events on a daily basis, in
conjunction with the calendar.  To use the diary feature, you must first create
a &ldquo;diary file&rdquo; containing a list of events and their dates.

But Org (See section 28) outperforms it!

## Appointments<a id="sec-36-5" name="sec-36-5"></a>

    ;;** 31.11 (info "(emacs)Appointments")
    
      (leuven--section "31.11 (emacs)Appointments")
    
      ;; Insinuate appt if `diary-file' exists.
      (if (file-readable-p "~/diary")
          (try-require 'appt)               ; Requires `diary-lib', which requires
                                            ; `diary-loaddefs'.
        (message "Appointment reminders lib `appt' not loaded (no diary file found)"))
    
      (with-eval-after-load "appt"
    
        ;; Send the first warning 60 minutes before an appointment.
        (setq appt-message-warning-time 60) ; [default: 12]
    
        ;; Warn every 15 minutes.
        (setq appt-display-interval 15)     ; [default: 3]

Send notifications using `notifications-notify` (which was added in Emacs 24).
It uses `notify-send` (if the `libnotify-bin` Ubuntu package is installed).

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> For Windows users: use \`todochicku.el&rsquo; (cross-platform?) and the snarl notifier</b><br  />
nil</div>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Diff between shell-command and call-process?  Blocking call?</b><br  />
nil</div>

    ;; Use a separate window to display appointment reminders.
    (setq appt-display-format 'window)
    
    ;; Function called to display appointment reminders *in a window*.
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

    ;; Turn appointment checking on (enable reminders).
    (when leuven-load-verbose
      (message "INFO- Enable appointment reminders..."))
    (appt-activate 1)
    (when leuven-load-verbose
      (message "INFO- Enable appointment reminders... Done"))

    ;; Enable appointment notification, several minutes beforehand.
    (add-hook 'diary-hook #'appt-make-list)

    (with-eval-after-load "org-agenda"
    
      ;; Keep your appointment list clean: if you delete an appointment from
      ;; your Org agenda file, delete the corresponding alert.
      (defadvice org-agenda-to-appt (before leuven-org-agenda-to-appt activate)
        "Clear the existing `appt-time-msg-list'."
        (setq appt-time-msg-list nil))
    
      ;; Add today's appointments (found in `org-agenda-files') each time the
      ;; agenda buffer is (re)built.
      (add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)
                                          ;! Don't use the `org-agenda-mode-hook'
                                          ;! because the Org agenda files would be
                                          ;! opened once by `org-agenda-to-appt',
                                          ;! and then killed by
                                          ;! `org-release-buffers' (because
                                          ;! `org-agenda-to-appt' closes all the
                                          ;! files it opened itself -- as they
                                          ;! weren't already opened), to be
                                          ;! finally re-opened!
    
      ;; ;; Add today's appointments (found in `org-agenda-files') each time
      ;; ;; such a file is saved.
      ;; (add-hook 'after-save-hook          ; VERY TIME CONSUMING (~ 30 s) at each save...
      ;;           (lambda ()
      ;;             (when (and (derived-mode-p 'org-mode) ; ... of an Org
      ;;                        (org-agenda-file-p)) ; agenda file...
      ;;               (org-agenda-to-appt))))
      )

    )                                   ; with-eval-after-load "appt" ends here.

## Advanced Calendar/Diary Usage<a id="sec-36-6" name="sec-36-6"></a>

    ;;** 31.15 (info "(emacs)Advanced Calendar/Diary Usage")
    
      (leuven--section "31.15 (emacs)Advanced Calendar/Diary Usage")
    
      ;; Get rid of some holidays.
      (setq holiday-general-holidays nil)   ; Too U.S.-centric holidays.
      (setq holiday-oriental-holidays nil)  ; Oriental holidays.
      (setq holiday-hebrew-holidays nil)    ; Religious holidays.
      (setq holiday-islamic-holidays nil)   ; Religious holidays.
      (setq holiday-bahai-holidays nil)     ; Baha'i holidays.
      (setq holiday-solar-holidays nil)     ; Sun-related holidays.
    
      ;; Mark dates of holidays in the calendar window.
      (setq calendar-mark-holidays-flag t)

After the calendar is loaded, `calendar-holidays` is the list of notable days for
the command `M-x holidays`.

    (defun leuven-insert-current-date (prefix)
      "Insert the current date in ISO format.
    With one PREFIX argument, add day of week.  With two PREFIX arguments, add day
    of week and time."
      (interactive "P")
      (let ((format (cond ((not prefix) "%Y-%m-%d")
                          ((equal prefix '(4)) "%Y-%m-%d %a")
                          ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
        (insert (format-time-string format))))
    
    (global-set-key (kbd "C-c .") #'leuven-insert-current-date)

## Calendar framework<a id="sec-36-7" name="sec-36-7"></a>

After displaying your `calfw` buffer, you can get a HTML buffer with `M-x
htmlfontify-buffer`.

<div class="warning">
`Calfw` requires `Google-maps`!?

</div>

    ;;* Calendar view framework on Emacs
    
      ;; Calendar view framework on Emacs.
      (with-eval-after-load "calfw"
    
        ;; Unicode characters.
        (setq cfw:fchar-junction ?╋
              cfw:fchar-vertical-line ?┃
              cfw:fchar-horizontal-line ?━
              cfw:fchar-left-junction ?┣
              cfw:fchar-right-junction ?┫
              cfw:fchar-top-junction ?┯
              cfw:fchar-top-left-corner ?┏
              cfw:fchar-top-right-corner ?┓))
    
      ;; Calendar view for org-agenda.
      (with-eval-after-load "calfw-org"
    
        ;; Remove some strings (tags and filenames) from item summary.
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
             ;; table layout.
             'display nil))))

    )                                       ; Chapter 31 ends here.

# Sending Mail<a id="sec-37" name="sec-37"></a>

Minimal configuration for sending a bug report, for example.

    ;;* 32 (info "(emacs)Sending Mail")
    
    (leuven--chapter leuven-load-chapter-32-sending-mail "32 Sending Mail"
    
      ;; Full name of this user.
      (setq user-full-name "John Doe")
    
      ;; Full mailing address of this user
      ;; (used in MAIL envelope FROM, and to select the default personality ID).
      (setq user-mail-address "johndoe@example.com")
    
      ;; Sending mail.
      (setq send-mail-function 'smtpmail-send-it)
    
      ;; Default SMTP server (overriden by `smtpmail-smtp-server').
      (setq smtpmail-default-smtp-server "smtp")
                                            ; SMTP process must be running
                                            ; there... and it should be Google's own
                                            ; mail server for GMail user mail
                                            ; addresses...
    
      ;; ;; SMTP service port number.
      ;; (setq smtpmail-smtp-service 587)
    
    )                                       ; Chapter 32 ends here.

# Gnus<a id="sec-38" name="sec-38"></a>

    ;;* 34 (info "(emacs)Gnus")
    
    (leuven--chapter leuven-load-chapter-34-gnus "34 Gnus"
    
      (global-set-key (kbd "C-c n")
        (lambda ()
          (interactive)
          (switch-or-start 'gnus "*Group*")))
    
      ;; Directory beneath which additional per-user Gnus-specific files are placed.
      (setq gnus-directory "~/.gnus.d/")    ; Should end with a directory separator.
    
      ;; A newsreader for GNU Emacs.
      (with-eval-after-load "gnus"
    
        ;; Package to compose an outgoing mail (Message, with Gnus paraphernalia).
        (setq mail-user-agent 'gnus-user-agent)
    
        ;; Reading mail with Gnus.
        (setq read-mail-command 'gnus))

## BBDB<a id="sec-38-1" name="sec-38-1"></a>

bbdb3: <https://savannah.nongnu.org/projects/bbdb>

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
    
      ;; Search the BBDB.
      (global-set-key (kbd "<C-f11>") #'bbdb)
    
      (with-eval-after-load "bbdb"
    
        ;; Coding system used for reading and writing `bbdb-file'.
        (setq bbdb-file-coding-system 'utf-8)
    
        ;; Ensure `~/.bbdb' never becomes non utf-8 again (it is defined with
        ;; `defconst', so it is reset whenever `bbdb.el' is loaded).
        (add-hook 'bbdb-load-hook
                  (lambda ()
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

### Interfaces<a id="sec-38-1-1" name="sec-38-1-1"></a>

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

### Reader-specific Features<a id="sec-38-1-2" name="sec-38-1-2"></a>

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

### Options<a id="sec-38-1-3" name="sec-38-1-3"></a>

You can add the author of a mail or posting to the BBDB by hitting `:`.

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

### Utilities<a id="sec-38-1-4" name="sec-38-1-4"></a>

BBDB 3 has a dial interface which allows you to call an external interface to
make a call.  This is done by modifying `bbdb-dial-function`.  There are examples
which interface with Skype to make a phone call.

    )                                       ; Chapter 34 ends here.

# Document Viewing<a id="sec-39" name="sec-39"></a>

If `doc-view` is not showing the PDF files as images in Emacs, it may be
because:

-   no PNG support is available in Emacs (see Images (See section 49.2)), or
-   some conversion utility for PDF files (Ghostscript) is not on your `PATH`.

In that case, `doc-view` falls back to show you the plain text contents of the PDF
if `doc-view-pdftotext-program` is on your `PATH`.

    ;;* 36 (info "(emacs)Document View")
    
    (leuven--chapter leuven-load-chapter-36-document-view "36 Document Viewing"
    
      ;; View PDF/PostScript/DVI files in Emacs.

<p class="verse">
From Tassilo Horn:<br  />
<br  />
>> <https://github.com/politza/pdf-tools> (available through MELPA).<br  />
><br  />
> Oh, that looks pretty impressive.  I haven&rsquo;t used it, though, so I<br  />
> cannot help with this.<br  />
<br  />
Ok, now I had to check it out, and Andreas, it&rsquo;s a really superb<br  />
package.  I&rsquo;m gonna use that now for reading PDF files instead of<br  />
doc-view (where I&rsquo;m the author) and Evince (which I use due to<br  />
doc-view&rsquo;s limitations).<br  />
<br  />
But Troy is correct.  The PDF he links makes emacs freeze, although I&rsquo;ve<br  />
been able to recover by hitting \`C-g&rsquo; repeatedly.  The result was that<br  />
the raw PDF data was shown.<br  />
<br  />
Before I had opened that file successfully with doc-view and then did<br  />
M-x pdf-view-mode.  That worked.  When doing it that way, it seems some<br  />
if not all of PDF Tools minor modes aren&rsquo;t active, so I guess one of<br  />
them does something that triggers the freeze.<br  />
<br  />
Andreas, do you plan to propose PDF Tools for inclusion in Emacs?  IMO,<br  />
that&rsquo;s really a package anybody&rsquo;s gonna love.  (Oh my god, how well the<br  />
syntex stuff/AUCTeX integration works!)<br  />
</p>

PDF Tools (`pdf-view-mode`) is a major mode for viewing PDFs in Emacs similar (but
much more advanced) than `doc-view-mode`.

It offers many features:
-   highlighting search,
-   highlighting selection,
-   annotate,
-   content tree.

It&rsquo;s the best PDF viewer on GNU/Linux (including the ones not running in Emacs).

    (when (and leuven--linux-p
               ;; (executable-find "epdfinfo")
               )
      (with-eval-after-load "pdf-tools-autoloads"
        (pdf-tools-install)))

## Navigation<a id="sec-39-1" name="sec-39-1"></a>

`doc-view` integrates with the usual bookmark facility.  So, simply use `C-x r m`
(`bookmark-set`) to jump back to the last page you&rsquo;ve read in a PDF document.

## Conversion<a id="sec-39-2" name="sec-39-2"></a>

In `doc-view-mode`, you can use `C-s` to do regexp search on the page images.

In addition, by pressing `C-c C-t`, you can open the **text**-only representation
(actually, the output of the command `pdftotext`) of the current doc in a new
buffer, which is more suitable for heavy manipulation of the text of the PDF.

Another option, without `doc-view`, is `! pdtotext ? - RET`.

`doc-view-enlarge` (`+`) and `doc-view-shrink` (`-`) work fine to zoom in or out.

    )                                       ; Chapter 36 ends here.

# Web Browsing<a id="sec-40" name="sec-40"></a>

## EWW (Emacs Web Browser)<a id="sec-40-1" name="sec-40-1"></a>

`EWW` is the new Web browser for Emacs.

See <https://www.gnu.org/software/emacs/manual/html_node/eww/index.html>.

`M-x eww` asks EWW to browse a URL.

-   **`&` (`eww-browse-with-external-browser`):** Open the Web page in an **external browser** (when JavaScript is needed or the
    &ldquo;design&rdquo; is just too bad).
    
    <div class="inlinetask">
    <b><span class="todo TODO">TODO</span> See package `eww-lnum` (or `ace-link`?)</b><br  />
    (define-key eww-mode-map (kbd &ldquo;f&rdquo;) #&rsquo;eww-lnum-follow)
    (define-key eww-mode-map (kbd &ldquo;F&rdquo;) #&rsquo;eww-lnum-universal)
    </div>

# Running Shell Commands from Emacs<a id="sec-41" name="sec-41"></a>

See customizations at <http://snarfed.org/why_i_run_shells_inside_emacs>.

    ;;* 38 Running (info "(emacs)Shell") Commands from Emacs
    
    (leuven--chapter leuven-load-chapter-38-shell "38 Running Shell Commands from Emacs"

    ;; Transform shell names to what they really are.
    (with-eval-after-load "sh-script"
    
      (add-to-list 'sh-alias-alist '(sh . bash)))
    
    ;; ;; Use shell from Cygwin/MinGW.
    ;; (setq shell-file-name "bash")
    ;; (setenv "SHELL" "/bin/bash")
    ;; (setq explicit-bash-args '("-i")) ; --noediting added in Emacs 24.4
    ;; (setq explicit-sh-args '("-i"))

## Single Shell<a id="sec-41-1" name="sec-41-1"></a>

`M-!` (`shell-command`) reads a line of text using the minibuffer and executes it as
a shell command, in a subshell made just for that command.

`M-|` (`shell-command-on-region`) passes the contents of the region as the standard
input to the shell command.  By prefixing it with `C-u`, it deletes the old region
and **replaces** it with the output from the shell command.

    ;;** 38.1 Single Shell
    
      (leuven--section "38.1 Single Shell")
    
      ;; Force interactive behavior (to get my handy shell aliases).
      ;; FIXME Fix for Zsh (zsh:1: command not found: shopt)
      ;; (defadvice shell-command (before leuven-shell-command activate)
      ;;   (ad-set-arg 0
      ;;               (concat "source ~/.bashrc; shopt -s -q expand_aliases;\n "
      ;;                       (ad-get-arg 0))))
    
      ;; ;; For single shell commands (= "the" reference).
      ;; (setq shell-file-name                 ; Must be in the `PATH'.
      ;;       (or (ignore-errors
      ;;             (file-name-nondirectory (or (executable-find "zsh")
      ;;                                         (executable-find "bash")
      ;;                                         (executable-find "sh"))))
      ;;           (when leuven--win32-p "cmdproxy.exe")))
      ;;
      ;; ;; Use `shell-file-name' as the default shell.
      ;; (setenv "SHELL" shell-file-name)
      ;;
      ;; ;; Switch used to have the shell execute its command line argument.
      ;; (setq shell-command-switch "-c")      ; `/c' did not work with XEmacs.
    
      ;; Quote process arguments to ensure correct parsing on Windows.
      (setq w32-quote-process-args t)
    
      ;; ;; Workaround for Cygwin when 'shell-file-name' is 'bash'.
      ;; (setq null-device "/dev/null"))

The above assignments may **NOT** be done **in an eval-after-load** as those variables
are used by the (La)TeX modes (default + AUCTeX) among others.

## Interactive Subshell<a id="sec-41-2" name="sec-41-2"></a>

To run a subshell interactively, type `M-x shell`.  This creates (or reuses)
a buffer named `*shell*`, and runs a shell subprocess with input coming from and
output going to that buffer.

    ;;** 38.2 Interactive Subshell
    
      (leuven--section "38.2 Interactive Subshell")
    
      ;; ;; For the interactive (sub)shell (and AUCTeX compilation?).
      ;; (setq explicit-shell-file-name shell-file-name)

## Shell Mode<a id="sec-41-3" name="sec-41-3"></a>



### Entering commands and fixing mistakes<a id="sec-41-3-1" name="sec-41-3-1"></a>

-   **`RET`:** **Execute** the **command (on** the **current line)**.

-   **`C-c C-u`:** **Delete input** (from the prompt) up to point (equivalent to `C-u` in Unix
    shells).

### Completion of filenames<a id="sec-41-3-2" name="sec-41-3-2"></a>

-   **`M-?`:** Display a list of possible completions for the filename at point.

### Manipulating the output from the last command<a id="sec-41-3-3" name="sec-41-3-3"></a>


-   **`C-c C-r`:** **Show output** from last command (move first line of output to top of window).

-   **`C-c C-o`:** **Delete output** (only) from last command.

### Sending signals<a id="sec-41-3-4" name="sec-41-3-4"></a>

-   **`C-c C-c`:** Interrupt current job (equivalent to `C-c` in Unix shells, `BREAK` signal).

-   **`C-c C-z`:** Suspend or stop current job (equivalent to `C-z` in Unix shells, `STOP`
         signal).

-   **`C-c C-d`:** Send `EOF` character.

    ;;** 38.3 Shell Mode
    
      (leuven--section "38.3 Shell Mode")
    
      ;; General command-interpreter-in-a-buffer stuff (Shell, SQLi, Lisp, R,
      ;; Python, ...).
      ;; (try-require 'comint)
      ;; (with-eval-after-load "comint"
    
        ;; Comint prompt is read only.
        (setq comint-prompt-read-only t)    ; Text is read-only (in ESS)?
    
        ;; No duplicates in command history.
        (setq-default comint-input-ignoredups t)
    
        ;; Input to interpreter causes windows showing the buffer to scroll
        ;; (insert at the bottom).
        (setq-default comint-scroll-to-bottom-on-input t)
    
        ;; Output to interpreter causes windows showing the buffer to scroll
        ;; (add output at the bottom).
        (setq-default comint-move-point-for-output t)
    
        ;; Maximum size in lines for Comint buffers.
        (setq comint-buffer-maximum-size (* 5 1024))
                                            ; If the function
                                            ; `comint-truncate-buffer' is added to
                                            ; `comint-output-filter-functions'.
    
        ;; Strip `^M' characters.
        (add-to-list 'process-coding-system-alist
                     '("sh" . (undecided-dos . undecided-unix))) ; `es' process.
        (add-to-list 'process-coding-system-alist
                     '("bash" . (undecided-dos . undecided-unix)))
        (add-to-list 'process-coding-system-alist
                     '("zsh" . (undecided-dos . undecided-unix)))
    
        ;; Show completion list when ambiguous.
        (setq comint-completion-autolist t)
    
        (defun leuven-comint-clear-buffer ()
          "Clear the Comint buffer."
          (interactive)
          (let ((comint-buffer-maximum-size 0))
            (comint-truncate-buffer)))
    
        (with-eval-after-load "comint"
          (define-key comint-mode-map (kbd "C-c C-k") #'leuven-comint-clear-buffer))
    
    ;; )

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Command Prompt Here in Emacs</b><br  />
See <http://tsdh.wordpress.com/category/lisp/emacs-lisp/> to quickly open a Shell
bufffer from Emacs.
</div>

## Shell Prompts<a id="sec-41-4" name="sec-41-4"></a>

    ;;** 38.4 Shell Prompts
    
      (leuven--section "38.4 Shell Prompts")
    
      ;; Regexp to match prompts in the inferior shell.
      (setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")
    
      ;; Regexp to recognize prompts in the inferior process.
      (setq comint-prompt-regexp shell-prompt-pattern)
                                            ;! only used if the variable
                                            ;! `comint-use-prompt-regexp' is non-nil

## Shell Command History<a id="sec-41-5" name="sec-41-5"></a>

    ;;** 38.5 Shell Command History
    
      (leuven--section "38.5 Shell Command History")
    
      (with-eval-after-load "comint"
    
        ;; Rejects short commands.
        (setq comint-input-filter
          #'(lambda (str)
              (and (not (string-match "\\`\\s *\\'" str))
                   (> (length str) 2))))    ; Ignore '!!' and kin.

### Shell History Ring<a id="sec-41-5-1" name="sec-41-5-1"></a>

-   **`C-p` (or `<up>`):** Fetch the next **earlier** command in the history (doesn&rsquo;t execute it).

-   **`C-n` (or `<down>`):** Fetch the next **later** command in the history (doesn&rsquo;t execute it).

-   **`M-p` (or `<C-up>`):** Fetch the next **earlier** command in the history matching the string typed so
    far (doesn&rsquo;t execute it).

-   **`M-n` (or `<C-down>`):** Fetch the next **later** command in the history matching the string typed so
    far (doesn&rsquo;t execute it).

-   **`M-r`:** Begin an incremental **regexp search of old commands**.

-   **`C-c C-l`:** Display the current buffer&rsquo;s **history** of shell commands.

    ;; Cycle backwards/forwards through input history.
    (define-key comint-mode-map
      (kbd "C-p") #'comint-previous-input) ; Shell.
    (define-key comint-mode-map
      (kbd "<up>") #'comint-previous-input) ; Shell + RStudio.
    (define-key comint-mode-map
      (kbd "C-n") #'comint-next-input)  ; Shell.
    (define-key comint-mode-map
      (kbd "<down>") #'comint-next-input) ; Shell + RStudio.
    
    ;; Search backwards/forwards through input history for match for current
    ;; input.
    (define-key comint-mode-map
      (kbd "M-p") #'comint-previous-matching-input-from-input) ; Shell.
    (define-key comint-mode-map
      (kbd "<C-up>") #'comint-previous-matching-input-from-input) ; RStudio.
    (define-key comint-mode-map
      (kbd "M-n") #'comint-next-matching-input-from-input) ; Shell.
    (define-key comint-mode-map
      (kbd "<C-down>") #'comint-next-matching-input-from-input) ; RStudio.
    
    (when (featurep 'helm-misc)
      ;; Use Helm to search `comint' history.
      (define-key comint-mode-map
        (kbd "C-c C-l") #'helm-comint-input-ring)))

### Shell History Copying<a id="sec-41-5-2" name="sec-41-5-2"></a>

Manipulating the transcript: Viewing older commands.

-   **`C-c C-p`:** Move point to **previous command prompt**.

-   **`C-c C-n`:** Move point to **next command prompt**.

## Directory Tracking<a id="sec-41-6" name="sec-41-6"></a>

    ;;** 38.6 Directory Tracking
    
      (leuven--section "38.6 Directory Tracking")

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Fix case when 2 Shell buffers are run in the same dir (same name error!)</b><br  />
apply: Buffer name \`\*shell d:/Users/fni/\*&rsquo; is in use
</div>

    (defun leuven--rename-buffer-to-curdir (&optional _string)
      "Change Shell buffer's name to current directory."
      (rename-buffer (concat "*shell " default-directory "*")))
    
    (add-hook 'shell-mode-hook
              (lambda ()
                (leuven--rename-buffer-to-curdir)
                (add-hook 'comint-output-filter-functions
                          #'leuven--rename-buffer-to-curdir nil t)))
                                          ; Local to Shell comint.

## Options<a id="sec-41-7" name="sec-41-7"></a>

    ;;** 38.7 Options
    
      (leuven--section "38.7 Options")
    
      ;; Disable command echoing.
      (setq-default comint-process-echoes t) ; for Linux (not needed for Cygwin)

### Paging in Shell<a id="sec-41-7-1" name="sec-41-7-1"></a>

If you ever tried to run a program like `git` under `M-x shell`, you will have come
across the warning &ldquo;terminal is not fully functional&rdquo; followed by unusable
behavior.  This is because `git` sends its output through a pager (probably `less`),
which requires a real terminal emulator (See section 41.8).

Setting the `PAGER` environment variable to `/bin/cat` (but only inside Emacs)
solves this problem:

    (setenv "PAGER" "/usr/bin/cat")

## Term Mode<a id="sec-41-8" name="sec-41-8"></a>


To run a subshell in a text **terminal emulator**, use `M-x term`.  This creates (or
reuses) a buffer named `*terminal*`, and runs a subshell with input coming from
your keyboard, and output going to that buffer.

The terminal emulator uses Term mode, which has two input modes.

-   In &ldquo;line mode&rdquo;, Term basically acts like Shell Mode (See section 41.3).

-   In &ldquo;char mode&rdquo;, each character is sent directly to the subshell, as terminal
    input; the sole exception is the terminal escape character, which by default
    is `C-c`.

<div class="note">


From my experience, none of the terminal emulators in Emacs (`term`, `ansi-term`,
`multi-term`) is supported under Windows.  It looks like the main reason is all of
them rely on low-level support for terminals (`stty`, etc.) which is not provided
by Windows.

Though, `term`, `ansi-term`, and `multi-term` do **work in Cygwin Emacs**.

</div>

    ;;** 38.9 Term Mode
    
      (leuven--section "38.9 Term Mode")

Emacs **Shell mode** doesn&rsquo;t support (all) terminal control codes, so `less` doesn&rsquo;t
work (&ldquo;page-at-a-time&rdquo; feature).

Shell mode buffers do not use ptys (pseudo-terminals), so programs running in
such a buffer (the shell, the programs run by the shell, etc.) can&rsquo;t perform
character-at-a-time input.  Each line is typed in full and only sent when `RET`
is pressed.  To see proof, run the `tty` command in a Shell mode buffer, and its
output will be `not a tty`.

Note that it may seem that this is not true because you can perform command-line
editing with `C-b`, `C-f`, etc.  But this is only because `shell-mode` itself
simulates the typical command-line editing features of shells like Bash, Zsh,
etc. using Emacs own internal editing capabilities.

Anyway, you can:

-   use `isearch` to search through the command output,
-   copy stuff into the kill ring or
-   use the rectangle functions.

Or you might just prefer the Emacs key bindings over the ones your shell offers.

On the other hand, **Term mode** provides a proper **terminal emulator** (though not as
good as the good old `xterm`).  You must use it for applications that are
interactive with your terminal (such as the command `top`).

<div class="note">
In Shell mode, use `M-x proced` instead of `top`.

</div>

<div class="note">
`MPlayer` commands do not work in the Emacs shell.  Use `term` for `/` and `*` to
decrease / increase volume.

</div>

You can **switch to line mode** (`C-c C-j`) when you need Term to *basically* act like
a Shell mode buffer, and move around the buffer pretty much like anywhere else
in Emacs.  Copy / kill-ring commands, `isearch`, etc. do work.

Then, you can use `C-c C-k` to **switch** back **to character mode**, the default, which
is like your normal **terminal** emulator except the escape key.

It&rsquo;s wrong to think that `M-x term` captures everything it seems (which would
defeat the purpose of running it inside Emacs):

-   In the default character mode, `C-c` is the key prefix.
-   In line mode, you don&rsquo;t need a prefix.

Try also to use `M-x ansi-term` (Emacs terminal that accepts the ANSI-terminal
escape sequences) that is much better than `term`:
-   with `M-x term`, you can run **only one shell**;
-   with `M-x ansi-term`, you can start more than one (`*ansi-term*<2>`,
    `*ansi-term*<3>`).

Note that `TERM` is different in both environments:
-   In `M-x shell`, `TERM=emacs`
-   In `M-x term`,  `TERM=eterm-color`

MultiTerm (<http://www.emacswiki.org/emacs/MultiTerm>) works much better than
shell or term.

&ldquo;Multi-term on POSIX hosts has let me switch from using screen, with one Emacs
screen and lots of shell screens; to just using Emacs, with lots of terminals
inside it.&rdquo;

    ;; Managing multiple terminal buffers in Emacs
    ;; (and fixing some troubles of `term-mode': key bindings, etc.).
    
    (with-eval-after-load "multi-term-autoloads"
    
      ;; (global-set-key (kbd "C-c t") #'multi-term-next)
      (global-set-key (kbd "C-c T") #'multi-term)) ; Create a new one.
    
    (with-eval-after-load "multi-term"
    
      (setq multi-term-program shell-file-name))

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Try *visual* commands (like `top` or `less`) in `multi-term`</b><br  />
nil</div>

    ;; ;; Run an inferior shell, with I/O through buffer `*shell*'.
    ;; (global-set-key
    ;;   (kbd "C-c !")
    ;;   (cond (leuven--win32-p 'shell)
    ;;         (t 'term)))
    
    ;; Toggle to and from the `*shell*' buffer.
    (global-set-key (kbd "C-!")
      (lambda ()
        (interactive)
        (switch-or-start 'shell "*shell*")))

## Remote Host<a id="sec-41-9" name="sec-41-9"></a>

    ;;** 38.10 Remote Host Shell
    
      (leuven--section "38.10 Remote Host Shell")
    
      ;; Load ssh.el file.
      (add-to-list 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")
      (autoload 'ssh "ssh"
        "Open a network login connection via `ssh'." t)
        ;; This is to run ESS remotely on another computer in my own Emacs, or just
        ;; plain old reading remote files.
    
      ;; See http://emacs.1067599.n5.nabble.com/SSH-inside-Emacs-td225528.html
      ;; - plink (with `dumb' terminal option?) as interactive shell
      ;; - ssh -t -t user@host
      ;; - Cygwin'ized Emacs
      ;; - MSYS (MinGW)

## Serial Terminal<a id="sec-41-10" name="sec-41-10"></a>

Since GNU Emacs 23, there&rsquo;s now support for serial port access.  The new command
`serial-term` starts an interactive terminal on a serial port.

## Helper for GNU Emacs on w32<a id="sec-41-11" name="sec-41-11"></a>

Let Emacs, for example, find the program `/usr/bin/gunzip`.

**But `cygwin-mount-activate` causes problems (Emacs crashes or unable to connect)
when reading mail with Gnus&#x2026;**

Those problems disappear as soon as we do `cygwin-mount-deactivate`&#x2026;

    ;; Let Windows Emacs recognize Cygwin paths (e.g. /usr/local/lib).
    (when (and leuven--win32-p
               (executable-find "mount")) ; Cygwin bin directory found.
    
      (with-eval-after-load "cygwin-mount-autoloads"
    
        (autoload 'cygwin-mount-activate "cygwin-mount"
          "Activate cygwin-mount- and cygwin-style-handling." t)
    
        ;; (cygwin-mount-activate)           ; This is sometimes VERY SLOW!
        ))

See `w32-settings.el` for more!

## Helper for Cygwin Emacs<a id="sec-41-12" name="sec-41-12"></a>

If you want the **backslashes** in the **Windows paths** returned by Everything to be
interpreted correctly, you should use `windows-path`.

    ;; Let Cygwin Emacs recognize Windows paths (e.g. C:\Program Files\).
    (when leuven--cygwin-p
    
      (try-require 'windows-path)
    
      (with-eval-after-load "windows-path"
    
        ;; Activate windows-path-style-handling.
        (windows-path-activate)))

## Emacs Speaks Statistics (ESS)<a id="sec-41-13" name="sec-41-13"></a>

See:
-   <http://ess.r-project.org/Manual/ess.html>,
-   <http://www.emacswiki.org/emacs/EmacsSpeaksStatistics>.

### 2 Installing ESS on your system<a id="sec-41-13-1" name="sec-41-13-1"></a>

    (leuven--section "Utilities -- ESS")
    
    ;; ESS: Emacs Speaks Statistics
    (autoload 'R "ess-site"
      "Call 'R', the 'GNU S' system from the R Foundation." t)
    
    (autoload 'R-mode "ess-site"
      "Major mode for editing R source." t)
    
    (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

### 3 Interacting with statistical programs<a id="sec-41-13-2" name="sec-41-13-2"></a>

Start an R session with `M-x R`.

<div class="note">
To run statistical processes under ESS, Windows users will need to make sure
that the directories for `R` or `Rterm` (see `inferior-R-program-name`) are in their
`PATH` environment variable.

</div>

**Customize** variables for ESS (set these variables **before** loading `ess-site`).

    ;; Start R in current working directory, don't ask user.
    (setq ess-ask-for-ess-directory nil)
    
    ;; New inferior ESS process appears in another window in the current frame.
    (setq inferior-ess-same-window nil)

    (when leuven--cygwin-p                 ; Using R from Cygwin.
    
      ;; Safe 8.3 name for 32-bit programs.
      (setq ess-program-files "c:/PROGRA~2")
    
      ;; Safe 8.3 name for 64-bit programs.
      (setq ess-program-files-64 "c:/PROGRA~1")
    
      ;; Program name for invoking an inferior ESS with `M-x R'.
      (setq inferior-R-program-name "R")) ; [Default: Rterm].

    ;; Accented characters on graphics.
    (add-to-list 'process-coding-system-alist
                 '("R.*" . iso-latin-1))

### 4 Interacting with the ESS process<a id="sec-41-13-3" name="sec-41-13-3"></a>

-   `inferior-ess-mode-map` for R console only
-   `comint-mode-map` for Shell console as well

### 5 Sending code to the ESS process<a id="sec-41-13-4" name="sec-41-13-4"></a>

    ;; ;; Display input commands in the process buffer.
    ;; (setq ess-eval-visibly 'nowait)       ; But avoid Emacs hanging on large
    ;;                                       ; evaluations.

### 7 Editing objects and functions<a id="sec-41-13-5" name="sec-41-13-5"></a>

Smart underscore:
-   Pressing underscore once inserts `<-`.
-   Pressing underscore twice inserts a literal underscore.

    ;; Default ESS indentation style.
    (setq ess-default-style 'DEFAULT)

#### Code folding<a id="sec-41-13-5-1" name="sec-41-13-5-1"></a>

    (with-eval-after-load "ess-site"
    
      ;; Code folding in ESS mode.
      (add-hook 'ess-mode-hook #'hs-minor-mode)

### 9 Completion<a id="sec-41-13-6" name="sec-41-13-6"></a>

#### Integration with auto-complete package<a id="sec-41-13-6-1" name="sec-41-13-6-1"></a>

<div class="note">
R docs are not in Info format.  Though, they are available via the quick help of
Auto-Complete.

</div>

    ;; Suffix appended by `ac-source-R-args' to candidates.
    (setq ess-ac-R-argument-suffix "=")

XXX Switch to `company-ess`, which does auto-start completion of function
arguments with 0 characters.

On <span class="timestamp-wrapper"><span class="timestamp">[2014-11-13 Thu]</span></span>, Vitalie Spinu wrote that he will add native Company support
in ESS in the near future.

### 10 Developing with ESS<a id="sec-41-13-7" name="sec-41-13-7"></a>

See
-   <http://code.google.com/p/ess-tracebug/>
-   <http://code.google.com/p/ess-tracebug/wiki/GettingStarted>.

    ;; Activate ess-tracebug every time r session starts.
    (add-hook 'ess-post-run-hook #'ess-tracebug)

### 11 Other ESS features and tools<a id="sec-41-13-8" name="sec-41-13-8"></a>

#### Describe object at point<a id="sec-41-13-8-1" name="sec-41-13-8-1"></a>

Get info for object at point, and display it in an electric buffer with the
command `ess-describe-object-at-point` (bound to `C-c C-d C-e`, repeat `C-e` or `e` to
cycle between `str()`, `head()`, `tail()` and `summary()`).

Use `other-window` to switch to `*ess-describe*` window.

It was inspired by Erik Iverson&rsquo;s `ess-R-object-tooltip`.

#### ElDoc<a id="sec-41-13-8-2" name="sec-41-13-8-2"></a>

Since ESS v12.02, ElDoc functionality (to report R function arguments) has been
moved into the core, and is active by default; so you don&rsquo;t need to configure
anything&#x2026;

When you visit a R mode, ElDoc will be turned on.  However, you will **first need
to associate the R buffer with an R process** so that args can be looked up &#x2013;
otherwise, ElDoc will silently not report anything.

So, e.g. try:
1.  `C-x C-f somefile.R`
2.  `M-x R` (so that `somefile.R` is associated with `*R*`)
3.  ElDoc should then work.

#### Highlighting<a id="sec-41-13-8-3" name="sec-41-13-8-3"></a>

    ;; Font-lock keywords for the R mode.
    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers . t) ; Default.
            (ess-R-fl-keyword:fun-defs . t) ; Default.
            (ess-R-fl-keyword:keywords . t) ; Default.
            (ess-R-fl-keyword:assign-ops . t) ; Default.
            (ess-R-fl-keyword:constants . t) ; Default.
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)))
    
    ;; Font-lock patterns used in inferior-R-mode buffers.
    (setq inferior-R-font-lock-keywords
          '((ess-S-fl-keyword:prompt . t) ; Default.
            (ess-R-fl-keyword:messages . t) ; Default.
            (ess-R-fl-keyword:modifiers . t) ; Default.
            (ess-R-fl-keyword:fun-defs . t) ; Default.
            (ess-R-fl-keyword:keywords . t) ; Default.
            (ess-R-fl-keyword:assign-ops . t) ; Default.
            (ess-R-fl-keyword:constants . t) ; Default.
            (ess-fl-keyword:matrix-labels . t) ; Default.
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)))

#### Rdired<a id="sec-41-13-8-4" name="sec-41-13-8-4"></a>

With your point on the line of a variable,

-   `p` will plot the object,
-   `v` will view it, and
-   `d` will mark the object for deletion (`x` will actually perform the deletion).

    ;; Prototype object browser for R, looks like dired mode.
    (autoload 'ess-rdired "ess-rdired"
      "View *R* objects in a dired-like buffer." t)

You could prefer to have the `*R dired*` buffer in a separate frame.  Check out
`special-display-buffer-names`.

Or have it dedicated in a window.  Check out
<https://github.com/emacsmirror/dedicated>.

#### Interaction with Org mode<a id="sec-41-13-8-5" name="sec-41-13-8-5"></a>

For anyone who wants to try it, after cloning into `~/elisp/polymode`, I only
needed to add this in my init (ESS was already in my init).

    (setq load-path
           (append '("~/elisp/polymode"  "~/elisp/polymode/modes")
                   load-path))
    (require 'poly-org)
    (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

When point is inside a src-block the local mode prevails. So, `C-c C-n` inside an
R src block sends the line point is on to the R process. `C-c C-c` sends the
region (or function or paragraph) to the process when point is in the body. To
execute `org-ctrl-c-ctrl-c` on the src block with the `C-c C-c` keying, it is
necessary to move point to before or after the body.

    )

## Proced<a id="sec-41-14" name="sec-41-14"></a>

    ;;* Proced
    
      ;; ;; Start Proced in a similar manner to Dired.
      ;; (global-set-key (kbd "C-x p") #'proced) ; Conflict with Bkmp.
    
      (with-eval-after-load "proced"
    
        ;; Current sort scheme for Proced listing.
        (setq-default proced-sort 'start)
    
        ;; Display of Proced buffer as process tree.
        (setq-default proced-tree-flag t))

    )

# Using Emacs Server<a id="sec-42" name="sec-42"></a>

<div class="note">
The Emacs version you&rsquo;re using and the `emacsclient` version you&rsquo;re using must be
from the **same Emacs version**&#x2026;  If the latter is not the cause of a refused
connection, re-try `M-x server-start` in Emacs.

</div>

`emacsclient` is waiting for Emacs to tell it it&rsquo;s done editing that file.
That&rsquo;s important if you use `emacsclient` as your `EDITOR`.  If you want
`emacsclient` to return immediately, use the `-n` option.

Do you use `emacsclient` in terminals?  If not, just use parameter `-c -n`, which
creates an X Window and doesn&rsquo;t wait for `(server-edit)`.

    ;;* 39 (info "(emacs)Emacs Server")
    
    (leuven--chapter leuven-load-chapter-39-emacs-server "39 Using Emacs as a Server"
    
      ;; Use Emacs as a server (with the `emacsclient' program).
      (unless noninteractive
        (idle-require 'server))             ; After init.
    
      (with-eval-after-load "server"
    
        ;; Test whether server is (definitely) running, avoiding the message of
        ;; "server-start" while opening another Emacs session.
        (or (equal (server-running-p) t)
    
            ;; Start the Emacs server.
            (server-start))                 ; ~ 0.20 s
    
        ;; Save file without confirmation before returning to the client.
        (defadvice server-edit (before save-buffer-if-needed activate)
          "Save current buffer before marking it as done."
          (when server-buffer-clients (save-buffer))))
    
    )                                       ; Chapter 39 ends here.

Since GNU Emacs 23, there is also a `--daemon` flag to start the server in a
convenient way.

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Understand the following:</b><br  />


    ;; (add-hook 'server-done-hook
    ;;        (lambda ()
    ;;          (shell-command "screen -r -X select `cat ~/tmp/emacsclient-caller`")))

and see
<http://blog.jr0cket.co.uk/2012/10/using-emacs-24-server-on-mac-osx-for.html>
for ideas about having the right `emacsclient` used to connect to the Emacs
server.
</div>

# Printing Hard Copies<a id="sec-43" name="sec-43"></a>

You can print any buffer with `C-u M-x ps-print-buffer-with-faces RET`.  Because
of the `C-u`, it will prompt for a file to place the PostScript into.  You can
then convert PS to PDF, if so desired.

You can print without faces (i.e. no colour) with `ps-print-buffer`.

You may wish to set `ps-print-landscape` to `t`.

See `(info "(emacs)Windows Printing")` for several methods of correctly setting up
printing on Windows.

    ;;* 40 (info "(emacs)Printing")
    
    (leuven--chapter leuven-load-chapter-40-printing "40 Printing Hard Copies"
    
      ;; Print Emacs buffer on line printer
      ;; for {lpr,print}-{buffer,region}.
      (with-eval-after-load "lpr"
    
        ;; Name of program for printing a file.
        (setq lpr-command (executable-find "enscript"))
                                        ; TODO Install `enscript'.
    
        ;; List of strings to pass as extra options for the printer program.
        (setq lpr-switches (list "--font=Courier8"
                                 "--header-font=Courier10"
                                 (format "--header=%s" (buffer-name))))
    
        ;; Name of a printer to which data is sent for printing.
        (setq printer-name t))
    
      (defun leuven-ps-print-buffer-with-faces-query ()
        "Query user before printing the buffer."
        (interactive)
        (when (y-or-n-p "Are you sure you want to print this buffer? ")
          (ps-print-buffer-with-faces)))
    
      ;; Generate and print a PostScript image of the buffer.
      (when leuven--win32-p
        ;; Override `Print Screen' globally used as a hotkey by Windows.
        (w32-register-hot-key (kbd "<snapshot>"))
        (global-set-key
          (kbd "<snapshot>") #'leuven-ps-print-buffer-with-faces-query))
    
      ;; Print text from the buffer as PostScript.
      (with-eval-after-load "ps-print"
    
        (defvar gsprint-program
          (concat leuven--windows-program-files-dir "Ghostgum/gsview/gsprint.exe")
          "Defines the Windows path to the gsview executable.")
    
        (leuven--file-exists-and-executable-p gsprint-program)
    
        (if (and gsprint-program
                 (executable-find gsprint-program))
    
            (progn
              ;; Name of a local printer for printing PostScript files.
              (setq ps-printer-name t)      ; Adjusted to run Ghostscript.
    
    
              ;; Name of program for printing a PostScript file.
              (setq ps-lpr-command gsprint-program)
                                            ; Tell Emacs where Ghostscript print
                                            ; utility is located.
    
              ;; List of extra switches to pass to `ps-lpr-command'.
              (setq ps-lpr-switches '("-query")))
                                            ; Tell Ghostscript to query which
                                            ; printer to use.
                                            ; '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")
    
          (setq ps-printer-name "//PRINT-SERVER/Brother HL-4150CDN") ; XXX
          (setq ps-lpr-command "")
          (setq ps-lpr-switches '("raw")))
    
        ;; (setq ps-error-handler-message 'system)
    
        ;; Size of paper to format for.
        (setq ps-paper-type 'a4)
        (setq ps-warn-paper-type nil)
    
        ;; Print in portrait mode.
        (setq ps-landscape-mode nil)
    
        ;; (setq ps-print-control-characters nil)
    
        ;; Number of columns.
        (setq ps-number-of-columns 1)
    
        (setq ps-left-margin 40)
        (setq ps-right-margin 56)
        (setq ps-bottom-margin 22)
        (setq ps-top-margin 32)
    
        ;; Page layout: Header [file-name     2001-06-18 Mon]
        (setq ps-print-header-frame nil)    ; No box around the header.
        ;; See http://www.emacswiki.org/emacs/PsPrintPackage-23.
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
        (setq ps-print-footer-frame nil)    ; No box around the footer.
        (setq ps-footer-frame-alist '((fore-color . "#666666")))
        (setq ps-footer-lines 1)
        (setq ps-footer-font-family 'Helvetica)
        (setq ps-footer-font-size 8)
        (setq ps-left-footer nil)
        (setq ps-right-footer (list "/pagenumberstring load")) ; Page n of m.
    
        (setq ps-font-family 'Courier)      ; See `ps-font-info-database'.
                                            ; Legitimate values include Courier,
                                            ; Helvetica, NewCenturySchlbk, Palatino
                                            ; and Times.
    
        ;; Font size, in points, for ordinary text, when generating PostScript.
        (setq ps-font-size 9.1)
    
        ;; Specify if face background should be used.
        (setq ps-use-face-background t)
    
        ;; Specify line spacing, in points, for ordinary text.
        (setq ps-line-spacing 3))
    
    )                                       ; Chapter 40 ends here.

# Sorting Text<a id="sec-44" name="sec-44"></a>

    ;;* 41 (info "(emacs)Sorting") Text
    
    (leuven--chapter leuven-load-chapter-41-sorting "41 Sorting Text"
    
      ;; Key binding.
      (global-set-key (kbd "C-c ^") #'sort-lines)
    
    )                                       ; Chapter 41 ends here.

# Saving Emacs Sessions<a id="sec-45" name="sec-45"></a>

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Check that there is no extra ~/.saves directory!</b><br  />
nil</div>

    ;;* 44 (info "(emacs)Saving Emacs Sessions")
    
    (leuven--chapter leuven-load-chapter-44-saving-emacs-sessions "44 Saving Emacs Sessions"
    
      (try-require 'saveplace)
      (with-eval-after-load "saveplace"
    
        ;; Automatically save place in each file.
        (setq-default save-place t))        ; default value for all buffers
    
        ;; Name of the file that records `save-place-alist' value.
        (setq save-place-file "~/.emacs.d/.places")
    
    )                                       ; Chapter 44 ends here.

# Hyperlinking and Navigation Features<a id="sec-46" name="sec-46"></a>

    ;;* 46 (info "(emacs)Hyperlinking")
    
    (leuven--chapter leuven-load-chapter-46-hyperlinking "46 Hyperlinking and Navigation Features"

## Following URLs<a id="sec-46-1" name="sec-46-1"></a>

    ;; Use proxy.
    (setq url-proxy-services              ;! Emacs expects just hostname and port
                                          ;! in `url-proxy-services', NOT prefixed
                                          ;! with "http://"
          `(("http"     . ,(getenv "http_proxy"))
            ("ftp"      . ,(getenv "http_proxy"))
            ("no_proxy" . "^.*example.com")))
            ;; Disable proxy for some hosts.

    ;;** Pass a URL to a WWW browser.
    
      (leuven--section "pass a URL to a WWW browser")
    
      ;; Default browser started when you click on some URL in the buffer.
      (setq browse-url-browser-function
            (cond ((or leuven--win32-p
                       leuven--cygwin-p)
                   'browse-url-default-windows-browser)
                  (leuven--mac-p
                   'browse-url-default-macosx-browser)
                  ((not (display-graphic-p)) ; Console.
                   'eww-browse-url)
                  (t                        ; Linux.
                   'browse-url-generic)))
    
      ;; ;; TEMP For testing purpose
      ;; (setq browse-url-browser-function 'eww-browse-url)
    
      ;; Name of the browser program used by `browse-url-generic'.
      (setq browse-url-generic-program (executable-find "gnome-open"))
                                            ; Defer the decision to Gnome.  We could
                                            ; use "firefox" or "google-chrome" as
                                            ; well.

    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "cygstart")

## Finding Files and URLs at Point<a id="sec-46-2" name="sec-46-2"></a>

    (leuven--section "FFAP")
    
    (unless (featurep 'helm-config)
    
      ;; Visit a file.
      (global-set-key (kbd "<f3>") #'find-file-at-point))
    
    ;; Find file (or URL) at point.
    (with-eval-after-load "ffap"
    
      ;; Function called to fetch an URL.
      (setq ffap-url-fetcher 'browse-url)); Could be `browse-url-emacs' or
                                          ; `eww-browse-url'.

## Google search<a id="sec-46-3" name="sec-46-3"></a>

    ;;** Web search
    
      (leuven--section "Web search")

-   **`C-c g SPC`:** `google-this-region`

-   **`C-c g n`:** `google-this-noconfirm`

-   **`C-c g t`:** `google-this`

`url-parse` and `url-cookies` are quite heavy to load, hence:

    ;; A set of functions and bindings to Google under point.
    (with-eval-after-load "google-this-autoloads"
    
      ;; Keybinding under which `google-this-mode-submap' is assigned.
      (setq google-this-keybind (kbd "C-c g"))
    
      (idle-require 'google-this))
    
    (with-eval-after-load "google-this"
    
      ;; Enable Google-This mode.
      (google-this-mode 1))

When I want to Google most of a line, I call `google-this-line` and edit the
prompt. And when I to Google the entire line, hitting `RET` one extra time doesn&rsquo;t
quite bother me.

If you&rsquo;d like to skip confirmation, you can define the following function and
bind it to a key:

    (defun google-this-line-noconfirm (prefix)
      "Google the current line without confirmationl.
    PREFIX determines quoting."
      (interactive "P")
      (google-this-line prefix 'noconfirm))

    (defun leuven-google-search-active-region-or-word-at-point ()
      "Create a Google search URL and send it to your web browser.
    If `transient-mark-mode' is non-nil and the mark is active, it defaults to the
    current region, else to the word at or before point."
      (interactive)
      (let ((query
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (find-tag-default))))      ; or (current-word) for word at point?
        (browse-url
         (concat
          "http://www.google.com/search?q="
          (url-hexify-string query)))))
    
    (defun leuven-duckduckgo-search-active-region-or-word-at-point ()
      "Create a DuckDuckGo search URL and send it to your web browser.
    If `transient-mark-mode' is non-nil and the mark is active, it defaults to the
    current region, else to the word at or before point."
      (interactive)
      (let ((query
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (find-tag-default))))      ; or (current-word) for word at point?
        (browse-url
         (concat
          "https://duckduckgo.com/?q="
          (url-hexify-string query)))))
    
    (global-set-key (kbd "C-c g G") #'leuven-google-search-active-region-or-word-at-point)
    (global-set-key (kbd "C-c g D") #'leuven-duckduckgo-search-active-region-or-word-at-point)

## How do I?<a id="sec-46-4" name="sec-46-4"></a>

Instant coding answers via Emacs.

    ;; A set of functions and bindings to Google under point.
    (with-eval-after-load "howdoi-autoloads"
    
      (global-set-key (kbd "C-c h n") #'howdoi-query)
      (global-set-key (kbd "C-c h d") #'howdoi-query-insert-code-snippet-at-point))

## Babel translator<a id="sec-46-5" name="sec-46-5"></a>

    ;;** Babel
    
      (leuven--section "Babel")
    
      ;; Interface to web translation services such as Babelfish.
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

Switch between different translation directions directly from minibuffer by
using `C-n` and `C-p` key bindings.

    ;; Emacs interface to Google Translate.
    (with-eval-after-load "google-translate-autoloads"
    
      ;; Translate a text using translation directions.
      (global-set-key (kbd "C-c t") #'google-translate-smooth-translate))
    
    ;; Just another UI to Google.
    (with-eval-after-load "google-translate-smooth-ui"
    
      ;; Translation directions.
      (setq google-translate-translation-directions-alist
            '(("fr" . "en") ("en" . "fr")
              ("fr" . "nl") ("nl" . "fr")
              ("fr" . "es") ("es" . "fr"))))

# Other Amusements<a id="sec-47" name="sec-47"></a>

    ;;* 47 Other (info "(emacs)Amusements")
    
    (leuven--chapter leuven-load-chapter-47-amusements "47 Other Amusements"
    
      ;; Define a default menu bar.
      (with-eval-after-load "menu-bar"
    
        ;; Get rid of the Games in the Tools menu.
        (define-key menu-bar-tools-menu [games] nil))
    
    )                                       ; Chapter 47 ends here.

# Customization<a id="sec-48" name="sec-48"></a>

Emacs 24 custom themes allow loading multiple themes simultaneously.  If you
don&rsquo;t want the previous theme to stay in effect, you will need to unload it
first.

Install Leuven Theme via the Emacs Lisp Packages (See section 7) (or use the one bundled in GNU
Emacs 24.4).

    ;;* 49 (info "(emacs)Customization")
    
    (leuven--chapter leuven-load-chapter-49-customization "49 Customization"
    
      (ignore-errors
        ;; Load custom theme "Leuven" and enable it.
        (load-theme 'leuven t))

## Color<a id="sec-48-1" name="sec-48-1"></a>

You can govern the sort order of colors now such as HSV or RGB distance from a
particular color&#x2026;

    ;; Color sort order for `list-colors-display'.
    (setq list-colors-sort '(rgb-dist . "#FFFFFF"))

## Variables<a id="sec-48-2" name="sec-48-2"></a>

    ;;** 49.3 (info "(emacs)Variables")
    
      (leuven--section "49.3 (emacs)Variables")
    
      ;; File local variables specifications are obeyed, without query --
      ;; RISKY!
      (setq enable-local-variables t)
    
      ;; Obey `eval' variables -- RISKY!
      (setq enable-local-eval t)
    
      ;; Record safe values for some local variables.
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

Have a look at (info "(emacs)Directory Variables&rdquo;).

## Key Bindings<a id="sec-48-3" name="sec-48-3"></a>

The keys `C-c LETTER` are [reserved for user functions](http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html).

You can get a list of all the disabled functions by typing:

    M-: (let (lst) (mapatoms (lambda (x) (if (get x 'disabled) (push x lst)))) lst) RET

    ;;** 49.4 Customizing (info "(emacs)Key Bindings")
    
      (leuven--section "49.4 Customizing (emacs)Key Bindings")

See as well Print keybindings (pkb).

    ;; Print the key bindings in a tabular form.
    (defun leuven-keytable (arg)
      "Print the key bindings in a tabular form."
      (interactive "sEnter a modifier string:")
      (with-output-to-temp-buffer "*Key table*"
        (let* ((i 0)
               (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
                           "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x"
                           "y" "z" "<RET>" "<down>" "<up>" "<right>"
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
      (setq truncate-lines t))

    ;; Guide the following key bindings automatically and dynamically.
    (with-eval-after-load "which-key-autoloads"
      (idle-require 'which-key))
    
    (with-eval-after-load "which-key"
    
      (which-key-mode)
    
      (which-key-setup-side-window-right-bottom)
    
      (setq which-key-idle-delay 0.4)
    
      (setq which-key-sort-order 'which-key-local-then-key-order)
    
      ;; Set the maximum length (in characters) for key descriptions (commands or
      ;; prefixes).
      (setq which-key-max-description-length 33))

## Syntax Table<a id="sec-48-4" name="sec-48-4"></a>

The syntax table contains information that tells Emacs how to operate on text,
words, sentences etc.  It will make Emacs know enough about all the symbols in
a buffer.  Syntax table is used for example for:

-   commands like `forward-word` (`M-f`) or `backward-kill-word` (`M-DEL`),
-   spell-checking of words,
-   expansion commands of abbrevs,
-   etc.

Evaluate `current-word` and see whether characters such as `-` and `_` are
considered part of the word (depending on the current major mode).

    ;;** 49.5 The (info "(emacs)Syntax") Table
    
      (leuven--section "49.5 The (emacs)Syntax Table")
    
      ;; Define "-" as part of a word.
      ;; (add-hook 'emacs-lisp-mode-hook
      ;;           (lambda ()
      ;;             (modify-syntax-entry ?- "w")))

    )                                       ; Chapter 49 ends here.

# Emacs Display<a id="sec-49" name="sec-49"></a>

    ;;* Emacs Display
    
    (leuven--chapter leuven-load-chapter-XX-emacs-display "XX Emacs Display"

## Faces<a id="sec-49-1" name="sec-49-1"></a>

You can get text properties of any char by typing `C-u C-x =`.

Under Windows, you can get the current **font** by typing `(w32-select-font)`
followed by `C-x C-e`.

You can find the current **font string** by typing
`M-x ielm RET (frame-parameters) RET` &#x2013; see the line `font`.

To check if some font is available in Emacs do following:

1.  Switch to the `*scratch*` buffer.

2.  Type
    
    `(prin1-to-string (x-list-fonts "font-you-want-to-check or pattern"))`

3.  Place the cursor after the last closing paren and hit `C-j`.  List of the names
    of available fonts matching given pattern will appear in the current buffer
    (`*scratch*`).

4.  For listing of all available fonts, use
    
    `(prin1-to-string (x-list-fonts "*"))`
    
    or
    
    `(dolist (i (x-list-fonts "*")) (princ i) (terpri))`
    
    for a better output.

Under Linux, use the `xfontsel` utility (or the command-line `xlsfonts`) to try out
different fonts.  After choosing a font, click the select button in `xfontsel`
window.  This will copy font name you choose to copy & paste buffer.

Now Emacs should start with that font.

    ;;** (info "(elisp)Faces")
    
      (leuven--section "Faces")

For reasons unknown to me, Emacs takes a long time to change fonts in an X
environment.

Rather than using `(set-default-font ...)` in `.emacs`, stick the font definition in
your `.Xresources` file (key &lsquo;Emacs\*font&rsquo;) and then use `xrdb -load` to activate it.
You will find that startup time is greatly improved!

Edit your `~/.Xresources` file to have a line with &ldquo;Emacs.font&rdquo;.  Then do a

`xrdb -merge ~/.Xresources`

or restart your X11 to validate the modification.  I let Emacs do this for me:

    (defun leuven--merge-x-resources ()
      (let ((file (file-name-nondirectory (buffer-file-name))))
        (when (or (string= file ".Xdefaults")
                  (string= file ".Xresources"))
          (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
          (message (format "Merged %s into X resource database" file)))))
    
    (add-hook 'after-save-hook #'leuven--merge-x-resources)

    ;; allow any scalable font
    (setq scalable-fonts-allowed t)

### UTF-8<a id="sec-49-1-1" name="sec-49-1-1"></a>

Fonts that have a good UTF-8 coverage are:

-   DejaVu Sans Mono
-   FreeMono (FreeSans, FreeSerif)
-   Monospace

None of them has all four variants, some have regular (medium) and bold or light
and regular, one regular and oblique.

### Anti-aliasing<a id="sec-49-1-2" name="sec-49-1-2"></a>

To see if anti-aliasing is active, use `xmag` (under Linux) or any of the other
magnifier applications.  The fonts should have gray edges.

### Adjust the height of the default face<a id="sec-49-1-3" name="sec-49-1-3"></a>

There are a few commands to adjust the font sizes:

-   **`C-x C-+`:** Increase the default face height by one step.

-   **`C-x C--`:** Decrease the default face height by one step.

-   **`C-x C-0`:** Resets to defaults.

Those bindings are global and repeatable, e.g. `C-x C-+ C-+ C-+`.

    (global-set-key (kbd "C-+") #'text-scale-increase)
    (global-set-key (kbd "C--") #'text-scale-decrease)

Zoom in/out with Ctrl + Scroll wheel on the mouse:

    (global-set-key (kbd "<C-wheel-up>") #'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") #'text-scale-decrease)

    )

## Images<a id="sec-49-2" name="sec-49-2"></a>


    ;;** 37.17 (info "(elisp)Images")

How do I get image support? See `image-library-alist`, a list of image types vs.
external libraries needed to display them.

    (describe-variable 'image-library-alist)

<div class="note">
You can download the corresponding dlls from ezwinports
<http://sourceforge.net/projects/ezwinports/files/> and put them into
emacs&rsquo;s \`bin\` directory.

There are some instructions in the &ldquo;Image support&rdquo; part on page
<https://ftp.gnu.org/gnu/emacs/windows/>.

</div>

If you use compiled Emacs binaries (available on the [GNU Website](http://ftp.gnu.org/gnu/emacs/windows/)), eventually,
copy those DLLs (`libpng16.dll`, for example) from an old Windows Emacs
installation to your Emacs `bin` directory, and restart Emacs for image support to
be enabled.

If you use Cygwin Emacs, install `libpng` via Cygwin as well.

You can do:

    (image-type-available-p 'png)

to check if you have `png` image support in your Emacs.

`image-library-alist` maps image type to a list of dlls which support it.

To check all image libraries at once:

    (mapcar (lambda (elt)
              (list (car elt) (image-type-available-p (car elt))))
            dynamic-library-alist)

If missing image libraries bother you, read `README.W32` (or `nt/INSTALL`).

For animated GIF (or multi-page TIFF):
-   `f` steps through image frame frame, and
-   `RET` animates.

No need for ImageMagick.

# Emacs Lisp<a id="sec-50" name="sec-50"></a>

## Variables<a id="sec-50-1" name="sec-50-1"></a>

### Local Variables<a id="sec-50-1-1" name="sec-50-1-1"></a>

    ;; Limit on number of Lisp variable bindings & unwind-protects.
    (setq max-specpdl-size 3000)          ; XEmacs 21.5.29

## GNU Emacs Internals<a id="sec-50-2" name="sec-50-2"></a>

### Garbage Collection<a id="sec-50-2-1" name="sec-50-2-1"></a>

By default Emacs will initiate GC every 0.76 MB allocated (`gc-cons-threshold` ==
800,000).

If you have a modern machine, I encourage you to add the following:

    ;; Speed up things by preventing garbage collections.
    (setq gc-cons-threshold (* 20 1024 1024)) ; 20 MB.
    
    ;; Don't display messages at start and end of garbage collection (as it hides
    ;; too many interesting messages).
    (setq garbage-collection-messages nil)

# Calc<a id="sec-51" name="sec-51"></a>

`C-x *` invokes the GNU Emacs Calculator.

## Introduction<a id="sec-51-1" name="sec-51-1"></a>

### Basic Commands<a id="sec-51-1-1" name="sec-51-1-1"></a>

Start the Calc: `C-x * c`.

### &ldquo;Quick Calculator&rdquo; Mode<a id="sec-51-1-2" name="sec-51-1-2"></a>

Run the Calculator in the minibuffer: `C-x * q` (`M-x quick-calc`).

## Embedded Mode<a id="sec-51-2" name="sec-51-2"></a>

Try the Embedded mode of Calc: `C-x * e` (no need to mark the region).

Similar commands: in place of `e`,

-   **`w`:** Start Embedded mode on the current &ldquo;word&rdquo;.

-   **`j`:** Operate on **assignments**.

You need to type `C-x * e` again to exit the Embedded mode.

# IRC client for Emacs<a id="sec-52" name="sec-52"></a>

See [List of Internet Relay Chat commands](http://en.wikipedia.org/wiki/List_of_IRC_commands):
-   `/whois`
-   `/leave`

    ;;* Emacs IRC client
    
    ;; Other IRC for Emacs: rcirc, circe, bitlbee, liece, riece, zenirc, erc
    ;; Circe is advised by Tassilo (contributor).
    
      (autoload 'circe "circe"
        "Connect to an IRC server." t)
    
      ;; Connect to the Freenode network
      (defun leuven-irc-connect ()
        "Connect to Freenode."
        (interactive)
        (circe "irc.freenode.net" "6667" "freenode"))
    
      ;; ... upon hitting `C-c i'.
      (global-set-key (kbd "C-c i")
        (lambda ()
          (interactive)
          (switch-or-start 'leuven-irc-connect "irc.freenode.net:6667")))
    
      (with-eval-after-load "circe"
    
    ;;** 1 Basics
    
        ;; Default channels to join whenever connecting to Freenode.
        (setq circe-server-auto-join-channels
              '(("^freenode$"
                 "#emacs"
                 ;; "#gnus"
                 ;; "#latex"
                 ;; "#ledger"
                 "#org-mode"
                 ;; "#stumpwm"
                 ;; "#zsh"
                 )))
    
    ;;** 2 Reference
    
    ;;*** 2.3 Configuration
    
        ;; Default nick.
        (setq circe-default-nick "johndoe")
    
        ;; Your "real name" on IRC.
        (setq circe-default-realname "John Doe")
    
        ;; Authentication info.
        (setq freenode-passwd "")
        (setq circe-nickserv-passwords
              `(("freenode" ,freenode-passwd)))
    
    ;;** 3 Fighting Information Overload
    
    ;;*** 3.1 Channels
    
        ;; When other people say things in buffers that are currently buried (no
        ;; window is showing them), the mode line will now show you the abbreviated
        ;; channel or nick name.  Use `C-c C-SPC' to switch to these buffers.
    
    ;;*** 3.3 Keywords
    
        ;; List of keywords to highlight.
        (setq lui-highlight-keywords
              '(
                ;; "[^<]vauban"
                "org" "beamer" "ledger" "tikz"))
    
        ;; Add IRC color support to LUI.
        (try-require 'lui-irc-colors)
        (with-eval-after-load "lui-irc-colors"
          (add-to-list 'lui-pre-output-hook 'lui-irc-colors))
    
    ;;** 4 Hacking and Tweaking
    
    ;;*** 4.2 Using fly spell mode
    
        (when (leuven--executable-ispell-program-name-p)
          (setq lui-flyspell-p t)
          (setq lui-flyspell-alist '(("." "american"))))
    
    ;;** Others
    
        (setq circe-highlight-nick-type 'occurrence)
    
        (try-require 'circe-highlight-all-nicks)
        (with-eval-after-load "circe-highlight-all-nicks"
          (enable-circe-highlight-all-nicks))
    
        ;; Format for messages to queries or channels.
        (setq circe-format-self-say "<{nick}> {body}")
    
        ;; Truncate the buffer (at the top) if it grows too much.
        (setq lui-max-buffer-size 30000)
    
        ;; Column at which Lui should break output.
        (setq lui-fill-column fill-column)
    
        )                                   ; with-eval-after-load "circe" ends here.

    ;; Jabber (GTalk).
    (try-require 'jabber-autoloads-XXX)
    (with-eval-after-load "jabber-autoloads-XXX"
      (setq jabber-account-list '(("johndoe@example.com"
                                   (:network-server . "talk.google.com")
                                   (:connection-type . ssl)
                                   (:port . 443))))
    
      (setq jabber-history-enabled t)
    
      (setq jabber-use-global-history nil)
    
      (setq jabber-history-dir (concat user-emacs-directory "jabber"))
    
      (setq jabber-vcard-avatars-retrieve nil)
    
      (setq jabber-chat-buffer-show-avatar nil))

# Emacs and Microsoft Windows/MS-DOS<a id="sec-53" name="sec-53"></a>

Eventually have a look as well at `input-decode-map`.

    ;;* App G Emacs and (info "(emacs)Microsoft Windows/MS-DOS")
    
    (leuven--chapter leuven-load-chapter-AppG-ms-dos "Appendix G Emacs and MS-DOS"
    
      ;; Divide key (needed in GNU Emacs for Windows).
      (global-set-key (kbd "<kp-divide>") (kbd "/"))
    
    )                                       ; Chapter G ends here.

# Profiler<a id="sec-54" name="sec-54"></a>

    ;;* Profiler
    
      (with-eval-after-load "profiler"
    
        (setq profiler-report-cpu-line-format
          '((100 left)                      ; The 100 above is increased from the
                                            ; default of 50 to allow the deeply
                                            ; nested call tree to be seen.
            (24 right ((19 right)
                       (5 right))))))

# Reporting Bugs<a id="sec-55" name="sec-55"></a>

To get a backtrace when a specific regexp is displayed in the echo area (through
calls to message), set the variable `debug-on-message`.

    ;; Recovery from Problems
    
    ;;* Reporting Bugs
    
    (leuven--chapter leuven-load-chapter-99-debugging "99 Debugging"
    
      ;; Get the backtrace when uncaught errors occur.
      (setq debug-on-error nil)             ; Was set to `t' at beginning of file.
    
      ;; Hit `C-g' while it's frozen to get an Emacs Lisp backtrace.
      (setq debug-on-quit nil)              ; Was set to `t' at beginning of file.
    
      (setq debug-on-entry 'user-error))

    (when (and (string-match "GNU Emacs" (version))
               leuven-load-verbose)
      (ad-disable-advice 'message 'before 'leuven-when-was-that)
      (ad-update 'message))

    (when leuven-load-verbose
      (message "| Chapter | Time |")
      (message "|---------+------|")
      (mapcar (lambda (el)                  ; FIXME Use `mapc' or `dolist'.
                (message el))
              (nreverse leuven--load-times-list))
      (message "|---------+------|")
      (message "|         | =vsum(@-I..@-II) |"))

    (message "Loading `%s'...done (in %.3f s)"
             load-file-name
             (- (float-time) leuven--before-time))
    (sit-for 0.3)

    (let ((elapsed (float-time (time-subtract (current-time)
                                              emacs-start-time))))
      (message "Loading `%s'... loaded in %.3f s" load-file-name elapsed))
    
    (add-hook 'after-init-hook
              `(lambda ()
                 (let ((elapsed (float-time (time-subtract (current-time)
                                                           emacs-start-time))))
                   (message "Loading %s...done (%.3fs) [after-init]"
                            ,load-file-name elapsed)))
      t)

    ;; (message "Emacs startup time: %s" (emacs-init-time))

# Leuven<a id="sec-56" name="sec-56"></a>

If you are like me, you want to know if there are:

-   changes to existing files
-   newly added files
-   deleted files

and specifically do not want to know about **untracked files**.

This should do it:

git status &#x2013;untracked-files=no &#x2013;porcelain

See <https://github.com/mordocai/.emacs.d/blob/master/init.el> for many Git
functions implemented in Emacs Lisp.

&ldquo;Package can be upgraded&rdquo;

    (defun leuven-update ()
      "Update Leuven Emacs Config to its latest version."
      (interactive)
      (leuven-emacs-version)
      (message "Updating Leuven...")
      (cd leuven--directory)
      (let ((ret (shell-command-to-string "LC_ALL=C git pull --rebase")))
        (if (string-match "\\(up to date\\|up-to-date\\)" ret)
            (message "Configuration already up-to-date.")
          (princ ret)
          (sit-for 3)
          (message "Configuration updated. Restart Emacs to complete the process."))))
    
    (defun leuven-show-latest-commits ()
      "List latest changes in Leuven Emacs Config."
      (interactive)
      (leuven-emacs-version)
      (message "Fetching last changes in Leuven...")
      (cd leuven--directory)
      (let ((ret (shell-command-to-string "LC_ALL=C git fetch --verbose"))
            (bufname "*Leuven latest commits*"))
        (if (string-match "\\(up to date\\|up-to-date\\)" ret)
            (message "Configuration already up-to-date.")
         (with-output-to-temp-buffer bufname
           (shell-command
            "LC_ALL=C git log --pretty=format:'%h %ad %s' --date=short HEAD..origin"
            bufname)
           (pop-to-buffer bufname)))))
    
    (defun leuven-emacs-version ()
      (interactive)
      (message "Leuven Emacs Config version %s" leuven--emacs-version))


Please address particular issues or suggestions **specifically**, using `M-x
leuven-send-bug-report`.

    (message "* --[ Loaded Leuven Emacs Config %s]--" leuven--emacs-version)

## Feature<a id="sec-56-1" name="sec-56-1"></a>

    (provide 'emacs-leuven)

## File Local Variables<a id="sec-56-2" name="sec-56-2"></a>

    ;; This is for the sake of Emacs.
    ;; Local Variables:
    ;; coding: utf-8-unix
    ;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode))
    ;; flycheck-emacs-lisp-initialize-packages: t
    ;; flycheck-mode: nil
    ;; ispell-local-dictionary: "american"
    ;; End:
    
    ;;; emacs-leuven.el ends here
