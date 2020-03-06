# Hey Emacs, this is a -*- org -*- file ...
#+TITLE:     Java YASnippets
#+AUTHOR:    Fabrice Niessen
#+EMAIL:     (concat "fniessen" at-sign "pirilampo.org")
#+DESCRIPTION:
#+KEYWORDS:  emacs, yasnippet, snippets, code templates
#+LANGUAGE:  en
#+OPTIONS:   H:4 num:nil toc:2
#+EXCLUDE_TAGS: noexport

#+PROPERTY:  header-args :eval never :padline no :tangle yes

#+SETUPFILE: https://fniessen.github.io/org-html-themes/setup/theme-readtheorg.setup

* Overview

A *Java snippet* is a frequently used *code template* which can be inserted into
a code you are typing by pressing the TAB key.

* Insert Snippet...

** Comments

*** [fixme] Add // FIXME

#+begin_src java :tangle ~/.emacs.d/snippets/java-mode/fixme.yasnippet
# name: Add // FIXME
# --
// FIXME: `(format-time-string "%Y-%m-%d")` ${1:todo} $0
#+end_src

*** [todo] Add // TODO

#+begin_src java :tangle ~/.emacs.d/snippets/java-mode/todo.yasnippet
# name: Add // TODO
# --
// TODO: `(format-time-string "%Y-%m-%d")` ${1:todo} $0
#+end_src

** Others

*** [fbc] findViewById with cast

*** [foreach] Create a foreach loop

*** [fori] Create iteration loop

*** [gone]

*** [ifn]

*** [inn]

*** [logm] Log method name and its arguments

#+begin_src java :tangle ~/.emacs.d/snippets/java-mode/logm.yasnippet
android.util.Log.d(TAG, "archiveTable() called with: " + "context = [" + context + "], table_name = [" + table_name + "], wo_id = [" + wo_id + "], wr_id = [" + wr_id + "]");
#+end_src
