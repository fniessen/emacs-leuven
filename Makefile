EMACS ?= emacs

ELISP_DIRS := lisp
ELISP_FILES := $(foreach dir,$(ELISP_DIRS),$(wildcard $(dir)/*.el))

.PHONY: all check check-parens compile clean

all: check compile

check: check-parens

check-parens:
	@$(EMACS) --batch --quick --eval "(dolist (directory (list \"lisp\")) (dolist (file (directory-files-recursively directory \"\\\\.el\")) (with-temp-buffer (emacs-lisp-mode) (message \"Checking %s\" file) (insert-file-contents file) (check-parens))))"

compile:
	$(EMACS) --batch --quick -L lisp -L site-lisp -f batch-byte-compile $(ELISP_FILES)

clean:
	find $(ELISP_DIRS) -type f -name '*.elc' -delete
