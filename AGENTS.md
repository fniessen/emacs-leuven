# Project Guidelines

## Project structure

- `lisp/` contains the Emacs Lisp configuration modules; `site-lisp/` contains
  bundled third-party and supporting libraries.
- `org/` contains notes and reference material. The main configuration sources
  are the `.txt` files in the repository root.
- Files in `lisp/` marked as generated name their source file in a header.
  Edit that source and use `make tangle` rather than editing generated output.

## Changes

- Follow the surrounding Emacs Lisp style and preserve the existing key-binding
  and configuration behavior unless the change explicitly calls for otherwise.
- Keep changes focused; do not modify bundled libraries or generated files
  unless required.

## Org mode headers

- Recursively scan `.txt` files from the Git repository root. Ensure each file
  starts with this exact line, adding it only when missing:

  ```text
  # Hey Emacs, this is a -*- org -*- file ...
  ```

- Preserve the remaining content and existing line endings. Do not follow
  symbolic links or modify files outside the repository.

## Validation

- `make check-parens` checks Emacs Lisp syntax structure.
- `make test` runs the ERT tests.
- `make compile` byte-compiles the Emacs Lisp modules.
- `make check` runs the source checks, including HTTP(S) link checks.
- `make lint-shell` checks shell scripts and requires ShellCheck.
- `make clean` removes generated `.elc` and editor backup files.
