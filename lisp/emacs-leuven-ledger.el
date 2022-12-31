;; Packages to be installed by the user.
(add-to-list 'package-selected-packages 'ledger-mode)

;; Helper code for use with the "ledger" command-line tool.
(add-to-list 'auto-mode-alist '("\\.dat\\'"     . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'"  . ledger-mode))

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
  (setq ledger-default-date-format "%Y/%m/%d"))

(provide 'emacs-leuven-ledger)

;; This is for the sake of Emacs.
;; Local Variables:
;; coding: utf-8-unix
;; flycheck-emacs-lisp-initialize-packages: t
;; flycheck-mode: nil
;; ispell-local-dictionary: "american"
;; End:

;;; ledger-leuven.el ends here
