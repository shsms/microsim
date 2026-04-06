((lisp-mode . ((eval . (progn
                         (if (not (boundp 'tulisp-etags-setup-done))
                             (progn
                               (setq-local tulisp-etags-setup-done t)
                               (tags-reset-tags-tables)
                               ;; Set up etags-regen for this project
                               (setq-local etags-regen-program "cargo")
                               (setq-local etags-regen-program-options
                                           `("run" "--bin" "microsim-etags" ,(expand-file-name (project-root (project-current)))))
                               (etags-regen-mode 1))))))))
