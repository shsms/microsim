((lisp-mode . ((eval . (progn
                         (if (not (boundp 'tulisp-etags-setup-done))
                             (let* ((project-root-dir (expand-file-name (project-root (project-current))))
                                   (tags-file (expand-file-name "TAGS" project-root-dir)))
                               (setq-local tulisp-etags-setup-done t)

                               (if (file-exists-p tags-file)
                                   (delete-file tags-file))

                               ;; Set up etags-regen for this project
                               (setq-local etags-regen-program "cargo")
                               (setq-local etags-regen-program-options
                                           `("run" "-q" "--bin" "microsim-etags" ,project-root-dir))
                               (etags-regen-mode 1)
                               (tags-reset-tags-tables))))))))
