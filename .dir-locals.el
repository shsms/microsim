((lisp-mode . ((eval . (progn
                         (if (not (boundp 'tulisp-etags-setup-done))
                             (let* ((project-root-dir (expand-file-name (project-root (project-current))))
                                   (tags-file (expand-file-name "TAGS" project-root-dir)))
                               (setq-local tulisp-etags-setup-done t)

                               (if (file-exists-p tags-file)
                                   (delete-file tags-file))

                               ;; Custom tags program
                               (setq-local etags-regen-program "cargo")
                               (setq-local etags-regen-program-options
                                           `("run" "-q" "--bin" "microsim-etags" ,project-root-dir))

                               (setq-local etags-regen-file-extensions
                                           '("rs" "lisp" "el"))

                               (etags-regen-mode 1)
                               (tags-reset-tags-tables)

                               (add-hook 'after-save-hook
                                         (lambda ()
                                           (when (and buffer-file-name
                                                      (member (file-name-extension buffer-file-name)
                                                              etags-regen-file-extensions)
                                                      (file-in-directory-p buffer-file-name
                                                                           project-root-dir)
                                                      (fboundp 'etags-regen--update-file))
                                             (ignore-errors
                                               (etags-regen--update-file buffer-file-name))))
                                         nil t))))))))
