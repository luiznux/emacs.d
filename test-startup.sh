#!/bin/sh -e
echo "Attempting startup..."
                  mkdir -p /home/runner/org/{agenda,work,personal,roam}; \
                  emacs -q --batch \
                  --eval '(progn
                            (let ((debug-on-error t)
                                  (user-emacs-directory default-directory)
                                  (early-init-file (expand-file-name "early-init.el"))
                                  (user-init-file (expand-file-name "init.el")))
                                (load-file early-init-file)
                                (run-with-timer 30 nil (lambda () (setq gc-cons-threshold 500000000)))
                                (load-file user-init-file)))'
echo "Startup successful!"
