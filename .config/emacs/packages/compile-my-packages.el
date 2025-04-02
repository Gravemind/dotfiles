#!/usr/bin/env -S emacs -Q -batch -l
;;
;; Compile all .el files in my-packages-load-path from my-packages.el
;;

(require 'bytecomp)

(let ((my-packages-el (expand-file-name "my-packages.el" (file-name-directory load-file-name))))

  ;; Compile and load my-packages.el
  (byte-compile-file my-packages-el)
  (load my-packages-el)

  ;; Set load-path
  (setq load-path (append my-packages-load-path load-path))

  ;; Compile all the files (non-recursively)
  (let* (
         (skip-count 0)
         (file-count 0)
         (fail-count 0)
         (byte-compile-ignore-files my-packages-byte-compile-ignore-files)
         (ignore-files-regexp
          (if byte-compile-ignore-files
              (mapconcat #'identity byte-compile-ignore-files "\\|")
            regexp-unmatchable))
         )
    (dolist (directory my-packages-load-path)
      (dolist (source (directory-files directory t))
        (let ((file (file-name-nondirectory source)))
          (if (and
               (string-match emacs-lisp-file-regexp source)
               (file-regular-p source)
               (not (string-match "\\`\\.#" file))
               (not (auto-save-file-name-p source))
               (not (member source (dir-locals--all-files directory)))
               (not (string-match-p ignore-files-regexp source))
               (not (file-symlink-p source))
               )
              (progn
                (cl-incf
                 (pcase (byte-recompile-file source nil 0)
                   ('no-byte-compile skip-count)
                   ('t file-count)
                   (_ fail-count)))
                ))
          ))
      )
    (message "Done (%d compiled, %d failed, %d skipped)" file-count fail-count skip-count)
    )
)
