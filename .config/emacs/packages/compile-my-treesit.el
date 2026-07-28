#!/usr/bin/env -S emacs -Q -batch -l
;;
;; Compile custom tree-sitter repos
;;

(defconst my-treesit-source-alist
  '(
   ;; (LANG URL REVISION SOURCE-DIR CC C++)
   (nix "tree-sitter-nix")
   ))

;; Make absolute path
(let* ((current-dir (file-name-directory load-file-name))
       (my-source-alist
        (mapcar (lambda (r)
                  `(
                    ,(car r)
                    ,(expand-file-name (cadr r) current-dir)
                    ,@(cddr r)
                    )) my-treesit-source-alist)
        ))

  (message "%s" my-source-alist)

  ;; Install grammars
  (setq treesit-language-source-alist my-source-alist)
  (dolist (ls treesit-language-source-alist)
    (treesit-install-language-grammar (car ls)))
)
