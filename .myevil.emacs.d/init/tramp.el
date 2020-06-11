;;; tramp.el -*- lexical-binding: t; -*-

;;
;; Tramp
;;

(use-package tramp
  :pin manual
  :defer t
  :bind (("C-x C-r" . sudo-edit-current-file)
         )
  :config
  (setq-default
   ;;tramp-verbose 2 ; warnings
   tramp-verbose 3 ; + connections (default)
   ;;tramp-verbose 6 ; verbose
   ;;tramp-verbose 10
   ;;tramp-connection-timeout 10
   tramp-connection-timeout 14400 ; 4h
   password-cache-expiry nil
   ;;tramp-chunksize 4050

   ;; Simpler prompt when `TERM=dump ssh ...`
   ;;   https://www.emacswiki.org/emacs/TrampMode#toc9
   ;;
   ;; bash: [[ "$TERM" == "dumb" ]] && { export PS1='$ '; return; }
   ;;   can be added to: ~/.bashrc, /etc/bash.bashrc, /etc/profile ...
   ;;
   ;; zsh: see ~/.zshrc
   ;;
   tramp-shell-prompt "$ "

   tramp-use-ssh-controlmaster-options nil
   ;;tramp-use-ssh-controlmaster-options t
   ;;tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=~/.ssh/tramp.%%C -o ControlPersist=no"

   tramp-remote-path '(tramp-own-remote-path
                       "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin"
                       "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
                       "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin"
                       "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")

   )

  ;;
  ;; Custom 'ssha' hop: '/ssha:user@addr:/home/user'
  ;; - Enables ssh agent forwarding
  ;; - Forces control master (the default disables it when multi-hop, even when first hop)
  ;;
  ;; See /usr/share/emacs/26.1/lisp/net/tramp-sh.el.gz
  ;;
  (add-to-list
   'tramp-methods
   '("ssha"
     (tramp-login-program        "ssh")
     (tramp-login-args           (("-l" "%u") ("-p" "%p") ; ("%c")

                                  ;; Force control master
                                  ("-o" "ControlMaster=auto" "-o" "ControlPath=~/.ssh/tramp.%%C" "-o" "ControlPersist=no")
                                  ;; Forward agent
                                  ("-A")

                                  ("-e" "none") ("%h")))
     (tramp-async-args           (("-q")))
     (tramp-remote-shell         "/bin/sh")
     (tramp-remote-shell-login   ("-l"))
     (tramp-remote-shell-args    ("-c"))))

  ;;
  ;; Custom 'sua' hop
  ;;   sudo with ssh agent forwarding
  ;;   see ~/bin/sua
  ;;
  (add-to-list
   'tramp-methods
   '("sua"
     (tramp-login-program        "~/bin/sua")
     ;; The password template must be masked.  Otherwise, it could be
     ;; interpreted as password prompt if the remote host echoes the command.
     (tramp-login-args           (("%u")
                                  ("-p" "P\"\"a\"\"s\"\"s\"\"w\"\"o\"\"r\"\"d\"\":")))
     ;; Local $SHELL could be a nasty one, like zsh or fish.  Let's override it.
     (tramp-login-env            (("SHELL") ("/bin/sh")))
     (tramp-remote-shell         "/bin/sh")
                                        ;(tramp-remote-shell-login   ("-l"))
     (tramp-remote-shell-args    ("-c"))
     (tramp-connection-timeout   10)))

  ;;
  ;; Open in tramp sudo.
  ;;   https://www.emacswiki.org/emacs/TrampMode#toc30
  ;; CHANGELOG:
  ;;   - modified to work in dired too
  ;;
  (defun sudo-edit-current-file ()
    (interactive)
    (require 'tramp)
    (let ((position (point))
          (fname (or buffer-file-name
                     dired-directory)))
      (find-alternate-file
       (if (file-remote-p fname)
           (let ((vec (tramp-dissect-file-name fname)))
             (tramp-make-tramp-file-name
              "sudo"
              (tramp-file-name-user vec)
              (tramp-file-name-host vec)
              (tramp-file-name-localname vec)))
         (concat "/sudo:root@localhost:" fname)))
      (goto-char position)))

)

;; might help tramp
;;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

