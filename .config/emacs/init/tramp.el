;;; tramp.el -*- lexical-binding: t; -*-

;;
;; Tramp
;;

(use-package tramp
  :pin manual
  :defer t
  :bind (
         ("C-x C-r" . doom/sudo-this-file)
         )
  :config

  (require 'tramp-cmds) ;; workaround missing autoloads for tramp-taint-remote-process-buffer etc.

  (setq-default
   ;; tramp-verbose 1 ; errors
   ;; tramp-verbose 2 ; warnings
   tramp-verbose 3 ; connections (default)
   ;; tramp-verbose 6 ; transfers

   tramp-connection-timeout 10 ; Timeout for establishing connection
   password-cache-expiry 1800

   ;; Use a ssh ControlMaster unique to tramp, to each remote host, and to each emacs process
   tramp-use-ssh-controlmaster-options t
   ;; tramp-use-ssh-controlmaster-options nil
   tramp-ssh-controlmaster-options (concat "-o ControlMaster=auto -o ControlPath=~/.ssh/tramp." (number-to-string (emacs-pid)) "%%C -o ControlPersist=1800")
   ;; tramp-ssh-controlmaster-options "-S none"

   tramp-remote-path '(
                       tramp-own-remote-path
                       "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin"
                       "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
                       "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin"
                       "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")

   )

  ;;
  ;; Faster tramp
  ;;   https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  ;;
  (setq-default
   ;; Don't create lock files for remote files
   remote-file-name-inhibit-locks t
   ;; ;; Use scp to copy directly between two remote hosts
   ;; tramp-use-scp-direct-remote-copying t
   ;; Don't auto-save remote files
   remote-file-name-inhibit-auto-save-visited t
   ;; Size before running out-of-band copy
   tramp-copy-size-limit (* 1 1024 1024)
   ;; Quick fix "gzip: stdin: unexpected end of file" !?
   tramp-inline-compress-start-size nil
   )

  ;; Use ssh controlmaster for compilation too
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

  ;;
  ;; Use direct async processes
  ;;   https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  ;;   https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1
  ;;
  ;; Declare an async profile
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  ;; Enable async profile for scp and ssh
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (connection-local-set-profiles
    '(:application tramp :protocol "ssh")
    'remote-direct-async-process)
  ;; Maybe fix DOS eol with magit
  (setq-default magit-tramp-pipe-stty-settings 'pty)


  ;;
  ;; Simpler prompt when `TERM=dump ssh ...`
  ;;   https://www.emacswiki.org/emacs/TrampMode#toc9
  ;;
  ;; bash: [[ "$TERM" == "dumb" ]] && { export PS1='$ '; return; }
  ;;   can be added to: ~/.bashrc, /etc/bash.bashrc, /etc/profile ...
  ;;
  ;; zsh: see ~/.zshrc
  ;;
  ;; (setq-default tramp-shell-prompt "$ ")
  ;; (setq
  ;;  ;; shell-prompt-pattern "$ "
  ;;  tramp-shell-prompt-pattern
  ;;  (concat ;; "\\(?:^\\|\r\\)"
  ;;          ;; "[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"
  ;;          "^[#$]+ "
  ;;          ))
  ;;
  (add-to-list 'tramp-remote-process-environment "TERM=dumb")
  (add-to-list 'tramp-remote-process-environment "GIT_PAGER=cat")

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

  ;; Open in tramp sudo.
  ;;   https://github.com/doomemacs/doomemacs/blob/master/lisp/lib/files.el
  ;; See also:
  ;;   https://www.emacswiki.org/emacs/TrampMode#toc30
  ;;
  (defun doom--sudo-file-path (file)
    (let ((host (or (file-remote-p file 'host) "localhost")))
      (concat "/" (when (file-remote-p file)
                    (concat (file-remote-p file 'method) ":"
                            (if-let (user (file-remote-p file 'user))
                                (concat user "@" host)
                              host)
                            "|"))
              "sudo:root@" host
              ":" (or (file-remote-p file 'localname)
                      file))))
  (defun doom/sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    ;; HACK: Disable auto-save in temporary tramp buffers because it could trigger
    ;;   processes that hang silently in the background, making those buffers
    ;;   inoperable for the rest of that session (Tramp caches them).
    (let ((auto-save-default nil)
          ;; REVIEW: use only these when we drop 28 support
          (remote-file-name-inhibit-auto-save t)
          (remote-file-name-inhibit-auto-save-visited t))
      (find-file (doom--sudo-file-path (expand-file-name file)))))
  (defun doom/sudo-this-file ()
    "Open the current file as root."
    (interactive)
    (doom/sudo-find-file
     (or (buffer-file-name (buffer-base-buffer))
         (when (or (derived-mode-p 'dired-mode)
                   (derived-mode-p 'wdired-mode))
           default-directory)
         (user-error "Cannot determine the file path of the current buffer"))))

  ;; (require 'docker-tramp)
  ;; (require 'docker-tramp-compat)

)

;; (use-package docker-tramp
;;   :disabled t
;;   :load-path (my-packages-directory "docker-tramp")
;;   :custom (docker-tramp-docker-executable "podman")
;;   ;; :after tramp
;;   )

;; might help tramp
;;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")
(setenv "GIT_PAGER" "cat")
