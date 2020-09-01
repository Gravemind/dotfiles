;;; modeline.el -*- lexical-binding: t; -*-
;;

;;
;; modeline (mode-line):
;;   wiki: https://www.emacswiki.org/emacs/ModeLineConfiguration
;;   doc: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;;   example: http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line
;;

(defface mode-line-buffer-id-modified
  '((t :inherit mode-line-buffer-id :foreground "orange"))
  "mode-line-buffer-id when buffer is modified."
  :group 'mode-line-faces
  :group 'basic-faces)

(defvar my/mode-line-buffer
  '(:eval (propertize
           "%b"
           'face (if (buffer-modified-p) 'mode-line-buffer-id-modified 'mode-line-buffer-id)
           ))
  "My mode-line-buffer-id.")
(put 'my/mode-line-buffer 'risky-local-variable t)

;; dired-k.el:197
(defsubst my/project-directory (file)
  ;; (or (locate-dominating-file file ".git")
  ;;     (locate-dominating-file file ".svn"))
  ;; FIXME: too slow! cache? use helm's?
  nil
)

(defvar my/mode-line-buffer-long
  '(:eval
    (let* ((bfname (or buffer-file-truename buffer-file-name dired-directory))
           (fname (when bfname (file-truename bfname))))
      (if fname
          (if-let ((_proj (my/project-directory fname)))
              (let* ((proj (directory-file-name (file-truename _proj)))
                     (proj-parent (directory-file-name (file-name-directory proj)))
                     (proj-name (file-name-nondirectory proj))
                     (sub-proj (file-relative-name fname proj)))
                (concat
                 (abbreviate-file-name proj-parent)
                 "/"
                 (propertize proj-name 'face 'mode-line-buffer-id)
                 (if (equal sub-proj "./") "" (concat "/" sub-proj)))
                )
            (abbreviate-file-name fname)
            )
        "%b")
      ))
  "My mode-line-buffer-id.")
(put 'my/mode-line-buffer-long 'risky-local-variable t)

(defvar my/mode-line-ro-indicator
  ;; !! if the icon/glyph is not from the main/first font directly, it will be slow to load !!
  ;; !!  is included in Hack, but NOT in DejaVu !!
  '(:eval (if buffer-read-only " " ""))
  "Mode line read-only indicator (awesome font).")
(put 'my/mode-line-ro-indicator 'risky-local-variable t)

;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline/7542
(defvar my/mode-line-left-margin-padd
  '(:eval
    (let* ((margins (window-margins))
           (left (car margins)))
      (if (and left (> left 0))
          ;;(propertize (format (format "%%%ds" left) " ") 'face 'default)
          (format (format "%%%ds" left) " ")
        "")))
  "Returns as much whitespaces as the window left margin")
(put 'my/mode-line-left-margin-padd 'risky-local-variable t)

(defun my/indent-info ()
  (require 'editorconfig)
  (require 'dtrt-indent)
  (concat
   ;; ↹ → ⇀ ⇁ ↦ ⇥
   (format "%s" (symbol-value (jo--indent-offset-var)))
   (if indent-tabs-mode "↹" "∙")
   (format "%s" tab-width)
   (if dtrt-indent-original-indent
       ".dtrt" "")
   (if (and editorconfig-properties-hash (> (hash-table-count editorconfig-properties-hash) 0))
       ".edco" "")
   ))

(defvar my/mode-line-indent-info
  '(:eval (my/indent-info))
  "Indentation info")
(put 'my/mode-line-indent-info 'risky-local-variable t)

(setq-default

 ;; Window margin padd here so it works for helm too (because helm use this first in its modeline)
 mode-line-buffer-identification '("" my/mode-line-ro-indicator my/mode-line-buffer)

 ;; Dummy variable where evil will insert its state tag
 my--evil-mode-line-placeholder ""

 mode-line-format
 '(""
   my/mode-line-left-margin-padd

   my--evil-mode-line-placeholder   ;; evil state. TODO needs to be at top-level, how do face it ?

   mode-line-buffer-identification  ;; buffer name
           ;; ro symbol
   ":%l:%c"                         ;; line column
   " %p"                            ;; scroll percent
   " " mode-line-mule-info          ;; encoding (%Z)
   " " my/mode-line-indent-info     ;; indent info
   ;; mode-line-client
   ;; mode-line-modified
   ;; mode-line-remote
   (vc-mode vc-mode)                ;; vc (branch)
   " " mode-line-modes              ;; modes
   mode-line-misc-info              ;; ?
   ;; mode-line-end-spaces          ;; trailing dashes in terminal mode
   )

 header-line-format
 '(""
   my/mode-line-left-margin-padd
   " "
   my/mode-line-ro-indicator
   my/mode-line-buffer-long
   )

 ;; frame title (X window title)
 frame-title-format '("" "%b")

 )

;; Don't let dired change the mode-line !
(add-hook 'dired-mode-hook (lambda () (setq mode-line-buffer-identification (default-value 'mode-line-buffer-identification))))

;; (defun my-update-header ()
;;   (mapc
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        ;; don't mess with buffers that don't have a header line
;;        (when header-line-format
;;          (let ((original-format (get 'header-line-format 'original))
;;                (inactive-face 'warning)) ; change this to your favorite inactive header line face
;;            ;; if we didn't save original format yet, do it now
;;            (when (not original-format)
;;              (put 'header-line-format 'original header-line-format)
;;              (setq original-format header-line-format))
;;            ;; check if this window is selected, set faces accordingly
;;            (if (eq window (selected-window))
;;                (setq header-line-format original-format)
;;              (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
;;    (window-list)))
;; (add-hook 'buffer-list-update-hook #'my-update-header)
