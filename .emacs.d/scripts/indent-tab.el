
(c++-mode)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 86 90))

(c-set-offset 'substatement-open 0)
(c-set-offset 'label 0)
(c-set-offset 'arglist-intro 4)
(c-set-offset 'arglist-close 0)
(c-set-offset 'brace-list-open 0)
(c-set-offset 'innamespace 4)
(setq c-basic-offset 4
      tab-width 4
      indent-tabs-mode t)

(defun iwb-save ()
  "Indent whole buffer and save buffer"
  (interactive)
  (delete-trailing-whitespace)
  (tabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max))
  (save-buffer)
  )
