;;; test.el --- test

;; Copyright (C) 2008  Aaron S. Hawley

;; Author: Aaron Hawley
;; Keywords: lisp

;;; Commentary:

;; Running M-x checkdoc on a function whose documentation string has
;; quotes.  Should get the error message: "forward-list: Scan error:
;; 'Unbalanced parentheses'".

;;; Code:

(defun function1 ()
  "Documentation with a \"string\"
This is the next line."
  (prin1 "Echo"))


(provide 'test)
;;; test.el ends here
