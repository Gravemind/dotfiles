;;; bw-base.el ---
;; 
;; Filename: bw-base.el
;; Description:
;; Author: Lennart Borgman <lennart dot borgman dot 073 at student at lu at se>
;; Maintainer:
;; Created: Wed Dec 07 17:39:37 2005
;; Version:
;; Last-Updated: Thu Dec 08 15:52:13 2005 (3600 +0100)
;; Keywords:
;; Compatibility:
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; This is supposed to be incorporated into Emacs 22 in windows.el
;; some time soon. I keep it here just for the record and for testing.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;(setq bw-dbg t)  ;
;; ;;(setq bw-dbg nil)  ;
;; (defvar bw-dbg nil
;;   "If non-nil debugging is turned on.
;; This writes some messages to *Messages* buffer and use `sit-for'
;; to show what is happening during the balance phase.")

;; (defun bw-dbg-sit-for(seconds &optional nodisp)
;;   "`sit-for' if `bw-dbg'."
;;   (when bw-dbg
;;     (sit-for seconds nodisp)))

;; (defun bw-dbg-message(fmt &rest args)
;;   "`message' if `bw-dbg'."
;;   (when bw-dbg
;;     (let* ((str (apply 'format fmt args))
;;            (indent "")
;;            (len (length str)))
;;       (while (< 0 (length str))
;;         (setq len 140)
;;         (when (> len (length str)) (setq len (length str)))
;;         (message "%s%s" indent (substring str 0 len))
;;         (setq str (substring str len))
;;         (setq indent "    ")))))

;; (defun bw-dbg-checkobj(obj)
;;   "Check if OBJ is an object that can be resized here."
;;   (unless (or (windowp obj)
;;               (and (assq 'r obj)
;;                    (listp (assq 'childs obj))))
;;     (error "Bad obj=%s" obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New code for `balance-windows'

;;; Translate from internal window tree format

(defun bw-get-tree (&optional window-or-frame)
  "Get a window split tree in our format.

WINDOW-OR-FRAME must be nil, a frame or a window.  If it is nil
then the whole window split tree for `selected-frame' is
returned.  If it is a frame then this is used instead.  If it is
a window then the smallest tree containing that window is
returned."
  (when window-or-frame
    (unless (or (framep window-or-frame)
                (windowp window-or-frame))
      (error "Not a frame or window: %s" frame)))
  (let ((subtree (bw-find-tree-sub window-or-frame)))
    (if (integerp subtree)
        nil
      (bw-get-tree-1 subtree))))

(defun bw-get-tree-1 (split)
  (if (windowp split)
      split
    (let ((dir (car split))
          (edges (car (cdr split)))
          (childs (cdr (cdr split))))
      (list
       (cons 'dir (if dir 'ver 'hor))
       (cons 'b (nth 3 edges))
       (cons 'r (nth 2 edges))
       (cons 't (nth 1 edges))
       (cons 'l (nth 0 edges))
       (cons 'childs (mapcar #'bw-get-tree-1 childs))))))

(defun bw-find-tree-sub (window-or-frame &optional get-parent)
  (let* ((window (when (windowp window-or-frame) window-or-frame))
         (frame (when (windowp window) (window-frame window)))
         (wt (car (window-tree frame))))
    (when (< 1 (length (window-list frame 0)))
      (if window
          (bw-find-tree-sub-1 wt window get-parent)
        wt))))

(defun bw-find-tree-sub-1 (tree win &optional get-parent)
  (unless (windowp win) (error "Not a window: %s" win))
  (if (memq win tree)
      (if get-parent
          get-parent
        tree)
    (let ((childs (cdr (cdr tree)))
          child
          subtree)
      (while (and childs (not subtree))
        (setq child (car childs))
        (setq childs (cdr childs))
        ;;(bw-dbg-message "    child=%s" child)(bw-dbg-sit-for 2)
        (when (and child (listp child))
          (setq subtree (bw-find-tree-sub-1 child win get-parent))))
      (if (integerp subtree)
          (progn
            (if (= 1 subtree)
                tree
              (1- subtree)))
        subtree
        ))))



;;; Window or object edges

(defun bw-l(obj)
  "Left edge of OBJ."
  (if (windowp obj) (nth 0 (window-edges obj)) (cdr (assq 'l obj))))
(defun bw-t(obj)
  "Top edge of OBJ."
  (if (windowp obj) (nth 1 (window-edges obj)) (cdr (assq 't obj))))
(defun bw-r(obj)
  "Right edge of OBJ."
  (if (windowp obj) (nth 2 (window-edges obj)) (cdr (assq 'r obj))))
(defun bw-b(obj)
  "Bottom edge of OBJ."
  (if (windowp obj) (nth 3 (window-edges obj)) (cdr (assq 'b obj))))





;;; Split directions

(defun bw-dir(obj)
  "Return window split tree direction if OBJ.
If OBJ is a window return 'both. If it is a window split tree
then return its direction."
  (if (symbolp obj)
      obj
    (if (windowp obj)
        'both
      (let ((dir (cdr (assq 'dir obj))))
        (unless (memq dir '(hor ver both))
          (error "Can't find dir in %s" obj))
        dir))))

(defun bw-eqdir(obj1 obj2)
  "Return t if window split tree directions are equal.
OBJ1 and OBJ2 should be either windows or window split trees in
our format. The directions returned by `bw-dir' are compared and
t is returned if they are `eq' or one of them is 'both."
  (let ((dir1 (bw-dir obj1))
        (dir2 (bw-dir obj2)))
    ;;(bw-dbg-message "dir1=%s, dir2=%s" dir1 dir2)
    (or (eq dir1 dir2)
        (eq dir1 'both)
        (eq dir2 'both))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building split tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bw-refresh-edges(obj)
  "Refresh the edge information of OBJ and return OBJ."
  (unless (windowp obj)
    ;;(bw-dbg-message "bw-re obj=%s" obj)
    (let ((childs (cdr (assq 'childs obj)))
          (ol 1000)
          (ot 1000)
          (or -1)
          (ob -1))
      (dolist (o childs)
        (when (> ol (bw-l o)) (setq ol (bw-l o)))
        (when (> ot (bw-t o)) (setq ot (bw-t o)))
        (when (< or (bw-r o)) (setq or (bw-r o)))
        (when (< ob (bw-b o)) (setq ob (bw-b o))))
      (setq obj (delq 'l obj))
      (setq obj (delq 't obj))
      (setq obj (delq 'r obj))
      (setq obj (delq 'b obj))
      (add-to-list 'obj (cons 'l ol))
      (add-to-list 'obj (cons 't ot))
      (add-to-list 'obj (cons 'r or))
      (add-to-list 'obj (cons 'b ob))
      ;;(bw-dbg-checkobj obj)
      ;;(bw-dbg-message "bw-re 2 obj=%s" obj)
      ))
  obj)




;;; Balance windows

(defun balance-windows(&optional window-or-frame)
  "Make windows the same heights or widths in window split subtrees.

When called non-interactively WINDOW-OR-FRAME may be either a
window or a frame. It then balances the windows on the implied
frame. If the parameter is a window only the corresponding window
subtree is balanced."
  (interactive)
  (let (
        (wt
;;          (if (and (called-interactively-p)
;;                   current-prefix-arg)
;;              (bw-get-tree (selected-window))
           (bw-get-tree window-or-frame)
;;            )
         )
        (w)
        (h)
        (tried-sizes)
        (last-sizes)
        (windows (window-list nil 0))
        (counter 0))
    ;;(bw-dbg-message "")
    ;;(bw-dbg-message "wt=%s" wt)
    (when wt
      ;;(bw-dbg-checkobj wt)
      (while (not (member last-sizes tried-sizes))
        (when last-sizes (setq tried-sizes (cons last-sizes tried-sizes)))
        (setq last-sizes (mapcar (lambda(w)
                                   (window-edges w))
                                 windows))
        ;;(bw-dbg-message "loop in bw-balance")(bw-dbg-sit-for 1)
        (when (eq 'hor (bw-dir wt))
          (setq w (- (bw-r wt) (bw-l wt))))
        (when (eq 'ver (bw-dir wt))
          (setq h (- (bw-b wt) (bw-t wt))))
        (bw-balance-sub wt w h)))))

(defun bw-adjust-window(window delta horizontal)
  "Wrapper around `adjust-window-trailing-edge' with error checking.
Arguments WINDOW, DELTA and HORIZONTAL are passed on to that function."
  ;;(bw-dbg-message "adjust-window %s %s" delta horizontal)
  (condition-case err
      (progn
        (adjust-window-trailing-edge window delta horizontal)
        ;;(bw-dbg-message "adjust ok")
        )
    (error
     ;;(message "adjust: %s" (error-message-string err))
     )))

(defun bw-balance-sub(wt w h)
  ;;(bw-dbg-message ">>>> bw-balance-sub w=%s h=%s %s" w h wt)
  ;;(bw-dbg-checkobj wt)
  (setq wt (bw-refresh-edges wt))
  (unless w (setq w (- (bw-r wt) (bw-l wt))))
  (unless h (setq h (- (bw-b wt) (bw-t wt))))
  (if (windowp wt)
      (progn
        ;;(bw-dbg-message "***** window RESIZE %s w=%s h=%s ltrb=%s" wt w h (window-edges wt))
        (when w
          (let ((dw (- w (- (bw-r wt) (bw-l wt)))))
            ;;(bw-dbg-message "dw=%s edges=%s" dw (window-edges wt))
            (when (/= 0 dw)
                (bw-adjust-window wt dw t)
              ;;(bw-dbg-sit-for 1)
              )))
        (when h
          (let ((dh (- h (- (bw-b wt) (bw-t wt)))))
            ;;(bw-dbg-message "dh=%s edges=%s" dh (window-edges wt))
            (when (/= 0 dh)
              (bw-adjust-window wt dh nil)
              ;;(bw-dbg-sit-for 1)
              )
            ;;(bw-dbg-message "   => %s" (mapcar (lambda(elt) (window-edges elt)) (window-list)))
            )))
    (let* ((childs (cdr (assq 'childs wt)))
           (lastchild (car (last childs)))
           ;;(dummy (bw-dbg-message "Not window %s %s %s lc=%s" wt w h (length childs)))
           (cw (when w (/ w (if (bw-eqdir 'hor wt) (length childs) 1))))
           (ch (when h (/ h (if (bw-eqdir 'ver wt) (length childs) 1)))))
      (dolist (c childs)
          (bw-balance-sub c cw ch)))))

;;; End of new code for `balance-windows'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'bw-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bw-base.el ends here
