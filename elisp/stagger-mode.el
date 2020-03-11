;;; stagger-mode --- stagger the case in a buffer
;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; !Ref Spongebob meme
(require 'cl-lib)

;;; Code:
(define-minor-mode stagger-mode
  "StAgGeR ThE CaSe iN ThE BuFfeR."
  :lighter " stagger"
  (add-to-list 'after-change-functions #'stagger-mode--do-it))

(defun stagger-mode--do-it (beg end _)
  "Inner function for actually performing staggers.  Start at (BEG) and end at (END)."
  (when stagger-mode
    (save-excursion
      (cl-loop for pos from beg below end
               for char = (char-after pos)
               for upcase = (upcase char)
               when (if (cl-oddp pos) (eql char upcase) (not (eql char upcase)))
               do (progn (setf (point) pos)
                         (delete-char 1)
                         (insert-before-markers upcase))))))

(provide 'stagger-mode)
;;; stagger-mode ends here
