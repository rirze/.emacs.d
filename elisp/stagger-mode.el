;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(define-minor-mode stagger-mode
  "StAgGeR ThE CaSe iN ThE BuFfeR."
  :lighter " stagger"
  (add-to-list 'after-change-functions #'stagger-mode--do-it))

(defun stagger-mode--do-it (beg end _)
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
