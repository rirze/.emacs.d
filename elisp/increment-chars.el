;; -*- lexical-binding: t; -*-
(defun increment-char-at-point (&optional arg)
  "Increment number or character at point."
  (interactive "*p")
  (condition-case nil
      (save-excursion
        (let ((chr  (+ (if arg arg 1) (char-after))))
          (unless (characterp chr) (error "Cannot increment char by one"))
          (delete-char 1)
          (insert chr)))
    (error (error "No character at point"))))


(defun increment-number-or-char-at-point (&optional arg)
  "Increment number or character at point."
  (interactive "*p")
  (let ((nump  nil))
    (save-excursion
      (skip-chars-backward "0123456789")
      (when (looking-at "[0123456789]+")
        (replace-match (number-to-string (+ (if arg arg 1) (string-to-number (match-string 0)))))
        (setq nump  t)))
    (unless nump
      (save-excursion
        (condition-case nil
            (increment-char-at-point arg))))))


(defun decrement-char-at-point (&optional arg)
  "Decrement number or character at point."
  (interactive)
  (increment-char-at-point (if arg (- arg) -1)))


(defun decrement-number-or-char-at-point (&optional arg)
  "Decrement number or character at point."
  (interactive "*p")
  (increment-number-or-char-at-point (- arg)))


(provide 'increment-chars)
