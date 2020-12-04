;; -*- lexical-binding: t; -*-

(defun aoc20/day02-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cl-loop while (search-forward-regexp "^\\([0-9]+\\)-\\([0-9]+\\) \\(.\\): \\(.*\\)$" nil t)
	     collect (let ((ht (make-hash-table)))
		       (puthash :min (string-to-number (match-string 1)) ht)
		       (puthash :max (string-to-number (match-string 2)) ht)
		       (puthash :char (aref (match-string 3) 0) ht)
		       (puthash :password (match-string 4) ht)
		       ht))))

(defun aoc20/valid-password-def-1? (password-def)
  (-let* (((&hash :min min :max max :char char :password password) password-def) 
	  (n (->> password
		  (seq-filter (-partial #'= char))
		  seq-length)))
    (and (<= n max) (>= n min))))

(defun aoc20/day02-1 ()
  (let ((passwords (aoc20/day02-read-input (find-file-noselect "day02.txt"))))
    (cl-loop for x in passwords
	     count (aoc20/valid-password-def-1? x))))

(defun aoc20/valid-password-def-2? (password-def)
  (-let (((&hash :min min :max max :char char :password password) password-def))
    (xor (= char (aref password (- min 1)))
	 (= char (aref password (- max 1))))))

(defun aoc20/day02-2 ()
  (let ((passwords (aoc20/day02-read-input (find-file-noselect "day02.txt"))))
    (cl-loop for x in passwords
	     count (aoc20/valid-password-def-2? x))))

(aoc20/day02-1)
(aoc20/day02-2)
