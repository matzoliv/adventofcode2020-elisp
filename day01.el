;; -*- lexical-binding: t; -*-

(defun aoc20/day01-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cl-loop while (search-forward-regexp "^[0-9]+$" nil t)
	     collect (string-to-number (match-string 0)))))

(defun aoc20/day01-1 ()
  (let ((numbers (aoc20/day01-read-input (find-file-noselect "day01.txt"))))
    (-let (((x y)
	    (seq-find
	     (-lambda ((x y))
	       (= (+ x y) 2020))
	     (stream-cartesian-product (list numbers numbers)))))
      (* x y))))

(defun aoc20/day01-2 ()
  (let ((numbers (aoc20/day01-read-input (find-file-noselect "day01.txt"))))
    (-let (((x y z)
	    (seq-find
	     (-lambda ((x y z))
	       (= (+ x y z) 2020))
	     (stream-cartesian-product (list numbers numbers numbers)))))
      (* x y z))))

(aoc20/day01-1)
(aoc20/day01-2)

