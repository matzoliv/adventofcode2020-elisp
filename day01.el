;; -*- lexical-binding: t; -*-

(defun aoc20/day01-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cl-loop while (search-forward-regexp "^[0-9]+$" nil t)
	     collect (string-to-number (match-string 0)))))

(byte-compile 'aoc20/day01-read-input)

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

(byte-compile 'aoc20/day01-2)

(setq numbers (aoc20/day01-read-input (find-file-noselect "day01.txt")))

(insert
 (pp
  (benchmark-run-compiled
      1
    (seq-length (stream-cartesian-product-breadth-first (list numbers numbers numbers))))))

(180.834676502 135 154.983854016)



(58.714495869 37 48.923379607000015)
(129.043733292 324 88.08161436199998)


(aoc20/day01-1)
(aoc20/day01-2)

