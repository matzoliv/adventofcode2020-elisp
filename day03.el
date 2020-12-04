;; -*- lexical-binding: t; -*-

(defun aoc20/day03-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (seq--into-vector
     (cl-loop while (not (= (point) (point-max)))
	      collect (let ((r (seq--into-vector
				(buffer-substring-no-properties
				 (line-beginning-position)
				 (line-end-position)))))
			(next-line)
			r)))))

(defun aoc20/count-tree (x-inc y-inc)
  (let* ((topo
	  (aoc20/day03-read-input (find-file-noselect "day03.txt")))
	 (height (length topo))
	 (width (length (aref topo 0))))
    (cl-loop while (< y height)
	     with y = 0
	     with x = 0
	     count (let ((c (aref (aref topo y) x)))
		     (setq y (+ y y-inc))
		     (setq x (mod (+ x x-inc) width))
		     (= c ?#)))))

(defun aoc20/day03-1 ()
  (aoc20/count-tree 3 1))

(defun aoc20/day03-2 ()
  (*
   (aoc20/count-tree 1 1)
   (aoc20/count-tree 3 1)
   (aoc20/count-tree 5 1)
   (aoc20/count-tree 7 1)
   (aoc20/count-tree 1 2)))

(aoc20/day03-1)
(aoc20/day03-2)
