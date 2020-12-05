;; -*- lexical-binding: t; -*-

(defun aoc20/day05-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (seq-into
     (aoc20/lines-stream)
     'list)))

(defun aoc20/day05-get-seat-position (code)
  (cl-loop for c across code 
	   with row-min = 0
	   with row-max = 128
	   with column-min = 0
	   with column-max = 8
	   do (case c
		  ((?F) (setq row-max (+ row-min (/ (- row-max row-min) 2))))
		  ((?B) (setq row-min (- row-max (/ (- row-max row-min) 2))))
		  ((?L) (setq column-max (+ column-min (/ (- column-max column-min) 2))))
		  ((?R) (setq column-min (- column-max (/ (- column-max column-min) 2)))))
	   finally (cl-return (list row-min column-min))))

(defun aoc20/get-seat-id (x)
  (-let (((row column) x))
    (+ (* row 8) column)))

(defun aoc20/day05-1 ()
  (cl-loop for seat in (aoc20/day05-read-input (find-file-noselect "day05.txt"))
	   maximize (-> seat
			aoc20/day05-get-seat-position
			aoc20/get-seat-id)))

(defun aoc20/day05-2 ()
  (let* ((seats
	  (-map #'aoc20/day05-get-seat-position
		(aoc20/day05-read-input (find-file-noselect "day05.txt"))))
	 (free-seats
	  (cl-set-difference
	   (seq-into (stream-cartesian-product (list (stream-range 1 127) (stream-range 0 8))) 'list)
	   seats
	   :test #'equal))
	 (seats-by-id
	  (let ((seats-by-id (make-hash-table :test #'equal)))
	    (seq-each
	     (-lambda (seat)
	       (puthash (aoc20/get-seat-id seat) seat seats-by-id))
	     seats)
	    seats-by-id)))
    (-let ((seat
	    (seq-find
	     (-lambda (seat)
	       (let ((id (aoc20/get-seat-id seat)))
		 (and (gethash (- id 1) seats-by-id)
		      (gethash (+ id 1) seats-by-id))))
	     free-seats)))
      (aoc20/get-seat-id seat))))

(aoc20/day05-1)
(aoc20/day05-2)

