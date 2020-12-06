(defun aoc20/day06-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (reverse
     (-map #'reverse
	   (seq-reduce (lambda (acc line)
			 (if (string-empty-p line)
			     (cons '() acc)
			   (cons (cons line (car acc))
				 (cdr acc))))
		       (aoc20/lines-stream)
		       '())))))

(defun aoc20/day06-1 ()
  (defun union-all (lists)
    (seq-reduce #'cl-union (cdr lists) (car lists)))
  
  (let ((input (aoc20/day06-read-input (find-file-noselect "day06.txt"))))
    (-sum
     (-map (-compose #'length
		     #'union-all
		     (-partial #'-map #'string-to-list))
	   input))))

(defun aoc20/day06-2 ()
  (defun intersection-all (lists)
    (seq-reduce #'cl-intersection (cdr lists) (car lists)))
  
  (let ((input (aoc20/day06-read-input (find-file-noselect "day06.txt"))))
    (-sum
     (-map (-compose #'length
		     #'intersection-all
		     (-partial #'-map #'string-to-list))
	   input))))


