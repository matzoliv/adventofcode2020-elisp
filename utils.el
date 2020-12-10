;; -*- lexical-binding: t; -*-

(use-package stream)

(defun stream-sub-vector (v offset count)
  (letrec ((step
            (lambda (offset count)
              (if (> count 0)
                  (stream-cons
                   (aref v offset)
                   (funcall step (+ offset 1) (- count 1)))
                (stream-empty)
                ))))
    (funcall step offset count)))

(defun stream-cartesian-product (init-seqs-arg)
  (let ((init-seqs (apply #'vector init-seqs-arg)))
    (letrec ((step
	      (lambda (state)
		(stream-cons
		 (seq-map #'seq-first state)
		 (let ((next-state (copy-sequence state))
		       (overflow t))
		   (cl-loop for i from (- (length state) 1) downto 0
			    do (let* ((s (aref next-state i))
				      (r (seq-rest s)))
				 (if (seq-empty-p r)
				     (aset next-state i (aref init-seqs i))
				   (progn
				     (aset next-state i r)
				     (setq overflow nil)
				     (cl-return)))))
		   (if overflow
		       (stream-empty)
		     (funcall step next-state)))))))
      (funcall step init-seqs))))

(defun aoc20/lines-stream ()
  (stream-cons
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))
   (progn
     (if (eq (forward-line 1) 1)
	 nil
       (aoc20/lines-stream)))))
