;; -*- lexical-binding: t; -*-

(use-package stream)

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


