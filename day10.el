(defun aoc20/day10-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (seq-into
     (seq-map #'cl-parse-integer (aoc20/lines-stream))
     'list)))

(defun aoc20/day10-1 ()
  (let* ((input (aoc20/day10-read-input (find-file-noselect "day10.txt")))
         (sorted (seq-into (sort input #'<) 'vector)))
    (cl-loop for i from 0 to (- (length sorted) 1)
             with last = 0
             with diff-1 = 0
             with diff-3 = 0
             do (progn
                  (pcase (- (aref sorted i) last)
                    (1 (setq diff-1 (+ diff-1 1)))
                    (3 (setq diff-3 (+ diff-3 1))))
                  (setq last (aref sorted i)))
             finally return (* diff-1 (+ diff-3 1)))))

(aoc20/day10-1)

(defun aoc20/day10-2 ()
  (let* ((input (aoc20/day10-read-input (find-file-noselect "day10.txt")))
         (input-list (sort (append (cons 0 (seq-into input 'list))
                                   (list (+ (seq-max input) 3)))
                           #'<))
         (memoized (make-hash-table :test #'equal)))
    (letrec ((count
              (lambda (adapts)
                (if (not (cdr adapts))
                    1
                  (let ((adapt (car adapts)))
                    (or (gethash adapts memoized)
                        (let* ((sub-counts
                                (seq-map (lambda (adapts-rest)
                                           (funcall count adapts-rest))
                                         (cl-loop with it = (cdr adapts)
                                                  while (and it (<= (- (car it) adapt) 3))
                                                  collect (let ((prev-it it))
                                                            (setq it (cdr it))
                                                            prev-it))))
                               (n (apply #'+ sub-counts)))
                          (puthash adapts n memoized)
                          n)))))))
      (funcall count input-list))))

(byte-compile 'aoc20/day10-2)

(aoc20/day10-2)
