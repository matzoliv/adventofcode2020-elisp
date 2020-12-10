(defun aoc20/day09-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (seq-into
     (seq-map #'cl-parse-integer (aoc20/lines-stream))
     'vector)))

(defun aoc20/day09-1 ()
  (let ((input (aoc20/day09-read-input (find-file-noselect "day09.txt"))))
    (cl-loop for i from 25 to (- (length input) 1)
             do (let* ((n (aref input i))
                       (pair (seq-find (-lambda ((x y))
                                         (= (+ x y) n))
                                       (stream-cartesian-product
                                        (list
                                         (stream-sub-vector input (- i 25) 25)
                                         (stream-sub-vector input (- i 25) 25)))
                                       nil)))
                  (unless pair
                    (cl-return n))))))

(byte-compile 'aoc20/day09-1)

(setq number (aoc20/day09-1))

(defun aoc20/day09-2 ()
  (let ((input (aoc20/day09-read-input (find-file-noselect "day09.txt"))))
    (cl-loop for i from 0 to (- (length input) 1)
             do (let ((maybe-j
                       (cl-loop for j from i to (- (length input) 1)
                                with sum = 0
                                while (< sum number)
                                do (setq sum (+ sum (aref input j)))
                                finally return (and (= sum number) j))))
                  (if maybe-j
                      (cl-return
                       (+ (seq-min (stream-sub-vector input i (+ (- maybe-j i) 1)))
                          (seq-max (stream-sub-vector input i (+ (- maybe-j i) 1))))))))))

(byte-compile 'aoc20/day09-2)
(aoc20/day09-2)
