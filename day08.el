(defun aoc20/day08-parse-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (apply #'vector
           (cl-loop while (search-forward-regexp "\\(\\w+\\) \\([+-][0-9]+\\)" (line-end-position) t)
                    collect (progn
                              (forward-line 1)
                              `((opcode . ,(match-string 1))
                                (arg . ,(cl-parse-integer (match-string 2)))))))))

(defun aoc20/day08-run (program)
  (let ((visited (make-hash-table))
        (pc 0)
        (acc 0))
    (while (and (< pc (length program))
                (not (gethash pc visited))) 
      (-let (((&alist 'opcode opcode 'arg arg) (aref program pc)))
        (puthash pc t visited)
        (pcase opcode
          ("nop" (setq pc (+ pc 1)))
          ("acc" (setq acc (+ acc arg))
           (setq pc (+ pc 1)))
          ("jmp" (setq pc (+ pc arg))))))
    `(,acc ,(gethash pc visited))))

(defun aoc20/day08-1 ()
  (-let* ((program (aoc20/day08-parse-input (find-file-noselect "day08.txt")))
          ((acc loop?) (aoc20/day08-run program)))
    acc))

(aoc20/day08-1)

(defun aoc20/day08-2 ()
  (let ((program (aoc20/day08-parse-input (find-file-noselect "day08.txt"))))
    (cl-loop for i from 0 to (- (length program) 1)
             do (-let (((&alist 'opcode opcode 'arg arg) (aref program i)))
                  (unless (string-equal opcode "acc")
                    (let ((program-copy (copy-sequence program)))
                      (aset program-copy i `((opcode . ,(pcase opcode ("nop" "jmp") ("jmp" "nop")))
                                             (arg . ,arg)))
                      (-let (((acc loop?) (aoc20/day08-run program-copy)))
                        (unless loop?
                          (cl-return acc)))))))))

(aoc20/day08-2)
