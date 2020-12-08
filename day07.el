(defun aoc20/day07-read-input (buffer)
  (defun read-relation ()
    (defun read-word ()
      (search-forward-regexp "\\w+")
      (match-string 0))
    (defun read-count ()
      (search-forward-regexp "[0-9]+")
      (cl-parse-integer (match-string 0)))
    (defun read-bag-desc ()
      (let ((adjective (read-word))
            (color (read-word)))
        ;; bags?
        (read-word)
        `((adjective . ,adjective)
          (color . ,color))))

    (let ((container (read-bag-desc)))
      ;; contain
      (read-word)
      `((container . ,container)
        (containees . ,(if (search-forward-regexp "no other bags." (line-end-position) t)
                           '()
                           (cl-loop while (not (string-equal (thing-at-point 'char t) "."))
                                    collect (let ((count (read-count))
                                                  (bag (read-bag-desc)))
                                              `((count . ,count)
                                                (bag . ,bag)))))))))
  
  (with-current-buffer buffer
    (goto-char (point-min))
    (cons (read-relation)
          (cl-loop while (and (= (forward-line 1) 0)
                              (not (eql (line-end-position) (point-max))))
                   collect (read-relation)))))

(defun aoc20/day07-1 ()
  (let ((relations-list (aoc20/day07-read-input (find-file-noselect "day07.txt")))
        (relations (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (seq-each (-lambda ((&alist 'container container
                                'containees containees))
                (seq-each (-lambda ((&alist 'count count 'bag bag))
                            (let ((containers (gethash bag relations '())))
                              (puthash bag (cons `((count . ,count) (bag . ,container)) containers) relations)))
                          containees))
              relations-list)
    (letrec ((count-containers
              (lambda (bag)
                (puthash bag t seen)
                (apply #'+ (-map (-lambda ((&alist 'bag container))
                                   (if (gethash container seen nil)
                                       0
                                     (+ 1 (funcall count-containers container))))
                                 (gethash bag relations))))))
      (funcall count-containers `((adjective . "shiny") (color . "gold"))))))

(aoc20/day07-1)

(defun aoc20/day07-2 ()
  (let ((relations-list (aoc20/day07-read-input (find-file-noselect "day07.txt")))
        (relations (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (seq-each (-lambda ((&alist 'container container
                                'containees containees))
                (puthash container containees relations))
              relations-list)
    (letrec ((count-bags
              (lambda (bag)
                (apply #'+ (-map (-lambda ((&alist 'bag bag 'count count))
                                   (+ count (* count (funcall count-bags bag))))
                                 (gethash bag relations))))))
      (funcall count-bags `((adjective . "shiny") (color . "gold"))))))

(aoc20/day07-2)
