;; -*- lexical-binding: t; -*-

(defun aoc20/day04-read-input (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (setq passport (make-hash-table :test #'equal))
    (setq passports '())
    (seq-each (lambda (line)
		(if (string-empty-p line)
		    (progn
		      (setq passports (cons passport passports))
		      (setq passport (make-hash-table :test #'equal)))
		  (seq-each (-lambda ((_ key value))
			      (puthash key value passport))
			    (-map (-partial #'s-match "\\([^:]+\\):\\([^ ]+\\)")
				  (s-split " " line)))))
	      (aoc20/lines-stream))
    (if (not (hash-table-empty-p passport))
	(cons passport passports)
      passports)))

(defvar passport-fields
  (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))

(defun aoc20/day04-1 ()
  (let ((passports
	 (aoc20/day04-read-input (find-file-noselect "day04.txt"))))
    (cl-loop for p in (reverse passports)
	     count (aoc20/passport-valid-1? p))))

(defun aoc20/passport-valid-1? (p)
  (let ((diff
	 (cl-set-difference passport-fields (hash-table-keys p) :test #'string-equal)))
    (or (not diff) (equal diff '("cid")))))

(defun aoc20/passport-valid-2? (p)
  (and
   (let ((byr (gethash "byr" p)))
     (and
      byr
      (s-match "^[0-9]\\{4\\}$" byr)
      (let ((n (cl-parse-integer byr)))
	(and (>= n 1920) (<= n 2002)))))
   (let ((iyr (gethash "iyr" p)))
     (and
      iyr
      (s-match "^[0-9]\\{4\\}$" iyr)
      (let ((n (cl-parse-integer iyr)))
	(and (>= n 2010) (<= n 2020)))))
   (let ((eyr (gethash "eyr" p)))
     (and
      eyr
      (s-match "^[0-9]\\{4\\}$" eyr)
      (let ((n (cl-parse-integer eyr)))
	(and (>= n 2020) (<= n 2030)))))
   (let* ((hgt (gethash "hgt" p))
	  (m (and hgt (s-match "^\\([0-9]+\\)\\(in\\|cm\\)$" hgt))))
     (and m
	  (-let* (((_ s unit) m)
		  (n (cl-parse-integer s)))
	    (cond
	     ((string-equal unit "cm") (and (>= n 150) (<= n 193)))
	     ((string-equal unit "in") (and (>= n 59) (<= n 76)))
	     (t nil)))))
   (let* ((hcl (gethash "hcl" p)))
     (and hcl
	  (s-match "^\\#[0-9a-f]+\\{4\\}$" hcl)))
   (let* ((ecl (gethash "ecl" p)))
     (and ecl
	  (s-match "^\\(amb\\|blu\\|brn\\|gry\\|grn\\|hzl\\|oth\\)$" ecl)))
   (let* ((pid (gethash "pid" p)))
     (and pid
	  (s-match "^[0-9]\\{9\\}$" pid)))))

(defun aoc20/day04-2 ()
  (let ((passports
	 (aoc20/day04-read-input (find-file-noselect "day04.txt"))))
    (cl-loop for p in (reverse passports)
	     count (aoc20/passport-valid-2? p))))

(aoc20/day04-1)
(aoc20/day04-2)

