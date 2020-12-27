(in-package :day-4)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-string *input-file*))

(defun part-1 ()
  (let ((passports (cl-ppcre:split "\\n\\n" (uiop:read-file-string "input"))))
    (loop :for p :in passports
          :for byr = (nth-value 1 (cl-ppcre:scan-to-strings "byr:(\\S+)\\b" p))
          :for iyr = (nth-value 1 (cl-ppcre:scan-to-strings "iyr:(\\S+)\\b" p))
          :for eyr = (nth-value 1 (cl-ppcre:scan-to-strings "eyr:(\\S+)\\b" p))
          :for hgt = (nth-value 1 (cl-ppcre:scan-to-strings "hgt:(\\S+)\\b" p))
          :for hcl = (nth-value 1 (cl-ppcre:scan-to-strings "hcl:(\\S+)\\b" p))
          :for ecl = (nth-value 1 (cl-ppcre:scan-to-strings "ecl:(\\S+)\\b" p))
          :for pid = (nth-value 1 (cl-ppcre:scan-to-strings "pid:(\\S+)\\b" p))
          :for cid = (nth-value 1 (cl-ppcre:scan-to-strings "cid:(\\S+)\\b" p))
          :with count = 0
          :when (and byr iyr eyr hgt hcl ecl pid)
            :do
               (incf count)
          :finally (return count))))


(defun validp (&key byr iyr eyr hgt hcl ecl pid)
  (flet ((valid-byr-p (byr)
           (<= 1920 (parse-integer byr) 2002))
         (valid-iyr-p (iyr)
           (<= 2010 (parse-integer iyr) 2020))
         (valid-eyr-p (eyr)
           (<= 2020 (parse-integer eyr) 2030))
         (valid-hgt-p (hgt)
           (or (and (search "cm" hgt) (<= 150 (parse-integer (subseq hgt 0 (search "cm" hgt))) 193))
               (and (search "in" hgt) (<= 59 (parse-integer (subseq hgt 0 (search "in" hgt))) 76))))
         (valid-hcl-p (hcl)
           (ppcre:scan-to-strings "^#([a-f]|[0-9]){6}$" hcl))
         (valid-ecl-p (ecl)
           (ppcre:scan-to-strings "^(amb|blu|brn|gry|grn|hzl|oth){1}$" ecl))
         (valid-pid-p (pid)
           (ppcre:scan-to-strings "^[0-9]{9}$" pid)))
    (and
         (valid-byr-p byr)
         (valid-iyr-p iyr)
         (valid-eyr-p eyr)
         (valid-hgt-p hgt)
         (valid-hcl-p hcl)
         (valid-ecl-p ecl)
         (valid-pid-p pid))))
  
(defun part-2 ()
  (let ((passports (cl-ppcre:split "\\n\\n" (uiop:read-file-string "input"))))
    (loop :for p :in passports
          :for byr = (nth-value 1 (cl-ppcre:scan-to-strings "byr:(\\S+)\\b" p))
          :for iyr = (nth-value 1 (cl-ppcre:scan-to-strings "iyr:(\\S+)\\b" p))
          :for eyr = (nth-value 1 (cl-ppcre:scan-to-strings "eyr:(\\S+)\\b" p))
          :for hgt = (nth-value 1 (cl-ppcre:scan-to-strings "hgt:(\\S+)\\b" p))
          :for hcl = (nth-value 1 (cl-ppcre:scan-to-strings "hcl:(\\S+)\\b" p))
          :for ecl = (nth-value 1 (cl-ppcre:scan-to-strings "ecl:(\\S+)\\b" p))
          :for pid = (nth-value 1 (cl-ppcre:scan-to-strings "pid:(\\S+)\\b" p))
          :for cid = (nth-value 1 (cl-ppcre:scan-to-strings "cid:(\\S+)\\b" p))
          :with count = 0
          :when (and
                 byr iyr eyr hgt hcl ecl pid
                 (validp
                   :byr (elt byr 0)
                   :iyr (elt iyr 0)
                   :eyr (elt eyr 0)
                   :hgt (elt hgt 0)
                   :hcl (elt hcl 0)
                   :ecl (elt ecl 0)
                   :pid (elt pid 0)))
            :do
               (incf count)
          :finally (return count))))
