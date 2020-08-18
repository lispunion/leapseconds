;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define-library (leapseconds)
  (export read-zoneinfo-leapseconds)
  (import (scheme base) (scheme char) (scheme file))
  (begin

    (define (split-at-whitespace s)
      (let loop ((fields '()) (a 0) (b 0))
        (define (field) (if (= a b) fields (cons (substring s a b) fields)))
        (cond ((= b (string-length s))
               (list->vector (reverse (field))))
              ((char-whitespace? (string-ref s b))
               (loop (field) (+ b 1) (+ b 1)))
              (else
               (loop fields a (+ b 1))))))

    (define (parse-zoneinfo-leapseconds-entry fields)
      (define months '(("Jun" 6 30) ("Dec" 12 31)))
      (define (bad) (error "Bad zoneinfo leapseconds entry:" fields))
      (unless (= 7 (vector-length fields)) (bad))
      (unless (string=? "Leap" (vector-ref fields 0)) (bad))
      (unless (string=? "S" (vector-ref fields 6)) (bad))
      (let* ((year   (string->number (vector-ref fields 1)))
             (month  (assoc (vector-ref fields 2) months))
             (day    (string->number (vector-ref fields 3)))
             (time   (vector-ref fields 4))
             (sign   (vector-ref fields 5))
             (offset
              (cond ((and (string=? "+" sign) (string=? "23:59:60" time)) +1)
                    ((and (string=? "-" sign) (string=? "23:59:59" time)) -1)
                    (else (bad)))))
        (unless (and (exact-integer? year) (<= 1972 year 9999)) (bad))
        (unless month (bad))
        (let ((month (list-ref month 1)) (days (list-ref month 2)))
          (unless (and (exact-integer? day) (<= 1 day days)) (bad))
          (vector year month day offset))))

    (define (read-zoneinfo-leapseconds-entries)
      (let loop ((leaps '()))
        (let ((line (read-line)))
          (cond ((eof-object? line)
                 (list->vector (reverse leaps)))
                ((or (string=? "" line) (char=? #\# (string-ref line 0)))
                 (loop leaps))
                (else
                 (loop (cons (parse-zoneinfo-leapseconds-entry
                              (split-at-whitespace line))
                             leaps)))))))

    (define (read-zoneinfo-leapseconds)
      (with-input-from-file "/usr/share/zoneinfo/leapseconds"
        read-zoneinfo-leapseconds-entries))))
