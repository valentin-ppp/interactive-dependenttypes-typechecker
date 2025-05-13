;; Terms for booleans - Scott encoding

(define-term
 '*true*
 '(FUN true-case top
       (FUN false-case top
            'true-case)))

(define-term
 '*false*
 '(FUN true-case top
       (FUN false-case top
            'false-case)))

(define-term
 '*bool*
 '(OR *true* *false*))
