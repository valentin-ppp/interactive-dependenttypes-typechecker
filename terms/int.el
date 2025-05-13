;; Terms for integers - Scott encoding

;; The int type
(define-term
 '*int*
 '(y (FUN self top
	  (FUN zero-case top
	       (FUN succ-case (FUN n top top)
		    (OR 'zero-case ('succ-case 'self)))))))

;; Some integers
(define-term
 '*0*
 '(FUN zero-case top
       (FUN succ-case (FUN n top top)
            'zero-case)))

(define-term
 '*1*
 '(FUN zero-case top
       (FUN succ-case (FUN n top top)
	    ('succ-case
             (FUN zero-case top
		  (FUN succ-case (FUN n top top)
		       'zero-case))))))

(define-term
 '*2*
 '(FUN zero-case top
       (FUN succ-case (FUN n top top)
	    ('succ-case
             (FUN zero-case top
		  (FUN succ-case (FUN n top top)
		       ('succ-case
			(FUN zero-case top
			     (FUN succ-case (FUN n top top)
				  'zero-case)))))))))

;;;;;;;;;;;;;;;;;;
;; Basic functions

(define-term
 '*succ*
 '(FUN n *int*
       (FUN zero-case top
	    (FUN succ-case (FUN n top top)
		 ('succ-case 'n)))))

;; Addition - has to be recursive with the Scott encoding
(define-term
 '*plus-factory*
 '(FUN obj top
       (y (FUN self 'obj
	       (FUN m *int*
		    (FUN n *int*
			 (('m
			   ;; m == 0 -> n
			   'n)
			  ;; m == S(a) -> S(a + n)
			  (FUN a *int*
			       (*succ* (('self 'a) 'n))))))))))

;; The following term is expanded, so that we can collapse it when we see it
;; (because we will encounter the expanded version, not (*plus-factory* (FUN a *int* (FUN b *int* *int*)))
(define-term
 '*plus* ; the next line is expanded version of (*plus-factory* (FUN a *int* (FUN b *int* *int*)))
 '(y (FUN self (FUN a *int* (FUN b *int* *int*)) (FUN m *int* (FUN n *int* (('m 'n) (FUN a *int* (*succ* (('self 'a) 'n)))))))))

;; Multiplication - same as addition
(define-term
 '*mult-factory*
 '(FUN obj top
       (y (FUN self 'obj
	       (FUN m *int*
		    (FUN n *int*
			 (('m
			   ;; m == 0 -> 0
			   *0*)
			  ;; m == S(a) -> n + (a * n)
			  (FUN a *int*
			       ((*plus* 'n) (('self 'a) 'n))))))))))

;; Same: this is '(*mult-factory* (FUN a *int* (FUN b *int* *int*))))
(define-term
 '*mult*
 '(y (FUN self (FUN a *int* (FUN b *int* *int*)) (FUN m *int* (FUN n *int* (('m *0*) (FUN a *int* ((*plus* 'n) (('self 'a) 'n)))))))))

;; Factorial
(define-term
 '*fact-factory*
 '(FUN obj top
       (y (FUN self 'obj
	       (FUN n *int*
		    (('n
		      ;; n == 0 -> 1
		      *1*)
		     ;; n == S(a) -> n * (fact a)
		     (FUN a *int*
			  ((*mult* 'n) ('self 'a)))))))))

(define-term
 '*fact*
 '(*fact-factory* (FUN a *int* *int*)))

;; Todo provide a proper definition,
;; These are just placeholders which generalise to *bool* to showcase the syracuse example
(define-term '*isOne*
	     '(FUN n *int* *bool*))
(define-term '*isEven*
	     '(FUN n *int* *bool*))

;; Syracuse function
(define-term
 '*syracuse-factory*
 '(FUN obj top
       (y (FUN self 'obj
	       (FUN n *int*
		    (((*isOne* 'n)
		      *1*)
		     (((*isEven* 'n)
		       ('self ((*div* 'n) *2*)))
		      ('self (*succ* ((*mult* *3*) 'n))))))))))
