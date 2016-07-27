(load "unit-test.lisp")
(load "scheme-in-cl-mixable.lisp")
(init-scheme-interp)

;; (trace scheme:macro-expand interp)
;; (untrace scheme:macro-expand interp)

(test "global var" (interp 'car) #'car)
(test "primitive func: car" (interp '(car '(0 1 2))) 0)
(test "primitive func: cons" (interp '(cons 'A 'B)) '(A . B))

(interp '(scheme:define gx 300))
(test "scheme:define 1"
	  300
	  (interp 'gx))

(interp '(scheme:define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))))
(test "scheme:define 2"
	  55
	  (interp '(sum 10)))

(test "lambda 1"
	  2
	  (interp '((scheme:lambda (x) (+ x 1)) 1)))

(test "lambda 2"
	  2
	  (interp '((scheme:lambda (f) (f 1))
				(scheme:lambda (x) (+ x 1)))))

(test "let 1"
	  2
	  (interp '(let ((x 1)) (+ x 1))))

(test "let 2"
	  3
	  (interp '(let ((x 1)) (let ((y 2)) (+ x y)))))

(test "let 3"
	  '(2 1)
	  (interp '(let ((x 1) (y 2)) (let ((x y) (y x)) (list x y)))))

(test "lexical var"
	  3
	  (interp '((scheme:lambda (x) (+ x 1)) 2)))

(test "lexical var, nested"
	  5
	  (interp '((scheme:lambda (x)
				  ((scheme:lambda (y)
					 (+ x y))
				   2))
				3)))

(test "lexical var, set!"
	  3
	  (interp '((scheme:lambda (x)
				  (scheme:set! x (+ x 1))
				  x)
				2)))

(test "lexical var, set!, parent"
	  3
	  (interp '((scheme:lambda (x)
				  ((scheme:lambda (y)
					 (scheme:set! x (+ y 1))
					 x)
				   2))
				1)))

(test "closure, infinite extent"
	  3
	  (interp '(((scheme:lambda (x)
				  (scheme:lambda (y) (+ x y)))
				 1)
				2)))

(test-error
 "ensure unbound for the following two tests"
 (interp 'x))

(test-error
 "lexical scope, unresolved"
 (interp '(let ((f (scheme:lambda () x)))
		   (let ((x 1))
			 (f)))))

(test-error
 "dynamic scope, unresolved"
 (interp '((let (((scheme:dynamic x) 1))
			 (scheme:lambda () x)))))

(test "lexical var, share, set!"
	  3
	  (interp '((scheme:lambda (p)
				  ((car p) 3)
				  ((cdr p)))
				((scheme:lambda (x)
				   (cons
					(scheme:lambda (y) (scheme:set! x y))
					(scheme:lambda () x)))
				 1))))

(interp '(scheme:define global-x 300))
(test "global var"
	  300
	  (interp 'global-x))
(interp '(scheme:set! global-x 400))
(test "global var, set!"
	  400
	  (interp 'global-x))

(test "global var, lexical var, screen"
	  3
	  (interp '((scheme:lambda (global-x) global-x) 3)))
(test "global var, lexical var, screen 2"
	  400
	  (interp 'global-x))
(interp '((scheme:lambda (global-x) (scheme:set! global-x 4) 3)))
(test "global var, lexical var, screen, set!"
	  400
	  (interp 'global-x))

(interp '(scheme:define (f (scheme:dynamic x)) (g)))
(interp '(scheme:define (g) x))
(test "dynamic scope 1"
	  3
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x)) (g)))
(interp '(scheme:define (g) (list (h) (let (((scheme:dynamic x) 1)) (h)))))
(interp '(scheme:define (h) x))
(test "dynamic scope 2"
	  '(3 1)
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x)) (g)))
(interp '(scheme:define (g) (list (h) (let ((x 1)) (h)))))
(interp '(scheme:define (h) x))
(test "dynamic scope 3: lexical var does not affect"
	  '(3 3)
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x)) (g)))
(interp '(scheme:define (g) (let ((x 1)) (list x (scheme:dynamic-reference x)))))
(test "dynamic-reference"
	  '(1 3)
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x)) (g)))
(interp '(scheme:define (g) (let (((scheme:dynamic x) 1)) (h))))
(interp '(scheme:define (h) (scheme:dynamic-reference x)))
(test "dynamic-reference 2"
	  '1
	  (interp '(f 3)))

(test "dynamic-reference 3: global"
	  400
	  (interp '(scheme:dynamic-reference global-x)))

(test "dynamic-reference 4"
	  3
	  (interp '(let (((scheme:dynamic x) 3))
				(let ((x 2))
				  (let ((x 1))
					(scheme:dynamic-reference x))))))

(test "dynamic-reference 5"
	  1
	  (interp '(let ((x 3))
				(let ((x 2))
				  (let (((scheme:dynamic x) 1))
					x)))))

(test "dynamic-reference 6"
	  3
	  (interp '(let (((scheme:dynamic x) 3))
				(let ((y 2))
				  x))))


(interp '(scheme:define (f g)
		  (let (((scheme:dynamic x) 2))
			(g))))
(test "dynamic-reference 7"
	  2
	  (interp '(let (((scheme:dynamic x) 1))
				(f (scheme:lambda () x)))))

(test "dynamic-reference 8"
	  2
	  (interp '(let ((x 1))
				  (let (((scheme:dynamic x) 2))
					(let ((y 3))
					  (let (((scheme:dynamic y) 4))
						x))))))

(test "dynamic-reference 9"
	  3
	  (interp '(let ((x 1))
				  (let (((scheme:dynamic x) 2))
					(let ((x 3))
					  (let ((y 4))
						(let (((scheme:dynamic y) 5))
						  x)))))))

(interp '(scheme:define (f) x))
(test "dynamic-reference 10"
	  2
	  (interp '(let ((x 1))
				  (let (((scheme:dynamic x) 2))
					(f)))))

(interp '(scheme:define (f) x))
(test "dynamic-reference 11"
	  2
	  (interp '(let (((scheme:dynamic x) 2))
				(let ((x 1))
				  (f)))))

(interp '(scheme:define (f p) (p)))
(test "dynamic-reference 12"
	  2
	  (interp '(let ((x 1))
				(let (((scheme:dynamic x) 2))
				  (f (scheme:lambda () x))))))

(interp '(scheme:define (f p) (p)))
(test "dynamic-reference 13"
	  1
	  (interp '(let (((scheme:dynamic x) 2))
				(let ((x 1))
				  (f (scheme:lambda () x))))))

(interp '(scheme:define (f p)
		  (let (((scheme:dynamic x) 3))
			(p))))
(test "dynamic-reference 14"
	  3
	  (interp '(let ((x 1))
				(let (((scheme:dynamic x) 2))
				  (f (scheme:lambda () x))))))

(test "dynamic-reference 15"
	  1
	  (interp '(let ((x 2))
				((let ((x 1))
				   (scheme:lambda () x))))))

(test "dynamic-reference 16"
	  1
	  (interp '(let (((scheme:dynamic x) 2))
				((let ((x 1))
				   (scheme:lambda () x))))))

(test "dynamic-reference 17"
	  2
	  (interp '(let (((scheme:dynamic x) 2))
				((let (((scheme:dynamic x) 1))
				   (scheme:lambda () x))))))

(test "dynamic-reference 18"
	  2
	  (interp '(let (((scheme:dynamic x) 1))
				(let ((f (scheme:lambda () x)))
				  (let (((scheme:dynamic x) 2))
					(f))))))

(test "dynamic-reference 19"
	  1
	  (interp '(let ((x 1))
				(let ((f (scheme:lambda () x)))
				  (let (((scheme:dynamic x) 2))
					(f))))))

(test "dynamic-reference 20"
	  1
	  (interp '(let ((x 1))
				(let ((f (scheme:lambda () x)))
				  (let ((x 2))
					(f))))))

(test "dynamic-reference 21"
	  1
	  (interp '(let (((scheme:dynamic x) 1))
				(let ((f (scheme:lambda () x)))
				  (let ((x 2))
					(f))))))

(interp '(scheme:define (f (scheme:dynamic x)) (g) x))
(interp '(scheme:define (g) (scheme:set! x 1)))
(test "dynamic var, set!"
	  1
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x)) (g) x))
(interp '(scheme:define (g) (let ((x 1)) (h))))
(interp '(scheme:define (h) (scheme:set! x 2)))
(test "dynamic var, set! 2"
	  2
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x))
		  (let ((r '()))
			(g)
			(scheme:set! r (cons x r))
			(h)
			(scheme:set! r (cons x r))
			r)))
(interp '(scheme:define (g) (scheme:set! x 1)))
(interp '(scheme:define (h) (scheme:set! x 2)))
(test "dynamic var, set! 3"
	  '(2 1)
	  (interp '(f 3)))

(interp '(scheme:define (f (scheme:dynamic x))
		  (let ((r '()))
			(g)
			(scheme:set! r (cons x r))
			(h)
			(scheme:set! r (cons x r))
			r)))
(interp '(scheme:define (g) (let (((scheme:dynamic x) 1)) (h))))
(interp '(scheme:define (h) (scheme:set! x 2)))
(test "dynamic var, set! 4"
	  '(2 3)
	  (interp '(f 3)))

(interp '(scheme:define r '()))
(interp '(scheme:define (f (scheme:dynamic x))
		  (scheme:set! r '())
		  (h)
		  (scheme:set! r (cons x r))
		  (g)
		  (scheme:set! r (cons x r))))
(interp '(scheme:define (g)
		  (let (((scheme:dynamic x) 1))
			(h)
			(scheme:set! r (cons x r)))))
(interp '(scheme:define (h) (scheme:set! x (+ x 1))))
(test "dynamic var, set! 5"
	  '(1 2 1)
	  (interp '(f 0)))


(interp '(scheme:define r '()))
(interp '(scheme:define (f (scheme:dynamic x))
		  (scheme:set! r '())
		  (h)
		  (scheme:set! r (cons x r))
		  (g)
		  (scheme:set! r (cons x r))))
(interp '(scheme:define (g)
		  (let ((x 1))
			(h)
			(scheme:set! r (cons x r)))))
(interp '(scheme:define (h) (scheme:set! x (+ x 1))))
(test "dynamic var, set! 6"
	  '(2 1 1)
	  (interp '(f 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; examples in the book

(interp
 '(scheme:define (bar x)
   (let ((y 2))
	 (let ((z 3))
	   (list x y z)))))
(test "4.2 bar"
	  '(1 2 3)
	  (interp '(bar 1)))

(interp '(scheme:set! x 'outside))
(test "4.2 let 1"
	  '(inside outside)
	  (interp '(let ((x 'inside)
					 (y x))
				(list x y))))

(test "4.2 let 2"
	  '(inside outside)
	  (interp '(let ((y x)
					 (x 'inside))
				(list x y))))

(test "4.2 let 3"
	  '(inside inside)
	  (interp '(let* ((x 'inside)
					  (y x))
				(list x y))))

(interp '(scheme:define (interest bal)
		  (* bal rate)))
(interp '(scheme:define (newbalance balance)
		  (let (((scheme:dynamic rate) 0.1))
			(+ balance (interest balance)))))
(test "4.3 dynamic"
	  110.0
	  (interp '(newbalance 100.0))
	  #'=)

(interp '(scheme:define (foo)
		  (let (((scheme:dynamic x) 1))
			(print x)
			(let (((scheme:dynamic x) 0))
			  (print x))
			(print x))))
(interp '(foo))

(interp '(scheme:define result '()))
(interp '(scheme:define (bar x) (scheme:set! result (cons x result))))
(interp '(scheme:define (foo)
		  (let (((scheme:dynamic x) 1))
			(bar x)
			(let (((scheme:dynamic x) 0))
			  (bar x))
			(bar x))))
(interp '(foo))
(test "4.3 dynamic 2" '(1 0 1) (interp 'result))

(test "hyperspec example"
	  3
	  (interp
	   '(let (((scheme:dynamic x) 1))            ;Binds a special variable x
		 (let ((x 2))          ;Binds a lexical variable x
		   (+ x                ;Reads a lexical variable x
			  (scheme:dynamic-reference x))))))
