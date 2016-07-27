(load "unit-test.lisp")
(load "scheme-in-cl-dynamic.lisp")

(init-scheme-interp)

;; (trace scheme:macro-expand interp)
;; (untrace scheme:macro-expand interp)

(test "car"
	  0
	  (interp '(car '(0 1 2)) nil))

(test "cons"
	  '(A . B)
	  (interp '(cons 'A 'B) nil))

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

(test "dynamic scope 1"
	  2
	  (interp '(let ((x 1))
				(let ((f (scheme:lambda () x)))
				  (let ((x 2))
					(f))))))

(interp '(scheme:define (f x) (g)))
(interp '(scheme:define (g) x))
(test "dynamic scope 2"
	  3
	  (interp '(f 3) nil))

(interp '(scheme:define (f x) (g)))
(interp '(scheme:define (g) (list (h) (let ((x 1)) (h)))))
(interp '(scheme:define (h) x))
(test "dynamic scope 3"
	  '(3 1)
	  (interp '(f 3) nil))

(interp '(scheme:define r '()))
(interp '(scheme:define (f x)
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
(test "dynamic scope 3"
	  '(1 2 1)
	  (interp '(f 0)))

(test-error
 "no closure"
 (interp '((let ((x 1)) (scheme:lambda () x)))))

