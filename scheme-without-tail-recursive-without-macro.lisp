(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defpackage scheme
  (:export #:scheme #:macro #:macro-expand #:begin #:set! #:lambda
           #:null? #:eq? #:equal? #:eqv? #:write #:display #:newline))

(defun scheme:scheme (&optional x)
  "A Scheme read-eval-print loop (using interp)"
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (if x
      (interp x nil)
    (loop (format t "~&==> ")
          (print (interp (read) nil)))))

(defun interp (x &optional env)
  "Interpret (evaluate) the expression <<x>> in the environment <<env>>.
   This version handles macros."
  (cond
   ((symbolp x) (get-var x env))
   ((atom x) x)
   ((case (first x)
      (quote  (second x))
      (scheme:begin  (last1 (mapcar #'(lambda (y) (interp y env))
                              (rest x))))
      (scheme:set!   (set-var! (second x) (interp (third x) env) env))
      (if            (if (interp (second x) env)
                         (interp (third x) env)
                       (interp (fourth x) env)))
      (scheme:lambda (let ((parms (second x))
                           (code (maybe-add 'scheme:begin (rest2 x))))
                       #'(lambda (&rest args)
                           (interp code (extend-env parms args env)))))
      (otherwise ;; a procedure application
       (apply (interp (first x) env)
              (mapcar #'(lambda (v) (interp v env)) (rest x))))))))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (scheme:null? null) (scheme:eq? eq) (scheme:equal? equal)
    (scheme:eqv? eql) (scheme:write prin1) (scheme:display princ)
    (scheme:newline terpri)))

(defun init-scheme-interp ()
  "Initialize the scheme interpreter with some global variables."
  ;; Define Scheme procedures as CL functions:
  (mapc #'init-scheme-proc *scheme-procs*)
  ;; Define the boolean `constants'. Unfortunately, this won't
  ;; stop someone from saying: (set! t nil)
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL function."
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
      (set-global-var! f (symbol-function f))))

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
    (set-global-var! var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
  (if (assoc var env)
      (second (assoc var env))
    (get-global-var var)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound scheme variable: ~a" var)
        val)))
