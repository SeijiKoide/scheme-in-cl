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
           #:null? #:eq? #:equal? #:eqv? #:write #:display #:newline
           #:define #:delay #:letrec #:else))

(defun scheme:scheme (&optional x)
  "A Scheme read-eval-print loop (using interp)"
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (if x
      (interp x nil)
    (loop (format t "~&==> ")
          (print (interp (read) nil)))))

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

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL function."
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
      (set-global-var! f (symbol-function f))))

(defun interp (x &optional env)
  "Evaluate the expression x in the environment env.
   This version is properly tail-recursive."
  (prog ()
    :INTERP
    (return
      (cond
       ((symbolp x) (get-var x env))
       ((atom x) x)
       ((scheme:macro (first x))
        (setf x (scheme:macro-expand x)) (go :INTERP))
       ((case (first x)
          (quote (second x))
          (scheme:begin (pop x) ; pop off the BEGIN to get at the args
                        ;; Now interpret all but the last expression
                        (loop while (rest x) do (interp (pop x) env))
                        ;; Finally, rename the last expression as x
                        (setf x (first x))
                        (go :INTERP))
          (scheme:set!  (let ((val (interp (third x) env)))
                          (when (proc-p val)
                            (setf (proc-name val) (second x)))
                          (set-var! (second x) val env)))
          (if           (setf x (if (interp (second x) env)
                                    (third x)
                                  (fourth x)))
              ;; That is, rename the right expression as x
              (go :INTERP))
          (scheme:lambda (make-proc :env env :parms (second x)
                           :code (maybe-add 'scheme:begin (rest2 x))))
          (otherwise   ;; a procedure application
           (let ((proc (interp (first x) env))
                 (args (mapcar #'(lambda (v) (interp v env)) (rest x))))
            (if (proc-p proc)
                 ;; Execute procedure with rename+goto
                 (progn
                   (setf x (proc-code proc))
                   (setf env (extend-env (proc-parms proc) args
                                         (proc-env proc)))
                   (go :INTERP))
               ;; else apply primitive procedure
               (apply proc args))))))))))

(defstruct (proc (:print-function print-proc))
  "Represent a Scheme procedure"
  code (env nil) (name nil) (parms nil))

(defun print-proc (proc &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (proc-name proc) '??)))

(defun scheme:macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme:macro)))

(defun scheme:macro-expand (x)
  "Macro-expand this Scheme expression."
  (if (and (listp x) (scheme:macro (first x)))
      (scheme:macro-expand
        (apply (scheme:macro (first x)) (rest x)))
    x))

(defmacro def-scheme-macro (name parmlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme:macro)
         #'(lambda ,parmlist ,@body)))

(def-scheme-macro let (bindings &rest body)
  `((scheme:lambda ,(mapcar #'first bindings) ,@body)
    ,@(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(scheme:begin ,@body)
    `(let (,(first bindings))
       (let* ,(rest bindings) ,@body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 't)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                       (and ,@(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or ,@(rest args))))))))

(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond ,@(rest clauses))))
        ((starts-with (first clauses) 'scheme:else)
         `(scheme:begin ,@(rest (first clauses))))
        (t `(if ,(first (first clauses))
                       (scheme:begin ,@(rest (first clauses)))
                       (cond ,@(rest clauses))))))

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                   #'(lambda (clause)
                       (if (starts-with clause 'scheme:else)
                           clause
                         `((member ,key-val ',(first clause))
                           ,@(rest clause))))
                 clauses)))))

(def-scheme-macro scheme:define (name &rest body)
  (if (atom name)
      `(scheme:begin (scheme:set! ,name ,@body) ',name)
    `(scheme:define ,(first name)
       (scheme:lambda ,(rest name) ,@body))))

(def-scheme-macro scheme:delay (computation)
  `(scheme:lambda () ,computation))

(def-scheme-macro scheme:letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(scheme:set! ,@v)) bindings)
     ,@body))
