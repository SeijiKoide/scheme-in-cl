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
           #:define #:delay #:letrec #:else
           #:call/cc #:call-with-current-continuation))

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

(defun interp (x env cc)
  "Evaluate the expression x in the environment env,
   and pass the result to the continuation cc."
  (cond
   ((symbolp x) (funcall cc (get-var x env)))
   ((atom x)    (funcall cc x))
   ((scheme:macro (first x))                
    (interp (scheme:macro-expand x) env cc))
   ((case (first x)
      (quote        (funcall cc (second x)))
      (scheme:begin (interp-begin (rest x) env cc))
      (scheme:set!  (interp (third x) env
                            #'(lambda (val)
                                (funcall cc (set-var! (second x)
                                                      val env)))))
      (if           (interp (second x) env
                            #'(lambda (pred)
                                (interp (if pred (third x) (fourth x))
                                        env cc))))
      (scheme:lambda (let ((parms (second x))
                           (code (maybe-add 'scheme:begin (rest2 x))))
                       (funcall
                        cc
                        #'(lambda (cont &rest args)
                            (interp code
                                    (extend-env parms args env)
                                    cont)))))
      (otherwise (interp-call x env cc))))))

(defun interp-call (call env cc)
  "Interpret the call (f x...) and pass the result to CC."
  (map-interp call env
              #'(lambda (fn-and-args)
                  (apply (first fn-and-args)
                         cc
                         (rest fn-and-args)))))

(defun map-interp (list env cc)
  "Interpret each element of LIST, and pass the list to CC."
  (if (null list)
      (funcall cc nil)
    (interp (first list) env
            #'(lambda (x)
                (map-interp (rest list) env
                            #'(lambda (y)
                                (funcall cc (cons x y))))))))

(defun interp-begin (body env cc)
  "Interpret each element of BODY, passing the last to CC."
  (interp (first body) env
          #'(lambda (val)
              (if (null (rest body))
                  (funcall cc val) ;; fix, hsuc 2/20/93; forgot to call cc
                (interp-begin (rest body) env cc)))))

(defun scheme:scheme (&optional x)
  "A Scheme read-eval-print loop (using interp).
   Handles call/cc by explicitly passing continuations."
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (if x
      (interp x nil #'print)
    (loop (format t "~&==> ")
      (interp (read) nil #'print))))

(defun init-scheme-proc (f)
  "Define a Scheme primitive procedure as a CL function."
  (if (listp f)
      (set-global-var! (first f)
                       #'(lambda (cont &rest args)
                           (funcall cont (apply (second f) args))))
      (init-scheme-proc (list f f))))

(defun call/cc (cc computation)
  "Make the continuation accessible to a Scheme procedure."
  (funcall computation cc
           ;; Package up CC into a Scheme function:
           #'(lambda (cont val)
               (declare (ignore cont))
               (funcall cc val))))

(set-global-var! 'scheme:call/cc #'call/cc)
(set-global-var! 'scheme:call-with-current-continuation #'call/cc)
