(defmacro test (title expected expr &optional (test '#'equal))
  (let ((x (gensym)) (y (gensym)))
	`(progn
	   (format t "~A ==> " ,title)
	   (let ((,x ,expr)
			 (,y ,expected))
		 (if (funcall ,test ,x ,y)
			 (progn (format t "ok~%") 'ok)
			 (progn (format t "error: ~S is expected, but got ~S~%" ,y ,x)
					'error))))))

(defmacro test-error (title expr)
  `(progn
	 (format t "~A ==> " ,title)
	 (handler-case ,expr
	   (:no-error (x)
		 (format t "error: ~S is returned normally~%" x)
		 'error)
	   (t ()
		 (format t "ok~%")
		 'ok))))
