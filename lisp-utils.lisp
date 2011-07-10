(in-package :lisp-utils)

(define-modify-macro appendf (&rest lists) append)

(defun compose (&rest fns)
  (labels ((rec (fns)
		(if (null fns)
		    #'identity
		  #'(lambda (arg)
		      (funcall (car fns)
			       (funcall (rec (cdr fns)) arg))))))
    (rec fns)))


(defmacro with-gensyms (arg-names &body body)
  (let ((let-list (loop for arg in arg-names collect
			`(,arg (gensym)))))
    `(let (,@let-list)
       ,@body)))

(defun cat-symbols (&rest symbols)
  (intern (apply #'concatenate (cons 'string (mapcar #'symbol-name symbols)))))

(defun to-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun remove-nils (list)
  (remove-if-not #'identity list))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro pincf (place &optional (delta 1))
  (with-gensyms (place-arg)
    `(let ((,place-arg ,place))
       (incf ,place ,delta)
       ,place-arg)))

(defmacro awhen (test &body body)
  `(let ((lisp-utils:it ,test))
     (when lisp-utils:it ,@body)))

(defmacro aif (test then &optional else)
  `(let ((lisp-utils:it ,test))
     (if lisp-utils:it ,then ,else)))

(defmacro aand (&rest clauses)
  (cond ((null clauses) t)
	((null (cdr clauses)) (car clauses))
	(t
	 `(aif ,(first clauses)
	       (aand ,@(rest clauses))))))

(defun limit (list n)
  (labels ((rec (built-list rem n)
	     (cond
	       ((null rem) built-list)
	       ((= 0 n) built-list)
	       (t
		(rec (cons (car rem) built-list) (cdr rem) (- n 1))))))
    (nreverse (rec '() list n))))

(define-modify-macro set-conc (&rest strings) (lambda (&rest strings) (apply #'concatenate (cons 'string strings))))

(defmacro bind-nil (vars &body body)
  `(let ,(mapcar #'(lambda (var) `(,var nil)) vars)
     ,@body))

(defmacro self-ref (sym &body body)
  (with-gensyms (body-result eval-body-fn)
    `(let ((,body-result))
       (labels ((,eval-body-fn () ,@body)
		(,sym () ,body-result))
	 (setf ,body-result (,eval-body-fn))
	 ,body-result))))
  