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

(defun alast (array)
  (let ((len (length array)))
    (when (> len 0)
      (aref array (- (length array) 1)))))

(defun afirst (array)
  (when (> (length array) 0)
    (aref array 0)))

(defmacro doarray ((var array &optional result) &body body)
  `(progn
     (loop for ,var across ,array do 
	  ,@body)
     ,result))

(defun gvector (&rest elements)
  (let ((array (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (element elements)
      (vector-push-extend element array))
    array))

(defun action-name (action)
  (first action))

(defun action-args (action)
  (second action))

(defun action-fn (action)
  (cddr action))

(defmacro def-fn-obj (name lambda-list &rest actions)
  (with-gensyms (dispatch-fn)
    `(defun ,(cat-symbols 'create- name) ,(cons '&key lambda-list)
       (labels (,@(append (mapcar #'(lambda (action)
				      `(,(action-name action)
					 ,(action-args action)
					 ,@(action-fn action)))
				  actions)
			  `((,dispatch-fn (sym)
					  (case sym
					    ,@(mapcar #'(lambda (action)
							  `(,(action-name action)
							     #',(action-name action)))
						       actions))))))
	 #',dispatch-fn))))

(defun send-message (obj message &rest args)
  (let ((action (funcall obj message)))
    (if args
	(apply action args)
	(funcall action))))

(defun get-action (obj action)
  (funcall obj action))