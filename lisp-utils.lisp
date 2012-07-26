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

(defun flatten (lst)
  (labels ((rec (built remaining)
	     (cond ((null remaining) built)
		   ((atom (car remaining)) (rec (cons (car remaining) built) (cdr remaining)))
		   (t
		    (rec (rec built (car remaining)) (cdr remaining))))))
    (nreverse (rec nil lst))))

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

(defun avg (&rest nums)
  (if (null nums)
      0
      (/ (apply #'+ nums) (length nums))))

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

(defun limiting-subseq (sequence start &optional end)
  (subseq sequence start (min (length sequence) end)))

(define-modify-macro set-conc (&rest strings) (lambda (&rest strings) (apply #'concatenate (cons 'string strings))))

(defun string-conc (&rest strings)
  (apply #'concatenate 'string strings))

(defun curry (fn &rest args)
  #'(lambda (&rest rest-args)
      (apply fn (append args rest-args))))

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

(defun alist-to-ht (alist)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (association alist)
      (symbol-macrolet ((hash-location (gethash (car association) ht)))
	(let ((hash-location-val hash-location))
	  (cond ((null hash-location-val) (setf hash-location (cdr association)))
		((atom hash-location-val) (setf hash-location (list (cdr association) hash-location-val)))
		(t (setf hash-location (cons (cdr association) hash-location-val)))))))
    ht))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-uuid-str ()
  (write-to-string (unicly:make-v4-uuid)))

(defmacro with-atomically-updated-file ((stream filespec &rest options) &body body)
  (with-gensyms (file-name pathspec)
    `(progn
       (let* ((,file-name (gen-uuid-str))
	      (,pathspec
		(merge-pathnames (make-pathname :name ,file-name)
				 (ensure-directories-exist
				  (make-pathname :directory 
						 (append (pathname-directory ,filespec)
							 '(".update-area")))))))
	 (with-open-file (,stream ,pathspec ,@options)
	   ,@body)
	 (rename-file ,pathspec ,filespec)))))

(defun hash-literal-transformer (stream subchar arg)
  (let ((sexp (read stream t)))
    (let ((hash-sym (gensym)))
      `(let ((,hash-sym (make-hash-table)))
	 ,@(loop for (key val) on sexp by #'cddr collect
		`(setf (gethash ,key ,hash-sym) ,val))
	 ,hash-sym))))

(set-dispatch-macro-character #\# #\h #'hash-literal-transformer)

(defun maphash-to-list (fn ht)
  (let ((list (list)))
    (maphash #'(lambda (k v) (push (funcall fn k v) list))
	     ht)
    (nreverse list)))
