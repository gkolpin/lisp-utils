(defpackage :lisp-utils
  (:use :common-lisp)
  (:export #:appendf
	   #:compose
	   #:with-gensyms
	   #:cat-symbols
	   #:to-keyword
	   #:remove-nils
	   #:mappend
	   #:pincf
	   #:awhen
	   #:aif
	   #:aand
	   #:limit
	   #:set-conc
	   #:string-conc
	   #:bind-nil
	   #:self-ref
	   #:it
	   #:alast
	   #:afirst
	   #:doarray
	   #:gvector
	   #:def-fn-obj
	   #:send-message
	   #:get-action
	   #:curry
	   #:alist-to-ht
	   #:limiting-subseq))
