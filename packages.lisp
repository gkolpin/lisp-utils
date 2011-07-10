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
	   #:bind-nil
	   #:self-ref
	   #:it))
