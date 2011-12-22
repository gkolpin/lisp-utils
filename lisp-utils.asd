(defsystem lisp-utils
  :description "lisp-utils: A collection of lisp utilities"
  :version "0.01"
  :author "Garrett Kolpin <gkolpin@gmail.com>"
  :depends-on (unicly)
  :components ((:file "lisp-utils" :depends-on ("packages"))
	       (:file "packages")))
