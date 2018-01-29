



(asdf:defsystem :blog-series-mop
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       cl-aa
	       cl-aa-misc
	       cl-fad
	       cl-ppcre
	       cl-store
	       cl-vectors
	       trivial-features
	       clx
	       trivial-dump-core)
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file package)
   (:file flavor)))
