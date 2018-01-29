






(in-package :flavor-internal)

(defclass flavor-base-class () ())




(defclass test-class () ())

(defmethod hello-world ((test-class test-class))
  (format t "Hello World"))


(defmacro defflavor (name instance-variables component-flavors &rest options)
  "instance-variables can be symbols, or list of symbol and initialization, e.g.

   (defflavor ship (x-velocity y-velocity
                   (x-position 0.0)
                   (y-position 0.0)))

   component-flavors are inheritted flavors (i.e. super-classes)
   searched from left to right for methods, and contribute their
   instance variables.

   options are:
    (:accessor-prefix sym)
    ;;(:default-handler function)
    ;;(:default-init-plist k1 v1 k2 v2...)
    (:documentation <args>...)
    ;;(:gettable-instance-variables v1 v2...)
    ;;(:included-flavors f1 f2...)
    ;;;(:init-keywords k1 k2...)
    ;;;(:inittable-instance-variables v1 v2...)
    ;;;(:method-combination (type order operation1 operation2...)...)
    ;;;(:method-order m1 m2...)
    (:ordered-instance-variables v1 v2...)
    (:outside-accessible-instance-variables v1 v2...)
    ;;;;(:required-flavors f1 f2...)
    ;;;(:required-instance-variables v1 v2...)
    ;;(:required-methods m1 m2...)
    ;;(:settable-instance-variables v1 v2...)
    ;;;(:special-instance-variables <variables>)
    ;;:abstract-flavor
    ;;:alias-flavor
    ;;:no-vanilla-flavor

   Many are rare or obsolete."
  (let ((copied-options (copy-list options)))
    `(progn
       (defflavor-internal ',name ',instance-variables ',component-flavors ',copied-options)
       ;; (compose-automatic-methods (compilation-flavor ',name))
       ;; (compose-automatic-methods (get ',name 'flavor))
       )))

(defun convert-instance-variable-to-slot (instance-variable-spec)
  (let ((instance-variable-spec (if (consp instance-variable-spec)
				    instance-variable-spec
				    (list instance-variable-spec))))
    (destructuring-bind (name &optional initform (initfunction (lambda ())))
	instance-variable-spec
      `(:name ,name
	      ,@(if initform
		    (list :initform initform)
		    '())
	      ,@(if initfunction
		    (list :initfunction initfunction)
		    '())))))

(defun defflavor-internal (name instance-variables component-flavors copied-options)
  (let* ((direct-superclasses (map 'list (lambda (component-flavor)
                                          (find-class component-flavor))
                                  component-flavors))
	 (direct-superclasses (if (null direct-superclasses)
				  '(flavor-base-class)
				  direct-superclasses))
	 (direct-slots (map 'list #'convert-instance-variable-to-slot
			    instance-variables)))
    (format t "direct-superclasses: ~a, direct-slots: ~a~%" direct-superclasses direct-slots)
    (closer-mop:ensure-class name
                  :direct-superclasses direct-superclasses
                  :direct-slots direct-slots)))

(defmethod print-object ((instance flavor-base-class) stream)
  (print-unreadable-object (instance stream :identity t)
    (format stream "CLOS Flavor ~%~s" (class-name (class-of instance))))
  instance)



;; All flavors 'secretly' derive from the flavor class

(defmacro flavor-defmethod ())

(defmacro flavor-defmethod (spec lambda-list . body)
  "Defines the method for flavor: flavor-name for the message
   operation, daemon-type can be one of the following

   :after
   :and
   :append
   :around
   :before
   :case
   :case-sub-operation
   :default
   :inverse-around
   :inverse-list
   :list
   :nconc
   :or
   :override
   :pass-on
   :progn

   Note :case-sub-operation must be provided for :case deamon-type, it
   is illegal otherwise."
  ;; (format t "spec ~a, lambda-list ~a, body ~a"
  ;; 	  spec lambda-list body)
  (flavor-defmethod-internal spec lambda-list body))

(defun flavor-defmethod-internal (&key lambda-list lambda-form gf-lambda-list gf-name qualifiers
				    specializers)
    (let ((generic-function
	   (closer-mop:ensure-generic-function gf-name :lambda-list gf-lambda-list)))
      (closer-mop:ensure-method generic-function lambda-form
				:lambda-list lambda-list
				:qualifiers qualifiers
				:specializers specializers)))

  ;; (cond ((consp (car rest))
  ;; 	 (destructuring-bind (spec lambda-list body)
  ;; 	     rest
  ;; 	   (flavor-notice-method function-spec)
  ;; 	     (fdefine-for-defmethod function-spec lambda-list t)
  ;; 	     `(defun ,function-spec ,(method-argument-list lambda-list function-spec)
  ;; 		(declare (:self-flavor ,flavor-name))
  ;; 		,@body)))
  ;; 	  (t
  ;; 	   ;; CLOS style defmethed
  ;; 	   (eval `(defmethed ,@rest))))


(defun run-flavor-defmethod-internal ()
  (let* ((spec '(ship :speed))
	 (gf-lambda-list '(type message alt))
	 (lambda-form '(lambda (message)
			(eval body)))
	 (lambda-list '(type  message))
	 (gf-name (intern (symbol-name (cadr spec))))
	 (body '(sqrt (+ (^ x-velocity 2)
		       (^ y-velocity 2)))))
    (flavor-defmethod-internal
     :lambda-list lambda-list
     :lambda-form lambda-form
     :gf-lambda-list gf-lambda-list
     :gf-name gf-name
     :qualifiers qualifiers
     :specializers specializers)))

(defun run-flavor-defmethod-internal-alt2 ()
  (let* ((spec '(ship :speed))
	 (lambda-list '())
	 (qualifiers '())
	 (gf-lambda-list '())
	 (specializers '())
	 (gf-name (intern (symbol-name (cadr spec))))
	 (body
	  ;;'(with-slots (x-velocity y-velocity) type
	  '(let ((x-velocity 4)
		  (y-velocity 5))
		 (sqrt (+ (expt x-velocity 2)
			(expt y-velocity 2)))))
	 (lambda-form `(lambda ,lambda-list
			 (eval ,body))))
    (let ((generic-function
	   (closer-mop:ensure-generic-function gf-name :lambda-list gf-lambda-list)))
      (closer-mop:ensure-method generic-function lambda-form
				:lambda-list lambda-list
				:qualifiers qualifiers
				:specializers specializers))))

(defun run-flavor-defmethod-internal-alt ()
  (let* ((spec '(ship :speed))
	 (gf-lambda-list '(type message alt))
	 (lambda-form '(lambda (message)
			(eval body)))
	 (lambda-list '(type  message))
	 (gf-name (intern (symbol-name (cadr spec))))
	 (body '(sqrt (+ (^ x-velocity 2)
		       (^ y-velocity 2))))
	 (generic-function
	  (closer-mop:ensure-generic-function gf-name :lambda-list gf-lambda-list)))
    (closer-mop:ensure-method generic-function lambda-form
			      :lambda-list lambda-list
			      :qualifiers '()
			      :specializers specializers)))

;; (PROGN
;;   (EVAL-WHEN (:COMPILE-TOPLEVEL :EXECUTE)
;;     (SB-PCL::COMPILE-OR-LOAD-DEFGENERIC 'DEFSTORAGE-PROCESSOR))
;;   (EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL)
;;     (SB-PCL::%DEFMETHOD-EXPANDER DEFSTORAGE-PROCESSOR NIL
;; 				 ((TYPE (EQL 'LISPM-FIXNUM)) ACCESSOR IGNORE
;; 				  SPEC WORD BIT)
;; 				 ((DECLARE (IGNORE IGNORE SPEC))
;; 				  (LET ((TO-BE-COMPILED NIL))
;; 				    (MULTIPLE-VALUE (WORD BIT)
;; 						    (DEFSTORAGE-ALIGN 'WORD WORD BIT))
;; 				    (MULTIPLE-VALUE (WORD BIT TO-BE-COMPILED)
;; 						    (DEFSTORAGE-PROCESSOR 'LISPM-FIXNUM-BYTES
;; 							ACCESSOR NIL '(4) WORD BIT))
;; 				    (VALUES WORD BIT TO-BE-COMPILED))))))
