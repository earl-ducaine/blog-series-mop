


(defpackage :flavor-internal
  (:use cl)
  (:export
   flavor-defmethod))

(defpackage :flavor
  (:nicknames :user-flavor)
  (:use cl)
  (:shadow defmethod)
  (:export
   defmethod))
