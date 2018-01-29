




;; Some none MOP interfaces


(defmethod some-operation ((a integer) (b float)) (list a b))

(defparameter some-operation
  (find-method #'some-operation '() (mapcar #'find-class '(integer float))))



(find-method #'some-operation '() (mapcar #'find-class '(integer float)))

(find-method #'some-operation '() (mapcar #'find-class '(integer integer)))

(find-method #'some-operation '() (mapcar #'find-class '(integer integer)) nil)






(dolist (slot (closer-mop:class-direct-slots (class-of some-operation))) (format t "~a~%" (closer-mop::slot-definition-name slot)))
