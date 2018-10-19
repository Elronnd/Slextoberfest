(defpackage #:templates
  (:documentation "Collection of html templates")
  (:export :*header*))

;(defparameter *header* '(a :href "/test" "linky!"))
(defparameter *header* '((a :href "/test" "linky!")
                         (br)
                         "Here is some normal text"))
