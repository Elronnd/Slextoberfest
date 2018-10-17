(defpackage #:templates
  (:documentation "Collection of html templates")
  (:export :*header*))

(defparameter *header* '((a :href "/test" "linky!")
                         (br)
                         (span "Here is some normal text")))
