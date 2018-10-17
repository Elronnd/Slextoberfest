(eval-when (:compile-toplevel :load-toplevel :execute)
    (ql:quickload '(:clack
                     :ningle)))

(defpackage #:web
  (:documentation "Web server")
  (:export :web-main
           :web-quit))

(defparameter *port* 4242)
(defparameter *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      (lambda (params)
        (html (list '(head (link :type "text/css" :rel "stylesheet" :href "/static/stylesheet.css")) (cons 'body *header*)))))
        ;'(head (link :type "text/css" :rel "stylesheet" :href "stylesheet.css"))

(defun web-quit ()
  (clack:stop *handler*))
(defun web-main ()
  (defparameter *handler*
    (clack:clackup *app* :port *port* :server :fcgi :silent nil :use-thread t)))
