(defpackage #:html
  (:documentation "Parse sexp into html strings")
  (:export :html))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "fset"))

; self-closing elements like <link>, <br>
(defparameter *self-closing-elems* (fset:set 'link 'br))

(defun html (lis)
  (format nil "<!doctype html>~%<html lang=\"en\">~A</html>" (html-to-str lis)))

; in '(b :style "background-color:black" (i "nein") "frobert"), handles (i "nein") "frobert"
(defun elems-rest (lis is-first)
  (let ((head (car lis)))
    (cond
      ((null lis) "")
      ((listp head) (format nil "~A~A"
                            (html-to-str (list (car lis)))
                            (elems-rest (cdr lis) nil)))
      ((stringp head) (format nil "~A~A~A"
                              (if is-first "" " ")
                              (car lis)
                              (elems-rest (cdr lis) nil)))
      ((keywordp head) (elems-rest (cddr lis) is-first))))) ;ignore


; and this handles the :style "background-color:black"
(defun keywords-rest (lis)
  (let ((head (car lis)))
    (cond
      ((null lis) "")
      ((keywordp head) (format nil " ~A=\"~A\"~A"
                               (string-downcase (symbol-name head)) (cadr lis)
                               (keywords-rest (cddr lis))))
      (T (keywords-rest (cdr lis))))))

(defun html-to-str (lis)
  (let ((this (car lis)))
    (if (null this)
        ""
        (format nil "<~A~A>~A"
                (string-downcase (symbol-name (car this)))
                (keywords-rest (cdr this))
                (if (fset:contains? *self-closing-elems* (car this))
                    ""
                    (format nil "~A</~A>~A"
                            (elems-rest (cdr this) T)
                            (string-downcase (symbol-name (car this)))
                            (html-to-str (cdr lis))))))))
      ;((contains? *self-closing-elems* (car this))
      ;  ""
