(defpackage #:html
  (:documentation "Parse sexp into html strings")
  (:export :html))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "fset"))

; self-closing elements like <link>, <br>
(defparameter *self-closing-elems* (fset:set 'link 'br))

; stringifies things in the way that I would like and probably not in a way that would be appreciated by anyone else
(defun stringify (obj)
  (cond
    ((listp obj) (html-to-str obj))
    ((stringp obj) obj)
    (T (write-to-string obj))))

(defun interp-fn (obj)
  (if (listp obj)
      (cond
        ((equalp (car obj) '$) (handle-interps (eval (cdr obj))))
        ((equalp (car obj) '@) (handle-interps (eval (cadr obj))))
        ((equalp (car obj) '@u) (cons '@span (map 'list #'eval (cdr obj)))) ; (u)pcase list into parent context
        ((equalp (car obj) '@f) (apply #'format (cons nil (cdr obj)))) ; (f)ormatted text
        (T (handle-interps obj)))
      obj))

(defun handle-interps (lis)
  (if (listp lis)
      (map 'list #'interp-fn lis)
      lis))


(defun html (lis)
  (format nil "<!doctype html><html lang=\"en\">~A</html>" (html-to-str (handle-interps lis))))

; in '(b :style "background-color:black" (i "nein") "frobert"), handles (i "nein") "frobert"
(defun elems-rest (lis &optional (is-first T))
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

(defun sym-or-string (a)
  (cond
    ((stringp a) a)
    ((symbolp a) (string-downcase (symbol-name a)))
    (T (error "Expected string or symbol"))))


; and this handles the :style "background-color:black"
(defun keywords-rest (lis)
  (let ((head (car lis)))
    (cond
      ((null lis) "")
      ((keywordp head) (format nil " ~A=\"~A\"~A"
                               (string-downcase (symbol-name head)) (sym-or-string (cadr lis))
                               (keywords-rest (cddr lis))))
      (T (keywords-rest (cdr lis))))))


(defun html-to-str (lis)
  (let ((this (car lis)))
    (cond
      ((null this) "")
      ((equalp (car this) '@span) (reduce (lambda (a b) (concatenate 'string a b))
                                          (map 'list #'elems-rest (cdr this))))
      (T
       (format nil "<~A~A>~A~A"
               (string-downcase (symbol-name (car this)))
               (keywords-rest (cdr this))
               (if (fset:contains? *self-closing-elems* (car this))
                   ""
                   (format nil "~A</~A>"
                           (elems-rest (cdr this) T)
                           (string-downcase (symbol-name (car this)))))
               (html-to-str (cdr lis)))))))
