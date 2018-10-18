(load "sob.asd")
(asdf:load-system :sob)

(defun reload ()
  (web-quit)
  (asdf:load-system :sob)
  (web-main))

(web-main)
