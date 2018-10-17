(load "sob.asd")
(asdf:load-system :sob)

(defun reload ()
  (asdf:load-system :sob)
  (web-quit)
  (web-main))

(web-main)
