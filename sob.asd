; vim: ft=lisp
(defpackage #:sob-asd ; SlextOBerfest
  (:use :cl :asdf))

(in-package :sob-asd)

(defsystem sob
  :name "Slextoberfest"
  :version "0.0.0"
  :maintainer "Elronnd"
  :author "Elronnd"
  :license "BSD-3"
  :description "Slash'EM Extended tournament in october."
  :long-description "Slash'EM Extended tournament in October."
  
  :components ((:file "./src/html")
               (:file "./src/templates")
               (:file "./src/web"
                      :depends-on ("./src/html"
                                   "./src/templates"))))
