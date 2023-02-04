(defsystem :app
  :serial t
  :depends-on (:cl-svg :local-time :yason)
  :components ((:file "lisp/svg-lib")))
