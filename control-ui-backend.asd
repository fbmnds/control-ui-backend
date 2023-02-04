(defsystem :app
  :serial t
  :depends-on (:cl-svg :local-time :yason :parse-float)
  :components ((:file "lisp/svg-lib")))
