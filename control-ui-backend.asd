(defsystem :control-ui-backend
  :serial t
  :depends-on (:cl-svg :local-time :yason :parse-float
               :bordeaux-threads :lparallel
               :hunchentoot :clack :websocket-driver :websocket-driver-client)
  :components ((:file "lisp/package")
               (:file "lisp/x")
               (:file "lisp/env")
               (:file "lisp/svg-lib")
               (:file "lisp/server")))
