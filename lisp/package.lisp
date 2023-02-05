(ql:quickload :cl-svg)
(ql:quickload :local-time)
(ql:quickload :yason)
(ql:quickload :parse-float)
(ql:quickload :woo)
(ql:quickload :clack)


(defpackage :svg-lib
  (:use :cl :cl-svg :parse-float)
  (:local-nicknames (#:lt #:local-time))
  (:export #:generate-svg))


