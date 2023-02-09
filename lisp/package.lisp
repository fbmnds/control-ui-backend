
(defpackage :svg-lib
  (:use :cl :cl-svg :parse-float)
  (:local-nicknames (#:lt #:local-time)
                    (#:ws #:websocket-driver)
                    (#:wsd #:websocket-driver-client)
                    (#:bt #:bordeaux-threads)
                    (#:lp #:lparallel)
                    (#:lpq #:lparallel.queue))
  (:export #:generate-svg))


