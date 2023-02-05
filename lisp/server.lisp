
(defpackage :rx
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)))

(in-package :rx)

(defun route (env-path path rc hdr body &optional ends-with)
  (when (if ends-with
            (a:ends-with-subseq path env-path)
            (a:starts-with-subseq path env-path))
    (if (pathnamep body)
        `(,rc ,hdr ,body)
        (let ((b `,body)) `(,rc ,hdr (,b))))))

(export 'route)


(in-package :svg-lib)

(defparameter *clack-server* nil)

(defun handler (env)
  (let (;;(js-hdr '(:content-type "application/javascript"))
        ;;(json-hdr '(:content-type "application/json"))
        (svg-hdr '(:content-type "image/svg+xml"))
        ;;(x-icon-hdr '(:content-type "image/x-icon"))
        ;;(plain-text-hdr '(:content-type "plain/text"))
        (path (getf env :path-info)))
    (handler-case
        (or
         (rx:route path "/svg" 200 svg-hdr
                   `,(progn (draw-svg :string)))
         ;;(rx:route path "/index.html"
         ;;          200 '(:access-control-allow-origin "*") *index*)
         ;;#-ecl (rx:route path "/assets/favicon.ico" 200 x-icon-hdr *favicon* t)
         ;;(rx:route path "/assets/data.csv" 200 plain-text-hdr *data* t)
         `(404 nil (,(format nil "Path not found~%"))))
      (t (e) (if *debug*
                 `(500 nil (,(format nil "Internal Server Error~%~A~%" e)))
                 `(500 nil (,(format nil "Internal Server Error"))))))))

(defun start (handler)
  (setf *clack-server* 
        (clack:clackup handler :server :woo :address "0.0.0.0" :port 7700)))

(defun stop ()
  (prog1
      (clack:stop *clack-server*)
    (setf *clack-server* nil)))
