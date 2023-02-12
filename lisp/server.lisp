
(defpackage :rx
  (:use :cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:route))

(in-package :rx)

(defun route (env-path path rc hdr body &optional ends-with)
  (when (if ends-with
            (a:ends-with-subseq path env-path)
            (a:starts-with-subseq path env-path))
    (if (pathnamep body)
        `(,rc ,hdr ,body)
        `(,rc ,hdr (,body)))))


(in-package :svg-lib)

(defparameter *clack-server* nil)
(defparameter *svg-thread* nil)
(defparameter *request-queue* (lpq:make-queue))
(defparameter *svg-intervall* 5)
(defparameter *svg-clients* (list "192.168.178.31"))

(defclass request () ())

(defclass broadcast-request (request) ())

(defclass register-request (request)
  ((url :initarg :url :accessor url)))

(defclass remove-request (request)
  ((url :initarg :url :accessor url)))

(defgeneric fulfill (request))

(defun send-svg(url)
  (let* ((svg (generate-svg :string))
         (url (str+ "ws://" url ":7700/"))
         (client (wsd:make-client url)))
    (progn
     (ws:on :open client (lambda () (format t "~&connected~%")))
     (ws:on :message client (lambda (message) (format t " ~a" message)))
     (ws:start-connection client)
     (ws:send client svg)
     (sleep 1))
    (ws:close-connection client)))

(defmethod fulfill ((request broadcast-request))
  (loop for url in *svg-clients* do (send-svg url)))

(defmethod fulfill ((request register-request))
  (let ((url (url request)))
    (unless (member url *svg-clients* :test 'equal)
      (push url *svg-clients*))))

(defmethod fulfill ((request remove-request))
  (let ((url (url request))) 
    (setf *svg-clients*
          (remove-if (lambda (%url) (equal %url url)) *svg-clients*))))

(defun start-svg-thread ()
  (setf *svg-thread*
        (bt:make-thread (lambda ()
                          (loop when (lpq:queue-empty-p *request-queue*)
                                  do (sleep *svg-intervall*)
                                do (fulfill (lpq:pop-queue *request-queue*))))
                        :name "svg")))

(defun handler (env)
  (let (;;(js-hdr '(:content-type "application/javascript"))
        ;;(json-hdr '(:content-type "application/json"))
        (svg-hdr '(:content-type "image/svg+xml"))
        ;;(x-icon-hdr '(:content-type "image/x-icon"))
        ;;(plain-text-hdr '(:content-type "plain/text"))
        (path (getf env :path-info)))
    (handler-case
        (or
         ;;(rx:route path "/index.html"
         ;;          200 '(:access-control-allow-origin "*") *index*)
         ;;#-ecl (rx:route path "/assets/favicon.ico" 200 x-icon-hdr *favicon* t)
         (when (x:starts-with "/svg" path)
           (let ((svg (generate-svg :string)))
             `(200 ,svg-hdr (,svg))))
         (when (x:starts-with "/register/192.168.178." path)
           (format t "register")
           (lpq:push-queue
            (make-instance 'register-request
                           :url (subseq path (length "/register/")))
            *request-queue*)
           `(200 nil ("")))
         (when (x:starts-with "/remove/192.168.178." path)
           (format t "remove")
           (lpq:push-queue
            (make-instance 'remove-request
                           :url (subseq path (length "/remove/")))
            *request-queue*)
           `(200 nil ("")))
         (when (x:starts-with "/broadcast" path)
           (format t "broadcast")
           (fullfill (make-instance 'broadcast-request))
           `(200 nil ("")))
         `(404 nil (,(format nil "Path not found~%"))))
      (t (e) (if *debug*
                 `(500 nil (,(format nil "Internal Server Error~%~A~%" e)))
                 `(500 nil (,(format nil "Internal Server Error"))))))))

(defun start (handler)
  (setf *clack-server* 
        (clack:clackup handler :server :woo :address "0.0.0.0" :port 7000)))

(defun stop ()
  (prog1
      (clack:stop *clack-server*)
    (setf *clack-server* nil)))
