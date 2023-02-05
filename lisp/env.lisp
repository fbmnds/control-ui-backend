
(in-package :svg-lib)

(defparameter *debug* t)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defparameter *relay-url*
  '("http://ESP-9BE861-11" "http://ESP-16E5F0" "http://ESP-4DCC5F"))

(defparameter *paths* (make-hash-table))

(defmacro path+ (pathname rel-path)
  `(merge-pathnames ,rel-path ,pathname))

(defun merge-project-path (key rel-path)
  (setf (gethash key *paths*)
        (merge-pathnames rel-path (gethash :project *paths*))))

(defun env-path (key) (gethash key *paths*))

(setf (gethash :home *paths*) (user-homedir-pathname))
(setf (gethash :project *paths*)
      (merge-pathnames #p"projects/control-ui-backend/" (gethash :home *paths*)))

(merge-project-path :svg #p"svg/")
(merge-project-path :database #p"data/heating.db")


(defparameter *database* (format nil "~a" (env-path :database)))
(defparameter *svg-file* (path+ (env-path :svg) "test.svg"))
