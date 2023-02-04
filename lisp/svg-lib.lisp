(ql:quickload :cl-svg)
(ql:quickload :local-time)
(ql:quickload :yason)
(ql:quickload :parse-float)



(defpackage :svg-lib
  (:use :cl :cl-svg :parse-float)
  (:local-nicknames (#:lt #:local-time)))

(in-package :svg-lib)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defparameter *s* nil)
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
  (draw scene (:rect :x 40 :y 40 :height 30 :width 30)
        :stroke "blue" :stroke-width 1 :fill "yellow")
  (draw scene (:rect :x 75 :y 75 :height 30 :width 30)
        :fill "purple")
  (setf *s* scene))
  (with-output-to-string (s) (stream-out s *s*))

(defparameter *n* nil)
(defparameter *m* nil)
(defparameter *h* nil)
(defparameter *w* nil)

(defparameter *data* nil)

(defparameter *left-right-margin* nil)
(defparameter *bottom-margin* nil)

(defparameter *temp* (make-array *n* :element-type 'float))
(defparameter *hum* (make-array *n* :element-type 'float))
(defparameter *ts* (make-array *n* :element-type 'float))

(defparameter *state* nil)
(defparameter *max-temp* nil)
(defparameter *min-temp* nil)
(defparameter *max-hum* nil)
(defparameter *min-hum* nil)
(defparameter *max-ts* nil)
(defparameter *min-ts* nil)
(defparameter *dh-temp* nil)
(defparameter *dh-hum* nil)
(defparameter *dw* nil)

(defparameter *lbl-width* nil)
(defparameter *lbl-m* nil)
(defparameter *lbl-text* nil)
(defparameter *lbl-pos* nil)

(defparameter *y1* (make-array *n* :element-type 'float))
(defparameter *y2* (make-array *n* :element-type 'float))
(defparameter *x* (make-array *n* :element-type 'float))

(defun set-input-parameter (&optional (width 300) (n 10) (height 200) (m 10))
  (setf *w* width
        *n* n
        *h* height
        *m* m))

(defun init-parameter ()
  (setf *data* nil)
  
  (setf *left-right-margin* nil)
  (setf *bottom-margin* nil)

  (setf *temp* (make-array *n* :element-type 'float))
  (setf *hum* (make-array *n* :element-type 'float))
  (setf *ts* (make-array *n* :element-type 'float))

  (setf *state* nil)
  (setf *max-temp* nil)
  (setf *min-temp* nil)
  (setf *max-hum* nil)
  (setf *min-hum* nil)
  (setf *max-ts* nil)
  (setf *min-ts* nil)
  (setf *dh-temp* nil)
  (setf *dh-hum* nil)
  (setf *dw* nil)

  (setf *lbl-width* 80)
  (setf *lbl-m* nil)
  (setf *lbl-text* nil)
  (setf *lbl-pos* nil)

  (setf *y1* (make-array *n* :element-type 'float))
  (setf *y2* (make-array *n* :element-type 'float))
  (setf *x* (make-array *n* :element-type 'float)))

(defun select-index (n m)
  (nreverse (loop for i from 0 to (1- m) collect (- n (floor (* i (/ n m)))))))

(defun fetch-data (&optional (n *n*))
  (assert (and (integerp n) (> n 0)))
  (let* ((cmd (str+
               "sqlite3 -json "
               (uiop:getenv "HOME")
               "/projects/control-ui-backend/data/heating.db"
               " 'select * from heating order by ts "
               (format nil "asc limit ~a;'" n)))
         (data (uiop:run-program cmd :force-shell t
                                     :output '(:string :stripped t))))
    (when (stringp data) (yason:parse data))))

(defun universal-timestamp (ts)
  (lt:timestamp-to-universal
   (lt:parse-timestring (substitute #\T #\  ts)
                        :allow-missing-elements t)))

(defun round10 (x) (/ (round (* 10 x)) 10))

(defun fmt10 (x) (format nil "~,1f" x))

(defun prepare-data (&optional (n *n*))
  (assert (setf *data* (fetch-data n)))
  (loop for d in *data*
        for %i from 0
        for temp = (gethash "temp" d)
        for hum = (gethash "hum" d)
        for ts = (gethash "ts" d)
        for uts = (universal-timestamp ts)
        do (progn
             (setf (aref *temp* %i) temp)
             (setf (aref *hum* %i) hum)
             (setf (aref *ts* %i) uts)
             (setf *state* (gethash "state" d)))
        maximize temp into max-temp
        minimize temp into min-temp
        maximize hum into max-hum
        minimize hum into min-hum
        maximize uts into max-ts
        minimize uts into min-ts
        finally (setf *max-temp* max-temp
                      *min-temp* min-temp
                      *max-hum* max-hum
                      *min-hum* min-hum
                      *max-ts* max-ts
                      *min-ts* min-ts)))

(defun set-parameter (&optional (left-right-margin 22) (bottom-margin 14))
  (setf *left-right-margin* left-right-margin)
  (setf *bottom-margin* bottom-margin)
  (let ((dtemp (- *max-temp* *min-temp*)))
    (cond ((< dtemp 1.0)
           (setf *min-temp* (- *min-temp* 0.5)
                 *max-temp* (+ *max-temp* (- 0.5 (/ dtemp 2.0)))))
          (t
           (setf *min-temp* (- *min-temp* 0.4)
                 *max-temp* (+ *max-temp* 0.2))))
    (setf *dh-temp* (- *max-temp* *min-temp*)))
  (let ((dhum (- *max-hum* *min-hum*)))
    (cond ((< dhum 1.0)
           (setf *min-hum* (- *min-hum* (- 0.5 (/ dhum 2.0)))
                 *max-hum* (+ *max-hum* 0.5)))
          (t
           (setf *min-hum* (- *min-hum* 0.2)
                 *max-hum* (+ *max-hum* 0.4))))
    (setf *dh-hum* (- *max-hum* *min-hum*)))
  (let ((dts (- *max-ts* *min-ts*)))
    (cond ((< dts 1830)
           (setf *min-ts* (- *min-ts* 915)
                 *max-ts* (+ *max-ts* 915)))
          (t
           (setf *min-ts* (- *min-ts* 305)
                 *max-ts* (+ *max-ts* 305))))
    (setf *dw* (- *max-ts* *min-ts*))))

(defmacro mx-b (f max min h)
  (let ((x (gensym)))
    `(defun ,f (,x) (* (/ (- ,max ,x) (- ,max ,min)) ,h))))

(defun transform-data (&optional (n *n*))
  (let ((h (- *h* *bottom-margin*))
        (w (- *w* (* 2 *left-right-margin*))))
    (assert (< 50 h))
    (assert (< 100 w))
    (mx-b mx-b-temp *max-temp* *min-temp* h)
    (assert (< (abs (- (mx-b-temp *min-temp*) h)) 0.001))
    (assert (< (abs (mx-b-temp *max-temp*)) 0.001))
    (mx-b mx-b-hum *max-hum* *min-hum* h)
    (assert (< (abs (- (mx-b-hum *min-hum*) h)) 0.001))
    (assert (< (abs (mx-b-hum *max-hum*)) 0.001))
    (mx-b mx-b-ts *min-ts* *max-ts* w)
    (assert (< (abs (- (mx-b-ts *max-ts*) w)) 0.001))
    (assert (< (abs (mx-b-ts *min-ts*)) 0.001))
    (loop for i from 0
          while (< i n)
          for temp = (aref *temp* i)
          for hum = (aref *hum* i)
          for ts = (aref *ts* i)
          do (progn
               (setf (aref *y1* i) (fmt10 (mx-b-temp temp)))
               (setf (aref *y2* i) (fmt10 (mx-b-hum hum)))
               (setf (aref *x* i)
                     (fmt10 (+ *left-right-margin* (mx-b-ts ts))))))))

(defun points (x y)
  (reduce (lambda (x acc) (str+ x " " acc))
          (loop for i from 0
                while (< i *n*)
                for %x = (aref x i)
                for %y = (aref y i)
                for s = (str+ %x "," %y)
                collect s)
          :initial-value ""))

(defmacro draw-polyline (factor)
  `(draw scene (:polyline
                :points (str+ lm ","
                              (fmt10 (* ,factor (- *h* *bottom-margin*)))
                              " " w ","
                              (fmt10 (* ,factor (- *h* *bottom-margin*)))))
         :stroke "grey" :stroke-width 1 :stroke-dasharray "3,3" :fill "none"))

(defun format-ts (uts)
  (let ((days #("So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"))
        (ts (lt:universal-to-timestamp uts)))
    (format nil "~a ~2,'0d:~2,'0d"
            (aref days (lt:timestamp-day-of-week ts))
            (lt:timestamp-hour ts)
            (lt:timestamp-minute ts))))

(defmacro make-strings (m)
  `(make-array ,m :element-type 'string :initial-element ""))

(defun time-labels (&optional (m *m*))
  (assert (and (integerp m) (> m 0)))
  (assert (and (integerp *n*) (> *n* 0)))
  (assert (and *x* *ts*))
  (let ((w (- *w* (* 2 *left-right-margin*))))
    (when (< w (* m *lbl-width*))
      (setf m (min *n* m (floor (/ w *lbl-width*)))))
    (cond ((= m 1)
           (let ((%i (+ (floor (/ *n* 2)))))
             (setf *lbl-m* 1
                   *lbl-text* (make-strings 1)
                   *lbl-pos* (make-strings 1)
                   (aref *lbl-text* 0) (format-ts (aref *ts* %i))
                   (aref *lbl-pos* 0) (aref *x* %i))))
          ((= m *n*)
           (setf *lbl-m* (- m 2)
                 *lbl-text* (make-strings *lbl-m*)
                 *lbl-pos* (make-strings *lbl-m*))
           (loop for i from 1
                 while (<= i *lbl-m*)
                 for text = (format-ts (aref *ts* (1- i)))
                 do (setf (aref *lbl-pos* (1- i)) (aref *x* (1- i))
                          (aref *lbl-text* (1- i)) text)))
          ((< (/ *n* 3) m)
           (setf *lbl-m* (min m (floor (/ *n* 2)))
                 *lbl-text* (make-strings *lbl-m*)
                 *lbl-pos*(make-strings *lbl-m*))
           (if (evenp m)
               (loop for i from 0
                 while (< i *lbl-m*)
                 for %j = (1+ (* 2 i))
                 for text = (format-ts (aref *ts* %j))
                 do (setf (aref *lbl-pos* i) (aref *x* %j)
                          (aref *lbl-text* i) text))
               (loop for i from 0
                 while (< i *lbl-m*)
                 for %j = (* 2 i)
                 for text = (format-ts (aref *ts* %j))
                 do (setf (aref *lbl-pos* i) (aref *x* %j)
                          (aref *lbl-text* i) text))))
          (t
           (setf *lbl-m* m
                 *lbl-text* (make-strings m)
                 *lbl-pos*(make-strings m))
           (let ((%di (floor (/ *n* m))))
             (loop for i from 1
                 
                   while (< i m)
                   for %j = (* %di i)
                   while (< %j *n*)
                   for text = (format-ts (aref *ts* (1- i)))
                   do (setf (aref *lbl-pos* (1- i)) (aref *x* (1- i))
                            (aref *lbl-text* (1- i)) text)))))))

(defun generate-svg ()
  (let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height *h* :width *w*))
        (lm (format nil "~a" *left-right-margin*))
        (h (format nil "~a" (- *h* *bottom-margin*)))
        (w (format nil "~a" (- *w* *left-right-margin*))))

    (draw scene (:rect :x 0 :y 0 :width *w* :height *h*) :fill "lavender")

    (loop for i from 1
          for %di = (* i (/ 1 *m*))
          while (< i *m*)
          do (draw-polyline %di)
             (text scene
                 (:x 1 :y (* (- 1 %di) (- *h* *bottom-margin*))
                  :font-size 10 :fill "green")
               (fmt10 (+ *min-temp* (* %di (- *max-temp* *min-temp*)))))
             (text scene
                 (:x (- *w* *left-right-margin* -1)
                  :y (* (- 1 %di) (- *h* *bottom-margin*))
                  :font-size 10 :fill "blue")
               (fmt10 (+ *min-hum* (* %di (- *max-hum* *min-hum*))))))
    
    (draw scene (:polyline :points (points *x* *y1*))
          :stroke "green" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (points *x* *y2*))
          :stroke "blue" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ lm ",0 " lm "," h))
          :stroke "green" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ lm "," h " " w "," h))
          :stroke "black" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ w ",0 " w "," h))
          :stroke "blue" :stroke-width 1 :fill "none")



    #|
    (loop for i from 1                  ; ;
    while (< i *n*)                     ; ;
    for %di = (* i (/ 1 *n*))           ; ;
    for x = (* %di (- *w* *left-right-margin*)) ; ;
    for y = (- *h* 3)                   ; ;
    for lb = (format-ts (aref *ts* i))  ; ;
    do (when (evenp i)                  ; ;
    (text scene                         ; ;
    (:x (- x 20) :y y :font-size 9 :fill "black") lb) ; ;
    (draw scene (:polyline              ; ;
    :points (str+ (fmt10 x) ",0 " (fmt10 x) "," h)) ; ;
    :stroke "lightgrey" :stroke-width 1 :fill "none"))) ; ;
    |#
    
    (loop for i from 0
          while (< i *lbl-m*)
          for pos = (aref *lbl-pos* i)
          for x = (- (parse-float pos) (/ *lbl-width* 2))
          for y = (- *h* 3)
          for lb = (aref *lbl-text* i)
          do (when (evenp i)
               (text scene
                   (:x x :y y :font-size 9 :fill "black") lb)
               (draw scene (:polyline
                            :points (str+ pos ",0 " pos "," h))
                     :stroke "lightgrey" :stroke-width 1 :fill "none")))
    
    
    (with-open-file (s #p"svg/test.svg" :direction :output :if-exists :supersede)
      (stream-out s scene))
    ;;(with-output-to-string (s) (stream-out s scene))
    ))

