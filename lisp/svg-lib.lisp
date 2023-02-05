
(in-package :svg-lib)

(defparameter *n* nil)
(defparameter *m* nil)
(defparameter *h* nil)
(defparameter *w* nil)
(defparameter *left-right-margin* nil)
(defparameter *bottom-margin* nil)

(defparameter *data* nil)

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

(defun init-parameter ()
  (setf *data* nil)
  
  (setf *left-right-margin* nil)
  (setf *bottom-margin* nil)

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

  (setf *lbl-m* nil)
  (setf *lbl-text* nil)
  (setf *lbl-pos* nil)

  (setf *y1* nil)
  (setf *y2* nil)
  (setf *x* nil))

(defun set-input-parameter (&optional
                              (width 300) (n 10)
                              (height 200) (m 10)
                              (left-right-margin 22)
                              (bottom-margin 14)
                              (lbl-width 60))
  (assert (< 200 width))
  (assert (< 100 height))
  (assert (< m height))
  (setf *w* width *h* height *n* n *m* m
        *left-right-margin* left-right-margin
        *bottom-margin* bottom-margin
        *lbl-width* lbl-width))

(defun universal-timestamp (ts)
  (lt:timestamp-to-universal
   (lt:parse-timestring (substitute #\T #\  ts)
                        :allow-missing-elements t)))

(defun round10 (x) (/ (round (* 10 x)) 10))

(defun fmt10 (x) (format nil "~,1f" x))

(defun select-index (n m)
  (assert (<= m n))
  (let* ((idx (loop for i from 0 to (1- m) collect (floor (* i (/ n m)))))
        (v (car (last idx))))
    (assert (= (length idx) m))
    (when (< v (1- n))
      (mapcar (lambda (x) (+ x (- n v 1))) idx))))

(defun prepare-data (data)
  (assert data)
  (setf *n* (length data))
  (setf *temp* (make-array *n* :element-type 'float))
  (setf *hum* (make-array *n* :element-type 'float))
  (setf *ts* (make-array *n* :element-type 'float))
  (loop for d in data
        for %i from 0
        for temp = (gethash "temp" d)
        for hum = (gethash "hum" d)
        for ts = (gethash "ts" d)
        for uts = (ignore-errors (universal-timestamp ts))
        do (when uts
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

(defun fetch-data (&optional
                     (n *n*) (w *w*)
                     (database *database*))
  (let* ((idx (when (< w n) (select-index n w)))
         (cmd (str+
               "sqlite3 -json "
               database
               " 'select * from heating "
               " where not temp is null and not hum is null "
               " order by ts "
               (format nil "asc limit ~a;'" n)))
         (data (uiop:run-program cmd :force-shell t
                                     :output '(:string :stripped t)))
         
         (data (yason:parse data)))
    (when idx (setf data (loop for i in idx collect (nth i data))))
    (prepare-data data)))

(defun set-parameter (&optional
                        (min-temp *min-temp*)
                        (max-temp *max-temp*)
                        (min-hum *min-hum*)
                        (max-hum *max-hum*)
                        (min-ts *min-ts*)
                        (max-ts *max-ts*))  
  (let ((dtemp (- max-temp min-temp))
        (dhum (- max-hum min-hum))
        (dts (- *max-ts* *min-ts*)))
    (cond ((< dtemp 1.0)
           (setf min-temp (- min-temp (- 0.5 (/ dtemp 2.0)))
                 max-temp (+ max-temp 0.5)))
          (t
           (setf min-temp (- min-temp 0.4)
                 max-temp (+ max-temp 0.2))))
    (cond ((< dhum 1.0)
           (setf min-hum (- min-hum 0.5)
                 max-hum (+ max-hum (- 0.5 (/ dhum 2.0)))))
          (t
           (setf min-hum (- min-hum 0.2)
                 max-hum (+ max-hum 0.4))))
    (cond ((< dts 1830)
           (setf min-ts (- min-ts 915)
                 max-ts (+ max-ts 915)))
          (t
           (setf min-ts (- min-ts 305)
                 max-ts (+ max-ts 305))))
    (setf *min-temp* min-temp
          *max-temp* max-temp
          *dh-temp* (- *max-temp* *min-temp*)
          *min-hum* min-hum
          *max-hum* max-hum
          *dh-hum* (- *max-hum* *min-hum*)
          *min-ts* min-ts
          *max-ts* max-ts
          *dw* (- *max-ts* *min-ts*))))

(eval-when '(:compile-toplevel :load-toplevel)
  (defmacro mx-b (f max min h)
    (let ((x (gensym)))
      `(defun ,f (,x) (* (/ (- ,max ,x) (- ,max ,min)) ,h)))))

(defmacro make-strings (m)
  `(make-array ,m :element-type 'string :initial-element ""))

(defun transform-data (&optional
                         (h *h*) (w *w*) (n *n*)
                         (left-right-margin *left-right-margin*)
                         (bottom-margin *bottom-margin*)
                         (arr-temp *temp*)
                         (arr-hum *hum*)
                         (arr-ts *ts*)
                         (min-temp *min-temp*)
                         (max-temp *max-temp*)
                         (min-hum *min-hum*)
                         (max-hum *max-hum*)
                         (min-ts *min-ts*)
                         (max-ts *max-ts*))
  (setf *x* (make-array n :element-type 'float))
  (setf *y1* (make-strings n))
  (setf *y2* (make-strings n))
  (let ((h (- h bottom-margin))
        (w (- w (* 2 left-right-margin))))
    (assert (< 50 h))
    (assert (< 100 w))
    (mx-b mx-b-temp max-temp min-temp h)
    (assert (< (abs (- (mx-b-temp min-temp) h)) 0.001))
    (assert (< (abs (mx-b-temp max-temp)) 0.001))
    (mx-b mx-b-hum max-hum min-hum h)
    (assert (< (abs (- (mx-b-hum min-hum) h)) 0.001))
    (assert (< (abs (mx-b-hum max-hum)) 0.001))
    (mx-b mx-b-ts min-ts max-ts w)
    (assert (< (abs (- (mx-b-ts max-ts) w)) 0.001))
    (assert (< (abs (mx-b-ts min-ts)) 0.001))
    (loop for i from 0
          while (< i n)
          for temp = (aref arr-temp i)
          for hum = (aref arr-hum i)
          for ts = (aref arr-ts i)
          do (progn
               (setf (aref *y1* i) (fmt10 (mx-b-temp temp))
                     (aref *y2* i) (fmt10 (mx-b-hum hum))
                     (aref *x* i) (+ left-right-margin (mx-b-ts ts)))))))

(defun points (x fmt-y)
  (reduce (lambda (x acc) (str+ x " " acc))
          (loop for i from 0
                while (< i *n*)
                for %x = (fmt10 (aref x i))
                for %y = (aref fmt-y i)
                for s = (str+ %x "," %y)
                collect s)
          :initial-value ""))

(defmacro draw-polyline (y w)
  `(draw scene
       (:polyline :points (str+ lm "," ,y " " ,w "," ,y))
       :stroke "grey" :stroke-width 1 :stroke-dasharray "3,3" :fill "none"))

(defun format-ts (uts)
  (let ((days #("So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"))
        (ts (lt:universal-to-timestamp uts)))
    (format nil "~a ~2,'0d:~2,'0d"
            (aref days (lt:timestamp-day-of-week ts))
            (lt:timestamp-hour ts)
            (lt:timestamp-minute ts))))

(defun draw-svg (&optional
                   output
                   (h *h*) (w *w*) (n *n*) (m *m*)
                   (left-right-margin *left-right-margin*)
                   (bottom-margin *bottom-margin*)
                   (lbl-width *lbl-width*)
                   (dh-temp *dh-temp*) (min-temp *min-temp*)
                   (dh-hum *dh-hum*) (min-hum *min-hum*)
                   (arr-x *x*) (arr-y1 *y1*) (arr-y2 *y2*) (arr-ts *ts*))
  (let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height h :width w))
         (lm (format nil "~a" left-right-margin))
         (h-bm (- h bottom-margin))
         (w-lm (- w left-right-margin))
         (h-bm-s (format nil "~a" h-bm))
         (w-lm-s (format nil "~a" w-lm)))

    (draw scene (:rect :x 0 :y 0 :width w :height h) :fill "lavender")

    (loop for i from 1
          for %di = (* i (/ 1 m))
          while (< i m)
          do (draw-polyline (fmt10 (* %di h-bm)) w-lm-s)
             (text scene
                 (:x 1 :y (* (- 1 %di) h-bm) :font-size 10 :fill "green")
               (fmt10 (+ min-temp (* %di dh-temp))))
             (text scene
                 (:x (- w-lm -1) :y (* (- 1 %di) h-bm)
                  :font-size 10 :fill "blue")
               (fmt10 (+ min-hum (* %di dh-hum)))))
     
    (draw scene (:polyline :points (points arr-x arr-y1))
          :stroke "green" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (points arr-x arr-y2))
          :stroke "blue" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ lm ",0 " lm "," h-bm-s))
          :stroke "green" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ lm "," h-bm-s " " w-lm-s "," h-bm-s))
          :stroke "black" :stroke-width 1 :fill "none")
    (draw scene (:polyline :points (str+ w-lm-s ",0 " w-lm-s "," h-bm-s))
          :stroke "blue" :stroke-width 1 :fill "none")

    (let (idx
          (n2 (floor (/ w-lm lbl-width))))
      (if (< n2 n)
          (setf idx (select-index n n2))
          (setf idx (loop for %i in idx while (< %i n) collect %i)))
      (loop for i in idx
            for x = (aref arr-x i)
            for y = (- h 3)
            for lb = (format-ts (aref arr-ts i))
            do (text scene
                   (:x (- x 22) :y y :font-size 9 :fill "black") lb)
               (draw scene (:polyline
                            :points (str+ (fmt10 x) ",0 " (fmt10 x) "," h-bm-s))
                     :stroke "lightgrey" :stroke-width 1 :fill "none")))
    
    (if (eql output :string)
        (with-output-to-string (s) (stream-out s scene))
        (with-open-file (s *svg-file* :direction :output :if-exists :supersede)
          (stream-out s scene)))))

(defun generate-svg (&optional
                       output
                       (width 300) (n 1200)
                       (height 200) (m 10)
                       (left-right-margin 22)
                       (bottom-margin 14)
                       (lbl-width 60))
  (init-parameter)
  (set-input-parameter
   width n height m left-right-margin bottom-margin lbl-width)
  (fetch-data)
  (set-parameter)
  (transform-data)
  (draw-svg output))
