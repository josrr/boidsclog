;;;; boids.lisp
;;;; Copyright 2025 José M. Á. Ronquillo Rivera

(in-package #:boids)

(defclass boids-parameters ()
  ((protected-range :initarg :protected-range :initform 8.0 :reader protected-range)
   (visible-range :initarg :visible-range :initform 40.0 :reader visible-range)
   (avoid-factor :initarg :avoid-factor :initform 0.05 :reader avoid-factor)
   (matching-factor :initarg :matching-factor :initform 0.05 :reader matching-factor)
   (centering-factor :initarg :centering-factor :initform 0.00005 :reader centering-factor)
   (turn-factor :initarg :turn-factor :initform 0.2 :reader turn-factor)))

(defclass boid ()
  ((group :initform (random 4) :reader group :type fixnum)
   (bias :initform (random 1.0) :reader bias :type single-float)
   (location :initarg :location :accessor location)
   (velocity :initarg :velocity :initform (3dv:vec2 (- (random 10.0) 5.0)
                                                    (- (random 10.0) 5.0))
             :accessor velocity)))

(defgeneric draw (object &optional pane &rest drawing-options))

(defmethod draw ((boid boid) &optional (pane *standard-output*) &rest drawing-options)
  ;;let ((boid-width 8.0) (boid-length 16.0))
  (with-accessors (;;(velocity velocity)
                   (location location)) boid
    #|(let* ((direction (if (zerop (3dv:vlength velocity))
    (3dv:vec2 0.0 -1.0)
    (3dv:v/ velocity (3dv:v2norm velocity))))
    (p0 (3dv:v* direction boid-length 0.5))
    (p1 (3dv:v* direction boid-length -0.5))
    (p2 (3dv:v* direction boid-length -0.5)))
    (3dv:nv+ p0 location)
    (3dv:nv+ p1 location (3dv:v* (3dv:vrot2 direction (/ pi 2)) boid-width 0.5))
    (3dv:nv+ p2 location (3dv:v* (3dv:vrot2 direction (/ pi -2)) boid-width 0.5))
    ;;(apply #'draw-polygon* pane (list (3dv:vx p0) (3dv:vy p0) (3dv:vx p1) (3dv:vy p1) (3dv:vx p2) (3dv:vy p2)) drawing-options)
    )|#
    #|(apply #'draw-point* pane (3dv:vx location) (3dv:vy location) :line-thickness boid-width drawing-options)|#
    (values (3dv:vx location) (3dv:vy location))))

(defgeneric update-location (object))
(defgeneric update-velocity (object boids parameters destination))
(defgeneric rule (object boids parameters number))

(defmethod update-location ((boid boid))
  (declare (optimize (speed 3)))
  (3dv:nv+ (location boid) (velocity boid)))


(defparameter *min-speed* 3.0)
(defparameter *max-speed* 40.0)
(declaim (type single-float *min-speed* *max-speed*))

(defmethod update-velocity ((boid boid) boids parameters destination)
  (declare (optimize (speed 3)))
  (3dv:nv+ (velocity boid)
           (rule boid boids parameters 1)
           (rule boid boids parameters 2)
           (rule boid boids parameters 3))
  ;;(3dv:v* (3dv:v- destination (location boid)) 0.001)
  (rule boid boids parameters 4)
  (let ((speed (3dv:vlength (velocity boid))))
    (declare (type single-float speed))
    (if (> speed *max-speed*)
        (3dv:nv* (velocity boid) (/ *max-speed* speed))
        (when (< speed *min-speed*)
          (3dv:nv* (velocity boid) (/ *min-speed* speed)))))
  (when (< (group boid) 2)
    (setf (3dv:vx (velocity boid))
          (+ (* (- 1.0f0 (bias boid)) (3dv:vx (velocity boid)))
             (* (if (zerop (group boid)) 1.0f0 -1.0f0)
                (bias boid))))))

;;; Separation
(defmethod rule ((boid boid) boids parameters (number (eql 1)))
  (declare (ignore number) (optimize (speed 3)))
  (loop with v-sum = (3dv:vec2)
        for boid1 in boids
        for offset = (3dv:v- (location boid1) (location boid))
        if (and (not (eq boid boid1)) (< (3dv:vlength offset)
                                         (the single-float (protected-range parameters))))
          do (3dv:nv- v-sum offset)
        finally (return (3dv:nv* v-sum (avoid-factor parameters)))))

;;; Cohesion
(defmethod rule ((boid boid) boids parameters (number (eql 2)))
  (declare (ignore number) (optimize (speed 3)))
  (loop with center = (3dv:vec2) and neighbors fixnum = 0
        for boid1 in boids
        if (and (not (eq boid boid1))
                (< (3dv:vdistance (location boid1) (location boid))
                   (the single-float (visible-range parameters))))
          do (3dv:nv+ center (location boid1))
             (incf neighbors)
        finally (return (if (> neighbors 0)
                            (3dv:nv* (3dv:nv- (3dv:nv/ center neighbors)
                                              (location boid))
                                     (centering-factor parameters))
                            (3dv:vec2)))))

;;; Alignment
(defmethod rule ((boid boid) boids parameters (number (eql 3)))
  (declare (ignore number) (optimize (speed 3)))
  (loop with result = (3dv:vec2) and neighbors fixnum = 0
        for boid1 in boids
        if (and (not (eq boid boid1))
                (< (3dv:vdistance (location boid1) (location boid))
                   (the single-float (visible-range parameters))))
          do (3dv:nv+ result (velocity boid1))
             (incf neighbors)
        finally (return (if (> neighbors 0)
                            (3dv:nv* (3dv:nv- (3dv:nv/ result neighbors)
                                              (velocity boid))
                                     (matching-factor parameters))
                            (3dv:vec2)))))

;;; Screen edges
(defmethod rule ((boid boid) boids parameters (number (eql 4)))
  (declare (ignore number) (optimize (speed 3)))
  (when (< (3dv:vx (location boid)) -312.0)
    (setf (3dv:vx (velocity boid)) (+ (3dv:vx (velocity boid)) (turn-factor parameters))))
  (when (> (3dv:vx (location boid)) 312.0)
    (setf (3dv:vx (velocity boid)) (- (3dv:vx (velocity boid)) (turn-factor parameters))))
  (when (< (3dv:vy (location boid)) -312.0)
    (setf (3dv:vy (velocity boid)) (+ (3dv:vy (velocity boid)) (turn-factor parameters))))
  (when (> (3dv:vy (location boid)) 312.0)
    (setf (3dv:vy (velocity boid)) (- (3dv:vy (velocity boid)) (turn-factor parameters)))))
