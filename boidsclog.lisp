;;;; boidsclog.lisp
;;;; Copyright 2025 José M. Á. Ronquillo Rivera

(in-package #:boidsclog)

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(defun draw-boids% (boids)
  (lambda (points)
    (loop for boid in boids
          for i from 0 by 2
          do (multiple-value-bind (x y) (boids:draw boid)
               (setf (aref points i) x
                     (aref points (1+ i)) y)))))

(defun draw-boids (boids)
  (lambda (points)
    (lparallel:pmap nil
                    (lambda (boid i)
                      (multiple-value-bind (x y) (boids:draw boid)
                        (setf (aref points i) x
                              (aref points (1+ i)) y)))
                    boids
                    (a:iota (length boids) :step 2))))

(defun update-boids (boids parameters)
  (lparallel:pmap nil
                  (lambda (boid)
                    (boids:update-velocity boid boids parameters (3dv:vec2)))
                  boids)
  #|(loop for boid in boids
        do (boids:update-velocity boid boids parameters (3dv:vec2)))|#
  (lparallel:pmap nil
                  (lambda (boid)
                    (boids:update-location boid))
                  boids)
  #|(loop for boid in boids
        do (boids:update-location boid))|#)

(defun on-new-window (body)
  (handler-case
      (progn
        (setf (title (html-document body)) "Boidsclog")
        (load-css (html-document body)
                  "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")
        (load-script (html-document body)
                     "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js")
        (clog-gui-initialize body)
        (add-class body "w3-blue")
        (let* ((actions (create-gui-menu-drop-down (create-gui-menu-bar body) :content "Boidsclog"))
               (container (create-div body :class "container-fluid"))
               (div-canvas (create-div (create-div container :class "row")
                                       :class "col-lg-6 mx-auto d-flex justify-content-center"))
               (canvas (create-canvas div-canvas :class "" :width *width* :height *height*))
               (boids-parameters (make-instance 'boids:boids-parameters))
               (num-of-boids 1200)
               (boids (loop repeat num-of-boids collect (make-instance 'boids:boid
                                                              :location (3dv:vec (- (random 20) 10)
                                                                                 (- (random 20) 10)))))
               (gl (create-webgl canvas))
               (quad (make-quad gl))
               (texture (make-texture gl num-of-boids *width* *height* (draw-boids boids)))
               (sb (create-style-block body))
               (pausep nil))
          (create-gui-menu-item actions :content "Pause" :on-click (lambda (obj)
                                                                     (declare (ignore obj))
                                                                     (setf pausep (not pausep))))
          ;;(create-gui-menu-item actions :content "Reset" :on-click (create-on-reset minskytron switches))
          (format *debug-io* "~D x ~D~%" (drawing-buffer-width gl) (drawing-buffer-height gl))
          (add-style sb :element "canvas" '(("width" "90vmin") ("height" "90vmin")))
          (add-class canvas "w3-black")
          (set-border canvas :medium :solid "#0066aa")
          (set-margin canvas "6px" "6px" "6px" "6px")
          ;;(setf (display div-canvas) :flex (align-items div-canvas) :center (justify-content div-canvas) :center)
          (enable-capability gl :BLEND)
          (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
          (clear-color gl 0.0f0 0.0f0 0.0f0 1.0f0)
          (clear-webgl gl :COLOR_BUFFER_BIT)
          (clear-color gl 0.0f0 0.0f0 0.0f0 0.05f0)
          (loop (when (or (not (validp body)) (connection-data-item body "done"))
                  (return))
                (unless pausep
                  (update-boids boids boids-parameters)
                  (draw quad (draw texture)))
                (sleep 1/60))))
    (error (c)
      (format *debug-io* "Lost connection.~%~%~A" c))))

(defun start ()
  "Start Boidsclog."
  (initialize 'on-new-window)
  (open-browser))
