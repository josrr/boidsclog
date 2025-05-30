;;;; package.lisp

(defpackage #:boidsclog
  (:use #:cl #:clog #:clog-gui #:clog-webgl)
  (:local-nicknames (#:3dv #:3d-vectors)
                    (#:a #:alexandria))
  (:export #:start))


(defpackage #:boids
  (:use #:cl)
  (:local-nicknames (#:3dv #:3d-vectors))
  (:export #:boid
           #:boids-parameters
           #:draw
           #:update-location
           #:update-velocity))
