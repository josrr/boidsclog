;;;; boidsclog.asd

;;;; Copyright (c) 2025 José M. Á. Ronquillo Rivera <jose@rufina.link>
;;;; This file is part of boidsclog.
;;;;
;;;; boidsclog is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; boidsclog is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with boidsclog.  If not, see <http://www.gnu.org/licenses/>.


(asdf:defsystem #:boidsclog
  :description "boids algorithm implemented with CLOG"
  :author "José M. Á. Ronquillo Rivera <jose@rufina.link>"
  :license  "GPLv3"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria
               #:clog
               #:jonathan
               #:rtg-math
               #:mathkit
               #:3d-matrices
               #:3d-vectors
               #:lparallel)
  :components ((:file "package")
               (:file "clog-webgl-patch")
               (:file "boids")
               (:file "gl")
               (:file "boidsclog")))
