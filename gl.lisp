;;;; gl.lisp
;;;; Copyright 2025 José M. Á. Ronquillo Rivera

(in-package #:boidsclog)

(defparameter *first-time* t)

(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program (compile-webgl-program webgl
                                        (compile-shader-source webgl :VERTEX_SHADER vertex-shader)
                                        (compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (use-program program)
    program))

(defclass gl-object ()
  ((webgl :initarg :webgl :reader webgl)
   (vbo :reader vbo)
   (vao :reader vao)
   (program :initarg :program :accessor program)
   (xy :initarg :xy :accessor xy)))

(defmethod initialize-instance :after ((instance gl-object) &rest initargs &key &allow-other-keys)
  (with-slots (vbo vao) instance
    (let ((webgl (getf initargs :webgl)))
      (setf vao (create-vertex-array webgl)
            vbo (create-webgl-buffer webgl)))))

(defgeneric draw (gl-object &optional texture)
  (:documentation "Draws a GL-OBJECT"))

(defclass texture (gl-object)
  ((num-points :initform 128 :initarg :num-points
               :accessor num-points)
   (fb :reader fb)
   (bf :reader bf)
   (points :initarg :points :accessor points)
   (words :initarg :words :accessor words)
   (u-model :initarg :model :accessor u-model)
   (u-view :initarg :view :accessor u-view)
   (u-proy :initarg :proy :accessor u-proy)
   (u-color :initarg :color :accessor u-color)
   (u-size :initarg :size :accessor u-size)
   (generator :initarg :generator :reader generator)))

(defmethod initialize-instance :after ((instance texture) &rest initargs &key &allow-other-keys)
  (with-slots (fb bf) instance
    (let ((webgl (getf initargs :webgl)))
      (setf fb (create-webgl-frame-buffer webgl)
            bf (create-webgl-texture webgl)))))

(defparameter *texture-v-shader* "#version 300 es
in vec2 posicion;
out vec3 Color;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform vec3 color;
uniform float size;

void main() {
  gl_PointSize=size;
  Color = color;
  gl_Position = proj*view*model*vec4(posicion, 0.0, 1.0);
}")

(defparameter *texture-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  vec2 coord = 2.0 * gl_PointCoord - 1.0;
  if ( dot(coord, coord) > 1.0 )
    discard;
  outColor = vec4(Color, 1.0);
}")

(defun make-texture (webgl num-points width height generator)
  (let* ((program (compile-program webgl *texture-v-shader* *texture-f-shader*))
         (med-width (/ width 2f0))
         (med-height (/ height 2f0))
         (*first-time* t)
         (m (make-instance 'texture
                           :webgl webgl
                           :num-points num-points
                           :program program
                           :xy (attribute-location program "posicion")
                           :points (make-array (* num-points 2)
                                               :element-type 'single-float
                                               :initial-element 0.0f0)
                           :model (uniform-location program "model")
                           :view (uniform-location program "view")
                           :proy (uniform-location program "proj")
                           :color (uniform-location program "color")
                           :size (uniform-location program "size")
                           :generator generator)))
    (uniform-matrix webgl 4 (u-view m) nil (coerce (rtg-math.matrix4:identity) 'list))
    (uniform-matrix webgl 4 (u-proy m) nil (coerce (kit.math:ortho-matrix (- med-width) med-width
                                                                          (- med-height) med-height
                                                                          0.0001 3000.0)
                                                   'list))
    (uniform-matrix webgl 4 (u-model m) nil (coerce (rtg-math.matrix4:identity) 'list))
    (bind-vertex-array (vao m))
    (bind-buffer (vbo m) :ARRAY_BUFFER)
    (enable-vertex-attribute-array webgl (xy m))
    (vertex-attribute-pointer webgl (xy m) 2 :FLOAT nil 8 0)
    (bind-frame-buffer (fb m) :DRAW_FRAMEBUFFER)
    (bind-texture (bf m) :TEXTURE_2D)
    (texture-image-2d webgl :TEXTURE_2D 0 :RGBA width height 0 :RGBA :UNSIGNED_BYTE nil)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MIN_FILTER :LINEAR)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MAG_FILTER :LINEAR)
    (frame-buffer-texture-2d webgl :DRAW_FRAMEBUFFER :COLOR_ATTACHMENT0 :TEXTURE_2D (bf m) 0)
    m))

(defmethod draw ((obj texture) &optional texture)
  (declare (ignore texture))
  (use-program (program obj))
  (bind-vertex-array (vao obj))
  (bind-frame-buffer (fb obj) :DRAW_FRAMEBUFFER)
  (clear-webgl (webgl obj) :COLOR_BUFFER_BIT)
  (funcall (generator obj) (points obj))
  (bind-buffer (vbo obj) :ARRAY_BUFFER)
  (buffer-data (vbo obj) (coerce (points obj) 'list) "Float32Array" :STATIC_DRAW)
  (let ((num-points (num-points obj)))
    (uniform-float (webgl obj) (u-color obj) 0.3 0.9 0.8)
    (uniform-float (webgl obj) (u-size obj) 4.0)
    (draw-arrays (webgl obj) :POINTS 0 num-points))
  (bf obj))

(defun texture-reset (texture)
  (clear-color (webgl texture) 0.0 0.0 0.0 1.0)
  (clear-webgl (webgl texture) :COLOR_BUFFER_BIT)
  (clear-color (webgl texture) 0.0 0.0 0.0 0.05))

(defun texture-restart (texture)
  (clear-color (webgl texture) 0.0 0.0 0.0 1.0)
  (clear-webgl (webgl texture) :COLOR_BUFFER_BIT)
  (clear-color (webgl texture) 0.0 0.0 0.0 0.05))

(defparameter *width* 1024)
(defparameter *height* 1024)

(defparameter *quad*
  (make-array 16 :element-type 'single-float
                 :initial-contents '(-1.0  1.0 0.0 1.0
                                      1.0  1.0 1.0 1.0
                                      1.0 -1.0 1.0 0.0
                                     -1.0 -1.0 0.0 0.0)))

(defparameter *quad-elems*
  (make-array 6 :element-type 'fixnum
                :initial-contents '(0 1 2
                                    2 3 0)))

(defparameter *quad-v-shader*
  "#version 300 es
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
    TexCoords = aTexCoords;
}")

(defparameter *quad-f-shader*
  "#version 300 es
precision highp float;
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    FragColor = texture(screenTexture, TexCoords);
}")

(defclass quad (gl-object)
  ((ebo :reader quad-ebo)
   (tex-coords :initarg :tex-coords :accessor quad-tex-coords)))

(defmethod initialize-instance :after ((instance quad) &rest initargs &key &allow-other-keys)
  (with-slots (vao ebo vbo) instance
    (bind-vertex-array vao)
    (let ((webgl (getf initargs :webgl))
          (xy (getf initargs :xy))
          (tex-coords (getf initargs :tex-coords)))
      (setf ebo (create-webgl-buffer webgl))
      (bind-buffer vbo :ARRAY_BUFFER)
      (bind-buffer ebo :ELEMENT_ARRAY_BUFFER)
      (buffer-data vbo (coerce *quad* 'list) "Float32Array" :STATIC_DRAW)
      (buffer-data ebo (coerce *quad-elems* 'list) "Uint16Array" :STATIC_DRAW)
      (vertex-attribute-pointer webgl xy 2 :FLOAT nil 16 0)
      (vertex-attribute-pointer webgl tex-coords 2 :FLOAT nil 16 8)
      (enable-vertex-attribute-array webgl xy)
      (enable-vertex-attribute-array webgl tex-coords))))

(defun make-quad (webgl)
  (let ((program (compile-program webgl *quad-v-shader* *quad-f-shader*)))
    (make-instance 'quad
                   :webgl webgl
                   :program program
                   :xy (attribute-location program "aPos")
                   :tex-coords (attribute-location program "aTexCoords"))))

(defmethod draw ((obj quad) &optional texture)
  (use-program (program obj))
  (bind-canvas-frame-buffer (webgl obj) :DRAW_FRAMEBUFFER)
  (bind-vertex-array (vao obj))
  (when texture
    (bind-texture texture :TEXTURE_2D))
  (draw-elements (webgl obj) :TRIANGLES 6 :UNSIGNED_SHORT 0))
