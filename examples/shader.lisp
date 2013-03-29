;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; shader.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects


(ql:quickload "cl-3d")
(ql:quickload "cl-glut-examples")


;; (defmacro with-gl-array((var type count) &body body)
;;   `(let ((,var (alloc-gl-array ,type ,count)))
;;      (unwind-protect (progn ,@body)
;;        (free-gl-array ,var))))

;; ----------------------------------------------------------------------------
(defclass shader-window (glut:window)
  ((vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (program :accessor program)
   (angle :accessor angle :initform 0.0))
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
                     :mode '(:double :rgb :depth) :title "shader.lisp"
                     :tick-interval (round 1000 60)))

(defvar *shader-vertex-program*
  "
// The location is 0 because that's the vertex attribute we associate with vertex positions.
attribute vec2 in_Position;

uniform mat4 projectionMatrix;
uniform float angle;

// This is interpolated and used in the fragment shader.
varying vec2 pos;

void main()
{
  mat2 rotationMatrix = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
  float scaleFactor = 1.0 + 0.5 * sin(1.75 * angle);
  vec2 vertPos = scaleFactor * rotationMatrix * in_Position.xy;
  pos = vertPos * 5.0;

  gl_Position = projectionMatrix * vec4(vertPos, 0.0, 1.0); 
} 
")

(defvar *shader-fragment-program*
  "

varying vec2 pos;

uniform float angle;

void main() 
{
  mat2 rotationMatrix = mat2( cos(angle), sin(angle), -sin(angle), cos(angle) );
  vec2 rpos = mod(rotationMatrix * pos, 2.0 );
  
  if ((rpos.x > 1.0 && rpos.y > 1.0 ) || (rpos.x < 1.0 && rpos.y < 1.0))
    gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0); 
  else
    gl_FragColor = vec4(0.5, 0.5, 0.7, 1.0);
} 
")

;;; First, we create buffers for our vertex and index
;;; data. Then, we create the vertex array object that we actually use
;;; for rendering directly. Finally, we load the shader objects.
(defmethod glut:display-window :before ((w shader-window))
  (let* ((vert (gl::new-shader :type :vertex
                              :source *shader-vertex-program*))
         (frag (gl::new-shader :type :fragment
                              :source *shader-fragment-program*))
         (pro (gl::new-program :vertex-shader vert
                              :fragment-shader frag)))
    (setf (vertex-shader w) vert)
    (setf (fragment-shader w) frag)
    (setf (program w) pro)
    (gl::use pro))
  (format t "init ~%"))

;; (defmacro defuniform (name)
;;   `(progn
;;     (defmethod ,name ((p program))
;;       (with-slots (program) p
;;         (gl:get-uniform-location program (symbol-name ',name))))
;;     (defmethod (setf ,name) ((p program) value)
;;       (with-slots (program) p
;;         (gl:uniformf (gl:get-uniform-location program (symbol-name ',name)) value)))))

;; (defuniform |angle|)

(defmethod glut:tick ((w shader-window))
  (let ((seconds-per-revolution 6))
    (incf  (angle w)
           (/ (* 2 pi) (* 60 seconds-per-revolution))))
  (with-slots ((program gl::program)) (program w)
    (gl:uniformf (gl:get-uniform-location program "angle") (angle w)))
  (glut:post-redisplay))

(defmethod glut:display ((w shader-window))
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl::use (program w))
  (draw-primitives-with-vbo (program w)
                            #(-0.5 -0.5 0.0
                              -0.5 0.5 0.0
                              0.5 -0.5 0.0
                              0.5 0.5 0.0)
                            #(0 2 1 1 2 3))
  (glut:swap-buffers)
  (format t "display~%"))

(defmethod glut:reshape ((w shader-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let ((right (max (float (/ width height)) 1.0))
        (top (max (float (/ height width)) 1.0)))
    (glu:ortho-2d (- right) right (- top) top))
  (with-slots ((program gl::program)) (program w)
    (when program
      (let ((proj-mat (gl:get-float :projection-matrix)))
        (gl:uniform-matrix
         (gl:get-uniform-location program "projectionMatrix")
         4
         (vector proj-mat)))))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w shader-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w shader-window))
  (when (slot-boundp w 'vs)
    (gl::del (vertex-shader w)))
  ;; (gl:delete-shader (vertex-shader w)))
  (when (slot-boundp w 'fs)
    (gl::del (fragment-shader w)))
  ;; (gl:delete-shader (fragment-shader w)))
  (when (slot-boundp w 'program)
    (gl::del (program w))))

(defun shader ()
  (let ((w (make-instance 'shader-window)))
    (unwind-protect
         (glut:display-window w)
      (when (not (glut::destroyed w))
        (setf (glut::destroyed w) t)
        (glut:destroy-window (glut:id w))))))
