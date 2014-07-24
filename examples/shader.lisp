;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; shader.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects


(ql:quickload "cl-3d") ;; (ql:quickload "cl-glut-examples") 

;; ;; (defmacro with-gl-array((var type count) &body body)
;; ;;   `(let ((,var (alloc-gl-array ,type ,count)))
;; ;;      (unwind-protect (progn ,@body)
;; ;;        (free-gl-array ,var))))

;; ;; ----------------------------------------------------------------------------
;; (defclass shader-window (glut:window)
;;   ((vs :accessor vertex-shader)
;;    (fs :accessor fragment-shader)
;;    (program :accessor program)
;;    (angle :accessor angle :initform 0.0))
;;   (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
;;                      :mode '(:double :rgb :depth) :title "shader.lisp"
;;                      :tick-interval (round 1000 60)))

;; (defvar *shader-vertex-program*
;;   "
;; // The location is 0 because that's the vertex attribute we associate with vertex positions.
;; attribute vec2 in_Position;

;; uniform mat4 projectionMatrix;
;; uniform float angle;

;; // This is interpolated and used in the fragment shader.
;; varying vec2 pos;

;; void main()
;; {
;;   mat2 rotationMatrix = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
;;   float scaleFactor = 1.0 + 0.5 * sin(1.75 * angle);
;;   vec2 vertPos = scaleFactor * rotationMatrix * in_Position.xy;
;;   pos = vertPos * 5.0;

;;   gl_Position = projectionMatrix * vec4(vertPos, 0.0, 1.0); 
;; } 
;; ")

;; (defvar *shader-fragment-program*
;;   "

;; varying vec2 pos;

;; uniform float angle;

;; void main() 
;; {
;;   mat2 rotationMatrix = mat2( cos(angle), sin(angle), -sin(angle), cos(angle) );
;;   vec2 rpos = mod(rotationMatrix * pos, 2.0 );

;;   if ((rpos.x > 1.0 && rpos.y > 1.0 ) || (rpos.x < 1.0 && rpos.y < 1.0))
;;     gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0); 
;;   else
;;     gl_FragColor = vec4(0.5, 0.5, 0.7, 1.0);
;; } 
;; ")

;; ;;; First, we create buffers for our vertex and index
;; ;;; data. Then, we create the vertex array object that we actually use
;; ;;; for rendering directly. Finally, we load the shader objects.
;; (defmethod glut:display-window :before ((w shader-window))
;;   (let* ((vert (gl::new-shader :type :vertex
;;                                :source *shader-vertex-program*))
;;          (frag (gl::new-shader :type :fragment
;;                               :source *shader-fragment-program*))
;;          (pro (gl::new-program :vertex-shader vert
;;                                :fragment-shader frag)))
;;     (setf (vertex-shader w) vert)
;;     (setf (fragment-shader w) frag)
;;     (setf (program w) pro)
;;     (gl::use pro))
;;   (format t "init ~%"))

;; ;; (defmacro defuniform (name)
;; ;;   `(progn
;; ;;     (defmethod ,name ((p program))
;; ;;       (with-slots (program) p
;; ;;         (gl:get-uniform-location program (symbol-name ',name))))
;; ;;     (defmethod (setf ,name) ((p program) value)
;; ;;       (with-slots (program) p
;; ;;         (gl:uniformf (gl:get-uniform-location program (symbol-name ',name)) value)))))

;; ;; (defuniform |angle|)

;; (defmethod glut:tick ((w shader-window))
;;   (let ((seconds-per-revolution 6))
;;     (incf  (angle w)
;;            (/ (* 2 pi) (* 60 seconds-per-revolution))))
;;   (with-slots ((program gl::program)) (program w)
;;     (gl:uniformf (gl:get-uniform-location program "angle") (angle w)))
;;   (glut:post-redisplay))

;; (defmethod glut:display ((w shader-window))
;;   (gl:clear-color 0.0 0.0 0.2 1.0)
;;   (gl:clear :color-buffer-bit :depth-buffer-bit)
;;   (gl::use (program w))
;;   (draw-primitives-with-vbo (program w)
;;                             #(-0.5 -0.5 0.0
;;                               -0.5 0.5 0.0
;;                               0.5 -0.5 0.0
;;                               0.5 0.5 0.0)
;;                             #(0 2 1 1 2 3))
;;   (glut:swap-buffers)
;;   (format t "display~%"))

;; (defmethod glut:reshape ((w shader-window) width height)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   ;; Ensure that projection matrix ratio always matches the window size ratio,
;;   ;; so the polygon will always look square.
;;   (let ((right (max (float (/ width height)) 1.0))
;;         (top (max (float (/ height width)) 1.0)))
;;     (glu:ortho-2d (- right) right (- top) top))
;;   (with-slots ((program gl::program)) (program w)
;;     (when program
;;       (let ((proj-mat (gl:get-float :projection-matrix)))
;;         (gl:uniform-matrix
;;          (gl:get-uniform-location program "projectionMatrix")
;;          4
;;          (vector proj-mat)))))
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))

;; (defmethod glut:keyboard ((w shader-window) key x y)
;;   (declare (ignore x y))
;;   (case key
;;     (#\Esc (glut:destroy-current-window))))

;; ;; Cleanup.
;; ;; Most of the objects we created have analogous deletion function.
;; (defmethod glut:close ((w shader-window))
;;   (when (slot-boundp w 'vs)
;;     (gl::del (vertex-shader w)))
;;   ;; (gl:delete-shader (vertex-shader w)))
;;   (when (slot-boundp w 'fs)
;;     (gl::del (fragment-shader w)))
;;   ;; (gl:delete-shader (fragment-shader w)))
;;   (when (slot-boundp w 'program)
;;     (gl::del (program w))))

;; (defun shader ()
;;   (let ((w (make-instance 'shader-window)))
;;     (unwind-protect
;;          (glut:display-window w)
;;       (when (not (glut::destroyed w))
;;         (setf (glut::destroyed w) t)
;;         (glut:destroy-window (glut:id w))))))

;; (defclass  shader ()
;;   (
;;    (id :initarg :id
;;          :initform nil
;;          :accessor id
;;          :documentation "The shader id generated by opengl")
;;    (src :initarg :src
;;          :initform (error ":src has to be specified")
;;          :accessor src
;;          :documentation "The shader src")
;;    )
;;   (:documentation "The generic shader class"))

;; (defclass vertex-shader(shader)
;;   ( )
;;   (:documentation "Vertex shader class"))

;; (defclass fragment-shader(shader)
;;   ( )
;;   (:documentation "Vertex shader class"))

;; (defmethod initialize-instance :after ((self shader) &key)
;;   ""
;;   (with-slots (id src) self
;;     (gl:shader-source id src)))

;; (defmethod compile ((self shader))
;;   (with-slots (id src) self
;;     (gl:compile-shader id)
;;     (if (gl:get-shader shader :compile-status)
;;         t
;;         (progn
;;           (print (gl:get-shader-info-log shader))
;;           (gl:delete-shader shader)
;;           nil)))

;; (defmethod initialize-instance :before ((self vertex-shader) &key )
;;   ""
;;   (with-slots (id) self
;;     (setf id (gl:create-shader :vertex-shader))))

;; (defun vertex-shader (src)
;;   (initialize-instance 'vertex-shader :src src))

;; (defmethod initialize-instance :before ((self fragment-shader) &key )
;;   ""
;;   (with-slots (id) self
;;     (setf id (gl:create-shader :vertex-shader))))


;; (in-package :cl-3d)

(defclass shader-vao-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0)) 
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
                     :mode '(:double :rgb :depth) :title "shader-vao.lisp"
                     :tick-interval (round 1000 60)))

(defparameter *shader-vao-vertex-program*
  "
attribute vec4 Position; // 1
attribute vec4 SourceColor; // 2
 
varying vec4 DestinationColor; // 3
 
void main(void) { // 4
    DestinationColor = SourceColor; // 5
    gl_Position = Position; // 6
}
")

(defparameter *shader-vao-fragment-program*
  "
varying vec4 DestinationColor; // 1
 
void main(void) { // 2
    gl_FragColor = DestinationColor; // 3
}
")


(defun load-shader(src type)
  "Function used to load a shader the shader types
are :vertex :fragment. If successfully loaded then a shader value is
returned else returns a nil"
  (let ((shader (if (eq type :vertex)
                    (gl:create-shader :vertex-shader)
                    (gl:create-shader :fragment-shader))))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status)
        (progn
          (print "success")
          shader)
        (progn
          (print "Error:")
          (print (gl:get-shader-info-log shader))
          (gl:delete-shader shader)
          nil))))



;;; Initialization 

;;; First, we create buffers for our vertex and index
;;; data. Then, we create the vertex array object that we actually use
;;; for rendering directly. Finally, we load the shader objects.
(defmethod glut:display-window :before ((w shader-vao-window))
  ;; An array buffer can be used to store verex position, colors,
  ;; normals, or other data. We need to allocate an GL array, copy the
  ;; data to the array, and tell OpenGL that the buffers data comes
  ;; from this GL array. Like most OpenGL state objects, we bind the
  ;; buffer before we can make changes to its state.
  (unless (gl::features-present-p (>= :glsl-version 3.3))
    (glut:destroy-current-window)
    (return-from glut:display-window nil))

  ;; Generate two buffers id's in the OpenGL machine and assign them to
  ;; vertex-buffer and index-buffer
  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer w) (elt buffers 0)
          (index-buffer w) (elt buffers 1)))
  
  ;; Create and bind the vertex buffer to generated ID
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  
  ;; Make gl memory and allocate vertices to it
  (let ((arr (gl:alloc-gl-array :float 12))
        (verts #(-0.5 -0.5 0.0 
                 -0.5 0.5 0.0 
                 0.5 -0.5 0.0 
                 0.5 0.5 0.0)))
    (dotimes (i (cl:length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    
    ;; Create Buffer Object data store and copy data
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer w))
  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
        (indexes #(0 2 1 1 2 3)))
    (dotimes (i (cl:length indexes))
      (setf (gl:glaref arr i) (aref indexes i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array w))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer (index-buffer w))

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0)

  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (load-shader *shader-vao-vertex-program*   :vertex))
        (fs (load-shader *shader-vao-fragment-program* :fragment)))
    (setf (vertex-shader w)   vs)
    (setf (fragment-shader w) fs)
    (setf (program w) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program w) vs)
    (gl:attach-shader (program w) fs)
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program (program w))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program w))))         

(defmethod glut:tick ((w shader-vao-window))
  (let ((seconds-per-revolution 6)) 
    (incf  (angle w)
           (/ (* 2 pi) (* 60 seconds-per-revolution))))
  ;; (gl:uniformf (gl:get-uniform-location (program w) "angle") (angle w))
  (unless (glut::destroyed w) (glut:post-redisplay)))

(defmethod glut:display ((w shader-vao-window))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; Since we never use any other program object, this is unnecessary
  ;; in this program. Typically, though, you'll have multiple program
  ;; objects, so you'll need to 'use' each one to activate it.
  (gl:use-program (program w))
  (gl:bind-vertex-array (vertex-array w))
  
  ;; This call actually does the rendering. The vertex data comes from
  ;; the currently-bound VAO. If the input array is null, the indices
  ;; will be taken from the element array buffer bound in the current
  ;; VAO.
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

  (glut:swap-buffers))

(defmethod glut:reshape ((w shader-vao-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let ((right (max (float (/ width height)) 1.0))
        (top (max (float (/ height width)) 1.0)))
    (glu:ortho-2d (- right) right (- top) top))
  (when (program w)
    (let ((proj-mat (gl:get-float :projection-matrix)))
      (gl:uniform-matrix 
       (gl:get-uniform-location (program w) "projectionMatrix") 
       4 
       (cl:vector proj-mat))))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w shader-vao-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-window (glut:id w)))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w shader-vao-window))
  ;; Note: It doesn't matter whether we delete the program or the
  ;; linked shaders first. If a shader is linked to a program, the
  ;; shader isn't destroyed until after the program is
  ;; destroyed. Similarly, if the program is destroyed, the shaders
  ;; are detached.
  (print "closing. This will destroy shaders")

  (when (slot-boundp w 'program)
    (print "program closing")
    (gl:delete-program (program w))
    (print "done"))

  (when (slot-boundp w 'vs)
    (print "vertex closing")
    (gl:delete-shader (vertex-shader w))
    (print "done")
    )

  (when (slot-boundp w 'fs)
    (print "fragment closing")
    (gl:delete-shader (fragment-shader w))
    (print "done"))

  (when (slot-boundp w 'vbuff)
    (print "buffers delete")
    (gl:delete-buffers (list (vertex-buffer w) (index-buffer w)))
    (print "done"))

  (when (slot-boundp w 'va)
    (print "array delete")
    (gl:delete-vertex-arrays (list (vertex-array w)))
    (print "done")))

(defun shader-vao ()
  (let ((w (make-instance 'shader-vao-window)))
    (unwind-protect
         (glut:display-window w)
      (when (not (glut::destroyed w))
        (setf (glut::destroyed w) t)
        ;; (glut:close w)
        (glut:destroy-window (glut:id w))))))
