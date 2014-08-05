;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; scene.lisp --- Scene engine logic
;;;;
;;;; Copyright (c) 2012, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; shader-vao.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects


(in-package #:cl-3d)

(defparameter *Vertex-SHADER*
  "
uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

attribute vec3 position;
attribute vec4 srcColor;

varying vec4 destColor;

void main()
{
  destColor = srcColor;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position,1.0);
}

")

(defparameter *Fragment-SHADER*
  "
varying vec4 destColor;v

void main()
{
  gl_FragColor = destColor;
}
")

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))



;; (defclass scene (glut:window)
;;   ((vs :accessor vertex-shader)
;;    (fs :accessor fragment-shader)
;;    (program :accessor program)
;;    (angle :accessor angle :initform 0.0))
;;   (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
;;                      :mode '(:double :rgb :depth) :title "shader-vao.lisp"
;;                      :tick-interval (round 1000 60)))



;; (defmethod glut:display-window :before ((w scene))
;;   (restartable
;;     (progn
;;       (let* ((vert (new-shader :type :vertex
;;                                :source *Vertex-SHADER*))
;;              (frag (new-shader :type :fragment
;;                                :source *Fragment-SHADER*))
;;              (pro (new-program :vertex-shader vert
;;                                :fragment-shader frag)))
;;         (setf (vertex-shader w) vert)
;;         (setf (fragment-shader w) frag)
;;         (setf (program w) pro)
;;         (use pro)))))

;;   ;; (format t "init ~%"))

;;   ;;     (unless (gl::features-present-p (>= :glsl-version 3.3))
;;   ;;       (glut:destroy-current-window)
;;   ;;       (return-from glut:display-window nil))
;;   ;;     (let ((buffers (gl:gen-buffers 2)))
;;   ;;       (setf (vertex-buffer w) (elt buffers 0)
;;   ;;             (index-buffer w) (elt buffers 1)))
;;   ;;     (gl:bind-buffer :array-buffer (vertex-buffer w))
;;   ;;     (let ((arr (gl:alloc-gl-array :float 12))
;;   ;;           (verts #(-0.5 -0.5 0.0
;;   ;;                    -0.5 0.5 0.0
;;   ;;                    0.5 -0.5 0.0
;;   ;;                    0.5 0.5 0.0)))
;;   ;;       (dotimes (i (length verts))
;;   ;;         (setf (gl:glaref arr i) (aref verts i)))
;;   ;;       (gl:buffer-data :array-buffer :static-draw arr)
;;   ;;       (gl:free-gl-array arr))

;;   ;;     ;; 0 is always reserved as an unbound object.
;;   ;;     (gl:bind-buffer :array-buffer 0)

;;   ;;     ;; An element array buffer stores vertex indices. We fill it in the
;;   ;;     ;; same way as an array buffer.
;;   ;;     (gl:bind-buffer :element-array-buffer (index-buffer w))
;;   ;;     (let ((arr (gl:alloc-gl-array :unsigned-short 6))
;;   ;;           (indexes #(0 2 1 1 2 3)))
;;   ;;       (dotimes (i (length indexes))
;;   ;;         (setf (gl:glaref arr i) (aref indexes i)))
;;   ;;       (gl:buffer-data :element-array-buffer :static-draw arr)
;;   ;;       (gl:free-gl-array arr))
;;   ;;     (gl:bind-buffer :element-array-buffer 0)
;;   ;;     (setf (vertex-array w) (gl:gen-vertex-array))
;;   ;;     (gl:bind-vertex-array (vertex-array w))
;;   ;;     (gl:bind-buffer :array-buffer (vertex-buffer w))
;;   ;;     (gl:enable-vertex-attrib-array 0)
;;   ;;     (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
;;   ;;     (gl:bind-buffer :element-array-buffer (index-buffer w))
;;   ;;     (gl:bind-vertex-array 0)
;;   ;;     (let ((vs (gl:create-shader :vertex-shader))
;;   ;;           (fs (gl:create-shader :fragment-shader)))
;;   ;;       (setf (vertex-shader w) vs)
;;   ;;       (setf (fragment-shader w) fs)
;;   ;;       (gl:shader-source vs *shader-vao-vertex-program*)
;;   ;;       (gl:compile-shader vs)
;;   ;;       (gl:shader-source fs *shader-vao-fragment-program*)
;;   ;;       (gl:compile-shader fs)
;;   ;;       ;; If the shader doesn't compile, you can print errors with:
;;   ;;       ;; (print (gl:get-shader-info-log vs))
;;   ;;       ;; (print (gl:get-shader-info-log fs))

;;   ;;       (setf (program w) (gl:create-program))
;;   ;;       (gl:attach-shader (program w) vs)
;;   ;;       (gl:attach-shader (program w) fs)
;;   ;;       (gl:link-program (program w))
;;   ;;       (gl:use-program (program w))))))

;; (defmethod glut:tick ((w scene))
;;   (restartable
;;     (progn
;;       (let ((seconds-per-revolution 6))
;;     (incf  (angle w)
;;            (/ (* 2 pi) (* 60 seconds-per-revolution))))
;;   (with-slots (program) (program w)
;;     (gl:uniformf (gl:get-uniform-location program "angle") (angle w)))
;;   (glut:post-redisplay))))

;; (defmethod glut:display ((w scene))
;;   (restartable
;;     (progn
;;   (gl:clear-color 0.0 0.0 0.2 1.0)
;;   (gl:clear :color-buffer-bit :depth-buffer-bit)
;;   (use (program w))

;;   ;; This call actually does the rendering. The vertex data comes from
;;   ;; the currently-bound VAO. If the input array is null, the indices
;;   ;; will be taken from the element array buffer bound in the current
;;   ;; VAO.
;;   (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

;;   (glut:swap-buffers))))

;; (defmethod glut:reshape ((w scene) width height)
;;   (restartable 
;;     (progn
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   ;; Ensure that projection matrix ratio always matches the window size ratio,
;;   ;; so the polygon will always look square.
;;   (let ((right (max (float (/ width height)) 1.0))
;;         (top (max (float (/ height width)) 1.0)))
;;     (glu:ortho-2d (- right) right (- top) top))
;;   (with-slots (program) (program w)
;;     (when program
;;       (let ((proj-mat (gl:get-float :projection-matrix)))
;;         (gl:uniform-matrix
;;          (gl:get-uniform-location program "projectionMatrix")
;;          4
;;          (vector proj-mat)))))
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))))

;; (defmethod glut:keyboard ((w scene) key x y)
;;   (declare (ignore x y))
;;   (case key
;;     (#\Esc (glut:destroy-current-window))))

;; ;; Cleanup.
;; ;; Most of the objects we created have analogous deletion function.
;; (defmethod glut:close ((w scene))
;; (restartable
;;   (progn
;;   (when (slot-boundp w 'vs)
;;     (gl:delete-shader (vertex-shader w)))
;;   (when (slot-boundp w 'fs)
;;     (gl:delete-shader (fragment-shader w)))
;;   (when (slot-boundp w 'program)
;;     (gl:delete-program (program w)))

;;   (when (slot-boundp w 'vbuff)
;;     (gl:delete-buffers (list (vertex-buffer w) (index-buffer w))))
;;   (when (slot-boundp w 'va)
;;     (gl:delete-vertex-arrays (list (vertex-array w)))))))

;; (defun scene ()
;;   (let ((w (make-instance 'scene)))
;;   #+(and sbcl (not sb-thread))(restartable
;;                                (sb-sys:serve-all-events 0))
;;     (unwind-protect
;;          (glut:display-window w)
;;       (when (not (glut::destroyed w))
;;         (setf (glut::destroyed w) t)
;;         (glut:destroy-window (glut:id w))))))
