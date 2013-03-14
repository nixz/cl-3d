;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; opengl.lisp --- opengl convenience objects
;;;;
;;;; Copyright (c) 2011, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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

(in-package #:cl-3d)

;; ----------------------------------------------------------------------shader
(defclass shader ()
  ((type :initarg :type
         :initform (error ":type must be specified (:vertex or :fragment)")
         :documentation "shader type :vertex or :fragment")
   (source :initarg :source
           :initform (error ":source must be specified")
           :documentation "source")
   shader)
  (:documentation "doc"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self shader) &key)
  ""
  (with-slots (shader type source) self
    (setf shader
          (cond
            ((eql type :vertex) (gl:create-shader :vertex-shader))
            ((eql type :fragment) (gl:create-shader :fragment-shader))))
    (gl:shader-source shader source)
    (gl:compile-shader shader)))

;; -----------------------------------------------------------------------cnstr
(defmethod new-shader(&key type source)
  ""
  (make-instance 'shader
                 :type type
                 :source source))
;; -----------------------------------------------------------------------dstr
(defmethod del ((self shader))
  (with-slots (shader) self
    (when (slot-boundp self 'shader)
      (gl:delete-shader shader))))

;; ---------------------------------------------------------------------program
(defclass program ()
  ((program :initform (gl:create-program)
            :allocation :instance
            :documentation "program"))
  (:documentation "doc"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self program)
                                       &key vertex-shader fragment-shader)
  ""
  (with-slots ((p program)) self
    (with-slots ((vs shader)) vertex-shader
      (with-slots ((fs shader)) fragment-shader
        (gl:attach-shader p vs)
        (gl:attach-shader p fs)))
    (gl:link-program p)))

;; -------------------------------------------------------------------------use
(defmethod use ((self program))
  ""
  (with-slots ((p program)) self
    (gl:use-program p)))

;; -----------------------------------------------------------------------cnstr
(defmethod new-program(&key vertex-shader fragment-shader)
  ""
  (make-instance 'program
                 :vertex-shader vertex-shader
                 :fragment-shader fragment-shader))

;; ------------------------------------------------------------------------dstr
(defmethod del ((self program))
  (with-slots (program) self
    (when (slot-boundp self 'program)
      (gl:delete-program program))))

;; -----------------------------------------------------------------------------
(defmacro with-gl-array((var type count) &body body)
  `(let ((,var (gl:alloc-gl-array ,type ,count)))
     (unwind-protect (progn ,@body)
       (gl:free-gl-array ,var))))

;; -----------------------------------------------------------------------------
(defun link-vertex-data(handle vertex-array)
  ""
  (gl:bind-buffer :array-buffer handle) ; bind the buffer to handle
  (with-gl-array (arr :float (length vertex-array))
    (dotimes (i (length vertex-array))
      (setf (gl:glaref arr i) (aref vertex-array i)))
    (gl:buffer-data :array-buffer :static-draw arr)))
;;  (gl:bind-buffer :array-buffer 0))  ;; 0 is always reserved as an unbound object.

;; -----------------------------------------------------------------------------
(defun link-index-data(handle index-array)
  ""
  (gl:bind-buffer :element-array-buffer handle) ;bind buffer to handle
  (with-gl-array (arr :unsigned-short (length index-array))
    (dotimes (i (length index-array))
      (setf (gl:glaref arr i) (aref index-array i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)))
;;    (gl:bind-buffer :element-array-buffer 0)) ; unbind buffer

;;; Initialization

;; ----------------------------------------------------------------------------
(defun draw-primitives-with-vbo(program vertex-data index-data)
  "Renders the vertex and index data"
  (with-slots (program) program
    (let* ((buffers (gl:gen-buffers 2))
           (vertex-buffer (elt buffers 0))
           (index-buffer (elt buffers 1)))
      ;; (format t "buffer0 = ~a " vertex-buffer)
      ;; (format t "buffer1 = ~a " index-buffer)
      (unwind-protect
           (progn
             (link-vertex-data vertex-buffer vertex-data)
             (link-index-data index-buffer  index-data)
             (gl:enable-vertex-attrib-array 0)
             (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
             (gl:bind-attrib-location program 0 "in_Position")
             (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short)
                               :count (length index-data)))
        (gl:delete-buffers (list vertex-buffer index-buffer ))))))


