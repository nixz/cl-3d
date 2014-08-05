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

;; ultimately all opengl calls must be made to an end function. The
;; end function in the opengl call typically aa shader which takes
;; some of the follwoing parameters.
;; (vertex-shader
;;  :uniform ((mat4 model)
;;            (mat4 view)
;;            (mat4 projection)
;;            (mat4 normal-matrix)
;;            (vec3 ec-light-dir))
;;  :attribute ((vec4 a-vert)
;;              (vec3 a-normal)
;;              (vec2 a-texcoord))
;;  :varying ((float v_diffuse)
;;            (vec2 v_texcoord))
;;  :main ((vec3 ec_normal))
;;  (setf ec_normal (normalize (* normal-matrix a-normal)))
;;  (setf v-diffuse (max (dot ec-light-dir, 
      
;; (defmacro defvshader (name &key uniform attribute varying main)
;;   "defines a vertex shader and associates it with a name"
;;   `(print "what is this"))

;; (defvshader name
;;     :uniform ((:mat4 model)
;;               (:mat4 view)
;;               (:mat4 projection)
;;               (:mat4 normal_matrix)
;;               (:vec3 ec_light_dir))
;;     :attribute ((:vec4 a_vert)
;;                 (:vec3 a_normal)
;;                 (:vec2 a_texcoord))
;;     :varying ((:float v_diffuse)
;;               (:vec2 v_texcoord))
;;     :main "")


;; ----------------------------------------------------------------------------
(defclass  matrix-stack ()
  ((data 
         :initform ()
         :accessor data
         :documentation "Stack containing matrix data"))
  (:documentation "Stack to maintain matrix multiplications etc"))

;; ----------------------------------------------------------------------------
(defmethod matrix-push ((self matrix-stack) matrix)
  "Push data into the matrix stack" 
  (with-slots (data) self
    (push matrix data)))

;; ----------------------------------------------------------------------------
(defmethod matrix-pop ((self matrix-stack))
  "Pop dta from matrix stack"
  (with-slots (data) self
    (pop data)))

;; ----------------------------------------------------------------------------
(defmethod matrix-multiply ((self matrix-stack))
  ""
  (with-slots (data) self
    (apply 'sb-cga:matrix* (reverse data))))

;; ----------------------------------------------------------------------------
(defclass  opengl ()
  ((stack :initarg :stack
         :initform (make-instance 'matrix-stack)
         :accessor stack
         :documentation "To do stack operations"))   
  (:documentation "The opengl class"))


;; ----------------------------------------------------------------------shader
(defclass shader ()
  ((type :initarg :type
         :initform (error ":type must be specified (:vertex or :fragment)")
         :documentation "shader type :vertex or :fragment")
   (source :initarg :source
           :initform (error ":source must be specified")
           :documentation "source")
   id)
  (:documentation "doc"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self shader) &key)
  ""
  (with-slots (id type source) self
    (setf id
          (cond
            ((eql type :vertex) (gl:create-shader :vertex-shader))
            ((eql type :fragment) (gl:create-shader :fragment-shader))))
    (gl:shader-source id source)
    (gl:compile-shader id)))

;; -----------------------------------------------------------------------cnstr
(defmethod new-shader(&key type source)
  ""
  (make-instance 'shader
                 :type type
                 :source source))
;; -----------------------------------------------------------------------dstr
(defmethod del ((self shader))
  (with-slots (id) self
    (when (slot-boundp self 'id)
      (gl:delete-shader id))))

;; ---------------------------------------------------------------------program
(defclass program ()
  ((id :initform (gl:create-program)
       :accessor id
       :documentation "id"))
  (:documentation "doc"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self program)
                                       &key vertex-shader fragment-shader)
  ""
  (with-slots ((p id)) self
    (with-slots ((vs id)) vertex-shader
      (with-slots ((fs id)) fragment-shader
        (gl:attach-shader p vs)
        (gl:attach-shader p fs)))
    (gl:link-program p)))

;; -------------------------------------------------------------------------use
(defmethod use ((self program))
  ""
  (with-slots ((p id)) self
    (gl:use-program p)))

;; -----------------------------------------------------------------------cnstr
(defmethod new-program(&key vertex-shader fragment-shader)
  ""
  (make-instance 'program
                 :vertex-shader vertex-shader
                 :fragment-shader fragment-shader))

;; ------------------------------------------------------------------------dstr
(defmethod del ((self program))
  (with-slots (id) self
    (when (slot-boundp self 'id)
      (gl:delete-program id))))

;; -----------------------------------------------------------------------------
(defmacro with-gl-array((var type count) &body body)
  `(let ((,var (gl:alloc-gl-array ,type ,count)))
     (unwind-protect (progn ,@body)
       (gl:free-gl-array ,var))))

;; -----------------------------------------------------------------------------
(defun link-vertex-data(handle vertex-array)
  ""
  (gl:bind-buffer :array-buffer handle) ; bind the buffer to handle
  (with-gl-array (arr :float (cl:length vertex-array))
    (dotimes (i (cl:length vertex-array))
      (setf (gl:glaref arr i) (aref vertex-array i)))
    (gl:buffer-data :array-buffer :static-draw arr)))
;;  (gl:bind-buffer :array-buffer 0))  ;; 0 is always reserved as an unbound object.

;; -----------------------------------------------------------------------------
(defun link-index-data(handle index-array)
  ""
  (gl:bind-buffer :element-array-buffer handle) ;bind buffer to handle
  (with-gl-array (arr :unsigned-short (cl:length index-array))
    (dotimes (i (cl:length index-array))
      (setf (gl:glaref arr i) (aref index-array i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)))
;;    (gl:bind-buffer :element-array-buffer 0)) ; unbind buffer

;;; Initialization

;; ----------------------------------------------------------------------------
(defun draw-primitives-with-vbo(program vertex-data index-data)
  "Renders the vertex and index data"
  (with-slots (id) program
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
             (gl:bind-attrib-location id 0 "in_Position")
             (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short)
                               :count (cl:length index-data)))
        (gl:delete-buffers (list vertex-buffer index-buffer ))))))

(defun render (program)
  ""
  )

(defun program (vertex-shader fragment-shader)
  ""
  )

(defun vertex-shader ()
  ""
  )

(defun uniform (type name)
  (concatenate :string "uniform " type " " name ";"))

