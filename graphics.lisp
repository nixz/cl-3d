;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; graphics.lisp --- Graphics engine to render
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
(in-package #:x3d)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

(defclass  graphics ()
  ()
  (:documentation "The abstract graphics API"))

(defclass shader-manager ()
  ((table :initform (make-hash-table :test 'eql)
         :accessor table))
  (:documentation
"Responsible for maintaining the state of the shaders with respect to the state
of OpenGL. It maintains a map of shaders with API to add and delete.
"))

(defgeneric add (shader-manager shader))

(defmethod add ((self shader-manager) shader)
""
)

(defclass  program-manager ()
  ()
  (:documentation "doc"))

(defclass actor ()
  ((lock :initform (bt:make-lock))
   (cv :initform (bt:make-condition-variable))
   (body :initarg :body
         :initform (nil)
         :documentation "about-slot")
   (vars 
   thread)
  (:documentation "Active Object"))

(defmethod initialize-instance :after ((self actor)&key)
  ""
  (with-slots (thread body) self
    (make-thread #'lambda())))

(defclass  opengl-2 (graphics)
  ((projection :initarg :projection
               :initform (sb-cga:identity-matrix)
               :accessor projection
               :documentation "projection matrix")
   (view :initarg :view
         :initform (sb-cga:identity-matrix)
         :accessor view
         :documentation "view matrix")
   (model :initarg :model
          :initform (vector (sb-cga:identity-matrix))
          :accessor model
          :documentation "model matrix")
   (vertex-shader-manager :initarg :shader-manager
         :initform (make-instance 'shader-manager)
         :accessor shader-manager
         :documentation "Manages a list of vertex shaders")
   (fragment-shader-mangager :initarg :fragment-shader-mangager
         :initform (make-instance 'shader-manager)
         :accessor fragment-shader-mangager
         :documentation "Manages a list of fragment shaders")
   (program-manager :initarg :program-manager
                    :initform (make-instance 'program-manager)
         :accessor program-manager
         :documentation "Manages the list of programs")
   (next-frame-hook-lock :initform (bt:make-lock "frame-hook-lock"))
   (next-frame-hook :initform nil
                     :accessor next-frame-hook))
  (:documentation "Using Opengl-2.0 API compatible with OpenGlES 2.0"))

(defmethod initialize-instance :after ((gl opengl-2)&key (width 500) (height 500))
  "Intialize the opengl window (context) using sdl"
  (sdl:init-sdl)
  (sdl:window width height
              :flags (logior sdl:sdl-opengl sdl:sdl-resizable))
  ;; This step is necessary
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address))

(defmethod del ((self opengl-2))
  "Destructor to close the window delete all opengl managers"
  (with-slots ((vsm vetex-shader-manager)
               (fsm fragment-shaader-manager)
               (pm program-manager)) self
    (del pm)
    (del fsm)
    (del vsm))
  (sdl:close-audio)                     ;close audio sdl
  (sdl:quit-sdl))                       ;close window

;; (defparameter *GL*)

(defun draw ()
  (gl:clear :color-buffer-bit)
  ;; we'll update this later too
  (sdl:update-display))

;; (ql:quickload "lispbuilder-sdl")
(defparameter *opengl* nil)
(defun start()
  (let ((self (make-instance 'opengl-2)))
    (setf *opengl* self)
    (unwind-protect
         (with-slots (next-frame-hook-lock next-frame-hook) self
           (sdl:with-events()
             (:quit-event () t)
             (:idle ()
                    #+(and sbcl (not sb-thread))(restartable
                             ppp                    (sb-sys:serve-all-events 0))
                    (restartable (draw))
                      (restartable
                       (bt:with-lock-held (next-frame-hook-lock)
                         (loop for i in next-frame-hook
                            do (funcall i))
                          (setf next-frame-hook nil))))))
      (del self))))

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex 0 0 0)
    (gl:color 0 1 0)
    (gl:vertex 0.5 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 0 0))
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

;; (defun main-loop ()
;;   (sdl:with-init ()
;;     (sdl:window 320 240 :flags sdl:sdl-opengl)
;;     ;; cl-opengl needs platform specific support to be able to load GL
;;     ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
;;     (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
;;     (sdl:with-events ()
;;       (:quit-event () t)
;;       (:idle ()
;;              ;; this lets slime keep working while the main loop is running
;;              ;; in sbcl using the :fd-handler swank:*communication-style*
;;              ;; (something similar might help in some other lisps, not sure which though)
;;              #+(and sbcl (not sb-thread)) (restartable
;;                                            (sb-sys:serve-all-events 0))
;;              (restartable (draw))))))


(bt:make-thread #'start :name "start")
(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0) ;;  hmm, i need to add an option to the syntax
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)   ;;  highlighting to show changed lines...
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0)) ;; (this one changed too)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defmethod next-frame ((opengl opengl-2) &rest body)
  (with-slots (next-frame-hook-lock next-frame-hook) opengl
    (bt:with-lock-held (next-frame-hook-lock)
     (progn (push (lambda () body) next-frame-hook)))))

;; change the background color
(next-frame *opengl*
  (format t "got to next frame ...~%") (finish-output)
  (gl:clear-color 0 1 0 1))

;; and to put it back
;; (next-frame *opengl*
;;   (gl:clear-color 0 0 0 1))
