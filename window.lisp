;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; window.lisp --- Glut windowing classes and methods
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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

(in-package :cl-3d)

(defun begin ()
  "initialize everything"
  (glfw::initialize) ; Initialize the window system
  )

(defun end ()
  "finalize everthing"
  (glfw:terminate) ; finalize the window system
  )

;;; ---------------------------------------------------------------------------
(defclass screen (xml-serializer)
  ((displays :initarg :displays
         :initform (error ":displays must be specified")
         :reader displays-changed
         :writer set-displays
         :allocation :instance
         :documentation "A collection of all the displays"))
  (:documentation "A screen contains one or more displays and the coordinates"))

;;; ---------------------------------------------------------------------------
(defclass display (xml-serializer)
  ((windows :initarg :windows
         :initform (error ":windows must be specified")
         :reader windows-changed
         :writer set-windows
         :allocation :instance
         :documentation "A display can consist of multiple windows"))
  (:documentation "A display consists of multiple windows"))

;;; ---------------------------------------------------------------------------
(defclass window (xml-serializer)
  ((monitor :initarg :monitor
         :initform (cffi:get-primary-monitor)
         :reader monitor-changed
         :writer set-monitor
         :allocation :instance
         :documentation "Which monitor is the window on")
   (alpha-size :initarg :alpha-size
         :initform 0
         :reader alpha-size-changed
         :writer set-alpha-size
         :allocation :instance
         :documentation "Alpha buffer size")
   (depth-size :initarg :depth-size
         :initform 16
         :reader depth-size-changed
         :writer set-depth-size
         :allocation :instance
         :documentation "depth buffer size")
   (stencil-size :initarg :stencil-size
         :initform 8
         :reader stencil-size-changed
         :writer set-stencil-size
         :allocation :instance
         :documentation "stencil buffer size")
   (window
         :allocation :instance
         :documentation "reference to the window created (this is a glfw window reference)"))
  (:documentation "A window for now is a fullscreen window with a single viewport."))

;;; ---------------------------------------------------------------------------
;; TEMPLATE FOR CREATING A NEW WINDOW
;;; ---------------------------------------------------------------------------
;;; (GLFW:initialize)
;; (glfw:create-window :monitor (glfw:get-primary-monitor) :title "Window test" 
;;                             :width 600 :height 400
;;                             :context-version-major 3 
;;                             :context-version-minor 2
;;                             ;; :client-api :opengl-es-api
;;                             :opengl-forward-compat 1
;;                             :opengl-profile :opengl-core-profile
;;                             )

;;; ---------------------------------------------------------------------------
;;; TEMPLATE FOR GETTTING WIDTH AND HEIGHT FROM MONITOR
;;; ---------------------------------------------------------------------------
;; CL-USER> (glfw:get-video-mode (glfw:get-primary-monitor))
;; (%CL-GLFW3::REFRESH-RATE 59 %CL-GLFW3::BLUE-BITS 8 %CL-GLFW3::GREEN-BITS 8
;;  %CL-GLFW3::RED-BITS 8 %CL-GLFW3::HEIGHT 900 %CL-GLFW3::WIDTH 1440)
;; CL-USER> (getf (glfw:get-video-mode (glfw:get-primary-monitor)) '%cl-glfw3::height)
;; 900
;; CL-USER> (getf (glfw:get-video-mode (glfw:get-primary-monitor)) '%cl-glfw3::width)
;; 1440

;;; ---------------------------------------------------------------------------
(defun new-window-glfw3 (&key
                           (width 640 width-p)
                           (height 480 height-p)
                           (title "") 
                           (monitor (cffi:get-primary-monitor) monitor-p)
                           no-frame
                           (alpha-size 0)
                           (depth-size 16)
                           (stencil-size 8) 
                           (red-size 8)
                           (green-size 8)
                           (blue-size 8)
                           (buffer-size 32)
                           (double-buffer t)
                           hidden
                           (resizable t))
  (let* ((width )
         (win (glfw:create-window 
               :title title :width width :height height 
               :context-version-major 3
               :context-version-minor 2
               :opengl-forward-compat 1
               :opengl-profile :opengl-core-profile
               :alpha-bits alpha-size
               :depth-bits depth-size
               :stencil-bits stencil-size
               :red-bits red-size
               :green-bits green-size
               :blue-bits blue-size
               ;; :visible hidden
               ;; :resizable resizable
               ;; :decorated no-frame
               :monitor fullscreen
               )))
    (values (glfw:get-current-context) glfw:*window*)))

;;; ---------------------------------------------------------------------------
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

;;; ---------------------------------------------------------------------------
(defmacro run-in-loop (&body body)
  `(loop until (glfw:window-should-close-p)
        do (progn ,@body)
        do (glfw:poll-events)))

;; ----------------------------------------------------------------------------
(defclass opengl-window (glut:window)
  ())
  
;; ----------------------------------------------------------------------------
(defmethod run ((slef opengl-window))
  (print "Inside opengl-window"))

;; ----------------------------------------------------------------------------
(defclass  opengl-window-mono (opengl-window)
  ()
  (:default-initargs :width 500 :height 500 :title "Drawing a simple scene"
                     :mode '(:double :rgb :depth)))

;; ----------------------------------------------------------------------------
(defclass  opengl-window-stereo (opengl-window)
  ()
  (:default-initargs :width 500 :height 500 :title "Drawing a simple scene"
                     :mode '(:double :rgb :depth :stereo)))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self opengl-window) &key)
  "initialize the scene"
  (setf glut:*run-main-loop-after-display* nil))

  ;; (glut:display-window self))


;; ----------------------------------------------------------------------------
(defmethod glut:display-window :before ((w opengl-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)              ; (gl:shade-model :flat)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test)) ; global stuff

;; ----------------------------------------------------------------------------
(defmethod glut:display ((self opengl-window))
  (glut:swap-buffers))

;; ----------------------------------------------------------------------------
(defmethod glut:display ((self opengl-window-stereo))
  (gl:draw-buffer :back-left)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:draw-buffer :back-right)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (run self)
  (call-next-method))

;; ----------------------------------------------------------------------------
(defmethod glut:display ((self opengl-window-mono))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (run self)
  (call-next-method))
