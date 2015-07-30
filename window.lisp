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

(defclass window (xml-serializer)
  ((enabled :initarg :enabled
            :initform t
            :reader enabled-changed
            :writer set-enabled
            :type
            :allocation :instance
            :documentation "If enabled is TRUE, then rendering to this window
            is enabled")
   (buffer :initarg :buffer
           :initform 2
           :allocation :instance
           :documentation "1 (single), 2 (double), 4 (quad)")
   (sample :initarg :sample
           :initform 1
           :allocation :instance
           :documentation "number of multi-samples of FSAA")
   (sample-filter-mode :initarg :sample-filter-mode
           :initform "auto"
           :reader sample-filter-mode-changed
           :writer set-sample-filter-mode
           :allocation :instance
           :documentation "defines the filter-method of resolving the color of
         multisampled pixels; hint: use the sample-field to set the number of
         samples")
   (position :initarg :position
           :initform (SFVec2f -1 -1)
           :reader position-changed
           :writer set-position
           :allocation :instance
           :documentation "Position of the window on the screen. If position
         is -1 -1, then the position is set by the window manager")
   (size :initarg :size
         :initform (SFVec2f 512 512)
         :reader size-changed
         :writer set-size
         :allocation :instance
         :documentation "Window size. If fullScreen is true, then window size
         is ignored")
   (full-screen :initarg :full-screen
         :initform NIL
         :reader full-screen-changed
         :writer set-full-screen
         :allocation  :instance
         :documentation "If fullScreen is TRUE, the output window covers the
         whole screen.")
   (draw-cursor :initarg :draw-cursor
         :initform t
         :reader draw-cursor-changed
         :writer set-draw-cursor
         :allocation :instance
         :documentation "If drawCurser is FALSE, no Cursor is shown")
   (draw-tool-bar :initarg :draw-tool-bar
         :initform t
         :reader draw-tool-bar-changed
         :writer set-draw-tool-bar
         :allocation :instance
         :documentation "If TRUE, the viewer tries to provide a toolbar")
   (border :initarg :border
         :initform t
         :allocation :instance
         :documentation "Show window decorations like border or title bar")
   (pipe :initarg :pipe
         :initform ""
         :allocation :instance
         :documentation "On X11 pipe is used to specify the X-Server e.g. :0
         or rubens:1.0")
   (description :initarg :description
         :initform ""
         :allocation :instance
         :documentation "Description of the window")
   (ignore-extensions :initarg :ignore-extensions
         :initform ""
         :allocation :class :instance
         :documentation "OpenGL extensions that should be ignored during rendering")
   (views :initarg :views
         :initform (error ":views must be specified")
         :writer set-views
         :allocation :instance
         :documentation "Viewareas that should be drawn in this window. If no
         Viewarea isgiven, a default Viewarea is generated.")
   (status-message :initarg :status-message
         :initform ""
         :writer set-status-message
         :allocation :instance
         :documentation "allows do set some kind of status line")
   (visible :initarg :visible
         :initform t
         :writer set-visible
         :allocation :instance
         :documentation "Show or hide the window on the screen"))
  (:documentation "An old fashioned window."))


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
