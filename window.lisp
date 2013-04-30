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
