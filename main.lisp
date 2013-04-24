;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; main.lisp --- Internal workings of the scenegraph. The main logic is here
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

;; ----------------------------------------------------------------------NEW-ID
;; (defparameter *NEW-ID* 0)

;; (defun new-id()
;;   (let ((store (gensym *NEW-ID*)))
;;     (incf *NEW-ID*)
;;     store))

;; -------------------------------------------------------------------------DEF
;; (defparameter *DEF* (make-hash-table  :test 'eql))

;; (defun def (name)
;;   "Looks up the *DEF* hashmap and returns the value if present else nil"
;;   (gethash name *DEF*))

;; (defun set-def (&key name value)
;;   "Sets the key pair into *DEF* hash-table and return the value"
;;   (progn
;;     (setf (gethash name *DEF*) value)
;;     value))

;; -----------------------------------------------------------------------ROUTE
;; These are mosly like callbacks but are automatically invoked when the values
;; are changed or perhaps these are handles to threads

;; -----------------------------------------------------------------------scene

;; ============================================================================
;; CONSTRUCTOR FUNCTIONS
;; ============================================================================
;; -----------------------------------------------------------------------scene
(defclass scene (glut:window xml-serializer)
  ((backgrounds
         :initform nil
         :accessor backgrounds
         :documentation "defines the background color")
   (viewpoints
         :initform nil
         :accessor viewpoints
         :documentation "defines the point of view of the scene")
   (navigationInfos
         :initform nil
         :accessor navigationInfos
         :documentation "the list of all NavigationInfo nodes")
   (shapes
         :initform nil
         :accessor shapes
         :documentation "Drawable object")
   (transforms
         :initform nil
         :accessor transforms
         :documentation "List of transformation objects"))
  (:default-initargs :width 500 :height 500 :title "Drawing a simple scene"
                     :mode '(:double :rgb :depth)))


(defmethod initialize-instance :after ((self Scene) &key)
  "initialize the scene"
  (mouse-reset)
  (glut:display-window self))

(defmethod glut:display-window :before ((w scene))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)              ; (gl:shade-model :flat)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test)) ; global stuff

(defmethod glut:display ((w scene))
  (run w)
  (glut:swap-buffers))

(defmethod glut:reshape ((w scene) width height)
  "Whenever the window is changed then this event is triggered"
  (progn
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0
                 0
                 (slot-value w 'glut::width)
                 (slot-value w 'glut::height))
    (setf (slot-value w 'glut::width) width)
    (setf (slot-value w 'glut::height) height)))

(defmethod glut:keyboard ((w scene) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:mouse ((window scene) button state x y)
  (mouse-init button state x y))

(defmethod glut:motion ((window scene) x y)
  (mouse-update x y)
  (glut:post-redisplay))
