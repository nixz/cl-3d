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

(in-package #:x3d)

;; ----------------------------------------------------------------------NEW-ID
(defparameter *NEW-ID* 0)

(defun new-id()
  (let ((store (gensym *NEW-ID*)))
    (incf *NEW-ID*)
    store))

;; -------------------------------------------------------------------------DEF
(defparameter *DEF* (make-hash-table  :test 'eql))

(defun def (name)
  "Looks up the *DEF* hashmap and returns the value if present else nil"
   (gethash name *DEF*))

(defun set-def (&key name value)
  "Sets the key pair into *DEF* hash-table and return the value"
  (progn
    (setf (gethash name *DEF*) value)
    value))

;; -----------------------------------------------------------------------ROUTE
;; These are mosly like callbacks but are automatically invoked when the values
;; are changed or perhaps these are handles to threads


;; ============================================================================
;; CONSTRUCTOR FUNCTIONS
;; ============================================================================
;; -----------------------------------------------------------------------scene
(defun scene (&rest rest)
  ""
  (apply #'list rest))

;; --------------------------------------------------------------------grouping
(defun transform(&key
                 (def (new-id))
                 (children ())
                 (metadata ())
                 (center '(0 0 0))
                 (rotation '(0 0 1 0))
                 (translation '(0 0 0))
                 (scale '(1 1 1))
                 (scaleOrientation '(0 0 1 0)))
  "generates a transformation matrix"
  (set-def :name def
           :value (make-instance 'transform
                                 :children children
                                 :metadata metadata
                                 :center (apply #'sf-vec3f center)
                                 :rotation (apply #'sf-rotation rotation)
                                 :translation (apply #'sf-vec3f translation)
                                 :scale (apply #'sf-vec3f scale)
                                 :scale-orientation (apply #'sf-rotation scaleOrientation))))

;; -----------------------------------------------------------------------shape
(defun appearance (&key
                   (def (new-id))
                   (fillProperties nil)
                   (lineProperties nil)
                   (material nil)
                   (metadata ())
                   (shaders nil)
                   (texture nil)
                   (textureTransform nil))
  ""
  (set-def :name def
           :value (make-instance 'appearance
                                 :fill-properties fillProperties
                                 :line-properties lineProperties
                                 :material material
                                 :metadata metadata
                                 :shaders shaders
                                 :texture texture
                                 :texture-transform textureTransform)))
(defun material (&key
                 (def (new-id))
                 (ambientIntensity 0.2)
                 (diffuseColor '(0.8 0.8 0.8))
                 (emissiveColor '(0 0 0))
                 (metadata nil)
                 (shininess 0.2)
                 (specularColor '(0 0 0))
                 (transparency 0))
  ""
  (set-def :name def
           :value (make-instance 'material
                                 :ambient-intensity ambientIntensity
                                 :diffuse-color diffuseColor
                                 :emissive-color emissiveColor
                                 :metadata metadata
                                 :shininess shininess
                                 :specular-color specularColor
                                 :transparency transparency)))

(defun shape (&key
              (def (new-id))
              (appearance nil)
              (geometry nil)
              (metadata nil)
              (bboxCenter '(0 0 0))
              (bboxSize '(-1 -1 -1)))
  ""
  (set-def :name def
           :value (make-instance 'shape
                                 :appearance appearance
                                 :geometry geometry
                                 :metadata metadata
                                 :bbox-center bboxCenter
                                 :bbox-size bboxSize)))

;; -----------------------------------------------------------------geometry-3d
(defun box (&key
            (def (new-id))
            (metadata nil)
            (size '(2 2 2))
            (solid t))
""
(set-def :name def
         :value (make-instance 'box
                               :metadata metadata
                               :size (apply 'sf-vec3f size)
                               :solid solid)))

;; ------------------------------------------------------------------navigation
(defun viewpoint (&key
                  (def (new-id))
                  (centerOfRotation '(0 0 0))
                  (description "")
                  (fieldOfView (/ pi 4))
                  (jump t)
                  (metadata ())
                  (orientation '(0 0 1 0))
                  (position '(0 0 10))
                  (retainUserOffsets nil))
  ""
  (set-def :name def
           :value (make-instance 'viewpoint
                                 :center-of-rotation (apply #'sf-vec3f
                                                            centerOfRotation)
                                 :description description
                                 :field-of-view (deg->rad fieldOfView)
                                 :jump jump
                                 :metadata metadata
                                 :orientation (apply #'sf-rotation
                                                     orientation)
                                 :position (apply #'sf-vec3f position)
                                 :retain-user-offsets retainUserOffsets)))

;; -----------------------------------------------------------------env-effects
(defun background(&key
                  (def (new-id))
                  (groundAngle ())
                  (groundColor ())
                  (backUrl ())
                  (bottomUrl ())
                  (frontUrl ())
                  (leftUrl ())
                  (rightUrl ())
                  (topUrl ())
                  (metadata ())
                  (skyAngle ())
                  (skyColor '(0 0 0))
                  (transparency nil))
  ""
  (set-def :name def
           :value (make-instance 'background
                                 :ground-angle groundAngle
                                 :ground-color groundColor
                                 :back-url backUrl
                                 :bottom-url bottomUrl
                                 :front-url frontUrl
                                 :left-url leftUrl
                                 :right-url rightUrl
                                 :top-url topUrl
                                 :sky-angle skyAngle
                                 :sky-color skyColor
                                 :transparency transparency)))
