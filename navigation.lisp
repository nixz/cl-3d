;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; navigation.lisp --- Definitions of the NAVIGATION Component in X3D
;;;;
;;;; Copyright (c) 2011-2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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
;; ----------------------------------------------------------------------------
(defclass X3DViewpointNode (X3DBindableNode)
  (
    (centerOfRotation :initarg :centerOfRotation
        :initform  "0 0 0"
        :accessor centerOfRotation
        :documentation "")
    (description :initarg :description
        :initform  ""
        :accessor description
        :documentation "")
    (jump :initarg :jump
        :initform  "true"
        :accessor jump
        :documentation "")
    (orientation :initarg :orientation
        :initform  "0 0 1 0"
        :accessor orientation
        :documentation "")
    (position :initarg :position
        :initform  "0 0 10"
        :accessor position
        :documentation "")
    (retainUserOffsets :initarg :retainUserOffsets
        :initform  "false"
        :accessor retainUserOffsets
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Billboard (X3DGroupingNode)
  (
    (axisOfRotation :initarg :axisOfRotation
        :initform  "0 1 0"
        :accessor axisOfRotation
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Collision (X3DGroupingNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass LOD (X3DGroupingNode)
  (
    (forceTransitions :initarg :forceTransitions
        :initform  "false"
        :accessor forceTransitions
        :documentation "")
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (range :initarg :range
        :initform  ""
        :accessor range
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NavigationInfo (X3DBindableNode)
  (
    (avatarSize :initarg :avatarSize
        :initform  "0.25 1.6 0.75"
        :accessor avatarSize
        :documentation "")
    (headlight :initarg :headlight
        :initform  "true"
        :accessor headlight
        :documentation "")
    (speed :initarg :speed
        :initform  "1"
        :accessor speed
        :documentation "")
    (type :initarg :type
        :initform  `("EXAMINE" "ANY")
        :accessor type
        :documentation "")
    (transitionType :initarg :transitionType
        :initform  `("ANIMATE")
        :accessor transitionType
        :documentation "")
    (transitionTime :initarg :transitionTime
        :initform  "1.0"
        :accessor transitionTime
        :documentation "")
    (visibilityLimit :initarg :visibilityLimit
        :initform  "0"
        :accessor visibilityLimit
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Viewpoint (X3DViewpointNode)
  (
    (fieldOfView :initarg :fieldOfView
        :initform  "0.7854"
        :accessor fieldOfView
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass OrthoViewpoint (X3DViewpointNode)
  (
    (fieldOfView :initarg :fieldOfView
        :initform  "-1 -1 1 1"
        :accessor fieldOfView
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ViewpointGroup (X3DNode)
  (
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (description :initarg :description
        :initform  ""
        :accessor description
        :documentation "")
    (displayed :initarg :displayed
        :initform  "true"
        :accessor displayed
        :documentation "")
    (retainUserOffsets :initarg :retainUserOffsets
        :initform  "false"
        :accessor retainUserOffsets
        :documentation "")
    (size :initarg :size
        :initform  "0 0 0"
        :accessor size
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ViewpointGroup) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

