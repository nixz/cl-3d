;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; humanoid-animation.lisp --- Definitions of the HUMANOID-ANIMATION Component in X3D
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
(defclass HAnimDisplacer (X3DGeometricPropertyNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (coordIndex :initarg :coordIndex
        :initform  ""
        :accessor coordIndex
        :documentation "")
    (displacements :initarg :displacements
        :initform  ""
        :accessor displacements
        :documentation "")
    (weight :initarg :weight
        :initform  "0.0"
        :accessor weight
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self HAnimDisplacer) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass HAnimHumanoid (X3DHumanoidNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (rotation :initarg :rotation
        :initform  "0 0 1 0"
        :accessor rotation
        :documentation "")
    (scale :initarg :scale
        :initform  "1 1 1"
        :accessor scale
        :documentation "")
    (scaleOrientation :initarg :scaleOrientation
        :initform  "0 0 1 0"
        :accessor scaleOrientation
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
        :documentation "")
    (info :initarg :info
        :initform  `()
        :accessor info
        :documentation "")
    (version :initarg :version
        :initform  ""
        :accessor version
        :documentation "")
    (bboxCenter :initarg :bboxCenter
        :initform  "0 0 0"
        :accessor bboxCenter
        :documentation "")
    (bboxSize :initarg :bboxSize
        :initform  "-1 -1 -1"
        :accessor bboxSize
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass HAnimJoint (X3DGroupingNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (rotation :initarg :rotation
        :initform  "0 0 1 0"
        :accessor rotation
        :documentation "")
    (scale :initarg :scale
        :initform  "1 1 1"
        :accessor scale
        :documentation "")
    (scaleOrientation :initarg :scaleOrientation
        :initform  "0 0 1 0"
        :accessor scaleOrientation
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
        :documentation "")
    (skinCoordIndex :initarg :skinCoordIndex
        :initform  ""
        :accessor skinCoordIndex
        :documentation "")
    (skinCoordWeight :initarg :skinCoordWeight
        :initform  ""
        :accessor skinCoordWeight
        :documentation "")
    (llimit :initarg :llimit
        :initform  ""
        :accessor llimit
        :documentation "")
    (ulimit :initarg :ulimit
        :initform  ""
        :accessor ulimit
        :documentation "")
    (limitOrientation :initarg :limitOrientation
        :initform  "0 0 1 0"
        :accessor limitOrientation
        :documentation "")
    (stiffness :initarg :stiffness
        :initform  "0 0 0"
        :accessor stiffness
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass HAnimSegment (X3DGroupingNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (mass :initarg :mass
        :initform  "0"
        :accessor mass
        :documentation "")
    (centerOfMass :initarg :centerOfMass
        :initform  "0 0 0"
        :accessor centerOfMass
        :documentation "")
    (momentsOfInertia :initarg :momentsOfInertia
        :initform  "0 0 0 0 0 0 0 0 0"
        :accessor momentsOfInertia
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass HAnimSite (X3DGroupingNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (rotation :initarg :rotation
        :initform  "0 0 1 0"
        :accessor rotation
        :documentation "")
    (scale :initarg :scale
        :initform  "1 1 1"
        :accessor scale
        :documentation "")
    (scaleOrientation :initarg :scaleOrientation
        :initform  "0 0 1 0"
        :accessor scaleOrientation
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
        :documentation "")
  )
  (:documentation ""))

