;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; shape.lisp --- Definitions of the SHAPE Component in X3D
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
(defclass X3DAppearanceNode (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DAppearanceNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DAppearanceChildNode (X3DNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DMaterialNode (X3DAppearanceChildNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DMaterialNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DShapeNode (X3DChildNode)
  (
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
(defclass Appearance (X3DAppearanceNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass FillProperties (X3DAppearanceChildNode)
  (
    (filled :initarg :filled
        :initform  "true"
        :accessor filled
        :documentation "")
    (hatched :initarg :hatched
        :initform  "true"
        :accessor hatched
        :documentation "")
    (hatchStyle :initarg :hatchStyle
        :initform  "1"
        :accessor hatchStyle
        :documentation "")
    (hatchColor :initarg :hatchColor
        :initform  "1 1 1"
        :accessor hatchColor
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self FillProperties) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass LineProperties (X3DAppearanceChildNode)
  (
    (applied :initarg :applied
        :initform  "true"
        :accessor applied
        :documentation "")
    (linetype :initarg :linetype
        :initform  "1"
        :accessor linetype
        :documentation "")
    (linewidthScaleFactor :initarg :linewidthScaleFactor
        :initform  "0"
        :accessor linewidthScaleFactor
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self LineProperties) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass Material (X3DMaterialNode)
  (
    (ambientIntensity :initarg :ambientIntensity
        :initform  "0.2"
        :accessor ambientIntensity
        :documentation "")
    (diffuseColor :initarg :diffuseColor
        :initform  "0.8 0.8 0.8"
        :accessor diffuseColor
        :documentation "")
    (emissiveColor :initarg :emissiveColor
        :initform  "0 0 0"
        :accessor emissiveColor
        :documentation "")
    (shininess :initarg :shininess
        :initform  "0.2"
        :accessor shininess
        :documentation "")
    (specularColor :initarg :specularColor
        :initform  "0 0 0"
        :accessor specularColor
        :documentation "")
    (transparency :initarg :transparency
        :initform  "0"
        :accessor transparency
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TwoSidedMaterial (X3DMaterialNode)
  (
    (ambientIntensity :initarg :ambientIntensity
        :initform  "0.2"
        :accessor ambientIntensity
        :documentation "")
    (backAmbientIntensity :initarg :backAmbientIntensity
        :initform  "0.2"
        :accessor backAmbientIntensity
        :documentation "")
    (diffuseColor :initarg :diffuseColor
        :initform  "0.8 0.8 0.8"
        :accessor diffuseColor
        :documentation "")
    (backDiffuseColor :initarg :backDiffuseColor
        :initform  "0.8 0.8 0.8"
        :accessor backDiffuseColor
        :documentation "")
    (emissiveColor :initarg :emissiveColor
        :initform  "0 0 0"
        :accessor emissiveColor
        :documentation "")
    (backEmissiveColor :initarg :backEmissiveColor
        :initform  "0 0 0"
        :accessor backEmissiveColor
        :documentation "")
    (shininess :initarg :shininess
        :initform  "0.2"
        :accessor shininess
        :documentation "")
    (backShininess :initarg :backShininess
        :initform  "0.2"
        :accessor backShininess
        :documentation "")
    (specularColor :initarg :specularColor
        :initform  "0 0 0"
        :accessor specularColor
        :documentation "")
    (backSpecularColor :initarg :backSpecularColor
        :initform  "0 0 0"
        :accessor backSpecularColor
        :documentation "")
    (transparency :initarg :transparency
        :initform  "0"
        :accessor transparency
        :documentation "")
    (backTransparency :initarg :backTransparency
        :initform  "0"
        :accessor backTransparency
        :documentation "")
    (separateBackColor :initarg :separateBackColor
        :initform  "false"
        :accessor separateBackColor
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Shape (X3DShapeNode)
  (
  )
  (:documentation ""))

