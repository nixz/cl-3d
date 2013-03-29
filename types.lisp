;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; types.lisp --- Definitions of the TYPES Component in X3D
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
(defclass boundingBoxSizeType ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass intensityType ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFBool ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFBool ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFColor ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFColor ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFColorRGBA ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFColorRGBA ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFDouble ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFDouble ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFFloat ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFFloatNonNegative ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFFloatPositive ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFFloat ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFImage ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFImage ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFInt32 ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFInt32 ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFRotation ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFRotation ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFString ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFString ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFTime ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFTime ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec2f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec2f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec2d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec2d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec3f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec3f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec3d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec3d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec4f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec4f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFVec4d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFVec4d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFMatrix3f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFMatrix3f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFMatrix3d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFMatrix3d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFMatrix4f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFMatrix4f ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SFMatrix4d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MFMatrix4d ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass accessTypeNames ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ArcClose2dTypeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass componentNames ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass fieldTypeName ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass fontStyleValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass fogTypeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass shaderPartTypeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass metaDirectionValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass networkModeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass profileNames ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass textureBoundaryModeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass textureMagnificationModeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass textureMinificationModeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass textureCompressionModeValues ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass x3dVersion ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass initializeOnlyAccessTypes ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass inputOnlyAccessTypes ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass outputOnlyAccessTypes ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass inputOutputAccessTypes ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass geoSystemType ()
  ()
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass jointName ()
  ()
  (:documentation ""))

