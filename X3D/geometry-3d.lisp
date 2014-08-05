;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; geometry-3d.lisp --- Definitions of the GEOMETRY-3D Component in X3D
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
(defclass Box (X3DGeometryNode)
  (
    (size :initarg :size
        :initform  "2 2 2"
        :accessor size
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Cone (X3DGeometryNode)
  (
    (bottomRadius :initarg :bottomRadius
        :initform  "1"
        :accessor bottomRadius
        :documentation "")
    (height :initarg :height
        :initform  "2"
        :accessor height
        :documentation "")
    (side :initarg :side
        :initform  "true"
        :accessor side
        :documentation "")
    (bottom :initarg :bottom
        :initform  "true"
        :accessor bottom
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Cylinder (X3DGeometryNode)
  (
    (bottom :initarg :bottom
        :initform  "true"
        :accessor bottom
        :documentation "")
    (height :initarg :height
        :initform  "2"
        :accessor height
        :documentation "")
    (radius :initarg :radius
        :initform  "1"
        :accessor radius
        :documentation "")
    (side :initarg :side
        :initform  "true"
        :accessor side
        :documentation "")
    (top :initarg :top
        :initform  "true"
        :accessor top
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ElevationGrid (X3DGeometryNode)
  (
    (height :initarg :height
        :initform  ""
        :accessor height
        :documentation "")
    (ccw :initarg :ccw
        :initform  "true"
        :accessor ccw
        :documentation "")
    (colorPerVertex :initarg :colorPerVertex
        :initform  "true"
        :accessor colorPerVertex
        :documentation "")
    (creaseAngle :initarg :creaseAngle
        :initform  "0"
        :accessor creaseAngle
        :documentation "")
    (normalPerVertex :initarg :normalPerVertex
        :initform  "true"
        :accessor normalPerVertex
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
    (xDimension :initarg :xDimension
        :initform  "0"
        :accessor xDimension
        :documentation "")
    (xSpacing :initarg :xSpacing
        :initform  "1.0"
        :accessor xSpacing
        :documentation "")
    (zDimension :initarg :zDimension
        :initform  "0"
        :accessor zDimension
        :documentation "")
    (zSpacing :initarg :zSpacing
        :initform  "1.0"
        :accessor zSpacing
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Extrusion (X3DGeometryNode)
  (
    (beginCap :initarg :beginCap
        :initform  "true"
        :accessor beginCap
        :documentation "")
    (ccw :initarg :ccw
        :initform  "true"
        :accessor ccw
        :documentation "")
    (convex :initarg :convex
        :initform  "true"
        :accessor convex
        :documentation "")
    (creaseAngle :initarg :creaseAngle
        :initform  "0.0"
        :accessor creaseAngle
        :documentation "")
    (crossSection :initarg :crossSection
        :initform  "1 1 1 -1 -1 -1 -1 1 1 1"
        :accessor crossSection
        :documentation "")
    (endCap :initarg :endCap
        :initform  "true"
        :accessor endCap
        :documentation "")
    (orientation :initarg :orientation
        :initform  "0 0 1 0"
        :accessor orientation
        :documentation "")
    (scale :initarg :scale
        :initform  "1 1"
        :accessor scale
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
    (spine :initarg :spine
        :initform  "0 0 0 0 1 0"
        :accessor spine
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass IndexedFaceSet (X3DComposedGeometryNode)
  (
    (convex :initarg :convex
        :initform  "true"
        :accessor convex
        :documentation "")
    (creaseAngle :initarg :creaseAngle
        :initform  "0"
        :accessor creaseAngle
        :documentation "")
    (colorIndex :initarg :colorIndex
        :initform  ""
        :accessor colorIndex
        :documentation "")
    (coordIndex :initarg :coordIndex
        :initform  ""
        :accessor coordIndex
        :documentation "")
    (normalIndex :initarg :normalIndex
        :initform  ""
        :accessor normalIndex
        :documentation "")
    (texCoordIndex :initarg :texCoordIndex
        :initform  ""
        :accessor texCoordIndex
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Sphere (X3DGeometryNode)
  (
    (radius :initarg :radius
        :initform  "1"
        :accessor radius
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

