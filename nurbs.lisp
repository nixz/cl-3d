;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; nurbs.lisp --- Definitions of the NURBS Component in X3D
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
(defclass X3DNurbsControlCurveNode (X3DNode)
  (
    (controlPoint :initarg :controlPoint
        :initform  ""
        :accessor controlPoint
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DNurbsSurfaceGeometryNode (X3DParametricGeometryNode)
  (
    (uClosed :initarg :uClosed
        :initform  "false"
        :accessor uClosed
        :documentation "")
    (vClosed :initarg :vClosed
        :initform  "false"
        :accessor vClosed
        :documentation "")
    (uDimension :initarg :uDimension
        :initform  "0"
        :accessor uDimension
        :documentation "")
    (vDimension :initarg :vDimension
        :initform  "0"
        :accessor vDimension
        :documentation "")
    (uKnot :initarg :uKnot
        :initform  ""
        :accessor uKnot
        :documentation "")
    (vKnot :initarg :vKnot
        :initform  ""
        :accessor vKnot
        :documentation "")
    (uOrder :initarg :uOrder
        :initform  "3"
        :accessor uOrder
        :documentation "")
    (vOrder :initarg :vOrder
        :initform  "3"
        :accessor vOrder
        :documentation "")
    (uTessellation :initarg :uTessellation
        :initform  "0"
        :accessor uTessellation
        :documentation "")
    (vTessellation :initarg :vTessellation
        :initform  "0"
        :accessor vTessellation
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DParametricGeometryNode (X3DGeometryNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CoordinateDouble (X3DCoordinateNode)
  (
    (point :initarg :point
        :initform  ""
        :accessor point
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self CoordinateDouble) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass Contour2D (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self Contour2D) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ContourPolyline2D (X3DNurbsControlCurveNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ContourPolyline2D) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass NurbsCurve (X3DParametricGeometryNode)
  (
    (closed :initarg :closed
        :initform  "false"
        :accessor closed
        :documentation "")
    (knot :initarg :knot
        :initform  ""
        :accessor knot
        :documentation "")
    (order :initarg :order
        :initform  "3"
        :accessor order
        :documentation "")
    (tessellation :initarg :tessellation
        :initform  "0"
        :accessor tessellation
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsCurve2D (X3DNurbsControlCurveNode)
  (
    (closed :initarg :closed
        :initform  "false"
        :accessor closed
        :documentation "")
    (knot :initarg :knot
        :initform  ""
        :accessor knot
        :documentation "")
    (order :initarg :order
        :initform  "3"
        :accessor order
        :documentation "")
    (tessellation :initarg :tessellation
        :initform  "0"
        :accessor tessellation
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self NurbsCurve2D) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass NurbsOrientationInterpolator (X3DChildNode)
  (
    (knot :initarg :knot
        :initform  ""
        :accessor knot
        :documentation "")
    (order :initarg :order
        :initform  "3"
        :accessor order
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsPatchSurface (X3DNurbsSurfaceGeometryNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsPositionInterpolator (X3DChildNode)
  (
    (knot :initarg :knot
        :initform  ""
        :accessor knot
        :documentation "")
    (order :initarg :order
        :initform  "3"
        :accessor order
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsSet (X3DChildNode)
  (
    (tessellationScale :initarg :tessellationScale
        :initform  "1.0"
        :accessor tessellationScale
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
(defclass NurbsSurfaceInterpolator (X3DChildNode)
  (
    (uDimension :initarg :uDimension
        :initform  "0"
        :accessor uDimension
        :documentation "")
    (vDimension :initarg :vDimension
        :initform  "0"
        :accessor vDimension
        :documentation "")
    (uKnot :initarg :uKnot
        :initform  ""
        :accessor uKnot
        :documentation "")
    (vKnot :initarg :vKnot
        :initform  ""
        :accessor vKnot
        :documentation "")
    (uOrder :initarg :uOrder
        :initform  "3"
        :accessor uOrder
        :documentation "")
    (vOrder :initarg :vOrder
        :initform  "3"
        :accessor vOrder
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsSweptSurface (X3DParametricGeometryNode)
  (
    (ccw :initarg :ccw
        :initform  "true"
        :accessor ccw
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsSwungSurface (X3DParametricGeometryNode)
  (
    (ccw :initarg :ccw
        :initform  "true"
        :accessor ccw
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsTextureCoordinate (X3DTextureCoordinateNode)
  (
    (controlPoint :initarg :controlPoint
        :initform  ""
        :accessor controlPoint
        :documentation "")
    (uDimension :initarg :uDimension
        :initform  "0"
        :accessor uDimension
        :documentation "")
    (vDimension :initarg :vDimension
        :initform  "0"
        :accessor vDimension
        :documentation "")
    (uKnot :initarg :uKnot
        :initform  ""
        :accessor uKnot
        :documentation "")
    (vKnot :initarg :vKnot
        :initform  ""
        :accessor vKnot
        :documentation "")
    (uOrder :initarg :uOrder
        :initform  "3"
        :accessor uOrder
        :documentation "")
    (vOrder :initarg :vOrder
        :initform  "3"
        :accessor vOrder
        :documentation "")
    (weight :initarg :weight
        :initform  ""
        :accessor weight
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass NurbsTrimmedSurface (X3DNurbsSurfaceGeometryNode)
  (
  )
  (:documentation ""))

