;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; rendering.lisp --- Definitions of the RENDERING Component in X3D
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
(defclass X3DColorNode (X3DGeometricPropertyNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DColorNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DComposedGeometryNode (X3DGeometryNode)
  (
    (ccw :initarg :ccw
        :initform  "true"
        :accessor ccw
        :documentation "")
    (colorPerVertex :initarg :colorPerVertex
        :initform  "true"
        :accessor colorPerVertex
        :documentation "")
    (normalPerVertex :initarg :normalPerVertex
        :initform  "true"
        :accessor normalPerVertex
        :documentation "")
    (solid :initarg :solid
        :initform  "true"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DCoordinateNode (X3DGeometricPropertyNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DGeometryNode (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DGeometryNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DGeometricPropertyNode (X3DNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DNormalNode (X3DGeometricPropertyNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DNormalNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ClipPlane (X3DChildNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (plane :initarg :plane
        :initform  "0 1 0 0"
        :accessor plane
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Color (X3DColorNode)
  (
    (color :initarg :color
        :initform  ""
        :accessor color
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ColorRGBA (X3DColorNode)
  (
    (color :initarg :color
        :initform  ""
        :accessor color
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Coordinate (X3DCoordinateNode)
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
(defmethod add-subobject ((self Coordinate) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass IndexedLineSet (X3DGeometryNode)
  (
    (colorPerVertex :initarg :colorPerVertex
        :initform  "true"
        :accessor colorPerVertex
        :documentation "")
    (colorIndex :initarg :colorIndex
        :initform  ""
        :accessor colorIndex
        :documentation "")
    (coordIndex :initarg :coordIndex
        :initform  ""
        :accessor coordIndex
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass IndexedTriangleFanSet (X3DComposedGeometryNode)
  (
    (index :initarg :index
        :initform  ""
        :accessor index
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass IndexedTriangleSet (X3DComposedGeometryNode)
  (
    (index :initarg :index
        :initform  ""
        :accessor index
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass IndexedTriangleStripSet (X3DComposedGeometryNode)
  (
    (index :initarg :index
        :initform  ""
        :accessor index
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass LineSet (X3DGeometryNode)
  (
    (vertexCount :initarg :vertexCount
        :initform  ""
        :accessor vertexCount
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Normal (X3DNormalNode)
  (
    (vector :initarg :vector
        :initform  ""
        :accessor vector
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PointSet (X3DGeometryNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TriangleFanSet (X3DComposedGeometryNode)
  (
    (fanCount :initarg :fanCount
        :initform  ""
        :accessor fanCount
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TriangleSet (X3DComposedGeometryNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TriangleStripSet (X3DComposedGeometryNode)
  (
    (stripCount :initarg :stripCount
        :initform  ""
        :accessor stripCount
        :documentation "")
  )
  (:documentation ""))

