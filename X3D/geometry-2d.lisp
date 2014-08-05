;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; geometry-2d.lisp --- Definitions of the GEOMETRY-2D Component in X3D
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
(defclass Arc2D (X3DGeometryNode)
  (
    (radius :initarg :radius
        :initform  "1"
        :accessor radius
        :documentation "")
    (startAngle :initarg :startAngle
        :initform  "0"
        :accessor startAngle
        :documentation "")
    (endAngle :initarg :endAngle
        :initform  "1.570796"
        :accessor endAngle
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ArcClose2D (X3DGeometryNode)
  (
    (radius :initarg :radius
        :initform  "1"
        :accessor radius
        :documentation "")
    (startAngle :initarg :startAngle
        :initform  "0"
        :accessor startAngle
        :documentation "")
    (endAngle :initarg :endAngle
        :initform  "1.570796"
        :accessor endAngle
        :documentation "")
    (closureType :initarg :closureType
        :initform  "PIE"
        :accessor closureType
        :documentation "")
    (solid :initarg :solid
        :initform  "false"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Circle2D (X3DGeometryNode)
  (
    (radius :initarg :radius
        :initform  "1"
        :accessor radius
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Disk2D (X3DGeometryNode)
  (
    (innerRadius :initarg :innerRadius
        :initform  "0"
        :accessor innerRadius
        :documentation "")
    (outerRadius :initarg :outerRadius
        :initform  "1"
        :accessor outerRadius
        :documentation "")
    (solid :initarg :solid
        :initform  "false"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Polyline2D (X3DGeometryNode)
  (
    (lineSegments :initarg :lineSegments
        :initform  ""
        :accessor lineSegments
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Polypoint2D (X3DGeometryNode)
  (
    (point :initarg :point
        :initform  ""
        :accessor point
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Rectangle2D (X3DGeometryNode)
  (
    (size :initarg :size
        :initform  "2 2"
        :accessor size
        :documentation "")
    (solid :initarg :solid
        :initform  "false"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TriangleSet2D (X3DGeometryNode)
  (
    (vertices :initarg :vertices
        :initform  ""
        :accessor vertices
        :documentation "")
    (solid :initarg :solid
        :initform  "false"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

