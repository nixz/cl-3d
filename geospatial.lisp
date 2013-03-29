;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; geospatial.lisp --- Definitions of the GEOSPATIAL Component in X3D
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
(defclass GeoCoordinate (X3DCoordinateNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
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
(defmethod add-subobject ((self GeoCoordinate) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass GeoElevationGrid (X3DGeometryNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (geoGridOrigin :initarg :geoGridOrigin
        :initform  "0 0 0"
        :accessor geoGridOrigin
        :documentation "")
    (height :initarg :height
        :initform  "0 0"
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
    (yScale :initarg :yScale
        :initform  "1"
        :accessor yScale
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
(defclass GeoLocation (X3DGroupingNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (geoCoords :initarg :geoCoords
        :initform  "0 0 0"
        :accessor geoCoords
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoLOD (X3DGroupingNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (rootUrl :initarg :rootUrl
        :initform  `()
        :accessor rootUrl
        :documentation "")
    (child1Url :initarg :child1Url
        :initform  `()
        :accessor child1Url
        :documentation "")
    (child2Url :initarg :child2Url
        :initform  `()
        :accessor child2Url
        :documentation "")
    (child3Url :initarg :child3Url
        :initform  `()
        :accessor child3Url
        :documentation "")
    (child4Url :initarg :child4Url
        :initform  `()
        :accessor child4Url
        :documentation "")
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (range :initarg :range
        :initform  "10"
        :accessor range
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoMetadata (X3DInfoNode)
  (
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (summary :initarg :summary
        :initform  `()
        :accessor summary
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoOrigin (X3DNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (geoCoords :initarg :geoCoords
        :initform  "0 0 0"
        :accessor geoCoords
        :documentation "")
    (rotateYUp :initarg :rotateYUp
        :initform  "false"
        :accessor rotateYUp
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self GeoOrigin) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass GeoPositionInterpolator (X3DInterpolatorNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (keyValue :initarg :keyValue
        :initform  ""
        :accessor keyValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoProximitySensor (X3DEnvironmentalSensorNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (geoCenter :initarg :geoCenter
        :initform  "0 0 0"
        :accessor geoCenter
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoTouchSensor (X3DTouchSensorNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass GeoTransform (X3DGroupingNode)
  (
    (geoCenter :initarg :geoCenter
        :initform  "0 0 0"
        :accessor geoCenter
        :documentation "")
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
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

;; ----------------------------------------------------------------------------
(defclass GeoViewpoint (X3DBindableNode)
  (
    (geoSystem :initarg :geoSystem
        :initform  ""GD" "WE""
        :accessor geoSystem
        :documentation "")
    (fieldOfView :initarg :fieldOfView
        :initform  "0.7854"
        :accessor fieldOfView
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
        :initform  "0 0 100000"
        :accessor position
        :documentation "")
    (description :initarg :description
        :initform  ""
        :accessor description
        :documentation "")
    (headlight :initarg :headlight
        :initform  "true"
        :accessor headlight
        :documentation "")
    (navType :initarg :navType
        :initform  `("EXAMINE" "ANY")
        :accessor navType
        :documentation "")
    (speedFactor :initarg :speedFactor
        :initform  "1.0"
        :accessor speedFactor
        :documentation "")
  )
  (:documentation ""))

