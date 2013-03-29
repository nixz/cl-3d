;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; pointing-device-sensor.lisp --- Definitions of the POINTING-DEVICE-SENSOR Component in X3D
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
(defclass X3DDragSensorNode (X3DPointingDeviceSensorNode)
  (
    (autoOffset :initarg :autoOffset
        :initform  "true"
        :accessor autoOffset
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DPointingDeviceSensorNode (X3DSensorNode)
  (
    (description :initarg :description
        :initform  ""
        :accessor description
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DTouchSensorNode (X3DPointingDeviceSensorNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CylinderSensor (X3DDragSensorNode)
  (
    (diskAngle :initarg :diskAngle
        :initform  "0.26179167"
        :accessor diskAngle
        :documentation "")
    (maxAngle :initarg :maxAngle
        :initform  "-1"
        :accessor maxAngle
        :documentation "")
    (minAngle :initarg :minAngle
        :initform  "0"
        :accessor minAngle
        :documentation "")
    (offset :initarg :offset
        :initform  "0"
        :accessor offset
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PlaneSensor (X3DDragSensorNode)
  (
    (maxPosition :initarg :maxPosition
        :initform  "-1 -1"
        :accessor maxPosition
        :documentation "")
    (minPosition :initarg :minPosition
        :initform  "0 0"
        :accessor minPosition
        :documentation "")
    (offset :initarg :offset
        :initform  "0 0 0"
        :accessor offset
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SphereSensor (X3DDragSensorNode)
  (
    (offset :initarg :offset
        :initform  "0 1 0 0"
        :accessor offset
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TouchSensor (X3DTouchSensorNode)
  (
  )
  (:documentation ""))

