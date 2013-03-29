;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; follower.lisp --- Definitions of the FOLLOWER Component in X3D
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
(defclass X3DChaserNode (X3DFollowerNode)
  (
    (duration :initarg :duration
        :initform  "1"
        :accessor duration
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DDamperNode (X3DFollowerNode)
  (
    (tau :initarg :tau
        :initform  "0.3"
        :accessor tau
        :documentation "")
    (tolerance :initarg :tolerance
        :initform  "-1"
        :accessor tolerance
        :documentation "")
    (order :initarg :order
        :initform  "3"
        :accessor order
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DFollowerNode (X3DChildNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ColorDamper (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0.8 0.8 0.8"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0.8 0.8 0.8"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CoordinateDamper (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass OrientationDamper (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 1 0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 1 0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PositionDamper (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PositionDamper2D (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TexCoordDamper2D (X3DDamperNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  ""
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  ""
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass OrientationChaser (X3DChaserNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 1 0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 1 0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PositionChaser (X3DChaserNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PositionChaser2D (X3DChaserNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0 0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0 0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ScalarChaser (X3DChaserNode)
  (
    (initialDestination :initarg :initialDestination
        :initform  "0"
        :accessor initialDestination
        :documentation "")
    (initialValue :initarg :initialValue
        :initform  "0"
        :accessor initialValue
        :documentation "")
  )
  (:documentation ""))

