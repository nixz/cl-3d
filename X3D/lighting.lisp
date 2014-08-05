;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; lighting.lisp --- Definitions of the LIGHTING Component in X3D
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
(defclass X3DLightNode (X3DChildNode)
  (
    (ambientIntensity :initarg :ambientIntensity
        :initform  "0"
        :accessor ambientIntensity
        :documentation "")
    (color :initarg :color
        :initform  "1 1 1"
        :accessor color
        :documentation "")
    (intensity :initarg :intensity
        :initform  "1"
        :accessor intensity
        :documentation "")
    (on :initarg :on
        :initform  "true"
        :accessor on
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass DirectionalLight (X3DLightNode)
  (
    (direction :initarg :direction
        :initform  "0 0 -1"
        :accessor direction
        :documentation "")
    (global :initarg :global
        :initform  "false"
        :accessor global
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PointLight (X3DLightNode)
  (
    (attenuation :initarg :attenuation
        :initform  "1 0 0"
        :accessor attenuation
        :documentation "")
    (location :initarg :location
        :initform  "0 0 0"
        :accessor location
        :documentation "")
    (radius :initarg :radius
        :initform  "100"
        :accessor radius
        :documentation "")
    (global :initarg :global
        :initform  "true"
        :accessor global
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SpotLight (X3DLightNode)
  (
    (attenuation :initarg :attenuation
        :initform  "1 0 0"
        :accessor attenuation
        :documentation "")
    (beamWidth :initarg :beamWidth
        :initform  "0.7854"
        :accessor beamWidth
        :documentation "")
    (cutOffAngle :initarg :cutOffAngle
        :initform  "1.570796"
        :accessor cutOffAngle
        :documentation "")
    (direction :initarg :direction
        :initform  "0 0 -1"
        :accessor direction
        :documentation "")
    (location :initarg :location
        :initform  "0 0 0"
        :accessor location
        :documentation "")
    (radius :initarg :radius
        :initform  "100"
        :accessor radius
        :documentation "")
    (global :initarg :global
        :initform  "true"
        :accessor global
        :documentation "")
  )
  (:documentation ""))

