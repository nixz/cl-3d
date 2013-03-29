;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; particle-systems.lisp --- Definitions of the PARTICLE-SYSTEMS Component in X3D
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
(defclass X3DParticleEmitterNode (X3DNode)
  (
    (speed :initarg :speed
        :initform  "0"
        :accessor speed
        :documentation "")
    (variation :initarg :variation
        :initform  "0.25"
        :accessor variation
        :documentation "")
    (mass :initarg :mass
        :initform  "0"
        :accessor mass
        :documentation "")
    (surfaceArea :initarg :surfaceArea
        :initform  "0"
        :accessor surfaceArea
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DParticleEmitterNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DParticlePhysicsModelNode (X3DNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DParticlePhysicsModelNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass BoundedPhysicsModel (X3DParticlePhysicsModelNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ForcePhysicsModel (X3DParticlePhysicsModelNode)
  (
    (force :initarg :force
        :initform  "0 -9.8 0"
        :accessor force
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass WindPhysicsModel (X3DParticlePhysicsModelNode)
  (
    (direction :initarg :direction
        :initform  "1 0 0"
        :accessor direction
        :documentation "")
    (gustiness :initarg :gustiness
        :initform  "0.1"
        :accessor gustiness
        :documentation "")
    (speed :initarg :speed
        :initform  "0.1"
        :accessor speed
        :documentation "")
    (turbulence :initarg :turbulence
        :initform  "0"
        :accessor turbulence
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ConeEmitter (X3DParticleEmitterNode)
  (
    (angle :initarg :angle
        :initform  "0.7854"
        :accessor angle
        :documentation "")
    (direction :initarg :direction
        :initform  "0 1 0"
        :accessor direction
        :documentation "")
    (position :initarg :position
        :initform  "0 0 0"
        :accessor position
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ExplosionEmitter (X3DParticleEmitterNode)
  (
    (position :initarg :position
        :initform  "0 0 0"
        :accessor position
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PointEmitter (X3DParticleEmitterNode)
  (
    (direction :initarg :direction
        :initform  "0 1 0"
        :accessor direction
        :documentation "")
    (position :initarg :position
        :initform  "0 0 0"
        :accessor position
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PolylineEmitter (X3DParticleEmitterNode)
  (
    (coordIndex :initarg :coordIndex
        :initform  "-1"
        :accessor coordIndex
        :documentation "")
    (direction :initarg :direction
        :initform  "0 1 0"
        :accessor direction
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SurfaceEmitter (X3DParticleEmitterNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass VolumeEmitter (X3DParticleEmitterNode)
  (
    (coordIndex :initarg :coordIndex
        :initform  "-1"
        :accessor coordIndex
        :documentation "")
    (direction :initarg :direction
        :initform  "0 1 0"
        :accessor direction
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ParticleSystem (X3DShapeNode)
  (
    (createParticles :initarg :createParticles
        :initform  "true"
        :accessor createParticles
        :documentation "")
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (lifetimeVariation :initarg :lifetimeVariation
        :initform  "0.25"
        :accessor lifetimeVariation
        :documentation "")
    (maxParticles :initarg :maxParticles
        :initform  "200"
        :accessor maxParticles
        :documentation "")
    (particleLifetime :initarg :particleLifetime
        :initform  "5"
        :accessor particleLifetime
        :documentation "")
    (particleSize :initarg :particleSize
        :initform  "0.02 0.02"
        :accessor particleSize
        :documentation "")
    (colorKey :initarg :colorKey
        :initform  ""
        :accessor colorKey
        :documentation "")
    (geometryType :initarg :geometryType
        :initform  "QUAD"
        :accessor geometryType
        :documentation "")
    (texCoordKey :initarg :texCoordKey
        :initform  ""
        :accessor texCoordKey
        :documentation "")
  )
  (:documentation ""))

