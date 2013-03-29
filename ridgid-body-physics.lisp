;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; ridgid-body-physics.lisp --- Definitions of the RIDGID-BODY-PHYSICS Component in X3D
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
(defclass X3DNBodyCollidableNode (X3DChildNode)
  (
    (bboxCenter :initarg :bboxCenter
        :initform  "0 0 0"
        :accessor bboxCenter
        :documentation "")
    (bboxSize :initarg :bboxSize
        :initform  "-1 -1 -1"
        :accessor bboxSize
        :documentation "")
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (rotation :initarg :rotation
        :initform  "0 0 1 0"
        :accessor rotation
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DNBodyCollisionSpaceNode (X3DNode)
  (
    (bboxCenter :initarg :bboxCenter
        :initform  "0 0 0"
        :accessor bboxCenter
        :documentation "")
    (bboxSize :initarg :bboxSize
        :initform  "-1 -1 -1"
        :accessor bboxSize
        :documentation "")
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
(defmethod add-subobject ((self X3DNBodyCollisionSpaceNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DRigidJointNode (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
    (mustOutput :initarg :mustOutput
        :initform  `(NONE)
        :accessor mustOutput
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DRigidJointNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass BallJoint (X3DRigidJointNode)
  (
    (anchorPoint :initarg :anchorPoint
        :initform  "0 0 0"
        :accessor anchorPoint
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CollidableOffset (X3DNBodyCollidableNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CollidableShape (X3DNBodyCollidableNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CollisionCollection (X3DChildNode)
  (
    (appliedParameters :initarg :appliedParameters
        :initform  `(BOUNCE)
        :accessor appliedParameters
        :documentation "")
    (bounce :initarg :bounce
        :initform  "0"
        :accessor bounce
        :documentation "")
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (frictionCoefficients :initarg :frictionCoefficients
        :initform  "0 0"
        :accessor frictionCoefficients
        :documentation "")
    (minBounceSpeed :initarg :minBounceSpeed
        :initform  "0.1"
        :accessor minBounceSpeed
        :documentation "")
    (slipFactors :initarg :slipFactors
        :initform  "0 0"
        :accessor slipFactors
        :documentation "")
    (softnessConstantForceMix :initarg :softnessConstantForceMix
        :initform  "0.0001"
        :accessor softnessConstantForceMix
        :documentation "")
    (softnessErrorCorrection :initarg :softnessErrorCorrection
        :initform  "0.8"
        :accessor softnessErrorCorrection
        :documentation "")
    (surfaceSpeed :initarg :surfaceSpeed
        :initform  "0 0"
        :accessor surfaceSpeed
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CollisionSensor (X3DChildNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass CollisionSpace (X3DNBodyCollisionSpaceNode)
  (
    (useGeometry :initarg :useGeometry
        :initform  "false"
        :accessor useGeometry
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Contact (X3DNBodyCollisionSpaceNode)
  (
    (appliedParameters :initarg :appliedParameters
        :initform  `(BOUNCE)
        :accessor appliedParameters
        :documentation "")
    (bounce :initarg :bounce
        :initform  "0"
        :accessor bounce
        :documentation "")
    (contactNormal :initarg :contactNormal
        :initform  "0 1 0"
        :accessor contactNormal
        :documentation "")
    (depth :initarg :depth
        :initform  "0"
        :accessor depth
        :documentation "")
    (frictionCoefficients :initarg :frictionCoefficients
        :initform  "0 0"
        :accessor frictionCoefficients
        :documentation "")
    (frictionDirection :initarg :frictionDirection
        :initform  "0 1 0"
        :accessor frictionDirection
        :documentation "")
    (minBounceSpeed :initarg :minBounceSpeed
        :initform  "0"
        :accessor minBounceSpeed
        :documentation "")
    (position :initarg :position
        :initform  "0 0 0"
        :accessor position
        :documentation "")
    (slipCoefficients :initarg :slipCoefficients
        :initform  "0 0"
        :accessor slipCoefficients
        :documentation "")
    (softnessConstantForceMix :initarg :softnessConstantForceMix
        :initform  "0.0001"
        :accessor softnessConstantForceMix
        :documentation "")
    (softnessErrorCorrection :initarg :softnessErrorCorrection
        :initform  "0.8"
        :accessor softnessErrorCorrection
        :documentation "")
    (surfaceSpeed :initarg :surfaceSpeed
        :initform  "0 0"
        :accessor surfaceSpeed
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass DoubleAxisHingeJoint (X3DRigidJointNode)
  (
    (anchorPoint :initarg :anchorPoint
        :initform  "0 0 0"
        :accessor anchorPoint
        :documentation "")
    (axis1 :initarg :axis1
        :initform  "0 0 0"
        :accessor axis1
        :documentation "")
    (axis2 :initarg :axis2
        :initform  "0 0 0"
        :accessor axis2
        :documentation "")
    (desiredAngularVelocity1 :initarg :desiredAngularVelocity1
        :initform  "0"
        :accessor desiredAngularVelocity1
        :documentation "")
    (desiredAngularVelocity2 :initarg :desiredAngularVelocity2
        :initform  "0"
        :accessor desiredAngularVelocity2
        :documentation "")
    (maxAngle1 :initarg :maxAngle1
        :initform  "3.141592653"
        :accessor maxAngle1
        :documentation "")
    (maxTorque1 :initarg :maxTorque1
        :initform  "0"
        :accessor maxTorque1
        :documentation "")
    (maxTorque2 :initarg :maxTorque2
        :initform  "0"
        :accessor maxTorque2
        :documentation "")
    (minAngle1 :initarg :minAngle1
        :initform  "-3.141592653"
        :accessor minAngle1
        :documentation "")
    (stop1Bounce :initarg :stop1Bounce
        :initform  "0"
        :accessor stop1Bounce
        :documentation "")
    (stop1ConstantForceMix :initarg :stop1ConstantForceMix
        :initform  "0.001"
        :accessor stop1ConstantForceMix
        :documentation "")
    (stop1ErrorCorrection :initarg :stop1ErrorCorrection
        :initform  "0.8"
        :accessor stop1ErrorCorrection
        :documentation "")
    (suspensionErrorCorrection :initarg :suspensionErrorCorrection
        :initform  "0.8"
        :accessor suspensionErrorCorrection
        :documentation "")
    (suspensionForce :initarg :suspensionForce
        :initform  "0"
        :accessor suspensionForce
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MotorJoint (X3DRigidJointNode)
  (
    (autoCalc :initarg :autoCalc
        :initform  "false"
        :accessor autoCalc
        :documentation "")
    (axis1Angle :initarg :axis1Angle
        :initform  "0"
        :accessor axis1Angle
        :documentation "")
    (axis1Torque :initarg :axis1Torque
        :initform  "0"
        :accessor axis1Torque
        :documentation "")
    (axis2Angle :initarg :axis2Angle
        :initform  "0"
        :accessor axis2Angle
        :documentation "")
    (axis2Torque :initarg :axis2Torque
        :initform  "0"
        :accessor axis2Torque
        :documentation "")
    (axis3Angle :initarg :axis3Angle
        :initform  "0"
        :accessor axis3Angle
        :documentation "")
    (axis3Torque :initarg :axis3Torque
        :initform  "0"
        :accessor axis3Torque
        :documentation "")
    (enabledAxes :initarg :enabledAxes
        :initform  "1"
        :accessor enabledAxes
        :documentation "")
    (motor1Axis :initarg :motor1Axis
        :initform  "0 0 0"
        :accessor motor1Axis
        :documentation "")
    (motor2Axis :initarg :motor2Axis
        :initform  "0 0 0"
        :accessor motor2Axis
        :documentation "")
    (motor3Axis :initarg :motor3Axis
        :initform  "0 0 0"
        :accessor motor3Axis
        :documentation "")
    (stop1Bounce :initarg :stop1Bounce
        :initform  "0"
        :accessor stop1Bounce
        :documentation "")
    (stop1ErrorCorrection :initarg :stop1ErrorCorrection
        :initform  "0.8"
        :accessor stop1ErrorCorrection
        :documentation "")
    (stop2Bounce :initarg :stop2Bounce
        :initform  "0"
        :accessor stop2Bounce
        :documentation "")
    (stop2ErrorCorrection :initarg :stop2ErrorCorrection
        :initform  "0.8"
        :accessor stop2ErrorCorrection
        :documentation "")
    (stop3Bounce :initarg :stop3Bounce
        :initform  "0"
        :accessor stop3Bounce
        :documentation "")
    (stop3ErrorCorrection :initarg :stop3ErrorCorrection
        :initform  "0.8"
        :accessor stop3ErrorCorrection
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass RigidBody (X3DNode)
  (
    (angularDampingFactor :initarg :angularDampingFactor
        :initform  "0.001"
        :accessor angularDampingFactor
        :documentation "")
    (angularVelocity :initarg :angularVelocity
        :initform  "0 0 0"
        :accessor angularVelocity
        :documentation "")
    (autoDamp :initarg :autoDamp
        :initform  "false"
        :accessor autoDamp
        :documentation "")
    (autoDisable :initarg :autoDisable
        :initform  "false"
        :accessor autoDisable
        :documentation "")
    (centerOfMass :initarg :centerOfMass
        :initform  "0 0 0"
        :accessor centerOfMass
        :documentation "")
    (disableAngularSpeed :initarg :disableAngularSpeed
        :initform  "0"
        :accessor disableAngularSpeed
        :documentation "")
    (disableLinearSpeed :initarg :disableLinearSpeed
        :initform  "0"
        :accessor disableLinearSpeed
        :documentation "")
    (disableTime :initarg :disableTime
        :initform  "0"
        :accessor disableTime
        :documentation "")
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (finiteRotationAxis :initarg :finiteRotationAxis
        :initform  "0 1 0"
        :accessor finiteRotationAxis
        :documentation "")
    (fixed :initarg :fixed
        :initform  "false"
        :accessor fixed
        :documentation "")
    (forces :initarg :forces
        :initform  ""
        :accessor forces
        :documentation "")
    (inertia :initarg :inertia
        :initform  "1 0 0 0 1 0 0 0 1"
        :accessor inertia
        :documentation "")
    (linearDampingFactor :initarg :linearDampingFactor
        :initform  "0.001"
        :accessor linearDampingFactor
        :documentation "")
    (linearVelocity :initarg :linearVelocity
        :initform  "0 0 0"
        :accessor linearVelocity
        :documentation "")
    (mass :initarg :mass
        :initform  "1"
        :accessor mass
        :documentation "")
    (position :initarg :position
        :initform  "0 0 0"
        :accessor position
        :documentation "")
    (orientation :initarg :orientation
        :initform  "0 0 1 0"
        :accessor orientation
        :documentation "")
    (torques :initarg :torques
        :initform  ""
        :accessor torques
        :documentation "")
    (useFiniteRotation :initarg :useFiniteRotation
        :initform  "false"
        :accessor useFiniteRotation
        :documentation "")
    (useGlobalGravity :initarg :useGlobalGravity
        :initform  "true"
        :accessor useGlobalGravity
        :documentation "")
    (containerField :initarg :containerField
        :initform  "bodies"
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass RigidBodyCollection (X3DChildNode)
  (
    (autoDisable :initarg :autoDisable
        :initform  "false"
        :accessor autoDisable
        :documentation "")
    (constantForceMix :initarg :constantForceMix
        :initform  "0.0001"
        :accessor constantForceMix
        :documentation "")
    (contactSurfaceThickness :initarg :contactSurfaceThickness
        :initform  "0"
        :accessor contactSurfaceThickness
        :documentation "")
    (disableAngularSpeed :initarg :disableAngularSpeed
        :initform  "0"
        :accessor disableAngularSpeed
        :documentation "")
    (disableLinearSpeed :initarg :disableLinearSpeed
        :initform  "0"
        :accessor disableLinearSpeed
        :documentation "")
    (disableTime :initarg :disableTime
        :initform  "0"
        :accessor disableTime
        :documentation "")
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (errorCorrection :initarg :errorCorrection
        :initform  "0.8"
        :accessor errorCorrection
        :documentation "")
    (gravity :initarg :gravity
        :initform  "0 -9.8 0"
        :accessor gravity
        :documentation "")
    (iterations :initarg :iterations
        :initform  "10"
        :accessor iterations
        :documentation "")
    (maxCorrectionSpeed :initarg :maxCorrectionSpeed
        :initform  "-1"
        :accessor maxCorrectionSpeed
        :documentation "")
    (preferAccuracy :initarg :preferAccuracy
        :initform  "false"
        :accessor preferAccuracy
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SingleAxisHingeJoint (X3DRigidJointNode)
  (
    (anchorPoint :initarg :anchorPoint
        :initform  "0 0 0"
        :accessor anchorPoint
        :documentation "")
    (axis :initarg :axis
        :initform  "0 0 0"
        :accessor axis
        :documentation "")
    (maxAngle :initarg :maxAngle
        :initform  "3.141592653"
        :accessor maxAngle
        :documentation "")
    (minAngle :initarg :minAngle
        :initform  "-3.141592653"
        :accessor minAngle
        :documentation "")
    (stopBounce :initarg :stopBounce
        :initform  "0"
        :accessor stopBounce
        :documentation "")
    (stopErrorCorrection :initarg :stopErrorCorrection
        :initform  "0.8"
        :accessor stopErrorCorrection
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass SliderJoint (X3DRigidJointNode)
  (
    (axis :initarg :axis
        :initform  "0 1 0"
        :accessor axis
        :documentation "")
    (maxSeparation :initarg :maxSeparation
        :initform  "1"
        :accessor maxSeparation
        :documentation "")
    (minSeparation :initarg :minSeparation
        :initform  "0"
        :accessor minSeparation
        :documentation "")
    (sliderForce :initarg :sliderForce
        :initform  "0"
        :accessor sliderForce
        :documentation "")
    (stopBounce :initarg :stopBounce
        :initform  "0"
        :accessor stopBounce
        :documentation "")
    (stopErrorCorrection :initarg :stopErrorCorrection
        :initform  "1"
        :accessor stopErrorCorrection
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass UniversalJoint (X3DRigidJointNode)
  (
    (anchorPoint :initarg :anchorPoint
        :initform  "0 0 0"
        :accessor anchorPoint
        :documentation "")
    (axis1 :initarg :axis1
        :initform  "0 0 0"
        :accessor axis1
        :documentation "")
    (axis2 :initarg :axis2
        :initform  "0 0 0"
        :accessor axis2
        :documentation "")
    (stop1Bounce :initarg :stop1Bounce
        :initform  "0"
        :accessor stop1Bounce
        :documentation "")
    (stop1ErrorCorrection :initarg :stop1ErrorCorrection
        :initform  "0.8"
        :accessor stop1ErrorCorrection
        :documentation "")
    (stop2Bounce :initarg :stop2Bounce
        :initform  "0"
        :accessor stop2Bounce
        :documentation "")
    (stop2ErrorCorrection :initarg :stop2ErrorCorrection
        :initform  "0.8"
        :accessor stop2ErrorCorrection
        :documentation "")
  )
  (:documentation ""))

