;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; distributed-interactive-simulation.lisp --- Definitions of the DISTRIBUTED-INTERACTIVE-SIMULATION Component in X3D
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
(defclass DISEntityManager (X3DChildNode)
  (
    (address :initarg :address
        :initform  "localhost"
        :accessor address
        :documentation "")
    (applicationID :initarg :applicationID
        :initform  "1"
        :accessor applicationID
        :documentation "")
    (port :initarg :port
        :initform  "0"
        :accessor port
        :documentation "")
    (siteID :initarg :siteID
        :initform  "0"
        :accessor siteID
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass DISEntityTypeMapping (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (category :initarg :category
        :initform  "0"
        :accessor category
        :documentation "")
    (country :initarg :country
        :initform  "0"
        :accessor country
        :documentation "")
    (domain :initarg :domain
        :initform  "0"
        :accessor domain
        :documentation "")
    (extra :initarg :extra
        :initform  "0"
        :accessor extra
        :documentation "")
    (kind :initarg :kind
        :initform  "0"
        :accessor kind
        :documentation "")
    (specific :initarg :specific
        :initform  "0"
        :accessor specific
        :documentation "")
    (subcategory :initarg :subcategory
        :initform  "0"
        :accessor subcategory
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self DISEntityTypeMapping) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass EspduTransform (X3DGroupingNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
    (marking :initarg :marking
        :initform  ""
        :accessor marking
        :documentation "")
    (siteID :initarg :siteID
        :initform  "0"
        :accessor siteID
        :documentation "")
    (applicationID :initarg :applicationID
        :initform  "1"
        :accessor applicationID
        :documentation "")
    (entityID :initarg :entityID
        :initform  "0"
        :accessor entityID
        :documentation "")
    (forceID :initarg :forceID
        :initform  "0"
        :accessor forceID
        :documentation "")
    (entityKind :initarg :entityKind
        :initform  "0"
        :accessor entityKind
        :documentation "")
    (entityDomain :initarg :entityDomain
        :initform  "0"
        :accessor entityDomain
        :documentation "")
    (entityCountry :initarg :entityCountry
        :initform  "0"
        :accessor entityCountry
        :documentation "")
    (entityCategory :initarg :entityCategory
        :initform  "0"
        :accessor entityCategory
        :documentation "")
    (entitySubCategory :initarg :entitySubCategory
        :initform  "0"
        :accessor entitySubCategory
        :documentation "")
    (entitySpecific :initarg :entitySpecific
        :initform  "0"
        :accessor entitySpecific
        :documentation "")
    (entityExtra :initarg :entityExtra
        :initform  "0"
        :accessor entityExtra
        :documentation "")
    (readInterval :initarg :readInterval
        :initform  "0.1"
        :accessor readInterval
        :documentation "")
    (writeInterval :initarg :writeInterval
        :initform  "1.0"
        :accessor writeInterval
        :documentation "")
    (networkMode :initarg :networkMode
        :initform  "standAlone"
        :accessor networkMode
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
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
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
        :documentation "")
    (address :initarg :address
        :initform  "localhost"
        :accessor address
        :documentation "")
    (port :initarg :port
        :initform  "0"
        :accessor port
        :documentation "")
    (multicastRelayHost :initarg :multicastRelayHost
        :initform  ""
        :accessor multicastRelayHost
        :documentation "")
    (multicastRelayPort :initarg :multicastRelayPort
        :initform  "0"
        :accessor multicastRelayPort
        :documentation "")
    (rtpHeaderExpected :initarg :rtpHeaderExpected
        :initform  "false"
        :accessor rtpHeaderExpected
        :documentation "")
    (deadReckoning :initarg :deadReckoning
        :initform  "0"
        :accessor deadReckoning
        :documentation "")
    (linearVelocity :initarg :linearVelocity
        :initform  "0 0 0"
        :accessor linearVelocity
        :documentation "")
    (linearAcceleration :initarg :linearAcceleration
        :initform  "0 0 0"
        :accessor linearAcceleration
        :documentation "")
    (fired1 :initarg :fired1
        :initform  "false"
        :accessor fired1
        :documentation "")
    (fired2 :initarg :fired2
        :initform  "false"
        :accessor fired2
        :documentation "")
    (collisionType :initarg :collisionType
        :initform  "0"
        :accessor collisionType
        :documentation "")
    (detonationLocation :initarg :detonationLocation
        :initform  "0 0 0"
        :accessor detonationLocation
        :documentation "")
    (detonationRelativeLocation :initarg :detonationRelativeLocation
        :initform  "0 0 0"
        :accessor detonationRelativeLocation
        :documentation "")
    (detonationResult :initarg :detonationResult
        :initform  "0"
        :accessor detonationResult
        :documentation "")
    (eventApplicationID :initarg :eventApplicationID
        :initform  "1"
        :accessor eventApplicationID
        :documentation "")
    (eventEntityID :initarg :eventEntityID
        :initform  "0"
        :accessor eventEntityID
        :documentation "")
    (eventNumber :initarg :eventNumber
        :initform  "0"
        :accessor eventNumber
        :documentation "")
    (eventSiteID :initarg :eventSiteID
        :initform  "0"
        :accessor eventSiteID
        :documentation "")
    (munitionStartPoint :initarg :munitionStartPoint
        :initform  "0 0 0"
        :accessor munitionStartPoint
        :documentation "")
    (munitionEndPoint :initarg :munitionEndPoint
        :initform  "0 0 0"
        :accessor munitionEndPoint
        :documentation "")
    (munitionSiteID :initarg :munitionSiteID
        :initform  "0"
        :accessor munitionSiteID
        :documentation "")
    (munitionApplicationID :initarg :munitionApplicationID
        :initform  "1"
        :accessor munitionApplicationID
        :documentation "")
    (munitionEntityID :initarg :munitionEntityID
        :initform  "0"
        :accessor munitionEntityID
        :documentation "")
    (fireMissionIndex :initarg :fireMissionIndex
        :initform  "0"
        :accessor fireMissionIndex
        :documentation "")
    (warhead :initarg :warhead
        :initform  "0"
        :accessor warhead
        :documentation "")
    (fuse :initarg :fuse
        :initform  "0"
        :accessor fuse
        :documentation "")
    (munitionQuantity :initarg :munitionQuantity
        :initform  "0"
        :accessor munitionQuantity
        :documentation "")
    (firingRate :initarg :firingRate
        :initform  "0"
        :accessor firingRate
        :documentation "")
    (firingRange :initarg :firingRange
        :initform  "0"
        :accessor firingRange
        :documentation "")
    (articulationParameterCount :initarg :articulationParameterCount
        :initform  "0"
        :accessor articulationParameterCount
        :documentation "")
    (articulationParameterDesignatorArray :initarg :articulationParameterDesignatorArray
        :initform  ""
        :accessor articulationParameterDesignatorArray
        :documentation "")
    (articulationParameterChangeIndicatorArray :initarg :articulationParameterChangeIndicatorArray
        :initform  ""
        :accessor articulationParameterChangeIndicatorArray
        :documentation "")
    (articulationParameterIdPartAttachedToArray :initarg :articulationParameterIdPartAttachedToArray
        :initform  ""
        :accessor articulationParameterIdPartAttachedToArray
        :documentation "")
    (articulationParameterTypeArray :initarg :articulationParameterTypeArray
        :initform  ""
        :accessor articulationParameterTypeArray
        :documentation "")
    (articulationParameterArray :initarg :articulationParameterArray
        :initform  ""
        :accessor articulationParameterArray
        :documentation "")
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
(defclass ReceiverPdu (X3DNetworkSensorNode X3DBoundedObject)
  (
    (whichGeometry :initarg :whichGeometry
        :initform  "1"
        :accessor whichGeometry
        :documentation "")
    (readInterval :initarg :readInterval
        :initform  "0.1"
        :accessor readInterval
        :documentation "")
    (writeInterval :initarg :writeInterval
        :initform  "1.0"
        :accessor writeInterval
        :documentation "")
    (networkMode :initarg :networkMode
        :initform  "standAlone"
        :accessor networkMode
        :documentation "")
    (siteID :initarg :siteID
        :initform  "0"
        :accessor siteID
        :documentation "")
    (applicationID :initarg :applicationID
        :initform  "1"
        :accessor applicationID
        :documentation "")
    (entityID :initarg :entityID
        :initform  "0"
        :accessor entityID
        :documentation "")
    (address :initarg :address
        :initform  "localhost"
        :accessor address
        :documentation "")
    (port :initarg :port
        :initform  "0"
        :accessor port
        :documentation "")
    (multicastRelayHost :initarg :multicastRelayHost
        :initform  ""
        :accessor multicastRelayHost
        :documentation "")
    (multicastRelayPort :initarg :multicastRelayPort
        :initform  "0"
        :accessor multicastRelayPort
        :documentation "")
    (rtpHeaderExpected :initarg :rtpHeaderExpected
        :initform  "false"
        :accessor rtpHeaderExpected
        :documentation "")
    (radioID :initarg :radioID
        :initform  "0"
        :accessor radioID
        :documentation "")
    (receivedPower :initarg :receivedPower
        :initform  "0.0"
        :accessor receivedPower
        :documentation "")
    (receiverState :initarg :receiverState
        :initform  "0"
        :accessor receiverState
        :documentation "")
    (transmitterSiteID :initarg :transmitterSiteID
        :initform  "0"
        :accessor transmitterSiteID
        :documentation "")
    (transmitterApplicationID :initarg :transmitterApplicationID
        :initform  "0"
        :accessor transmitterApplicationID
        :documentation "")
    (transmitterEntityID :initarg :transmitterEntityID
        :initform  "0"
        :accessor transmitterEntityID
        :documentation "")
    (transmitterRadioID :initarg :transmitterRadioID
        :initform  "0"
        :accessor transmitterRadioID
        :documentation "")
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
(defclass SignalPdu (X3DNetworkSensorNode X3DBoundedObject)
  (
    (whichGeometry :initarg :whichGeometry
        :initform  "1"
        :accessor whichGeometry
        :documentation "")
    (readInterval :initarg :readInterval
        :initform  "0.1"
        :accessor readInterval
        :documentation "")
    (writeInterval :initarg :writeInterval
        :initform  "1.0"
        :accessor writeInterval
        :documentation "")
    (networkMode :initarg :networkMode
        :initform  "standAlone"
        :accessor networkMode
        :documentation "")
    (siteID :initarg :siteID
        :initform  "0"
        :accessor siteID
        :documentation "")
    (applicationID :initarg :applicationID
        :initform  "1"
        :accessor applicationID
        :documentation "")
    (entityID :initarg :entityID
        :initform  "0"
        :accessor entityID
        :documentation "")
    (address :initarg :address
        :initform  "localhost"
        :accessor address
        :documentation "")
    (port :initarg :port
        :initform  "0"
        :accessor port
        :documentation "")
    (multicastRelayHost :initarg :multicastRelayHost
        :initform  ""
        :accessor multicastRelayHost
        :documentation "")
    (multicastRelayPort :initarg :multicastRelayPort
        :initform  "0"
        :accessor multicastRelayPort
        :documentation "")
    (rtpHeaderExpected :initarg :rtpHeaderExpected
        :initform  "false"
        :accessor rtpHeaderExpected
        :documentation "")
    (radioID :initarg :radioID
        :initform  "0"
        :accessor radioID
        :documentation "")
    (encodingScheme :initarg :encodingScheme
        :initform  "0"
        :accessor encodingScheme
        :documentation "")
    (tdlType :initarg :tdlType
        :initform  "0"
        :accessor tdlType
        :documentation "")
    (sampleRate :initarg :sampleRate
        :initform  "0"
        :accessor sampleRate
        :documentation "")
    (samples :initarg :samples
        :initform  "0"
        :accessor samples
        :documentation "")
    (dataLength :initarg :dataLength
        :initform  "0"
        :accessor dataLength
        :documentation "")
    (data :initarg :data
        :initform  ""
        :accessor data
        :documentation "")
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
(defclass TransmitterPdu (X3DNetworkSensorNode X3DBoundedObject)
  (
    (whichGeometry :initarg :whichGeometry
        :initform  "1"
        :accessor whichGeometry
        :documentation "")
    (readInterval :initarg :readInterval
        :initform  "0.1"
        :accessor readInterval
        :documentation "")
    (writeInterval :initarg :writeInterval
        :initform  "1.0"
        :accessor writeInterval
        :documentation "")
    (networkMode :initarg :networkMode
        :initform  "standAlone"
        :accessor networkMode
        :documentation "")
    (siteID :initarg :siteID
        :initform  "0"
        :accessor siteID
        :documentation "")
    (applicationID :initarg :applicationID
        :initform  "1"
        :accessor applicationID
        :documentation "")
    (entityID :initarg :entityID
        :initform  "0"
        :accessor entityID
        :documentation "")
    (address :initarg :address
        :initform  "localhost"
        :accessor address
        :documentation "")
    (port :initarg :port
        :initform  "0"
        :accessor port
        :documentation "")
    (multicastRelayHost :initarg :multicastRelayHost
        :initform  ""
        :accessor multicastRelayHost
        :documentation "")
    (multicastRelayPort :initarg :multicastRelayPort
        :initform  "0"
        :accessor multicastRelayPort
        :documentation "")
    (rtpHeaderExpected :initarg :rtpHeaderExpected
        :initform  "false"
        :accessor rtpHeaderExpected
        :documentation "")
    (radioID :initarg :radioID
        :initform  "0"
        :accessor radioID
        :documentation "")
    (antennaLocation :initarg :antennaLocation
        :initform  "0 0 0"
        :accessor antennaLocation
        :documentation "")
    (antennaPatternLength :initarg :antennaPatternLength
        :initform  "0"
        :accessor antennaPatternLength
        :documentation "")
    (antennaPatternType :initarg :antennaPatternType
        :initform  "0"
        :accessor antennaPatternType
        :documentation "")
    (cryptoKeyID :initarg :cryptoKeyID
        :initform  "0"
        :accessor cryptoKeyID
        :documentation "")
    (cryptoSystem :initarg :cryptoSystem
        :initform  "0"
        :accessor cryptoSystem
        :documentation "")
    (frequency :initarg :frequency
        :initform  "0"
        :accessor frequency
        :documentation "")
    (inputSource :initarg :inputSource
        :initform  "0"
        :accessor inputSource
        :documentation "")
    (lengthOfModulationParameters :initarg :lengthOfModulationParameters
        :initform  "0"
        :accessor lengthOfModulationParameters
        :documentation "")
    (modulationTypeDetail :initarg :modulationTypeDetail
        :initform  "0"
        :accessor modulationTypeDetail
        :documentation "")
    (modulationTypeMajor :initarg :modulationTypeMajor
        :initform  "0"
        :accessor modulationTypeMajor
        :documentation "")
    (modulationTypeSpreadSpectrum :initarg :modulationTypeSpreadSpectrum
        :initform  "0"
        :accessor modulationTypeSpreadSpectrum
        :documentation "")
    (modulationTypeSystem :initarg :modulationTypeSystem
        :initform  "0"
        :accessor modulationTypeSystem
        :documentation "")
    (power :initarg :power
        :initform  "0.0"
        :accessor power
        :documentation "")
    (radioEntityTypeCategory :initarg :radioEntityTypeCategory
        :initform  "0"
        :accessor radioEntityTypeCategory
        :documentation "")
    (radioEntityTypeCountry :initarg :radioEntityTypeCountry
        :initform  "0"
        :accessor radioEntityTypeCountry
        :documentation "")
    (radioEntityTypeDomain :initarg :radioEntityTypeDomain
        :initform  "0"
        :accessor radioEntityTypeDomain
        :documentation "")
    (radioEntityTypeKind :initarg :radioEntityTypeKind
        :initform  "0"
        :accessor radioEntityTypeKind
        :documentation "")
    (radioEntityTypeNomenclature :initarg :radioEntityTypeNomenclature
        :initform  "0"
        :accessor radioEntityTypeNomenclature
        :documentation "")
    (radioEntityTypeNomenclatureVersion :initarg :radioEntityTypeNomenclatureVersion
        :initform  "0"
        :accessor radioEntityTypeNomenclatureVersion
        :documentation "")
    (relativeAntennaLocation :initarg :relativeAntennaLocation
        :initform  "0 0 0"
        :accessor relativeAntennaLocation
        :documentation "")
    (transmitFrequencyBandwidth :initarg :transmitFrequencyBandwidth
        :initform  "0"
        :accessor transmitFrequencyBandwidth
        :documentation "")
    (transmitState :initarg :transmitState
        :initform  "0"
        :accessor transmitState
        :documentation "")
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

