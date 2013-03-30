;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; core.lisp --- Definitions of the CORE Component in X3D
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
(defclass X3DBindableNode (X3DChildNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DChildNode (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DChildNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DInfoNode (X3DChildNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DMetadataObject (X3DNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (reference :initarg :reference
        :initform  ""
        :accessor reference
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DMetadataObject) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DNode (xml-serializer)
  (
    (DEF :initarg :DEF
        :initform  ""
        :accessor DEF
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DSensorNode (X3DChildNode)
  (
    (enabled :initarg :enabled
        :initform  "true"
        :accessor enabled
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MetadataDouble (X3DMetadataObject)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MetadataFloat (X3DMetadataObject)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MetadataInteger (X3DMetadataObject)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MetadataSet (SceneGraphStructureNodeType)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (reference :initarg :reference
        :initform  ""
        :accessor reference
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self MetadataSet) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass MetadataString (X3DMetadataObject)
  (
    (value :initarg :value
        :initform  `()
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass WorldInfo (X3DInfoNode)
  (
    (info :initarg :info
        :initform  `()
        :accessor info
        :documentation "")
    (title :initarg :title
        :initform  ""
        :accessor title
        :documentation "")
  )
  (:documentation ""))

