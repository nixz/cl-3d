;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; programmable-shaders.lisp --- Definitions of the PROGRAMMABLE-SHADERS Component in X3D
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
(defclass X3DProgrammableShaderObject ()
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DShaderNode (X3DAppearanceChildNode)
  (
    (language :initarg :language
        :initform  ""
        :accessor language
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DShaderNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DVertexAttributeNode (X3DGeometricPropertyNode)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DVertexAttributeNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ComposedShader (X3DProgrammableShaderObject)
  (
    (language :initarg :language
        :initform  ""
        :accessor language
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ComposedShader) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass FloatVertexAttribute (X3DVertexAttributeNode)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
    (numComponents :initarg :numComponents
        :initform  "4"
        :accessor numComponents
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Matrix3VertexAttribute (X3DVertexAttributeNode)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Matrix4VertexAttribute (X3DVertexAttributeNode)
  (
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PackagedShader (X3DProgrammableShaderObject)
  (
    (language :initarg :language
        :initform  ""
        :accessor language
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self PackagedShader) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ProgramShader (X3DShaderNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ShaderPart (X3DNodeMixedContent)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (type :initarg :type
        :initform  ""
        :accessor type
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ShaderPart) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ShaderProgram (X3DProgrammableShaderObject)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (type :initarg :type
        :initform  ""
        :accessor type
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ShaderProgram) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

