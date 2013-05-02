;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; texturing.lisp --- Definitions of the TEXTURING Component in X3D
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
(defclass X3DTextureNode (X3DAppearanceChildNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DTextureNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DTexture2DNode (X3DTextureNode)
  (
    (repeatS :initarg :repeatS
        :initform  "true"
        :accessor repeatS
        :documentation "")
    (repeatT :initarg :repeatT
        :initform  "true"
        :accessor repeatT
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DTextureCoordinateNode (X3DGeometricPropertyNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DTextureCoordinateNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3DTextureTransformNode (X3DAppearanceChildNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DTextureTransformNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ImageTexture (X3DTexture2DNode)
  (
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TextureProperties (X3DAppearanceChildNode)
  (
    (anisotropicDegree :initarg :anisotropicDegree
        :initform  "0"
        :accessor anisotropicDegree
        :documentation "")
    (borderColor :initarg :borderColor
        :initform  "0 0 0 0"
        :accessor borderColor
        :documentation "")
    (borderWidth :initarg :borderWidth
        :initform  "0"
        :accessor borderWidth
        :documentation "")
    (boundaryModeS :initarg :boundaryModeS
        :initform  "REPEAT"
        :accessor boundaryModeS
        :documentation "")
    (boundaryModeT :initarg :boundaryModeT
        :initform  "REPEAT"
        :accessor boundaryModeT
        :documentation "")
    (boundaryModeR :initarg :boundaryModeR
        :initform  "REPEAT"
        :accessor boundaryModeR
        :documentation "")
    (magnificationFilter :initarg :magnificationFilter
        :initform  "FASTEST"
        :accessor magnificationFilter
        :documentation "")
    (minificationFilter :initarg :minificationFilter
        :initform  "FASTEST"
        :accessor minificationFilter
        :documentation "")
    (textureCompression :initarg :textureCompression
        :initform  "FASTEST"
        :accessor textureCompression
        :documentation "")
    (texturePriority :initarg :texturePriority
        :initform  "0"
        :accessor texturePriority
        :documentation "")
    (generateMipMaps :initarg :generateMipMaps
        :initform  "false"
        :accessor generateMipMaps
        :documentation "")
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self TextureProperties) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass MovieTexture (X3DSoundSourceNode)
  (
    (description :initarg :description
        :initform  ""
        :accessor description
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (repeatS :initarg :repeatS
        :initform  "true"
        :accessor repeatS
        :documentation "")
    (repeatT :initarg :repeatT
        :initform  "true"
        :accessor repeatT
        :documentation "")
    (speed :initarg :speed
        :initform  "1.0"
        :accessor speed
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MultiTexture (X3DTextureNode)
  (
    (alpha :initarg :alpha
        :initform  "1"
        :accessor alpha
        :documentation "")
    (color :initarg :color
        :initform  "1 1 1"
        :accessor color
        :documentation "")
    ;; (function :initarg :function
    ;;     :initform  `()
    ;;     :accessor function
    ;;     :documentation "")
    (mode :initarg :mode
        :initform  `()
        :accessor mode
        :documentation "")
    (source :initarg :source
        :initform  `()
        :accessor source
        :documentation "")
    (transparent :initarg :transparent
        :initform  "false"
        :accessor transparent
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MultiTextureCoordinate (X3DTextureCoordinateNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass MultiTextureTransform (X3DTextureTransformNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass PixelTexture (X3DTexture2DNode)
  (
    (image :initarg :image
        :initform  "0 0 0"
        :accessor image
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TextureCoordinate (X3DTextureCoordinateNode)
  (
    (point :initarg :point
        :initform  ""
        :accessor point
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TextureCoordinateGenerator (X3DTextureCoordinateNode)
  (
    (mode :initarg :mode
        :initform  "SPHERE"
        :accessor mode
        :documentation "")
    (parameter :initarg :parameter
        :initform  ""
        :accessor parameter
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass TextureTransform (X3DTextureTransformNode)
  (
    (center :initarg :center
        :initform  "0 0"
        :accessor center
        :documentation "")
    (rotation :initarg :rotation
        :initform  "0"
        :accessor rotation
        :documentation "")
    (scale :initarg :scale
        :initform  "1 1"
        :accessor scale
        :documentation "")
    (translation :initarg :translation
        :initform  "0 0"
        :accessor translation
        :documentation "")
  )
  (:documentation ""))

