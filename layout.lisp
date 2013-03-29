;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; layout.lisp --- Definitions of the LAYOUT Component in X3D
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
(defclass X3DLayoutNode (X3DChildNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Layout (X3DLayoutNode)
  (
    (align :initarg :align
        :initform  `("CENTER" "CENTER")
        :accessor align
        :documentation "")
    (offset :initarg :offset
        :initform  "0 0"
        :accessor offset
        :documentation "")
    (offsetUnits :initarg :offsetUnits
        :initform  `("WORLD" "WORLD")
        :accessor offsetUnits
        :documentation "")
    (scaleMode :initarg :scaleMode
        :initform  `("NONE" "NONE")
        :accessor scaleMode
        :documentation "")
    (size :initarg :size
        :initform  "1 1"
        :accessor size
        :documentation "")
    (sizeUnits :initarg :sizeUnits
        :initform  `("WORLD" "WORLD")
        :accessor sizeUnits
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass LayoutGroup (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self LayoutGroup) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass LayoutLayer (X3DLayerNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ScreenFontStyle (X3DFontStyleNode)
  (
    (family :initarg :family
        :initform  `("SERIF")
        :accessor family
        :documentation "")
    (horizontal :initarg :horizontal
        :initform  "true"
        :accessor horizontal
        :documentation "")
    (justify :initarg :justify
        :initform  `("BEGIN")
        :accessor justify
        :documentation "")
    (language :initarg :language
        :initform  ""
        :accessor language
        :documentation "")
    (leftToRight :initarg :leftToRight
        :initform  "true"
        :accessor leftToRight
        :documentation "")
    (pointSize :initarg :pointSize
        :initform  "12.0"
        :accessor pointSize
        :documentation "")
    (spacing :initarg :spacing
        :initform  "1.0"
        :accessor spacing
        :documentation "")
    (style :initarg :style
        :initform  "PLAIN"
        :accessor style
        :documentation "")
    (topToBottom :initarg :topToBottom
        :initform  "true"
        :accessor topToBottom
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ScreenGroup (X3DGroupingNode)
  (
  )
  (:documentation ""))

