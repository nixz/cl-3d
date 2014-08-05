;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; text.lisp --- Definitions of the TEXT Component in X3D
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
(defclass X3DFontStyleNode (X3DNode)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DFontStyleNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass FontStyle (X3DFontStyleNode)
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
    (size :initarg :size
        :initform  "1.0"
        :accessor size
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
(defclass Text (X3DGeometryNode)
  (
    (string :initarg :string
        :initform  `()
        :accessor string
        :documentation "")
    (length :initarg :length
        :initform  ""
        :accessor length
        :documentation "")
    (maxExtent :initarg :maxExtent
        :initform  "0.0"
        :accessor maxExtent
        :documentation "")
    (solid :initarg :solid
        :initform  "false"
        :accessor solid
        :documentation "")
  )
  (:documentation ""))

