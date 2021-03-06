;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; scripting.lisp --- Definitions of the SCRIPTING Component in X3D
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
(defclass X3DScriptNode (X3DChildNode) ; ? where is this  X3DURLObject)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DScriptNode) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass Script (X3DScriptNode)
  (
    (directOutput :initarg :directOutput
        :initform  "false"
        :accessor directOutput
        :documentation "")
    (mustEvaluate :initarg :mustEvaluate
        :initform  "false"
        :accessor mustEvaluate
        :documentation "")
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (fields
        :initform  nil
        :accessor fields
        :documentation "")
    (![CDATA[S
        :initform nil
        :accessor ![CDATA[S
        :documentation "")
    (![CDATA[ecmascripts
        :initform nil
        :accessor ![CDATA[ecmascripts
        :documentation "")
    (![CDATA[javascripts
        :initform nil
        :accessor ![CDATA[javascripts
        :documentation "")
  )
  (:documentation ""))

