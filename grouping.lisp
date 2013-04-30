;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; grouping.lisp --- Definitions of the GROUPING Component in X3D
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
(defclass X3DBoundedObject ()
  (
    (bboxCenter :initarg :bboxCenter
        :initform  "0 0 0"
        :accessor bboxCenter
        :documentation "")
    (bboxSize :initarg :bboxSize
        :initform  "-1 -1 -1"
        :accessor bboxSize
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass X3DGroupingNode (X3DChildNode X3DBoundedObject)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Group (X3DGroupingNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass StaticGroup (X3DGroupingNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Switch (X3DGroupingNode)
  (
    (whichChoice :initarg :whichChoice
        :initform  "-1"
        :accessor whichChoice
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass Transform (X3DGroupingNode)
  (
    (center :initarg :center
        :initform  "0 0 0"
        :accessor center
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
    (translation :initarg :translation
        :initform  "0 0 0"
        :accessor translation
        :documentation "")
  )
  (:documentation ""))

