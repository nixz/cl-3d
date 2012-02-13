;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; env-effects.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                      24. Environmental Effects Component
;;;; Copyright (c) 2011, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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
(in-package #:x3d)

(defclass  x3d-background-node (x3d-bindable-node)
  ((ground-angle :initarg :ground-angle
         :initform ()
         :accessor ground-angle
         :type mf-float
         :allocation :instance
         :documentation "")
   (ground-color :initarg :ground-color
         :initform ()
         :accessor ground-color
         :type mf-color
         :allocation :instance
         :documentation "")
   (sky-angle :initarg :sky-angle
         :initform ()
         :accessor sky-angle
         :type mf-floatp
         :allocation :instance
         :documentation "")
   (sky-color :initarg :sky-color
         :initform ()
         :accessor sky-color
         :type mf-color
         :allocation :instance
         :documentation "")
   (transparency :initarg :transparency
         :initform (error ":transparency must be specified")
         :accessor transparency
         :type sf-float
         :allocation :instance
         :documentation ""))
  (:documentation
"
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

24.3.1 X3DBackgroundNode

X3DBackgroundNode : X3DBindableNode {
  SFBool  [in]     set_bind
  MFFloat [in,out] groundAngle   []      [0,π/2]
  MFColor [in,out] groundColor   []      [0,1]
  SFNode  [in,out] metadata      NULL    [X3DMetadataObject]
  MFFloat [in,out] skyAngle      []      [0,π]
  MFColor [in,out] skyColor      0 0 0   [0,1]
  SFFloat [in,out] transparency  0       [0,1]
  SFTime  [out]    bindTime
  SFBool  [out]    isBound
}

X3DBackgroundNode is the abstract type from which all backgrounds
inherit. X3DBackgroundNode is a bindable node that, when bound, defines the
panoramic background for the scene. For complete information on backgrounds, see
24.2.1 Backgrounds.
"))

(defclass  background (x3d-background-node)
  ((back-url :initarg :back-url
         :initform ()
         :accessor back-url
         :type mf-string
         :allocation :instance
         :documentation "")
   (bottom-url :initarg :bottom-url
         :initform ()
         :accessor bottom-url
         :type mf-string
         :allocation :instance
         :documentation "")
   (front-url :initarg :front-url
         :initform ()
         :accessor front-url
         :type mf-string
         :allocation :instance
         :documentation "")
   (left-url :initarg :left-url
         :initform ()
         :accessor left-url
         :type mf-string
         :allocation :instance
         :documentation "")
   (right-url :initarg :right-url
         :initform ()
         :accessor right-url
         :type mf-string
         :allocation :instance
         :documentation "")
   (top-url :initarg :top-url
         :initform ()
         :accessor top-url
         :type mf-string
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

24.4.1 Background

Background : X3DBackgroundNode {
  SFBool   [in]     set_bind
  MFFloat  [in,out] groundAngle  []    [0,π/2]
  MFColor  [in,out] groundColor  []    [0,1]
  MFString [in,out] backUrl      []    [URI]
  MFString [in,out] bottomUrl    []    [URI]
  MFString [in,out] frontUrl     []    [URI]
  MFString [in,out] leftUrl      []    [URI]
  SFNode   [in,out] metadata     NULL  [X3DMetadataObject]
  MFString [in,out] rightUrl     []    [URI]
  MFString [in,out] topUrl       []    [URI]
  MFFloat  [in,out] skyAngle     []    [0,π]
  MFColor  [in,out] skyColor     0 0 0 [0,1]
  SFFloat  [in,out] transparency 0     [0,1]
  SFTime   [out]    bindTime
  SFBool   [out]    isBound
}

A background node that uses six static images to compose the backdrop. The
common fields of the Background node are described in 24.2 Concepts. For the
backUrl, bottomUrl, frontUrl, leftUrl, rightUrl, topUrl fields, browsers shall
support the JPEG (see 2.[JPEG]) and PNG (see ISO/IEC 15948) image file formats,
and in addition, may support any other image format (EXAMPLE CGM) that can be
rendered into a 2D image. Support for the GIF (see [GIF]) format is
recommended (including transparency) . More detail on the url fields can be
found in 9.2.1 URLs.
"))
