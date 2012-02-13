;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; geometry-3d.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                      13 Geometry3D Component.
;;;;
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

(defclass  box (x3d-geometry-node)
  ((size :initarg :size
         :initform (sf-vec3f 2 2 2)
         ;; :accessor size
         :reader size-changed
         :writer set-size
         :type 'sf-vec3f
         :allocation :instance
         :documentation "")
   (solid :initarg :solid
         :initform t
         ;; :accessor solid
         :reader solid-changed
         :writer set-solid
         :type 'sf-bool
         :allocation :instance
         :documentation ""))
  (:documentation "
13.3.1 Box

Box : X3DGeometryNode {
  SFNode  [in,out] metadata NULL  [X3DMetadataObject]
  SFVec3f []       size     2 2 2 (0,∞)
  SFBool  []       solid    TRUE
}

The Box node specifies a rectangular parallelepiped box centred at (0, 0, 0) in
the local coordinate system and aligned with the local coordinate axes. By
default, the box measures 2 units in each dimension, from -1 to +1. The size
field specifies the extents of the box along the X-, Y-, and Z-axes respectively
and each component value shall be greater than zero. Figure 13.1 illustrates the
Box node.

[Figure 13.1 — Box node]?

Textures are applied individually to each face of the box. On the front (+Z),
back (-Z), right (+X), and left (-X) faces of the box, when viewed from the
outside with the +Y-axis up, the texture is mapped onto each face with the same
orientation as if the image were displayed normally in 2D. On the top face of
the box (+Y), when viewed from above and looking down the Y-axis toward the
origin with the -Z-axis as the view up direction, the texture is mapped onto the
face with the same orientation as if the image were displayed normally in 2D. On
the bottom face of the box (-Y), when viewed from below looking up the Y-axis
toward the origin with the +Z-axis as the view up direction, the texture is
mapped onto the face with the same orientation as if the image were displayed
normally in 2D. TextureTransform affects the texture coordinates of the Box (see
18.4.8 TextureTransform).

The solid field determines whether the box is visible when viewed from the
inside. 11.2.3 Common geometry fields provides a complete description of the
solid field.
"))

