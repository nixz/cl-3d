;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; shape.lisp ---  Implementation of ISO/IEC 19775-1:2008:
;;;;                 12. Shape Component.
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

;; -----------------------------------------------------------------------class
(defclass  x3d-appearance-child-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

12.3.1 X3DAppearanceChildNode

X3DAppearanceChildNode : X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for the child nodes of the X3DAppearanceNode type.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-appearance-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

12.3.2 X3DAppearanceNode

X3DAppearanceNode : X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all Appearance nodes.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-material-node (x3d-appearance-child-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

12.3.3 X3DMaterialNode

X3DMaterialNode : X3DAppearanceChildNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all Material nodes.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-shape-node (x3d-child-node x3d-bounded-object)
  ((appearance :initarg :appearance
               :initform nil
               :accessor appearance
               :type sf-node
               :allocation :instance
               :documentation "")
   (geometry :initarg :geometry
             :initform nil
             :accessor geometry
             :type sf-node
             :allocation :instance
             :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

12.3.4 X3DShapeNode

X3DShapeNode : X3DChildNode, X3DBoundedObject {
  SFNode  [in,out] appearance NULL     [X3DAppearanceNode]
  SFNode  [in,out] geometry   NULL     [X3DGeometryNode]
  SFNode  [in,out] metadata   NULL     [X3DMetadataObject]
  SFVec3f []       bboxCenter 0 0 0    (-∞,∞)
  SFVec3f []       bboxSize   -1 -1 -1 [0,∞) or −1 −1 −1
}

This is the base node type for all Shape nodes.
"))

;; -----------------------------------------------------------------------class
(defclass  shape (x3d-shape-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

12.4.5 Shape

Shape : X3DShapeNode {
  SFNode  [in,out] appearance NULL     [X3DAppearanceNode]
  SFNode  [in,out] geometry   NULL     [X3DGeometryNode]
  SFNode  [in,out] metadata   NULL     [X3DMetadataObject]
  SFVec3f []       bboxCenter 0 0 0    (-∞,∞)
  SFVec3f []       bboxSize   -1 -1 -1 [0,∞) or −1 −1 −1
}

The Shape node has two fields, appearance and geometry, that are used to create
rendered objects in the world. The appearance field contains an Appearance node
that specifies the visual attributes (e.g., material and texture) to be applied
to the geometry. The geometry field contains a geometry node. The specified
geometry node is rendered with the specified appearance nodes applied. See 12.2
Concepts for more information.

17 Lighting component contains details of the X3D lighting model and the
interaction between Appearance nodes and geometry nodes.

If the geometry field is NULL, the object is not drawn.

The bboxCenter and bboxSize fields specify a bounding box that encloses the
Shape node's geometry. This is a hint that may be used for optimization
purposes. The results are undefined if the specified bounding box is smaller
than the actual bounding box of the geometry at any time. A default bboxSize
value, (-1, -1, -1), implies that the bounding box is not specified and, if
needed, is calculated by the browser. A description of the bboxCenter and
bboxSize fields is contained in 10.2.2 Bounding boxes.
"))


