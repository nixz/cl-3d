;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; grouping.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                   10. Grouping Component.
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
(in-package #:cl-3d)

;; -----------------------------------------------------------------------class
(defclass  x3d-bounded-object ()
  ((bbox-center :initarg :bbox-center
                :initform (sf-vec3f 0 0 0)
                :accessor bbox-center
                :type sf-vec3f
                :allocation :instance
                :documentation "")
   (bbox-size :initarg :bbox-size
              :initform (sf-vec3f -1 -1 -1)
              :accessor bbox-size
              :type sf-vec3f
              :allocation :instance
              :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.3.1 X3DBoundedObject

X3DBoundedObject {
  SFVec3f [] bboxCenter 0 0 0    (-∞,∞)
  SFVec3f [] bboxSize   -1 -1 -1 [0,∞) or −1 −1 −1
}

This abstract node type is the basis for all node types that have bounds
specified as part of the definition.

The bboxCenter and bboxSize fields specify a bounding box that encloses the
grouping node's children. This is a hint that may be used for optimization
purposes. The results are undefined if the specified bounding box is smaller
than the actual bounding box of the children at any time. A default bboxSize
value, (-1, -1, -1), implies that the bounding box is not specified and, if
needed, is calculated by the browser. A description of the bboxCenter and
bboxSize fields is contained in 10.2.2 Bounding boxes.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-grouping-node (x3d-child-node x3d-bounded-object)
  ((children :initarg :children
             :initform ()
             :accessor children
             ;:type mf-node
             :allocation :instance
             :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.3.2 X3DGroupingNode

X3DGroupingNode : X3DChildNode, X3DBoundedObject {
  MFNode  [in]     addChildren             [X3DChildNode]
  MFNode  [in]     removeChildren          [X3DChildNode]
  MFNode  [in,out] children       []       [X3DChildNode]
  SFNode  [in,out] metadata       NULL     [X3DMetadataObject]
  SFVec3f []       bboxCenter     0 0 0    (-∞,∞)
  SFVec3f []       bboxSize       -1 -1 -1 [0,∞) or −1 −1 −1
}

This abstract node type indicates that concrete node types derived from it
contain children nodes and is the basis for all aggregation.

More details on the children, addChildren, and removeChildren fields can be
found in 10.2.1 Grouping and children node types.
"))

;; ----------------------------------------------------------------------method
(defmethod add-children((self x3d-grouping-node) add-list)
  "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.2.1 (part)

All grouping nodes have addChildren and removeChildren inputOnly fields. The
addChildren event appends nodes to the children field of a grouping node. Any
nodes passed to the addChildren inputOnly field that are already in the children
list of the grouping node are ignored. For example, if the children field
contains the nodes Q, L and S (in order) and the group receives an addChildren
event containing (in order) nodes A, L, and Z, the result is a children field
containing (in order) nodes Q, L, S, A, and Z.
"
  (dolist (x add-list)
    (unless (find x (children self))
        (setf (children self)
              (append (children self) (list x)))))
  (children self))

;; ----------------------------------------------------------------------method
(defmethod remove-children((self x3d-grouping-node) del-list)
  "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.2.1 (part)

The removeChildren event removes nodes from the children field of the grouping
node . Any nodes in the removeChildren event that are not in the children list
of the grouping node are ignored. For example, if the children field contains
the nodes Q, L, S, A and Z and it receives a removeChildren event containing
nodes A, L, and Z, the result is Q, S.
"
  (dolist (x del-list)
    (setf (children self)
          (remove-if #'(lambda (y)
                   (eql x y))
               (children self))))
  (children self))

;; -----------------------------------------------------------------------class
(defclass  group (x3d-grouping-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.4.1 Group

Group : X3DGroupingNode {
  MFNode  [in]     addChildren             [X3DChildNode]
  MFNode  [in]     removeChildren          [X3DChildNode]
  MFNode  [in,out] children       []       [X3DChildNode]
  SFNode  [in,out] metadata       NULL     [X3DMetadataObject]
  SFVec3f []       bboxCenter     0 0 0    (-∞,∞)
  SFVec3f []       bboxSize       -1 -1 -1 [0,∞) or −1 −1 −1
}

A Group node contains children nodes without introducing a new
transformation. It is equivalent to a Transform node containing an identity
transform.

More details on the children, addChildren, and removeChildren fields can be
found in 10.2.1 Grouping and children node types.

The bboxCenter and bboxSize fields specify a bounding box that encloses the
Group node's children. This is a hint that may be used for optimization
purposes. The results are undefined if the specified bounding box is smaller
than the actual bounding box of the children at any time. A default bboxSize
value, (-1, -1, -1), implies that the bounding box is not specified and, if
needed, is calculated by the browser. A description of the bboxCenter and
bboxSize fields is contained in 10.2.2 Bounding boxes.
"))

;; -----------------------------------------------------------------------class
(defclass  transform  (x3d-grouping-node)
  ((center :initarg :center
           :initform (sf-vec3f 0 0 0)
           :accessor center
           :type sf-vec3f
           :allocation :instance
           :documentation "")
   (rotation :initarg :rotation
             :initform (sf-rotation 0 0 1 0)
             :accessor rotation
             :type sf-rotation
             :allocation :instance
             :documentation "")
   (scale :initarg :scale
          :initform (sf-vec3f 1 1 1)
          :accessor scale
          :type sf-vec3f
          :allocation :instance
          :documentation "")
   (scale-orientation :initarg :scale-orientation
                     :initform (sf-rotation 0 0 1 0)
                     :accessor scale-orientation
                     :type sf-rotation
                     :allocation :instance
                     :documentation "")
   (translation :initarg :translation
               :initform (sf-vec3f 0 0 0)
               :accessor translation
               :type sf-vec3f
               :allocation :instance
               :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

10.4.4 Transform

Transform : X3DGroupingNode {
  MFNode     [in]     addChildren               [X3DChildNode]
  MFNode     [in]     removeChildren            [X3DChildNode]
  SFVec3f    [in,out] center           0 0 0    (-∞,∞)
  MFNode     [in,out] children         []       [X3DChildNode]
  SFNode     [in,out] metadata         NULL     [X3DMetadataObject]
  SFRotation [in,out] rotation         0 0 1 0  [-1,1] or (-∞,∞)
  SFVec3f    [in,out] scale            1 1 1    (-∞, ∞)
  SFRotation [in,out] scaleOrientation 0 0 1 0  [-1,1] or (-∞,∞)
  SFVec3f    [in,out] translation      0 0 0    (-∞,∞)
  SFVec3f    []       bboxCenter       0 0 0    (-∞,∞)
  SFVec3f    []       bboxSize         -1 -1 -1 [0,∞) or −1 −1 −1
}

The Transform node is a grouping node that defines a coordinate system for its
children that is relative to the coordinate systems of its ancestors. See 4.3.5
Transformation hierarchy and 4.3.6 Standard units and coordinate system for a
description of coordinate systems and transformations.

10.2.1 Grouping and children node types, provides a description of the children,
addChildren, and removeChildren fields.

The bboxCenter and bboxSize fields specify a bounding box that encloses the
children of the Transform node. This is a hint that may be used for optimization
purposes. The results are undefined if the specified bounding box is smaller
than the actual bounding box of the children at any time. A default bboxSize
value, (-1, -1, -1), implies that the bounding box is not specified and, if
needed, shall be calculated by the browser. The bounding box shall be large
enough at all times to enclose the union of the group's children's bounding
boxes; it shall not include any transformations performed by the group
itself (i.e., the bounding box is defined in the local coordinate system of the
children). The results are undefined if the specified bounding box is smaller
than the true bounding box of the group. A description of the bboxCenter and
bboxSize fields is provided in 10.2.2 Bounding boxes.

The translation, rotation, scale, scaleOrientation and center fields define a
geometric 3D transformation consisting of (in order):

a. a (possibly) non-uniform scale about an arbitrary point;
b. a rotation about an arbitrary point and axis;
c. a translation.

The center field specifies a translation offset from the origin of the local
coordinate system (0,0,0). The rotation field specifies a rotation of the
coordinate system. The scale field specifies a non-uniform scale of the
coordinate system. Scale values may have any value: positive,
negative (indicating a reflection), or zero. A value of zero indicates that any
child geometry shall not be displayed. The scaleOrientation specifies a rotation
of the coordinate system before the scale (to specify scales in arbitrary
orientations). The scaleOrientation applies only to the scale operation. The
translation field specifies a translation to the coordinate system.

Given a 3-dimensional point P and Transform node, P is transformed into point P'
in its parent's coordinate system by a series of intermediate
transformations. In matrix transformation notation, where C (center),
SR (scaleOrientation), T (translation), R (rotation), and S (scale) are the
equivalent transformation matrices,

  P' = T * C * R * SR * S * -SR * -C * P
The following Transform node:

Transform {
  center           C
  rotation         R
  scale            S
  scaleOrientation SR
  translation      T
  children         [
    # Point P (or children holding other geometry)
  ]
}
is equivalent to the nested sequence of:

Transform {
  translation T
  children Transform {
    translation C
    children Transform {
      rotation R
      children Transform {
        rotation SR
        children Transform {
          scale S
          children Transform {
            rotation -SR
            children Transform {
              translation -C
              children [
                # Point P (or children holding other geometry)
              ]
}}}}}}}
"))
