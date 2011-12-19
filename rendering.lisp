;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; rendering.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                    11. Rendering Component.
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
(defclass  x3d-geometric-property-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

11.3.4 X3DGeometricPropertyNode

X3DGeometricPropertyNode : X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all geometric property node types defined in X3D
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-geometry-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

11.3.5 X3DGeometryNode

X3DGeometryNode : X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all geometry in X3D.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-normal-node (x3d-geometric-property-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

11.3.6 X3DNormalNode

X3DNormalNode : X3DGeometricPropertyNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all normal node types in X3D. All normals are
specified in nodes derived from this abstract node type.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-color-node (x3d-geometric-property-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

11.3.1 X3DColorNode

X3DColorNode : X3DGeometricPropertyNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for color specifications in X3D.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-composed-geometry-node (x3d-geometry-node)
  ((attrib :initarg :attrib
           :initform (error ":attrib must be specified")
           :accessor attrib
           :type mf-node
           :allocation :instance
           :documentation "")
   (color :initarg :color
          :initform (error ":color must be specified")
          :accessor color
          :type sf-node
          :allocation :instance
          :documentation "")
   (coord :initarg :coord
          :initform (error ":coord must be specified")
          :accessor coord
          :type sf-node
          :allocation :instance
          :documentation "")
   (fog-coord :initarg :fog-coord
              :initform (error ":fog-coord must be specified")
              :accessor fog-coord
              :type sf-node
              :allocation :instance
              :documentation "")
   (normal :initarg :normal
           :initform (error ":normal must be specified")
           :accessor normal
           :type sf-node
           :allocation :instance
           :documentation "")
   (tex-coord :initarg :tex-coord
              :initform (error ":tex-coord must be specified")
              :accessor tex-coord
              :type sf-node
              :allocation :instance
              :documentation "")
   (ccw :initarg :ccw
        :initform (error ":ccw must be specified")
        :accessor ccw
        :type sf-bool
        :allocation :instance
        :documentation "")
   (color-per-vertex :initarg :color-per-vertex
                     :initform (error ":color-per-vertex must be specified")
                     :accessor color-per-vertex
                     :reader color-per-vertex
                     :writer color-per-vertex
                     :type sf-bool
                     :allocation :instance
                     :documentation "")
   (normal-per-vertex :initarg :normal-per-vertex
                      :initform (error ":normal-per-vertex must be specified")
                      :accessor normal-per-vertex
                      :reader normal-per-vertex
                      :writer normal-per-vertex
                      :type sf-bool
                      :allocation :instance
                      :documentation "")
   (solid :initarg :solid
          :initform (error ":solid must be specified")
          :accessor solid
          :reader solid
          :writer solid
          :type sf-bool
          :allocation :instance
          :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

11.3.2 X3DComposedGeometryNode

X3DComposedGeometryNode : X3DGeometryNode {
  MFNode [in,out] attrib          []   [X3DVertexAttributeNode]
  SFNode [in,out] color           NULL [X3DColorObject]
  SFNode [in,out] coord           NULL [X3DCoordinateNode]
  SFNode [in,out] fogCoord        []   [FogCoordinate]
  SFNode [in,out] metadata        NULL [X3DMetadataObject]
  SFNode [in,out] normal          NULL [X3DNormalNode]
  SFNode [in,out] texCoord        NULL [X3DTextureCoordinateNode]
  SFBool []       ccw             TRUE
  SFBool []       colorPerVertex  TRUE
  SFBool []       normalPerVertex TRUE
  SFBool []       solid           TRUE
}

This is the base node type for all composed 3D geometry in X3D.

A composed geometry node type defines an abstract type that composes geometry
from a set of nodes that define individual components. Composed geometry may
have color, coordinates, normal and texture coordinates supplied. The rendered
output of the combination of these is dependent on the concrete node
definition. However, in general, the following rules shall be applied for all
nodes:

- If the color field is not NULL, it shall contain an X3DColorNode node whose
  colours are applied to the vertices or faces of the X3DComposedGeometryNode as
  follows:

- If colorPerVertex is FALSE, colours are applied to each face. If
  colorPerVertex is true, colours are applied to each vertex.

- If the color field is NULL, the geometry shall be rendered normally using the
  Material and texture defined in the Appearance node (see 12.2.2 Appearance
  node for details).

- If normalPerVertex is FALSE, colours are applied to each face. If
  normalPerVertex is true, colours are applied to each vertex.

- If the normal field is not NULL, it shall contain a Normal node whose normals
  are applied to the vertices or faces of the X3DComposedGeometryNode in a
  manner exactly equivalent to that described above for applying colours to
  vertices/faces (where normalPerVertex corresponds to colorPerVertex and
  normalIndex corresponds to colorIndex).

- If the normal field is NULL, the browser shall automatically generate normals
  in accordance with the node's definition. If the node does not define a
  behaviour, the default is to generate an averaged normal for all faces that
  share that vertex.

- If the texCoord field is not NULL, it shall contain a TextureCoordinate node.

If the attrib field is not empty it shall contain a list of per-vertex attribute
information for programmable shaders as specified in 32.2.2.4 Per-vertex
attributes.

If the fogCoord field is not empty, it shall contain a list of per-vertex depth
values for calculating fog depth as specified in 24.2.2.5 Fog colour
calculation.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-coordinate-node (x3d-geometric-property-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

11.3.3 X3DCoordinateNode

X3DCoordinateNode : X3DGeometricPropertyNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all coordinate node types in X3D. All coordinates
are specified in nodes derived from this abstract node type.
"))

;; -----------------------------------------------------------------------class
(defclass  clip-plane (x3d-child-node)
  ((enabled :initarg :enabled
            :initform (error ":enabled must be specified")
            :accessor enabled
            :type sf-bool
            :allocation :instance
            :documentation "")
   (plane :initarg :plane
          :initform (error ":plane must be specified")
          :accessor plane
          :type sf-vect4f
          :allocation :instance
          :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

11.4.1 ClipPlane

ClipPlane : X3DChildNode {
  SFBool  [in,out] enabled  TRUE
  SFNode  [in,out] metadata NULL    [X3DMetadataObject]
  SFVec4f [in,out] plane    0 1 0 0 [0,1]
}

The ClipPlane node specifies a single plane equation that will be used to clip
the geometry. The plane field specifies a four-component plane equation that
describes the inside and outside half space. The first three components are a
normalized vector describing the direction of the plane's normal direction.
"))

;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
;; -----------------------------------------------------------------------class
