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
(in-package #:cl-3d)

;; -----------------------------------------------------------------------class
(defclass  x3d-appearance-child-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

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
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

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
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

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
               :reader appearance-changed
               :writer set-appearance
               :type sf-node
               :allocation :instance
               :documentation "")
   (geometry :initarg :geometry
             :initform nil
             :reader geometry-changed
             :writer set-geometry
             :type sf-node
             :allocation :instance
             :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

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
(defclass  appearance (x3d-appearance-node)
  ((fill-properties :initarg :fill-properties
                    :initform nil
                    :reader fill-properties-changed
                    :writer set-fill-properties
                    :type sf-node
                    :allocation :instance
                    :documentation "")
   (line-properties :initarg :line-properties
                    :initform nil
                    :reader line-properties-changed
                    :writer set-line-properties
                    :type sf-node
                    :allocation :instance
                    :documentation "")
   (material :initarg :material
             :initform nil
             :reader material-changed
             :writer set-material
             :type sf-node
             :allocation :instance
             :documentation "")
   (shaders :initarg :shaders
            :initform ()
            :reader shaders-changed
            :writer set-shaders
            :type mf-node
            :allocation :instance
            :documentation "")
   (texture :initarg :texture
            :initform nil
            :reader texture-changed
            :writer set-texture
            :type sf-node
            :allocation :instance
            :documentation "")
   (texture-transform :initarg :texture-transform
                      :initform nil
                      :reader texture-transform-changed
                      :writer set-texture-transform
                      :type sf-node
                      :allocation :instance
                      :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

12.4.1 Appearance

Appearance : X3DAppearanceNode {
  SFNode [in,out] fillProperties   NULL [FillProperties]
  SFNode [in,out] lineProperties   NULL [LineProperties]
  SFNode [in,out] material         NULL [X3DMaterialNode]
  SFNode [in,out] metadata         NULL [X3DMetadataObject]
  MFNode [in,out] shaders          []   [X3DShaderNode]
  SFNode [in,out] texture          NULL [X3DTextureNode]
  SFNode [in,out] textureTransform NULL [X3DTextureTransformNode]
}

The Appearance node specifies the visual properties of geometry. The value for
each of the fields in this node may be NULL. However, if the field is non-NULL,
it shall contain one node of the appropriate type.

The material field, if specified, shall contain a Material node. If the material
field is NULL or unspecified, lighting is off (all lights are ignored during
rendering of the object that references this Appearance) and the unlit object
colour is (1, 1, 1). Details of the X3D lighting model are in 17 Lighting
component.

The texture field, if specified, shall contain one of the various types of
texture nodes (see 18 Texturing component). If the texture node is NULL or the
texture field is unspecified, the object that references this Appearance is not
textured.

The textureTransform field, if specified, shall contain a TextureTransform node
as defined in 18.4.8 TextureTransform. If the textureTransform is NULL or
unspecified, the textureTransform field has no effect.

The lineProperties field, if specified, shall contain a LineProperties node as
specified in 12.4.3 LineProperties. If lineProperties is NULL or unspecified,
the lineProperties field has no effect.

The fillProperties field, if specified, shall contain a FillProperties node as
specified in 12.4.2 FillProperties. If fillProperties is NULL or unspecified,
the fillProperties field has no effect.

The shaders field contains a listing, in order of preference, of nodes that
describe programmable shaders that replace the fixed rendering requirements of
this part of ISO/IEC 19775 with user-provided functionality. If the field is not
empty, one shader node is selected and the fixed rendering requirements defined
by this specification are ignored. The field shall contain one of the various
types of shader nodes as specified in 31 Programmable shaders component.
"))

;; -----------------------------------------------------------------------class
(defclass  material (x3d-material-node)
  ((ambient-intensity :initarg :ambient-intensity
         :initform 0.2
         :accessor ambient-intensity
         :type sf-float
         :allocation :instance
         :documentation "")
   (diffuse-color :initarg :diffuse-color
         :initform '(0.8 0.8 0.8)
         :accessor diffuse-color
         :type sf-color
         :allocation :instance
         :documentation "")
   (emissive-color :initarg :emissive-color
         :initform '(0 0 0)
         :accessor emissive-color
         :type sf-color
         :allocation :instance
         :documentation "")
   (shininess :initarg :shininess
         :initform 0.2
         :accessor shininess
         :type sf-float
         :allocation :instance
         :documentation "")
   (specular-color  :initarg :specular-color
         :initform '(0 0 0)
         :accessor specular-color
         :type sf-color
         :allocation :instance
         :documentation "")
   (transparency :initarg :transparency
         :initform 0
         :accessor transparency
         :type sf-float
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

12.4.4 Material

Material : X3DMaterialNode {
  SFFloat [in,out] ambientIntensity 0.2         [0,1]
  SFColor [in,out] diffuseColor     0.8 0.8 0.8 [0,1]
  SFColor [in,out] emissiveColor    0 0 0       [0,1]
  SFNode  [in,out] metadata         NULL        [X3DMetadataObject]
  SFFloat [in,out] shininess        0.2         [0,1]
  SFColor [in,out] specularColor    0 0 0       [0,1]
  SFFloat [in,out] transparency     0           [0,1]
}

The Material node specifies surface material properties for associated geometry
nodes and is used by the X3D lighting equations during rendering. 17 Lighting
component contains a detailed description of the X3D lighting model equations.

All of the fields in the Material node range from 0.0 to 1.0.

The fields in the Material node determine how light reflects off an object to
create colour:

a. The ambientIntensity field specifies how much ambient light from light
   sources this surface shall reflect. Ambient light is omnidirectional and
   depends only on the number of light sources, not their positions with respect
   to the surface. Ambient colour is calculated as ambientIntensity ×
   diffuseColor.

b. The diffuseColor field reflects all X3D light sources depending on the angle
   of the surface with respect to the light source. The more directly the
   surface faces the light, the more diffuse light reflects.

c. The emissiveColor field models \"glowing\" objects. This can be useful for
   displaying pre-lit models (where the light energy of the room is computed
   explicitly), or for displaying scientific data.

d. The specularColor and shininess fields determine the specular
   highlights (e.g., the shiny spots on an apple). When the angle from the light
   to the surface is close to the angle from the surface to the viewer, the
   specularColor is added to the diffuse and ambient colour calculations. Lower
   shininess values produce soft glows, while higher values result in sharper,
   smaller highlights.

e. The transparency field specifies how \"clear\" an object is, with 1.0 being
   completely transparent, and 0.0 completely opaque.
"))


;; -----------------------------------------------------------------------class
(defclass  shape (x3d-shape-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

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
