;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; programmable-shaders.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                              31. Programmable Shaders Component
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

;; -----------------------------------------------------------------------class
(defclass  x3d-programmable-shader-object ()
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

31.3.1 X3DProgrammableShaderObject

X3DProgrammableShaderObject {
}

This abstract node type is the base type for all node types that specify
arbitrary fields for interfacing with per-object attribute values.

A concrete X3DProgrammableShaderObject node instance is used to program
behaviour for a shader in a scene. The shader is able to receive and process
events that are sent to it. Each event that can be received shall be declared in
the shader node using the same field syntax as is used in a prototype
definition:

inputOnly type name

The type can be any of the standard X3D fields (as defined in 5 Field type
reference). The name shall be an identifier that is unique for this shader node
and is used to map the value to the shader program's uniform variable of the
same name. If a shader program does not have a matching uniform variable, the
field value is ignored.

OutputOnly fields are not required to generate output events from a
shader. Current hardware shader technology does not support this capability,
though future versions may.

It is recommended that user-defined field or event names defined in shader nodes
follow the naming conventions described in Part 2 of ISO/IEC 19775.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-shader-node (x3d-appearance-child-node)
  ((acivate :initarg :acivate
         :initform nil
         ;; :accessor acivate
         :writer set-acivate
         :allocation :instance
         :documentation "")
   (is-selected :initarg :is-selected
         :initform nil
         ;; :accessor is-selected
         :reader is-selected-changed
         ;; :writer set-is-selected
         ;; :type
         :allocation :instance
         :documentation "")
   (is-valid :initarg :is-valid
         :initform (error ":is-valid must be specified")
         ;; :accessor is-valid
         :reader is-valid-changed
         ;; :writer set-is-valid
         ;; :type
         :allocation :instance
         :documentation "")
   (language :initarg :language
         :initform ""
         ;; :accessor language
         :reader language-changed
         :writer set-language
         ;; :type
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

31.3.2 X3DShaderNode

X3DShaderNode : X3DAppearanceChildNode {
  SFBool   [in]     activate
  SFNode   [in,out] metadata   NULL [X3DMetadataObject]
  SFBool   [out]    isSelected
  SFBool   [out]    isValid
  SFString []       language   \"\"   [\"CG\"|\"GLSL\"|\"HLSL\"|...]
}

This abstract node type is the base type for all node types that specify a
programmable shader.

The isSelected output field is used to indicate that this shader instance is the
one selected for use by the browser. A TRUE value indicates that this instance
is in use. A FALSE value indicates that this instance is not in use. The rules
for when a browser decides to select a particular node instance are described in
31.2.2.3 Selecting an appropriate shader.

The isValid field is used to indicate whether the current shader objects can be
run as a shader program.

EXAMPLE There are no syntax errors and the hardware can support all the required
features.

The definition of this field may also add additional semantics on a per-language
basis.

The language field is used to indicate to the browser which shading language is
used for the source file(s). This field may be used as a hint for the browser if
the shading language is not immediately determinable from the source (e.g., a
generic MIME type of text/plain is returned). A browser may use this field for
determining which node instance will be selected and to ignore languages that it
is not capable of supporting. Three basic language types are defined for this
specification and others may be optionally supported by a browser.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-vertex-attribute-node (x3d-geometric-property-node)
  ((name :initarg :name
         :initform ""
         ;; :accessor name
         :reader name-changed
         :writer set-name
         ;; :type
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

31.3.3 X3DVertexAttributeNode

X3DVertexAttributeNode : X3DGeometricPropertyNode {
  SFNode   [in,out] metadata NULL [X3DMetadataObject]
  SFString []       name     \"\"
}

This abstract node type is the base type for all node types that specify
per-vertex attribute information to the shader.

The name field describes a name that is mapped to the shading language-specific
name for describing per-vertex data. The appropriate shader language annex (see
Table 31.2) annex contains language-specific binding information.
"))

;; -----------------------------------------------------------------------class
(defclass  composed-shader (x3d-shader-node x3d-programmable-shader-object)
  ((parts :initarg :parts
         :initform ()
         ;; :accessor parts
         :reader parts-changed
         :writer set-parts
         ;; :type
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

31.4.1 ComposedShader

ComposedShader : X3DShaderNode, X3DProgrammableShaderObject {
  SFBool    [in]     activate
  SFNode    [in,out] metadata   NULL [X3DMetadataObject]
  MFNode    [in,out] parts      []   [ShaderPart]
  SFBool    [out]    isSelected
  SFBool    [out]    isValid
  SFString  []       language   \"\"

  # And any number of:
  fieldType []       fieldName
  fieldType [in]     fieldName
  fieldType [out]    fieldName
  fieldType [in,out] fieldName
}

The ComposedShader node defines a shader where the individual source files are
not individually programmable. All access to the shading capabilities is defined
through a single interface that applies to all parts.

EXAMPLE  OpenGL Shading Language (GLSL)

The isValid field adds an additional semantic indicating whether the current
shader parts can be linked together to form a complete valid shader program.

The activate field forces the shader to activate the contained objects. The
conditions under which a activate may be required are described in I.5 Event
model.
"))


(defclass shader-part (x3d-node x3d-url-object)
  ((type :initarg :type
         :initform "VERTEX"
         ;; :accessor type
         :reader type-changed
         :writer set-type
         ;; :type
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

31.4.7 ShaderPart

ShaderPart : X3DNode, X3DUrlObject {
  SFNode   [in,out] metadata NULL       [X3DMetadataObject]
  MFString [in,out] url      []         [URI]
  SFString []       type     \"VERTEX\"   [\"VERTEX\"|\"FRAGMENT\"]
}

The ShaderPart node defines the source for a single object to be used by a
ComposedShader node. The source is not required to be a complete shader for all
of the vertex/fragment processing.

The type field indicates whether this object shall be compiled as a vertex
shader, fragment shader, or other future-defined shader type.

The shader source is read from the URL specified by the url field. When the url
field contains no values ([]), this object instance is ignored. The url field is
defined in 9.2.1 URLs. Shader source files shall be plain text encoded as
specified for MIME type text/plain and interpreted according to the type field.
"))
