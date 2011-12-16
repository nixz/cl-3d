;; -----------------------------------------------------------------------class
(defclass  x3d-metadata-object ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name
         :type sf-string
         :allocation  :instance
         :documentation "")
   (reference :initarg :reference
              :initform (error ":reference must be specified")
              :accessor reference
              :type
              :allocation :instance
              :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

7.3.4 X3DMetadataObject

X3DMetadataObject {
  SFString [in,out] name      ""
  SFString [in,out] reference ""
}

This abstract interface is the basis for all metadata nodes. The interface is
inherited by all metadata nodes.

The specification of the reference field is optional. If provided, it identifies
the metadata standard or other specification that defines the name field. If the
reference field is not provided or is empty, the meaning of the name field is
considered implicit to the characters in the string.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-node ()
  ((metadata :initarg :metadata
             :initform (error ":metadata must be specified")
             :accessor metadata
             :reader metadata
             :writer metadata
             :type x3d-metadata-object
             :allocation :instance
             :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

7.3.5 X3DNode

X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This abstract node type is the base type for all nodes in the X3D system.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-child-node (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

7.3.2 X3DChildNode

X3DChildNode : X3DNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This abstract node type indicates that the concrete nodes that are instantiated
based on it may be used in children, addChildren, and removeChildren fields.

More details on the children, addChildren, and removeChildren fields can be
found in 10.2.1 Grouping and children node types.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-bindable-node (x3d-child-node)
  ((set-bind :initarg :set-bind
             :initform (error ":set-bind must be specified")
             :writer set-bind
             :type sf-bool
             :allocation :instance
             :documentation "")
   (bind-time :initarg :bind-time
              :initform (error ":bind-time must be specified")
              :reader bind-time
              :type sf-time
              :allocation :instance
              :documentation "")
   (is-bound :initarg :is-bound
             :initform (error ":is-bound must be specified")
             :reader is-bound
             :type sf-bool
             :allocation :instance
             :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

7.3.1 X3DBindableNode

X3DBindableNode : X3DChildNode {
  SFBool [in]     set_bind
  SFNode [in,out] metadata NULL [X3DMetadataObject]
  SFTime [out]    bindTime
  SFBool [out]    isBound
}

X3DBindableNode is the abstract base type for all bindable children nodes,
including Background, TextureBackground, Fog, NavigationInfo and Viewpoint. For
complete discussion of bindable behaviors, see 7.2.2 Bindable children nodes.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-info-node (x3d-child-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

7.3.3 X3DInfoNode

X3DInfoNode : X3DChildNode {
  SFNode [in,out] metadata NULL [X3DMetadataObject]
}

This is the base node type for all nodes that contain only information without
visual semantics.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-sensor-node (x3d-child-node)
  ((enabled :initarg :enabled
            :initform (error ":enabled must be specified")
            :accessor enabled
            :type sf-bool
            :allocation :instance
            :documentation "")
   (is-active :initarg :is-active
              :initform (error ":is-active must be specified")
              :reader is-active
              :type sf-bool
              :allocation :instance
              :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008

7.3.7 X3DSensorNode

X3DSensorNode  : X3DChildNode {
  SFBool [in,out] enabled  TRUE
  SFNode [in,out] metadata NULL [X3DMetadataObject]
  SFBool [out]    isActive
}

This abstract node type is the base type for all sensors.
"))

;; -----------------------------------------------------------------------class
(defclass  x3d-prototype-instance (x3d-node)
  ()
  (:documentation "
ISO/IEC 19775-1:2008

7.3.6 X3DPrototypeInstance

X3DPrototypeInstance : X3DNode {
  SFNode [in,out] metdata NULL [X3DMetadataObject]
}

This abstract node type is the base type for all prototype instances in the X3D
system. Any user-defined nodes declared with PROTO or EXTERNPROTO are
instantiated using this base type. An X3DPrototypeInstance may be place anywhere
in the scene graph where it is legal to place the first node declared within the
prototype instance. For example, if the base type of first node is
X3DAppearanceNode, that prototype may be instantiated anywhere in the scene
graph that allows for an appearance node (EXAMPLE Shape).
"))
