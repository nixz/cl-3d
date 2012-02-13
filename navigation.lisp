;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; navigation.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                     23. Navigation Component
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
(defclass  x3d-viewpoint-node (x3d-bindable-node)
  ((center-of-rotation :initarg :center-of-rotation
                       :initform (sf-vec3f 0 0 0)
                       :accessor center-of-rotation
                       :type sf-vec3f
                       :allocation :instance
                       :documentation "")
   (description :initarg :description
                :initform ""
                :accessor description
                :type sf-string
                :allocation  :instance
                :documentation "")
   (jump :initarg :jump
         :initform t
         :accessor jump
         :type sf-bool
         :allocation  :instance
         :documentation "")
   (orientation :initarg :orientation
                :initform (sf-rotation 0 0 1 0)
                :accessor orientation
                :type sf-rotation
                :allocation :instance
                :documentation "")
   (position  :initarg :position
              :initform (sf-vec3f 0 0 10)
              :accessor position
              :type sf-vec3f
              :allocation :instance
              :documentation "")
   (retain-user-offsets :initarg :retain-user-offsets
                        :initform nil
                        :accessor retain-user-offsets
                        :type 'sf-bool
                        :allocation :instance
                        :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

23.3.1 X3DViewpointNode

X3DViewpointNode : X3DBindableNode {
  SFBool     [in]     set_bind
  SFVec3f    [in,out] centerOfRotation  0 0 0     (-∞,∞)
  SFString   [in,out] description       ""
  SFBool     [in,out] jump              TRUE
  SFNode     [in,out] metadata          NULL      [X3DMetadataObject]
  SFRotation [in,out] orientation       0 0 1 0   [-1,1],(-∞,∞)
  SFVec3f/d  [in,out] position          0 0 10    (-∞,∞)
  SFBool     [in,out] retainUserOffsets FALSE
  SFTime     [out]    bindTime
  SFBool     [out]    isBound
}

A node of node type X3DViewpointNode defines a specific location in the local
coordinate system from which the user may view the scene. X3DViewpointNode nodes
are bindable children nodes (see 7.2.2 Bindable children nodes) and thus there
exists an X3DViewpointNode stack in the browser in which the top-most
X3DViewpointNode node on the stack is the currently active X3DViewpointNode
node. If a TRUE value is sent to the set_bind field of an X3DViewpointNode node,
it is moved to the top of the X3DViewpointNode node stack and activated. When an
X3DViewpointNode node is at the top of the stack, the user's view is
conceptually re-parented as a child of the X3DViewpointNode node. All subsequent
changes to the X3DViewpointNode node's coordinate system change the user's
view (e.g., changes to any ancestor transformation nodes or to the
X3DViewpointNode node's position or orientation fields). Sending a set_bind
FALSE event removes the X3DViewpointNode node from the stack and produces
isBound FALSE and bindTime events. If the popped X3DViewpointNode node is at the
top of the X3DViewpointNode stack, the user's view is re-parented to the next
entry in the stack. More details on binding stacks can be found in 7.2.2
Bindable children nodes. When an X3DViewpointNode node is moved to the top of
the stack, the existing top of stack X3DViewpointNode node sends an isBound
FALSE event and is pushed down the stack.

An author can automatically move the user's view through the world by binding
the user to either an X3DViewpointNode node and then animating either the
X3DViewpointNode node or the transformations above it. Browsers shall allow the
user view to be navigated relative to the coordinate system defined by the
X3DViewpointNode node (and the transformations above it) even if the
X3DViewpointNode node or its ancestors' transformations are being animated.

The bindTime field sends the time at which the X3DViewpointNode node is bound or
unbound. This can happen:

a. during loading;
b. when a set_bind event is sent to the X3DViewpointNode node;
c. when the browser binds to the X3DViewpointNode node through its user
   interface described below.

The position and orientation fields of the X3DViewpointNode node specify
relative locations in the local coordinate system. Position is relative to the
coordinate system's origin (0,0,0), while orientation specifies a rotation
relative to the default orientation. In the default position and orientation,
the viewer is on the Z-axis looking down the −Z-axis toward the origin with +X
to the right and +Y straight up. X3DViewpointNode nodes are affected by the
transformation hierarchy.

Navigation types (see 23.4.4 NavigationInfo) that require a definition of a down
vector (e.g., terrain following) shall use the negative Y-axis of the coordinate
system of the currently bound X3DViewpointNode node. Likewise, navigation types
that require a definition of an up vector shall use the positive Y-axis of the
coordinate system of the currently bound X3DViewpointNode node. The orientation
field of the X3DViewpointNode node does not affect the definition of the down or
up vectors. This allows the author to separate the viewing direction from the
gravity direction.

The jump field specifies whether the user's view \"jumps\" to the position and
orientation of a bound X3DViewpointNode node or remains unchanged. This jump is
instantaneous and discontinuous in that no collisions are performed and no
ProximitySensor nodes are checked in between the starting and ending jump
points. If the user's position before the jump is inside a ProximitySensor the
exitTime of that sensor shall send the same timestamp as the bind
field. Similarly, if the user's position after the jump is inside a
ProximitySensor the enterTime of that sensor shall send the same timestamp as
the bind field. Regardless of the value of jump at bind time, the relative
viewing transformation between the user's view and the current X3DViewpointNode
node shall be stored with the current X3DViewpointNode node for later use when
un-jumping (i.e., popping the X3DViewpointNode binding stack from an
X3DViewpointNode node with jump TRUE). The following summarizes the bind stack
rules (see 7.2.2 Bindable children nodes) with additional rules regarding
X3DViewpointNode nodes (displayed in boldface type):

d. During read, the first encountered X3DViewpointNode node is bound by pushing
   it to the top of the X3DViewpointNode node stack. If an X3DViewpointNode node
   name is specified in the URL that is being read, this named X3DViewpointNode
   node is considered to be the first encountered X3DViewpointNode node. Nodes
   contained within Inline nodes (see 9.4.2 Inline), within the strings passed
   to the Browser.createX3DFromString() method, or within files passed to the
   Browser.createX3DFromURL() method (see 2.[I19775-2]) are not candidates for
   the first encountered X3DViewpointNode node. The first node within a
   prototype instance is a valid candidate for the first encountered
   X3DViewpointNode node. The first encountered X3DViewpointNode node sends an
   isBound TRUE event.

e. When a set_bind TRUE event is received by an X3DViewpointNode node,

  1. If it is not on the top of the stack: The relative transformation from the
     current top of stack X3DViewpointNode node to the user's view is stored
     with the current top of stack X3DViewpointNode node. The current top of
     stack node sends an isBound FALSE event. The new node is moved to the top
     of the stack and becomes the currently bound X3DViewpointNode node. The new
     X3DViewpointNode node (top of stack) sends an isBound TRUE event. If jump
     is TRUE for the new X3DViewpointNode node, the user's view is
     instantaneously \"jumped\" to match the values in the position and
     orientation fields of the new X3DViewpointNode node.

  2. If the node is already at the top of the stack, this event has no affect.

f. When a set_bind FALSE event is received by an X3DViewpointNode node in the
   stack, it is removed from the stack. If it was on the top of the stack,

  1. it sends an isBound FALSE event,

  2. the next node in the stack becomes the currently bound X3DViewpointNode
     node (i.e., pop) and issues an isBound TRUE event,

  3. if its jump field value is TRUE, the user's view is
     instantaneously \"jumped\" to the position and orientation of the next
     X3DViewpointNode node in the stack with the stored relative transformation
     of this next X3DViewpointNode node applied.

g. If a set_bind FALSE event is received by a node not in the stack, the event
   is ignored and isBound events are not sent.

h. When a node replaces another node at the top of the stack, the isBound TRUE
   and FALSE events from the two nodes are sent simultaneously (i.e., with
   identical timestamps).

i. If a bound node is deleted, it behaves as if it received a set_bind FALSE
   event (see c. above).

The jump field may change after an X3DViewpointNode node is bound. The rules
described above still apply. If jump was TRUE when the X3DViewpointNode node is
bound, but changed to FALSE before the set_bind FALSE is sent, the
X3DViewpointNode node does not un-jump during unbind. If jump was FALSE when the
X3DViewpointNode node is bound, but changed to TRUE before the set_bind FALSE is
sent, the X3DViewpointNode node does perform the un-jump during unbind.

Note that there are two other mechanisms that result in the binding of a new
X3DViewpointNode:

j. An Anchor node's url field specifies a \"#X3DViewpointNodeName\".

k. A script invokes the loadURL() method and the URL argument specifies
   a \"#X3DViewpointNodeName\".

Both of these mechanisms override the jump field value of the specified
X3DViewpointNode node (#X3DViewpointNodeName) and assume that jump is TRUE when
binding to the new X3DViewpointNode. The behaviour of the viewer transition to
the newly bound X3DViewpointNode depends on the currently bound NavigationInfo
node's type field value (see 23.4.4 NavigationInfo).

The fieldOfView field specifies a preferred minimum viewing angle from this
X3DViewpointNode in radians. A small field of view roughly corresponds to a
telephoto lens; a large field of view roughly corresponds to a wide-angle
lens. The field of view shall be greater than zero and smaller than π. The value
of fieldOfView represents the minimum viewing angle in any direction axis
perpendicular to the view. For example, a browser with a rectangular viewing
projection shall have the following relationship:

   display width    tan(FOVhorizontal/2)
   -------------- = -------------------
   display height   tan(FOVvertical/2)

where the smaller of display width or display height determines which angle
equals the fieldOfView (the larger angle is computed using the relationship
described above). The larger angle shall not exceed π and may force the smaller
angle to be less than fieldOfView in order to sustain the aspect ratio.

The description field specifies a textual description of the X3DViewpointNode
node. This may be used by browser-specific user interfaces. If an
X3DViewpointNode's description field is empty it is recommended that the browser
not present this X3DViewpointNode in its browser-specific user interface.

The centerOfRotation field specifies a center about which to rotate the user's
eyepoint when in EXAMINE mode. If the browser does not provide the ability to
spin around the object in EXAMINE mode, or LOOKAT is not in the list of allowed
navigation modes, this field shall be ignored. For additional information, see
23.4.4 NavigationInfo and 22.4.1 ProximitySensor.

The URL syntax \".../scene.wrl#X3DViewpointNodeName\" specifies the user's initial
view when loading \"scene.wrl\" to be the first X3DViewpointNode node in the X3D
file that appears as DEF X3DViewpointNodeName X3DViewpointNode {...}. This
overrides the first X3DViewpointNode node in the X3D file as the initial user
view, and a set_bind TRUE message is sent to the X3DViewpointNode node
named \"X3DViewpointNodeName\". If the X3DViewpointNode node
named \"X3DViewpointNodeName\" is not found, the browser shall use the first
X3DViewpointNode node in the X3D file (i.e., the normal default behaviour). The
URL syntax \"#X3DViewpointNodeName\" (i.e., no file name) specifies an
X3DViewpointNode within the existing X3D file. If this URL is loaded (e.g.,
Anchor node's url field or loadURL() method is invoked by a Script node), the
X3DViewpointNode node named \"X3DViewpointNodeName\" is bound (a set_bind TRUE
event is sent to this X3DViewpointNode node).

The retainUserOffsets field indicates whether a viewpoint needs to retain (TRUE)
or reset to zero (FALSE) any prior user navigation offsets from defined
viewpoint position, orientation. When an node of type X3DViewpointNode is bound,
user navigation offsets are reinitialized if the associated retainUserOffsets is
TRUE.
"))


(defclass  viewpoint (x3d-viewpoint-node)
  ((field-of-view  :initarg :field-of-view
         :initform (radian (/ pi 4))
         :accessor field-of-view
         :type sf-float
         :allocation :instance
         :documentation ""))
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

23.4.6 Viewpoint

Viewpoint : X3DViewpointNode {
  SFBool     [in]     set_bind
  SFVec3f    [in,out] centerOfRotation  0 0 0   (-∞,∞)
  SFString   [in,out] description       ""
  SFFloat    [in,out] fieldOfView       π/4     (0,π)
  SFBool     [in,out] jump              TRUE
  SFNode     [in,out] metadata          NULL    [X3DMetadataObject]
  SFRotation [in,out] orientation       0 0 1 0 [-1,1],(-∞,∞)
  SFVec3f    [in,out] position          0 0 10  (-∞,∞)
  SFBool     [in,out] retainUserOffsets FALSE
  SFTime     [out]    bindTime
  SFBool     [out]    isBound
}

The Viewpoint node defines a viewpoint that provides a perspective view of the
scene. A perspective view is one in which all projectors coalesce at position.

The fieldOfView field specifies a preferred minimum viewing angle from this
viewpoint in radians. A small field of view roughly corresponds to a telephoto
lens; a large field of view roughly corresponds to a wide-angle lens. The field
of view shall be greater than zero and smaller than π. The value of fieldOfView
represents the minimum viewing angle in any direction axis perpendicular to the
view.

A browser with a rectangular viewing projection has the following relationship:

display width    tan(FOVhorizontal/2)
-------------- = -------------------
display height   tan(FOVvertical/2)

where the smaller of display width or display height determines which angle
equals the fieldOfView (the larger angle is computed using the relationship
described above). The larger angle shall not exceed π and may force the smaller
angle to be less than fieldOfView in order to sustain the aspect ratio.
"))

