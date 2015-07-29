;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; main.lisp --- Internal workings of the scenegraph. The main logic is here
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

;; ----------------------------------------------------------------------NEW-ID
;; (defparameter *NEW-ID* 0)

;; (defun new-id()
;;   (let ((store (gensym *NEW-ID*)))
;;     (incf *NEW-ID*)
;;     store))

;; -------------------------------------------------------------------------DEF
;; (defparameter *DEF* (make-hash-table  :test 'eql))

;; (defun def (name)
;;   "Looks up the *DEF* hashmap and returns the value if present else nil"
;;   (gethash name *DEF*))

;; (defun set-def (&key name value)
;;   "Sets the key pair into *DEF* hash-table and return the value"
;;   (progn
;;     (setf (gethash name *DEF*) value)
;;     value))

;; -----------------------------------------------------------------------ROUTE
;; These are mosly like callbacks but are automatically invoked when the values
;; are changed or perhaps these are handles to threads

;; ---------------------------------------------------------------------*SCENE*
(defparameter *SCENE* NIL)

;; --------------------------------------------------------------------------VR
(defclass VR (glut:window xml-serializer)
  ((device
         :initform (make-instance 'Devices)
         :accessor device
         :documentation "The list of input and output devices")
   (scenes
         :initform *SCENE*
         :accessor scenes
         :documentation "The list of all virtual x3d scenes"))
   (:default-initargs :width 500 :height 500 :title "Drawing a simple scene"
                      :mode '(:double :rgb :depth)))

  ;; (:documentation "The VR is analogous to a complex number with a Real
  ;; Part (aka physical device descriptions) and Virtual Part (the scene
  ;; defined in X3d"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self VR) &key)
  "Simply displays the window when the object is initialized"
  (glut:display-window self))

;; ------------------------------------------------------------------------*VR*
(defun main (id)
  ;; (cepl-osx:start)
  (setf *news* id)
  (make-instance 'VR)
)

;; ----------------------------------------------------------------------------
(defmethod glut:display-window :before ((self VR))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)              ; (gl:shade-model :flat)
  ;; (gl:light-model :light-model-local-viewer 1)
  (gl:light-model :light-model-two-side 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test)) ; global stuff

;; ----------------------------------------------------------------------------
(defmethod glut:reshape ((self VR) width height)
  "Whenever the window is changed then this event is triggered"
  (progn
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0
                 0
                 (slot-value self 'glut::width)
                 (slot-value self 'glut::height))
    (setf (slot-value self 'glut::width) width)
    (setf (slot-value self 'glut::height) height)))

;; ----------------------------------------------------------------------------
(defmethod glut:display ((self VR))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (run self)
  (glut:swap-buffers))

;; ----------------------------------------------------------------------------
(defmethod glut:keyboard ((self VR) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

;; ----------------------------------------------------------------------------
(defmethod glut:mouse ((self VR) button state x y)
  (initiate (2d-mouse (device self)) button state x y))

;; ----------------------------------------------------------------------------
(defmethod glut:motion ((self VR) x y)
  (update (2d-mouse (device self)) x y)
  (glut:post-redisplay))

;; -----------------------------------------------------------------------scene
(defclass scene (SceneGraphStructureNodeType)
  (
   (backgrounds
         :initform nil
         :accessor backgrounds
         :documentation "defines the background color")
   (viewpoints
         :initform nil
         :accessor viewpoints
         :documentation "defines the point of view of the scene")
   (navigationInfos
         :initform nil
         :accessor navigationInfos
         :documentation "the list of all NavigationInfo nodes")
   (shapes
         :initform nil
         :accessor shapes
         :documentation "Drawable object")
   (transforms
         :initform nil
         :accessor transforms
         :documentation "List of transformation objects")
   (groups
         :initform nil
         :accessor groups
         :documentation "List of grouping objects")
   (collisions
         :initform nil
         :accessor collisions
         :documentation "List of collision objects")
   (lods
         :initform nil
         :accessor lods
         :documentation "List of LOD objects")
   (switchs
         :initform nil
         :accessor switchs
         :documentation "List of switch objects")
   (inlines
         :initform nil
         :accessor inlines
         :documentation "List of inlines objects")
   (staticgroups
         :initform nil
         :accessor staticgroups
         :documentation "List of staticgroup objects")
   (billboards
         :initform nil
         :accessor billboards
         :documentation "List of billboard objects")
   (externProtoDeclares
         :initform nil
         :accessor externProtoDeclares
         :documentation "List of externProtoDeclare objects")
   (protoDeclares
         :initform nil
         :accessor protoDeclares
         :documentation "")
   (worldinfos
         :initform nil
         :accessor worldinfos
         :documentation "")
   (timesensors
         :initform nil
         :accessor timesensors
         :documentation "")
   (orientationInterpolators
         :initform nil
         :accessor orientationInterpolators
         :documentation "")
   (coordinateInterpolators
         :initform nil
         :accessor coordinateInterpolators
         :documentation "")
   (scalarInterpolators
         :initform nil
         :accessor scalarInterpolators
         :documentation "")
   (integertriggers
         :initform nil
         :accessor integertriggers
         :documentation "")
   (booleantriggers
         :initform nil
         :accessor booleantriggers
         :documentation "")
   (booleanfilters
         :initform nil
         :accessor booleanfilters
         :documentation "")
   (booleantoggles
         :initform nil
         :accessor booleantoggles
         :documentation "")
   (timetriggers
         :initform nil
         :accessor timetriggers
         :documentation "")
   (colorInterpolators
         :initform nil
         :accessor colorInterpolators
         :documentation "")
   (integersequencers
         :initform nil
         :accessor integersequencers
         :documentation "")
   (booleansequencers
         :initform nil
         :accessor booleansequencers
         :documentation "")
   (normalinterpolators
         :initform nil
         :accessor normalinterpolators
         :documentation "")
   (coordinateinterpolator2ds
         :initform nil
         :accessor coordinateinterpolator2ds
         :documentation "")
   (positioninterpolators
         :initform nil
         :accessor positioninterpolators
         :documentation "")
   (positioninterpolator2ds
         :initform nil
         :accessor positioninterpolator2ds
         :documentation "")
   (sounds
         :initform nil
         :accessor sounds
         :documentation "")
   (stringsensors
         :initform nil
         :accessor stringsensors
         :documentation "")
    (keysensors
         :initform nil
         :accessor keysensors
         :documentation "")
    (scripts
         :initform nil
         :accessor scripts
         :documentation "")
    (spotlights
         :initform nil
         :accessor spotlights
         :documentation "")
    (directionallights
         :initform nil
         :accessor directionallights
         :documentation "")
   (pointlights
         :initform nil
         :accessor pointlights
         :documentation "")
   (texturebackgrounds
         :initform nil
         :accessor texturebackgrounds
         :documentation "")
   (fogs
         :initform nil
         :accessor fogs
         :documentation "")
   (visibilitysensors
         :initform nil
         :accessor visibilitysensors
         :documentation "")
   (loadsensors
         :initform nil
         :accessor loadsensors
         :documentation "")
   (proximitysensors
         :initform nil
         :accessor proximitysensors
         :documentation "")
   (anchors
         :initform nil
         :accessor anchors
         :documentation "")
   (metadatabooleans
         :initform nil
         :accessor metadatabooleans
         :documentation "")
   (metadatadouble
         :initform nil
         :accessor metadatadouble
         :documentation "")
   (metadatafloat
         :initform nil
         :accessor metadatafloat
         :documentation "")
   (metadatainteger
         :initform nil
         :accessor metadatainteger
         :documentation "")
   (metadatastring
         :initform nil
         :accessor metadatastring
         :documentation "")
   (metadataset
         :initform nil
         :accessor metadataset
         :documentation "")
   (viewpointgroups
         :initform nil
         :accessor viewpointgroups
         :documentation "")
   )
  (:documentation "X3D scene class"))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after ((self scene) &key)
  (setf *SCENE* self))
