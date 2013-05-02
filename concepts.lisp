;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; concepts.lisp --- Definitions of the CONCEPTS Component in X3D
;;;;
;;;; Copyright (c) 2011-2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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
;; ----------------------------------------------------------------------------
(defclass X3DPrototype (SceneGraphStructureNodeType)
  (
    (name
        :initform NIL
        :accessor name
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self X3DPrototype) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass SceneGraphStructureNodeType (xml-serializer)
  (
   (!--s
         :initform nil
         :accessor !--s
         :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass WildcardNodeType (X3DNode)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass component (SceneGraphStructureNodeType)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (level :initarg :level
        :initform NIL
        :accessor level
        :documentation "")
  )
  (:documentation ""))

;; ;; ----------------------------------------------------------------------------
;; (defclass EXPORT (SceneGraphStructureNodeType)
;;   (
;;     (localDEF :initarg :localDEF
;;         :initform  ""
;;         :accessor localDEF
;;         :documentation "")
;;     (AS
;;         :initform NIL
;;         :accessor AS
;;         :documentation "")
;;   )
;;   (:documentation ""))

;; ;; ----------------------------------------------------------------------------
;; (defmethod add-subobject ((self EXPORT) (stuff X3DNode))
;;    (add-object-to-slot self stuff 'containerField))

;; ;; ----------------------------------------------------------------------------
;; (defclass IMPORT (SceneGraphStructureNodeType)
;;   (
;;     (inlineDEF :initarg :inlineDEF
;;         :initform  ""
;;         :accessor inlineDEF
;;         :documentation "")
;;     (importedDEF
;;         :initform NIL
;;         :accessor importedDEF
;;         :documentation "")
;;     (AS :initarg :AS
;;         :initform  ""
;;         :accessor AS
;;         :documentation "")
;;   )
;;   (:documentation ""))

;; ;; ----------------------------------------------------------------------------
;; (defmethod add-subobject ((self IMPORT) (stuff X3DNode))
;;    (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass IS (SceneGraphStructureNodeType)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass connect (SceneGraphStructureNodeType)
  (
    (nodeField
        :initform NIL
        :accessor nodeField
        :documentation "")
    (protoField
        :initform NIL
        :accessor protoField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self connect) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass field (SceneGraphStructureNodeType)
  (
    (name
        :initform NIL
        :accessor name
        :documentation "")
    (accessType :initarg :accessType
        :initform  ""
        :accessor accessType
        :documentation "")
    (type :initarg :type
        :initform  ""
        :accessor type
        :documentation "")
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
    (appinfo :initarg :appinfo
        :initform  ""
        :accessor appinfo
        :documentation "")
    ;; (documentation :initarg :documentation
    ;;     :initform  ""
    ;;     :accessor documentation
    ;;     :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self field) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass fieldValue (SceneGraphStructureNodeType)
  (
    (name
        :initform NIL
        :accessor name
        :documentation "")
    (value :initarg :value
        :initform  ""
        :accessor value
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self fieldValue) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass head (SceneGraphStructureNodeType)
  (
   (metas
         :initform nil
         :accessor metas
         :documentation "")
   (components
         :initform nil
         :accessor components
         :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass meta (SceneGraphStructureNodeType)
  (
    (name :initarg :name
        :initform  ""
        :accessor name
        :documentation "")
    (content :initarg :content
        :initform  ""
        :accessor content
        :documentation "")
    (dir :initarg :dir
        :initform  ""
        :accessor dir
        :documentation "")
    (http-equiv :initarg :http-equiv
        :initform  ""
        :accessor http-equiv
        :documentation "")
    (lang :initarg :lang
        :initform  ""
        :accessor lang
        :documentation "")
    (scheme :initarg :scheme
        :initform  ""
        :accessor scheme
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ExternProtoDeclare (X3DPrototype)
  (
    (url :initarg :url
        :initform  `()
        :accessor url
        :documentation "")
    (appinfo :initarg :appinfo
        :initform  ""
        :accessor appinfo
        :documentation "")
    ;; (documentation :initarg :documentation
    ;;     :initform  ""
    ;;     :accessor documentation
    ;;     :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ProtoDeclare (X3DPrototype)
  (
    (appinfo :initarg :appinfo
        :initform  ""
        :accessor appinfo
        :documentation "")
    ;; (documentation :initarg :documentation
    ;;     :initform  ""
    ;;     :accessor documentation
    ;;     :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ProtoInterface (SceneGraphStructureNodeType)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ProtoBody (SceneGraphStructureNodeType)
  (
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defclass ProtoInstance (X3DPrototype)
  (
    (containerField
        :initform NIL
        :accessor containerField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ProtoInstance) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass ROUTE (SceneGraphStructureNodeType)
  (
    (fromNode :initarg :fromNode
        :initform  ""
        :accessor fromNode
        :documentation "")
    (fromField
        :initform NIL
        :accessor fromField
        :documentation "")
    (toNode :initarg :toNode
        :initform  ""
        :accessor toNode
        :documentation "")
    (toField
        :initform NIL
        :accessor toField
        :documentation "")
  )
  (:documentation ""))

;; ----------------------------------------------------------------------------
(defmethod add-subobject ((self ROUTE) (stuff X3DNode))
   (add-object-to-slot self stuff 'containerField))

;; ----------------------------------------------------------------------------
(defclass X3D (SceneGraphStructureNodeType)
  (
    (version :initarg :version
        :initform  ""
        :accessor version
        :documentation "")
    (profile :initarg :profile
        :initform  ""
        :accessor profile
        :documentation "")
    (xsd
        :initform ""
        :accessor xsd
        :documentation "")
    (noNamespaceSchemaLocation
        :initform ""
        :accessor noNamespaceSchemaLocation
        :documentation "")
    (head
        :initform ""
        :accessor head
        :documentation "")
    (scene
          :initform ""
          :accessor scene
          :documentation "")
  )
  (:documentation ""))

