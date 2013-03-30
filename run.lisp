;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; run.lisp --- run generic method provides logic for what operations to
;;;;              perform on each node
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

(defgeneric run(self)
  (:documentation "Generic method to evaluate various nodes as they are traversed"))

;; ---------------------------------------------------------------------grouping
;; (defmethod run ((self transform))
;;   (format t "Transform~%")
;;   (let ((C (sb-cga:translate (center self)))
;;         (R (apply #'sb-cga:rotate-around (rotation self)))
;;         (S (sb-cga:scale (scale self)))
;;         (SR (apply #'sb-cga:rotate-around (scale-orientation self)))
;;         (Tx (sb-cga:translate (translation self))))
;;     (let ((-SR (sb-cga:inverse-matrix SR))
;;           (-C (sb-cga:inverse-matrix C)))
;;       (sb-cga:matrix* Tx C R SR S -SR -C))))

;; ------------------------------------------------------------------navigation
(defmethod run ((self viewpoint))
  (format t "Viewpoint~%")
  (with-slots (centerOfRotation orientation position) self
    (let ((centerOfRotation (SFVec3f centerOfRotation))
          (orientation (SFRotation orientation))
          (position (SFVec3f position)))
      (let ((C (sb-cga:translate centerOfRotation))
            (R (apply #'sb-cga:rotate-around orientation))
            (Tx (sb-cga:translate position)))
    (let ((-C (sb-cga:inverse-matrix C)))
      (sb-cga:inverse-matrix (sb-cga:matrix* Tx R))))))) ;; this is the LOOKAT configuration

;; ----------------------------------------------------------------------------
(defmethod get-projection ((self viewpoint) aspect near far)
  (with-slots (fieldOfView) self
    (let ((fieldOfView (degrees (SFFloat fieldOfView))))
      (perspective fieldOfView aspect near far))))

;; ----------------------------------------------------------------------------
(defmethod get-view ((self viewpoint))
  (run self))

;; -----------------------------------------------------------------geometry-3d
;; .........................................................................Box
(defmethod run ((self box))
  "create a vertex and index buffers"
  (format t "Box~%")
  (with-slots (size solid) self
    (let ((size (SFVec3f size))
          (solid (SFBool solid)))
      (let ((x (elt size 0))
            (y (elt size 1))
            (z (elt size 2)))
        (gl:scale x y z)                      ; model transform
        (if solid       
            (glut:solid-cube 1)                   ; shape
            (glut:wire-cube 1))))))

;; ........................................................................Cone
(defmethod run ((self cone))
  "create a vertex and index buffers"
  (format t "Cone~%")
  (with-slots (bottomRadius height side bottom solid) self
    (let ((bottomRadius (SFFloat bottomRadius))
          (height (SFFloat height))
          (side (SFBool side))
          (bottom (SFBool bottom))
          (solid (SFBool solid)))
      (gl:rotate -90 1 0 0)
      (gl:translate 0 0 (- (/ height 2)))
      (if solid       
          (glut:solid-cone bottomRadius height 20 1)                   ; shape
          (glut:wire-cone bottomRadius height 20 1)))))

;; ....................................................................Cylinder
(defmethod run ((self Cylinder))
  "create a vertex and index buffers"
  (format t "Cylinder~%")
  (with-slots (radius height side bottom top solid) self
    (let ((radius (SFFloat radius))
          (height (SFFloat height))
          (side (SFBool side))
          (bottom (SFBool bottom))
          (top (SFBool top))
          (solid (SFBool solid)))
      (gl:rotate -90 1 0 0)
      (gl:translate 0 0 (- (/ height 2)))
      (glut:solid-cylinder radius height 20 1))))


;; (let* ((+x (abs (/ (elt size 0) 2)))
;;        (+y (abs (/ (elt size 1) 2)))
;;        (+z (abs (/ (elt size 2) 2)))
;;        (-x (- +x))
;;        (-y (- +y))
;;        (-z (- +z)))
;;   (let ((verts (mf-float +x +y +z   ;right-top-front
;;                          -x +y +z   ;left-top-front
;;                          -x -y +z   ;left-bottom-front
;;                          +x -y +z   ;right-bottom-front
;;                          +x +y -z   ;right-top-back
;;                          -x +y -z   ;left-top-back
;;                          -x -y -z   ;left-bottom-back
;;                          +x -y -z)) ;righ-bottom-back
;;         (indexes (mf-uint 0 1 2 0 2 3    ;front
;;                           4 5 6 4 6 7    ;back
;;                           0 4 7 0 7 3    ;right
;;                           1 5 6 1 6 7    ;left
;;                           1 5 4 1 4 0    ;top
;;                           2 6 7 2 7 3))) ;bottom
;;   (list :vertex-array verts :index-array indexes)))))

;; -----------------------------------------------------------------env-effects
(defmethod run ((self background))
  "
TODO: Make an actual background. For now just use the sky color and set he
background
"
  (format t "Background~%")
  (with-slots (skyColor) self
    (let ((skyColor (list<-str skyColor)))
      (let ((color (append skyColor '(0))))
        (apply #'gl:clear-color color)
        (gl:clear :color-buffer :depth-buffer)))))

;; -----------------------------------------------------------------------shape
(defmethod run ((self shape))
  ""
  (format t "Shape~%")
  (with-slots (containerField) self
    (dolist (element containerField)
      (run element))))

;; ------------------------------------------------------------------appearance
(defmethod run ((self appearance))
  ""
  (format t "Appearance~%")
  (with-slots (containerField) self
    (dolist (element containerField)
      (run element))))

;; ------------------------------------------------------------------appearance
(defmethod run((self material))
  ""
  (format t "Material~%")
  ;; (gl:light :light0 :specular '(1.0 1.0 1.0 0)) ;white-specular-light
  ;; (gl:light :light0 :ambient '(0.0 0.0 0.0 0)) ;black-ambient-light
  ;; (gl:light :light0 :diffuse '(1.0 1.0 1.0 0))  ;white-diffuse-light
  (with-slots (ambientIntensity diffuseColor specularColor emissiveColor shininess) self
    (let ((ambient-intensity (SFFloat ambientIntensity))
          (diffuse-color (SfColor diffuseColor))
          (specular-color (SFColor specularColor))
          (emissive-color (SFColor emissiveColor))
          (shininess  (SFFloat shininess)))
      (gl:material :front :ambient (map 'cl:vector #'(lambda (x)
                                                       (* ambient-intensity x))
                                        diffuse-color))
      (gl:material :front :diffuse diffuse-color)
      (gl:material :front :specular specular-color)
      (gl:material :front :emission emissive-color)
      (gl:material :front :shininess shininess))))
