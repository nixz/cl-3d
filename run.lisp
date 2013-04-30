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

;; --------------------------------------------------------------------navigation
(defmethod navigate ((view Viewpoint) nav-mat)
  ""
  (with-slots (centerOfRotation orientation position) view
    (let ((centerOfRotation (SFVec3f centerOfRotation))
          (orientation (SFRotation orientation))
          (position (SFVec3f position)))
      (let ((C (sb-cga:translate centerOfRotation))
            (R (apply #'sb-cga:rotate-around orientation))
            (Tx (sb-cga:translate position))
            (nTx (extract-translation nav-mat))
            (nR (extract-rotation nav-mat)))
        (let ((-C (sb-cga:inverse-matrix C))
              (-R (sb-cga:inverse-matrix R))
              (-Tx (sb-cga:inverse-matrix Tx))
              (-nTx (sb-cga:inverse-matrix nTx))
              (-nR (sb-cga:inverse-matrix nR)))
          ;; We are moving the world so invert the matrix
          (sb-cga:matrix* (sb-cga:inverse-matrix (sb-cga:matrix* Tx R nTx nR))))))))

          ;; (sb-cga:matrix* (sb-cga:inverse-matrix (sb-cga:matrix* Tx R nTx))
          ;;                 nR))))))

(defmethod run ((self Viewpoint))
  ""
  (with-slots (centerOfRotation orientation position) self
    (let ((centerOfRotation (SFVec3f centerOfRotation))
          (orientation (SFRotation orientation))
          (position (SFVec3f position)))
      (let ((C (sb-cga:translate centerOfRotation))
            (R (apply #'sb-cga:rotate-around orientation))
            (Tx (sb-cga:translate position)))
        (let ((-C (sb-cga:inverse-matrix C))
              (-R (sb-cga:inverse-matrix R))
              (-Tx (sb-cga:inverse-matrix Tx)))
          ;; We are moving the world so invert the matrix
          (sb-cga:matrix* Tx R))))))
  
;; ----------------------------------------------------------------------------
(defun projection (Eye<-Screen Eye<-VWorld width height &key eye)
  "Sets the projection matrix based on the center of the screen"
  (gl:matrix-mode :projection)          ; projection
  (let ((x (sb-cga:mref Eye<-Screen 0 3))
        (y (sb-cga:mref Eye<-Screen 1 3))
        (z (sb-cga:mref Eye<-Screen 2 3)))
    (let ((left (- x (/ width 2)))
          (right (+ x (/ width 2)))
          (bottom (- y (/ height 2)))
          (top (+ y (/ height 2)))
          (near 0.1)
          (far 1000.0))
      (let* ((scale (/ (- near) z))
             (proj (frustum (* scale left)
                            (* scale right)
                            (* scale bottom)
                            (* scale top)
                            near
                            far)))
        (gl:load-matrix (sb-cga:matrix* proj Eye<-VWorld)))))
  (cond ((eq eye :right) (gl:draw-buffer :back-right))
        (t (gl:draw-buffer :back-left))))

;; -----------------------------------------------------------------------scene
(defmethod run((self Scene))
  (with-slots (backgrounds navigationInfos viewpoints shapes transforms) self
    (let ((BACKGROUND (if (first backgrounds)
                          (first backgrounds)
                          (make-instance 'Background)))
          (NAVIGATION (if (first navigationInfos)
                          (first navigationInfos)
                          (make-instance 'NavigationInfo)))
          (VIEWPOINT (if (first viewpoints)
                         (first viewpoints)
                         (make-instance 'Viewpoint))))
      (run BACKGROUND)
      (gl:matrix-mode :modelview)       ; model-view
      (gl:with-pushed-matrix
        (gl:mult-matrix (run Viewpoint))
        (dolist (shape shapes)
          (when shape (run shape)))
        (dolist (tx transforms)
          (when tx (run tx)))))))

;; --------------------------------------------------------------------------VR
(defmethod run ((self VR))
  (when *SCENE*
  (with-slots (screen head 2d-mouse) (device self)
    (let ((projections (compute-projection head screen))
          (is-stereo (is-stereo head)))
      (if is-stereo
          (progn
            (gl:matrix-mode :projection)          ; projection
            (gl:load-matrix (getf projections :left))
            (gl:draw-buffer :back-left)
            (run *SCENE*)
            (gl:matrix-mode :projection)          ; projection
            (gl:load-matrix (getf projections :right))
            (gl:draw-buffer :back-right)
            (run *SCENE*))
          (progn
            (gl:matrix-mode :projection)          ; projection
            (gl:load-matrix (getf projections :center))
            (gl:draw-buffer :back-left)
            (run *SCENE*)))))))

;; ---------------------------------------------------------------------grouping
(defmethod run ((self Transform))
  (format t "Transform~%")
  (with-slots (center rotation scale scaleOrientation translation containerField) self
    (let ((center (SFVec3f center))
          (rotation (SFRotation rotation))
          (scale (SFVec3f scale))
          (scaleOrientation (SFRotation scaleOrientation))
          (translation (SFVec3f translation)))
      (let* ((mat (transform translation center rotation scale scaleOrientation))
             (-mat (sb-cga:inverse-matrix mat)))
        (gl:mult-matrix mat)
        (dolist (child containerField)
          (run child))
        (gl:mult-matrix -mat)))))

;; ...................................................................Viewpoint
(defmethod run ((self Viewpoint))
  (format t "Viewpoint~%")
  (with-slots (centerOfRotation orientation position) self
    (let ((centerOfRotation (SFVec3f centerOfRotation))
          (orientation (SFRotation orientation))
          (position (SFVec3f position)))
      (let ((C (sb-cga:translate centerOfRotation))
            (R (apply #'sb-cga:rotate-around orientation))
            (Tx (sb-cga:translate position)))
        (let ((-C (sb-cga:inverse-matrix C))
              (-R (sb-cga:inverse-matrix R)))
          (sb-cga:inverse-matrix (sb-cga:matrix* Tx C R -C))))))) ;; this is the LOOKAT configuration

;; ----------------------------------------------------------------------------
;; (defmethod projection ((self Viewpoint) aspect near far)
;;   (with-slots (fieldOfView) self
;;     (let ((fieldOfView (degrees (SFFloat fieldOfView))))
;;       (perspective fieldOfView aspect near far))))

;; -----------------------------------------------------------------geometry-3d
;; .........................................................................Box
(defmethod run ((self Box))
  "create a vertex and index buffers"
  (format t "Box~%")
  (with-slots (size solid) self
    (let ((size (SFVec3f size))
          (solid (SFBool solid)))
      (let ((x (elt size 0))
            (y (elt size 1))
            (z (elt size 2)))
        (gl:with-pushed-matrix
          (gl:scale x y z)                      ; model transform
          (if solid
              (glut:solid-cube 1)                   ; shape
              (glut:wire-cube 1)))))))

;; ........................................................................Cone
(defmethod run ((self Cone))
  "create a vertex and index buffers"
  (format t "Cone~%")
  (with-slots (bottomRadius height side bottom solid) self
    (let ((bottomRadius (SFFloat bottomRadius))
          (height (SFFloat height))
          (side (SFBool side))
          (bottom (SFBool bottom))
          (solid (SFBool solid)))
      (gl:with-pushed-matrix
        (gl:rotate -90 1 0 0)
        (gl:translate 0 0 (- (/ height 2)))
        (if solid
            (glut:solid-cone bottomRadius height 20 1)                   ; shape
            (glut:wire-cone bottomRadius height 20 1))))))

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
      (gl:with-pushed-matrix
        (gl:rotate -90 1 0 0)
        (gl:translate 0 0 (- (/ height 2)))
        (glut:solid-cylinder radius height 20 1)))))


;; ......................................................................Sphere
(defmethod run ((self Sphere))
  "TODO: Eventually create a vertex and index buffers"
  (format t "Sphere~%")
  (with-slots (radius solid) self
    (let ((radius (SFFloat radius))
          (solid (SFBool solid)))
      (gl:with-pushed-matrix
        (gl:rotate -90 1 0 0)
        (if solid
            (glut:solid-sphere radius 30 30)
            (glut:wire-sphere radius 30 30))))))

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

;; ------------------------------------------------------------------------Text
(defmethod run ((self Text))
  (format t "Text~%")
  (with-slots (length maxExtent string lineBounds origin textBounds solid) self
    ;; (let ((length (SFFloat (cl:length string))))
    ;;       (maxExtent (SFFloat maxExtent))
    ;;       (string (MFString string))
    ;;       (lineBounds (MFVec2f lineBounds))
    ;; (let ((origin (SFVec3f origin))))
    ;;       (textBounds (SFVec2f textBounds))
    ;;       (solid (SFBool solid))))
    ;;   (let ((x (elt origin 0))
    ;;         (y (elt origin 1))
    ;;         (z (elt origin 2)))
    (gl:with-pushed-matrix
      (gl:scale (/ 1 104.76) (/ 1 104.76) 0.0)
      (gl:translate 0 -119.05 0)      ; Origin is upper left corner
                                      ; alternatively can use
                                      ; (gl:translate (* -26.19 length) -119.05 0)
      (gl:line-width 5)
      (glut:stroke-string +stroke-roman+ string))))

;; -----------------------------------------------------------------env-effects
(defmethod run ((self Background))
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
(defmethod run ((self Shape))
  ""
  (format t "Shape~%")
  (with-slots (containerField) self
    (let ((1st (first containerField))
          (2nd (second containerField)))
      (if (typep 1st (class-of (make-instance 'appearance)))
          (progn (run 1st) (run 2nd))
          (progn (run 2nd) (run 1st))))))

;; ------------------------------------------------------------------Appearance
(defmethod run ((self Appearance))
  ""
  (format t "Appearance~%")
  (with-slots (containerField) self
    (dolist (element containerField)
      (run element))))

;; --------------------------------------------------------------------Material
(defmethod run((self Material))
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
