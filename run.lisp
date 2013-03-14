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
(defmethod run ((self transform))
  (let ((C (sb-cga:translate (center self)))
        (R (apply #'sb-cga:rotate-around (rotation self)))
        (S (sb-cga:scale (scale self)))
        (SR (apply #'sb-cga:rotate-around (scale-orientation self)))
        (Tx (sb-cga:translate (translation self))))
    (let ((-SR (sb-cga:inverse-matrix SR))
          (-C (sb-cga:inverse-matrix C)))
      (sb-cga:matrix* Tx C R SR S -SR -C))))

;; ------------------------------------------------------------------navigation
(defmethod run ((self viewpoint))
  (let ((C (sb-cga:translate (center-of-rotation self)))
        (R (apply #'sb-cga:rotate-around (orientation self)))
        (Tx (sb-cga:translate (position self))))
    (let ((-C (sb-cga:inverse-matrix C)))
      (sb-cga:inverse-matrix (sb-cga:matrix* Tx R))))) ;; this is the LOOKAT configuration

(defmethod get-projection ((self viewpoint) aspect near far)
  (perspective (slot-value self 'field-of-view) aspect near far))

(defmethod get-view ((self viewpoint))
  (run self))

;; -----------------------------------------------------------------geometry-3d
(defmethod run ((self box))
  "create a vertex and index buffers"
  (format t "Box~%")
  (with-slots (size) self
             (let* ((+x (abs (/ (elt size 0) 2)))
                    (+y (abs (/ (elt size 1) 2)))
                    (+z (abs (/ (elt size 2) 2)))
                    (-x (- +x))
                    (-y (- +y))
                    (-z (- +z)))
               (let ((verts (mf-float +x +y +z   ;right-top-front
                                      -x +y +z   ;left-top-front
                                      -x -y +z   ;left-bottom-front
                                      +x -y +z   ;right-bottom-front
                                      +x +y -z   ;right-top-back
                                      -x +y -z   ;left-top-back
                                      -x -y -z   ;left-bottom-back
                                      +x -y -z)) ;righ-bottom-back
                     (indexes (mf-uint 0 1 2 0 2 3    ;front
                                       4 5 6 4 6 7    ;back
                                       0 4 7 0 7 3    ;right
                                       1 5 6 1 6 7    ;left
                                       1 5 4 1 4 0    ;top
                                       2 6 7 2 7 3))) ;bottom
               (list :vertex-array verts :index-array indexes)))))

;; -----------------------------------------------------------------env-effects
(defmethod run ((self background))
  "
TODO: Make an actual background. For now just use the sky color and set he
background
"
  (let ((color (append (slot-value self 'sky-color) '(0))))
    (apply #'gl:clear-color color)
    (gl:clear :color-buffer)))

(defmethod run ((self shape))
  ""
  (format t "Shape~%")
  (with-slots (geometry appearance) self
    (print (run geometry))
    (run appearance)))

(defmethod run ((self appearance))
  ""
  (format t "Appearance~%")
  (with-slots (material) self
    (run material)))


(defmethod run((self material))
  ""
  (format t "Material~%"))
