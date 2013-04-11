;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; util.lisp --- Some utility functions
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



(defun radians (x) (* x (/ pi 180)))
(defun degrees (x) (* x (/ 180 pi)))

;; ----------------------------------------------------------------------------
(defun frustum (xmin xmax ymin ymax znear zfar)
  (let ((a (/ (* 2 znear)
              (- xmax xmin)))
        (b (/ (* 2 znear)
              (- ymax ymin)))
        (c (/ (+ xmin xmax)
              (- xmax xmin)))
        (d (/ (+ ymin ymax)
              (- ymax ymin)))
        (e (/ (- (+ znear zfar))
              (- zfar znear)))
        (f (/ (* -2 znear zfar)
              (- zfar znear))))
    (sb-cga:matrix   a  0.0     c  0.0
                   0.0    b     d  0.0
                   0.0  0.0     e    f
                   0.0  0.0  -1.0  0.0)))

;; ----------------------------------------------------------------------------
(defun perspective (fov aspect znear zfar)
  (let* ((ymax (coerce (* znear
                          (tan (deg->rad (/ fov 2))))
                       'single-float))
         (ymin (- ymax))
         (xmin (* ymin aspect))
         (xmax (* ymax aspect)))
    (frustum xmin xmax
             ymin ymax
             znear zfar)))

;; ----------------------------------------------------------------------------
(defmethod sf-color (x y z)
  (let ((a (coerce x 'single-float))
        (b (coerce y 'single-float))
        (c (coerce z 'single-float)))
    (vector a b c 1.0)))

;; (defmethod MFColor (x y z)
;;   (let ((a (coerce x 'single-float))
;;         (b (coerce y 'single-float))
;;         (c (coerce z 'single-float)))
;;     (cl:vector a b c 1.0)))

;; ----------------------------------------------------------------------------
(defmethod IntensityType(str)
  (coerce (read-from-string str) 'single-float))

;; ----------------------------------------------------------------------------
(defmethod MFColor(str)
  (apply #'cl:vector 
         (mapcar (lambda (x)
                   (coerce x 'single-float))
                 (with-input-from-string (in str)
                   (loop for x = (read in nil nil) 
                      while x 
                      collect x)))))

;; ----------------------------------------------------------------------------
(defmethod SFColor(str)
  (let ((float-list (append (list<-str str) '(1.0))))
    (apply #'cl:vector float-list)))

;; ----------------------------------------------------------------------------
(defmethod BoundingBoxSizeType(str)
  (apply #'cl:vector
         (mapcar (lambda (x)
                   (coerce x 'single-float))
                 (with-input-from-string (in str)
                   (loop for x = (read in nil nil) 
                      while x 
                      collect x)))))

;; ----------------------------------------------------------------------------
(defmethod SFBool(str)
  (if (string= "true" str)
      t
      nil))

;; ----------------------------------------------------------------------------
(defmethod SFFloat((str cl:string))
  (coerce (with-input-from-string(in str)
            (read in nil nil))
          'single-float))

;; ----------------------------------------------------------------------------
(defun list<-str (str)
  (mapcar (lambda (x)
            (coerce x 'single-float))
          (with-input-from-string (in str)
            (loop for x = (read in nil nil) 
               while x 
               collect x))))

;; ----------------------------------------------------------------------------
(defmethod val<-str (str)
  (coerce (with-input-from-string(in str)
            (read in nil nil))
          'single-float))

;; ----------------------------------------------------------------------------
(defmethod SFRotation(str)
  (let ((float-list (list<-str str)))
    (list (sb-cga:vec (first float-list)
                      (second float-list)
                      (third float-list))
          (fourth float-list))))

(defmethod SFRotation((val cl:list))
  val)


;; ----------------------------------------------------------------------------
(defgeneric SFVec3f (x &optional y z))

(defmethod SFVec3F(x &optional y z)
  (SFVec3F (list x y z)))

(defmethod SFVec3f ((str cl:string) &optional (y NIL) (z NIL))
  (let ((float-list (list<-str str)))
  (apply #'sb-cga:vec float-list)))

(defmethod SFVec3F((val cl:list) &optional (y NIL) (z NIL))
  (apply #'sb-cga:vec (mapcar (lambda (x)
                                (coerce x 'single-float))
                              val)))
(defmethod SFVec3F((val cl:vector) &optional (y NIL) (z NIL))
  val)

;; ----------------------------------------------------------------------------
(defun transform (translation center rotation scale scaleOrientation)
  (let ((C (sb-cga:translate center))
        (R (apply #'sb-cga:rotate-around rotation))
        (S (sb-cga:scale scale))
        (SR (apply #'sb-cga:rotate-around scaleOrientation))
        (Tx (sb-cga:translate translation)))
    (let ((-SR (sb-cga:inverse-matrix SR))
          (-C (sb-cga:inverse-matrix C)))
      (sb-cga:matrix* Tx C R SR S -SR -C))))


  ;;       ;; (setf orientation (list rot-vector (coerce (radians rot-value) 'single-float))))
  ;;       ;; (setf view-rotx (+ (car origrot) (- y (cadr origclick))))
  ;;       ;; (setf view-roty (+ (cadr origrot) (- x (car origclick))))
  ;;       (glut:post-redisplay)))))

