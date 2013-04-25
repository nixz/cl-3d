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

;; ----------------------------------------------------------------------------
(defun radians (x)
  (declare (single-float x))
  (* x (/ pi 180)))

;; ----------------------------------------------------------------------------
(defun degrees (x)
  (declare (single-float x))
  (* x (/ 180 pi)))

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
    (sb-cga:matrix   a    0.0     c  0.0
                     0.0    b     d  0.0
                     0.0  0.0     e    f
                     0.0  0.0  -1.0  0.0)))

;; ----------------------------------------------------------------------------
(defun perspective (fov aspect znear zfar)
  (let* ((ymax (SFFloat (* znear
                           (tan (radians (SFFloat (/ fov 2)))))))
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

(defmethod SFFloat(val)
  (coerce val 'single-float))

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
(defun SFTranslation+ (a b)
  (sb-cga:vec+ a b))

;; ----------------------------------------------------------------------------
(defun SFTranslation<-SFMatrix4F (mat)
  "Returns the translation component from matrix"
  (let ((m03 (sb-cga:mref mat 0 3))
        (m13 (sb-cga:mref mat 1 3))
        (m23 (sb-cga:mref mat 2 3)))
    (SFVec3F m03 m13 m23)))

;; ----------------------------------------------------------------------------
(defun SFMatrix4F<-SFTranslation(vec)
  "Makes a transformation matrix from translations"
  (sb-cga:translate vec))

;; ----------------------------------------------------------------------------
(defun extract-translation(mat)
  "Returns the translation matrix"
  (let ((m03 (sb-cga:mref mat 0 3))
        (m13 (sb-cga:mref mat 1 3))
        (m23 (sb-cga:mref mat 2 3)))
    (sb-cga:matrix 1.0 0.0 0.0 m03
                   0.0 1.0 0.0 m13
                   0.0 0.0 1.0 m23
                   0.0 0.0 0.0 1.0)))

;; ----------------------------------------------------------------------------
(defun extract-rotation (mat)
  "Return the rotation matrix"
  (let ((m00 (sb-cga:mref mat 0 0))
        (m01 (sb-cga:mref mat 0 1))
        (m02 (sb-cga:mref mat 0 2))
        (m10 (sb-cga:mref mat 1 0))
				(m11 (sb-cga:mref mat 1 1))
				(m12 (sb-cga:mref mat 1 2))
        (m20 (sb-cga:mref mat 2 0))
				(m21 (sb-cga:mref mat 2 1))
				(m22 (sb-cga:mref mat 2 2)))
    (sb-cga:matrix m00 m01 m02 0.0
                   m10 m11 m12 0.0
                   m20 m21 m22 0.0
                   0.0 0.0 0.0 1.0)))

;; ----------------------------------------------------------------------------
(defgeneric SFRotation (x &optional y z a))

(defmethod SFRotation(x &optional y z a)
  (let ((x (coerce x 'single-float))
        (y (coerce y 'single-float))
        (z (coerce z 'single-float))
        (a (coerce a 'single-float)))
    (list (SFVec3f x y z) a)))

(defmethod SFRotation((str cl:string) &optional (y NIL) (z NIL) (a NIL))
  "Usage (SFRotation \"x y z angle\")"
  (let ((float-list (list<-str str)))
    (list (sb-cga:vec (first float-list)
                      (second float-list)
                      (third float-list))
          (fourth float-list))))

(defmethod SFRotation((val cl:list) &optional (y NIL) (z NIL) (a NIL))
  "Pass throught a list"
  (coerce (second val) 'single-float)
  val)

(defmethod SFRotation ((vec cl:vector) &optional y (z NIL) (a NIL))
  (list vec (coerce y 'single-float)))

;; ----------------------------------------------------------------------------
(defmethod SFMatrix4F<-SFRotation (rot)
  (apply #'sb-cga:rotate-around (SFRotation rot)))

;; ----------------------------------------------------------------------------
(defmethod SFRotation<-SFMatrix4F (mat)
  "Takes a matrix and gives a SFRotation

void SFMatrix::getSFRotation(SFRotation *rotation)
{
	float x, y, z, w;
	float m[4][4];
	getValue(m);

	float w2 = 1.0f / 4.0f * (1.0f + m[0][0] + m[1][1] + m[2][2]);

	if(0.0f < w2)
	{
		w = (float)sqrt(w2);
		x = (m[1][2] - m[2][1]) / (4.0f * w);
		y = (m[2][0] - m[0][2]) / (4.0f * w);
		z = (m[0][1] - m[1][0]) / (4.0f * w);
	}
	else
	{
		w = 0.0f;
		float x2 = 0.0f;
		float m1122 = m[1][1] + m[2][2];
		if(m1122 != 0.0f)
			x2 = -1.0f / (2.0f * m1122);
		if(0.0f < x2)
		{
			x = (float)sqrt(x2);
			y = m[0][1] / (2.0f * x);
			z = m[0][2] / (2.0f * x);
		}
		else
		{
			x = 0.0f;
			float y2 = 0.0f;
			float m22 = 1.0f - m[2][2];
			if(m22 != 0.0f)
				y2 = 1.0f / (2.0f * m22);
			if(0.0f < y2)
			{
				y = (float)sqrt(y2);
				z = m[1][2] / (2.0f * y);
			}
			else
			{
				y = 0.0f;
				z = 1.0f;
			}
		}
	}

	float angle = (float)acos(2.0f * w * w - 1.0f);
	float value[4];
	if(angle != 0.0f)
	{
		value[0] = x / (float)sin(angle);
		value[1] = y / (float)sin(angle);
		value[2] = z / (float)sin(angle);
		value[3] = angle;
	}
	else
	{
		value[0] = 0.0f;
		value[1] = 0.0f;
		value[2] = 1.0f;
		value[3] = 0.0f;
	}

	rotation->setValue(value);
}
"
  (let ((m00 (sb-cga:mref mat 0 0))
        (m01 (sb-cga:mref mat 0 1))
        (m02 (sb-cga:mref mat 0 2))
        (m10 (sb-cga:mref mat 1 0))
        (m11 (sb-cga:mref mat 1 1))
        (m12 (sb-cga:mref mat 1 2))
        (m20 (sb-cga:mref mat 2 0))
        (m21 (sb-cga:mref mat 2 1))
        (m22 (sb-cga:mref mat 2 2))
        (w 0.0)
        (x 0.0)
        (y 0.0)
        (z 0.0)
        (angle 0.0))
    (let ((w2 (float (* 1/4 (+ 1.0 m00 m11 m22)))))
      (if (< 0.0 w2)
          (progn
            (setf w (sqrt w2))
            (setf x (/ (- m12 m21) (* 4.0 w)))
            (setf y (/ (- m20 m02) (* 4.0 w)))
            (setf z (/ (- m01 m10) (* 4.0 w))))
          (progn
            (let ((x2 0.0)
                  (m1122 (+ m11 m22)))
              (when (/= m1122 0.0)
                (setf x2 (/ -1.0 (* 2 m1122))))
              (if (< 0.0 x2)
                  (progn
                    (setf x (sqrt x2))
                    (setf y (/ m01 (* 2.0 x)))
                    (setf z (/ m02 (* 2.0 x))))
                  (progn
                    (let ((y2 0.0)
                          (m22 (- 1.0 m22)))
                      (when (/= m22 0.0)
                        (setf y2 (/ 1.0 (* 2.0 m22))))
                      (if (< 0.0 y2)
                          (progn
                            (setf y (sqrt y2))
                            (setf z (/ m12 (* 2.0 y))))
                          (progn
                            (setf z 1.0))))))))))

    (setf angle (SFFloat (realpart (acos (- (* 2.0 w w) 1.0)))))
    (if (/= angle 0.0)
        (let ((x (/ x (sin angle)))
              (y (/ y (sin angle)))
              (z (/ z (sin angle))))
          (SFRotation (SFVec3f x y z) angle))
        (SFRotation 0 0 1 0))))

;; ----------------------------------------------------------------------------
(defmethod SFRotation+ (a b)
  (let ((a (SFMatrix4F<-SFRotation a))
        (b (SFMatrix4F<-SFRotation b)))
    (SFRotation<-SFMatrix4F (sb-cga:matrix* a b))))

;; ----------------------------------------------------------------------------
(defmethod SFRotation-reorient (v1 v2)
  ""
  (let ((nv1 (sb-cga:normalize v1))
        (nv2 (sb-cga:normalize v2)))
    (if (sb-cga:vec~ nv1 nv2)
        (list (SFVec3f 0 0 1) (SFFloat 0.0))
        (let* ((vec (sb-cga:normalize (sb-cga:cross-product nv1 nv2)))
               (cross (sb-cga:dot-product nv1 nv2))
               (angle  (SFFloat (radians 1.0))))
                                        ;(SFFloat (realpart (acos (sb-cga:dot-product nv1 nv2))))))
          (list vec angle)))))
