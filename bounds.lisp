;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; bounds.lisp --- Visitor to calculate the bounds
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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

(defmethod bboxExtent ((self X3DBoundedObject))
  (with-slots (bboxSize) self
    (let ((bboxSize (SFVec3f bboxSize)))
      (sb-cga:vec/ bboxSize 2.0))))

(defmethod bboxMin ((self X3DBoundedObject))
  (with-slots (bboxCenter) self
    (let ((center (SFVec3f bboxCenter))
          (extent (SFVec3f (bboxExtent self))))
      (sb-cga:vec- center extent))))

(defmethod bboxMax((self X3DBoundedObject))
  (with-slots (bboxCenter) self
    (let ((center (SFVec3f bboxCenter))
          (extent (SFVec3f (bboxExtent self))))
      (sb-cga:vec+ center extent))))

(defgeneric bounds(self)
  "A generic function to calculate the bounds of the scene")

(defmethod bounds((self Box))
  "Calculating the bounds of a box"
  (with-slots (bboxCenter bboxSize size) self
    (setf bboxCenter (SFVec3f 0 0 0))
    (setf bboxSize (SFVec3f size))))

(defmethod bounds((self Cone))
  "Calculating the bounds of a Cone"
  (with-slots (bboxCenter bboxSize height bottomRadius) self
    (let ((x (* 2 bottomRadius))
          (y height)
          (z (* 2 bottomRadius)))
      (setf bboxCenter (SFVec3f 0 0 0))
      (setf bboxSize (SFVec3f x y z)))))

(defmethod bounds((self Cylinder))
  "Calculating the bounds of a Cylinder"
  (with-slots (bboxCenter bboxSize height radius) self
    (let ((x (* 2 radius))
          (y height)
          (z (* 2 radius)))
      (setf bboxCenter (SFVec3f 0 0 0))
      (setf bboxSize (SFVec3f x y z)))))

(defmethod bounds((self Sphere))
  "Calculating the bounds of a Sphere"
  (with-slots (bboxCenter bboxSize radius) self
    (let ((x (* 2 radius))
          (y (* 2 radius))
          (z (* 2 radius)))
      (setf bboxCenter (SFVec3f 0 0 0))
      (setf bboxSize (SFVec3f x y z)))))

(defun min-max(center size)
  (let ((half (sb-cga:vec/ size 2.0)))
    (let ((min (sb-cga:vec- center half))
          (max (sb-cga:vec+ center half)))
      (list :min min :max max))))

(defun vec-abs(vec)
  "absolute value of vector"
  )

(defun center-size(min max)
  (let ((center (sb-cga:vec/ (sb-cga:vec- max min) 2))
        (size (mod(sb-cga:vec- max min)

(defmethod bounds((self Transform))
  (format t "Transform~%")
  (with-slots (bboxCenter center rotation scale scaleOrientation translation containerField) self
    (let ((center (SFVec3f center))
          (rotation (SFRotation rotation))
          (scale (SFVec3f scale))
          (scaleOrientation (SFRotation scaleOrientation))
          (translation (SFVec3f translation)))
      (let ((mat (transform translation center rotation scale scaleOrientation))
            (bboxCenter-old bboxCenter))
        (setf bboxCenter (sb-cga:transform-point bboxCenter-old mat))))))


