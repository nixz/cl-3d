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

(in-package #:x3d)

(defun deg->rad (x) (* x (/ pi 180)))
(defun rad->deg (x) (* x (/ 180 pi)))

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
