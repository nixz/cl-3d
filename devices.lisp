;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; devices.lisp --- Low level device code
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

;; ----------------------------------------------------------------------------
;; TODO: Implement VRPN device interface. The following structure is
;; to read stuff from VRPN.

;; (defstruct Button
;;   time                                  ; time when state changed
;;   id                                    ; button-id
;;   state                                 ; state of the button
;; )

;; (defstruct Analog
;;   time                                  ; timestamp
;;   numChannel                            ; total number of analog channels
;;   channel                               ; the channel list
;; )

;; (defstruct Tracker
;;   time                                  ; timestamp
;;   sensor                                ; which sensor is reporting
;;   pos                                   ; position of the sensor
;;   quat                                  ; orientation of the sensor
;; )

;; ----------------------------------------------------------------------------
(defclass  2d-mouse ()
  ((rot-x :initform 0.0
         :accessor rot-x
         :documentation "rotation in x axis")
   (rot-y :initform 0.0
         :accessor rot-y
         :documentation "rotation in y axis")
   (trans-x :initform 0.0
         :accessor trans-x
         :documentation "translation in x axis")
   (trans-y :initform 0.0
         :accessor trans-y
         :documentation "translation in y axis")
   (trans-z :initform 0.0
         :accessor trans-z
         :documentation "translation in z axis")
   (orig-click :initform nil
         :accessor orig-click
         :documentation "Initial position on click")
   (orig-rot :initform nil
         :accessor orig-rot
         :documentation "Updated position")
   (orig-trans :initform nil
         :accessor orig-trans
         :documentation "Updated position")
   (button :initform :left-button
         :accessor button
         :documentation "rotation in y axis")
   (state :initform :up
         :accessor state
         :documentation "rotation in y axis"))
  (:documentation "Manages mouse interation. Left button click and
  drag changes the rotation angles rot-x and rot-y. These keep track of
  the orientation and is used to generate a rotation matrix." ))

;; ----------------------------------------------------------------------------
(defmethod rotation ((self 2d-mouse))
  "Gets the rotation matrix of a 2d-mouse device"
  (with-slots (rot-x rot-y) self
    (sb-cga:matrix* (sb-cga:rotate-around (SFVec3F 1 0 0) (SFFloat (radians (SFFloat rot-x))))
                    (sb-cga:rotate-around (SFVec3F 0 1 0) (SFFloat (radians (SFFloat rot-y))))
                    (sb-cga:rotate-around (SFVec3F 0 0 1) 0.0))))

(defmethod translation ((self 2d-mouse))
  "Gets the translation matrix of a 2d-mouse device"
  (with-slots (trans-z) self
    (sb-cga:translate (sb-cga:vec 0.0 0.0 trans-z))))

;; ----------------------------------------------------------------------------
(defmethod initiate ((self 2d-mouse) b s x y)
  "sets the initial state of the mouse"
  (with-slots (rot-x rot-y trans-z orig-click orig-rot orig-trans button state) self
    (setf button b)
    (setf state s)
    (when (eq button :left-button)
        (if (eq state :down)
            (progn (setf orig-rot (list rot-x rot-y))
                   (setf orig-click (list x y)))
            (setf orig-click ())))
    (when (eq button :right-button)
      (if (eq state :down)
          (progn (setf orig-trans trans-z)
                 (setf orig-click (list x y)))
          (setf orig-click ())))))

;; ----------------------------------------------------------------------------
(defmethod update ((self 2d-mouse) x y)
  "Handles updates on the mouse"
  (with-slots (rot-x rot-y trans-z orig-click orig-rot orig-trans button state) self
    (when (and (eq button :left-button)
               (eq state :down))
      (setf rot-x (+ (car orig-rot) (- y (cadr orig-click))))
      (setf rot-y (+ (cadr orig-rot) (- x (car orig-click)))))
    (print button)
    (print state)
    (when (and (eq button :right-button)
               (eq state :down))
      (let* ((x-x0 (- x (car orig-click)))
             (y-y0 (- y (cadr orig-click)))
             (direction (if (<  y-y0 0) .1 -.1))
             (distance (* direction (sqrt (+ (* x-x0 x-x0) (* y-y0 y-y0))))))
        (print x-x0)
        (print y-y0)
        (print distance)
        (setf trans-z (+ orig-trans distance))))))

;; ----------------------------------------------------------------------------
(defparameter *MOUSE* (make-instance '2d-mouse))

;; ----------------------------------------------------------------------------
(defun mouse-reset()
  (setf *MOUSE* (make-instance '2d-mouse)))

;; ----------------------------------------------------------------------------
(defun mouse-init (button state x y)
  (initiate *MOUSE* button state x y))

;; ----------------------------------------------------------------------------
(defun mouse-update (x y)
  ""
  (update *MOUSE* x y))

;; ----------------------------------------------------------------------------
(defun mouse-rotate()
  ""
  (sb-cga:matrix* (translation *MOUSE*) (rotation *MOUSE*)))
