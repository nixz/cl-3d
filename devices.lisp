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
(defclass Screen ()
  ((o    :initarg :o
         :initform (SFVec3F -1.0 -1.0 -2.4142075) ;
         :accessor o
         :documentation "The lower left corder of the display (Origin)")
   (x    :initarg :x
         :initform (SFVec3F  1.0 -1.0 -2.4142075)
         :accessor x
         :documentation "The lower right corder of the display")
   (y    :initarg :y
         :initform (SFVec3F -1.0  1.0 -2.4142075)
         :accessor y
         :documentation "about-slot"))
  (:documentation "Class to configure display coordinates wrt some
  base coordinates"))

;; ............................................................................
(defmethod width ((self Screen))
  "Calculates the width of the screen"
  (with-slots (o x) self
    (sb-cga:vec-length (sb-cga:vec- o x))))

;; ............................................................................
(defmethod height ((self Screen))
  "Calculates the width of the screen"
  (with-slots (o y) self
    (sb-cga:vec-length (sb-cga:vec- o y))))

;; ----------------------------------------------------------------------------
;; Until there is real head tracking this class is dummy. The run
;; method of this class returns the transformation matrix for the
;; tracker
(defclass  User-Head ()
  ((position
         :initform (SFVec3F 0 0 0)
         :accessor position
         :documentation "The position of the tracker. We use the
         default units to be meters")
   (orientiation
         :initform NIL
         :accessor orientiation
         :documentation "the orientation of the tracker (usually a
         quaternion)")
   (iod
         :initform 0.063
         :accessor iod
         :documentation "Inter occular distance. Default = 0.063
         meters")
   (is-stereo
         :initform nil
         :accessor is-stereo
         :documentation "If the user-head has stereo capability"))
  (:documentation "The tracked user-head. Returns a transformation matrix on run."))


;; ----------------------------------------------------------------------------
(defmethod compute-projection ((user-head User-Head) (screen Screen))
  "uses the user-head and the screen and computes three projections i.e the
left eye the right eye and the center"
  (let* ((width (width screen))
         (height (height screen))
         (ipd (iod user-head))
         (Base<-Screen (un screen)) ; In Real world
         (Base<-Head (run user-head))     ; In Real world
         (Head<-Base (sb-cga:inverse-matrix Base<-Head))
         (Base<-VWorld (sb-cga:identity-matrix)) ; Virtual world (const)
         (Head<-Screen (sb-cga:matrix* Head<-Base Base<-Screen))
         ;; Center eye calculations
         (Head<-AlignedEyeCenter (sb-cga:matrix*
                                  (sb-cga:translate (SFVec3F 0.0 0.0 0.0))
                                  (extract-rotation Head<-Screen)))
         (AlignedEyeCenter<-Head (sb-cga:inverse-matrix Head<-AlignedEyeCenter))
         (AlignedEyeCenter<-VWorld (sb-cga:matrix* AlignedEyeCenter<-Head
                                                   Head<-Base
                                                   Base<-VWorld))
         (AlignedEyeCenter<-Screen (sb-cga:matrix* AlignedEyeCenter<-Head
                                                   Head<-Screen))
         ;; Left eye calculations
         (Head<-AlignedEyeLeft (sb-cga:matrix*
                                (sb-cga:translate (SFVec3F (/ ipd -2) 0.0 0.0))
                                (extract-rotation Head<-Screen)))
         (AlignedEyeLeft<-Head (sb-cga:inverse-matrix Head<-AlignedEyeLeft))
         (AlignedEyeLeft<-VWorld (sb-cga:matrix* AlignedEyeLeft<-Head
                                                 Head<-Base
                                                 Base<-VWorld))
         (AlignedEyeLeft<-Screen (sb-cga:matrix* AlignedEyeLeft<-Head
                                                 Head<-Screen))
         ;; Right eye calculations
         (Head<-AlignedEyeRight (sb-cga:matrix*
                                 (sb-cga:translate (SFVec3F (/ ipd 2) 0.0 0.0))
                                 (extract-rotation Head<-Screen)))
         (AlignedEyeRight<-Head (sb-cga:inverse-matrix Head<-AlignedEyeRight))
         (AlignedEyeRight<-VWorld (sb-cga:matrix* AlignedEyeRight<-Head
                                                  Head<-Base
                                                  Base<-VWorld))
         (AlignedEyeRight<-Screen (sb-cga:matrix* AlignedEyeRight<-Head
                                                  Head<-Screen)))
    (flet ((projection (Eye<-Screen Eye<-VWorld width height)
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
                   (sb-cga:matrix* proj Eye<-VWorld))))))
        (list :left
              (projection AlignedEyeLeft<-Screen
                          AlignedEyeLeft<-VWorld
                          width height)
              :right
              (projection AlignedEyeRight<-Screen
                          AlignedEyeRight<-VWorld
                          width height)
              :center
              (projection AlignedEyeCenter<-Screen
                          AlignedEyeCenter<-VWorld
                          width height)))))

;; ----------------------------------------------------------------------------
(defparameter *news* nil)

;; ----------------------------------------------------------------------------
;; Screen to tracking base
(defclass Devices (xml-serializer)
  (;; (screen
   ;;       :initform (make-instance 'Screen)
   ;;       :accessor screen
   ;;       :documentation "Defines the screen device")
   (screen
         :initform 
         (cond ((eq *news* 0) (make-instance 'Screen
                                             :o (SFVec3F -1.0 -1.0 -1.0)
                                             :x (SFVec3F  1.0 -1.0 -1.0)
                                             :y (SFVec3F -1.0  1.0 -1.0)))
               ((eq *news* 1)  (make-instance 'Screen
                                             :o (SFVec3F -1.0 -1.0  1.0)
                                             :x (SFVec3F -1.0 -1.0 -1.0)
                                             :y (SFVec3F -1.0  1.0  1.0)))
               ((eq *news* 2) (make-instance 'Screen
                                             :o (SFVec3F  1.0 -1.0 -1.0)
                                             :x (SFVec3F  1.0 -1.0  1.0)
                                             :y (SFVec3F  1.0  1.0 -1.0)))
                ((eq *news* 3)  (make-instance 'Screen
                                             :o (SFVec3F -1.0 -1.0  1.0)
                                             :x (SFVec3F  1.0 -1.0  1.0)
                                             :y (SFVec3F -1.0 -1.0 -1.0)))
                (t (make-instance 'Screen)))
         :accessor screen)
   (user-head
         :initform (make-instance 'User-Head)
         :accessor user-head
         :documentation "Tracked user-head device")
   (2d-mouse
         :initform (make-instance '2d-mouse)
         :accessor 2d-mouse
         :documentation "Mouse interaction device"))
  (:documentation "doc"))

;; ----------------------------------------------------------------------------
(defmethod run ((self Screen))
  "Returns the transformation matrix of the screen"
  (with-slots (o x y) self
    (let* ((x-> (sb-cga:vec- o x))
          (y-> (sb-cga:vec- o y))
          (center (sb-cga:vec/ (sb-cga:vec+ x y) 2.0)))
      (let ((z-> (sb-cga:cross-product x-> y->))
            (oz-> (SFVec3f 0 0 1)))
        (let ((rot (sb-cga:reorient oz-> z->))
              (trans (sb-cga:translate center)))
          (sb-cga:matrix* trans rot))))))

;; ----------------------------------------------------------------------------
(defmethod run ((self User-Head))
  "For now there is no head transformation. And we simply return the
identity matrix"
  (sb-cga:identity-matrix))

;; ----------------------------------------------------------------------------
(defclass  2d-mouse ()
  ((rot-x :initform 0.0
         :accessor rot-x
         :documentation "rotation in x axis")
   (rot-y :initform 0.0
         :accessor rot-y
         :documentation "rotation in y axis")
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
    (sb-cga:matrix* (sb-cga:rotate-around (SFVec3F 1 0 0)
                                          (SFFloat (radians (SFFloat rot-x))))
                    (sb-cga:rotate-around (SFVec3F 0 1 0)
                                          (SFFloat (radians (SFFloat rot-y))))
                    (sb-cga:rotate-around (SFVec3F 0 0 1) 0.0))))

;; ----------------------------------------------------------------------------
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
    (when (and (eq button :right-button)
               (eq state :down))
      (let* ((x-x0 (- x (car orig-click)))
             (y-y0 (- y (cadr orig-click)))
             (direction (if (<  y-y0 0) .1 -.1))
             (distance (* direction (sqrt (+ (* x-x0 x-x0) (* y-y0 y-y0))))))
        (setf trans-z (+ orig-trans distance))))))

;; ----------------------------------------------------------------------------
(defmethod run ((self 2d-mouse))
  "multiplies the translation and rotation matrices to provide a
transformation matrix"
  (sb-cga:matrix* (translation self) (rotation self)))
