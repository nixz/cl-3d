;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; test-scene.lisp ---
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

(require 'cl-3d)
(defvar *SCENE*)
(use-package 'cl-3d)
(defmethod shape (&key appearance geometry metadata bbox-center bbox-size)
  "")

(defmethod world-info (&key 
(WorldInfo :info "an introductory scene"
           :title "Hello X3D Authors")
(Viewpoint :description "Hello, world"
           :orientation `(0 1 0 3.14159)
           :position `(0 0 -8))
;; OrbitalTimeInterval ROUTE:  [from fraction_changed to SpinThoseThings.set_fraction ]
(DEF Orbital-time-Interval
    (TimeSensor :cycleInterval 12.0
                :loop t))
;; SpinThoseThings ROUTEs:  [from OrbitalTimeInterval.fraction_changed to set_fraction ] [from value_changed to EarthCoordinateSystem.rotation ]
(DEF SpinThoseThings
    (OrientationInterpolator :key '(0.00 0.25 0.50 0.75 1.00)
                             :keyValue '(0 1 0 0 0 1 0 1.57079 0 1 0 3.14159 0 1 0 4.7123889 0 1 0 6.2831852)))

(ROUTE :fromNode OrbitalTimeInterval
       :fromField fraction_changed
       :toNode SpinThoseThings
       :toField set_fraction)

;; EarthCoordinateSystem ROUTE:  [from SpinThoseThings.value_changed to rotation ] -->
(DEF earth-coordinate-system
    (Transform
     :children (DEF mini-world
                   (Group
                    (Shape
                     (Appearance
                      (ImageTexture
                       :url='("earth-topo.png" "earth-topo.gif" "earth-topo-small.gif" "http://www.web3d.org/x3d/content/examples/Basic/course/earth-topo.png" "http://www.web3d.org/x3d/content/examples/Basic/course/earth-topo.gif"))
                     (Sphere)))
                 (Transform DEF='SimpleGeoStationarySatellite'
                            rotation='1 0 0 .3'
                            scale='0.1 0.3 0.1'
                            translation='0 0 5'
                            (Shape
                             (Appearance
                              (Material diffuseColor='0.9 0.1 0.1') )
                             (Text string='"Hello X3D Authors !!"'
                                   solid='false'
                                   (FontStyle size='3')))))))
(ROUTE fromNode='SpinThoseThings'
       fromField='value_changed'
       toNode='EarthCoordinateSystem'
       toField='rotation')

(setf temp (cl-3d::transform :children (list  1 2 3)))

(Transform :def 'simple-geo-stationarysatellite
           :rotation '(1 0 0 0.3)
           :scale '(0.1 0.3 1.0)
           :translation '(0 0 5))
