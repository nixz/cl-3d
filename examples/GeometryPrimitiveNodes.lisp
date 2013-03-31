;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; GeometryPrimitiveNodes.lisp --- render all the primitives
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
;;;; ==========================================================================
;;;; XML part of code is from http://x3dgraphics.com/examples/X3dForWebAuthors/
;;;; Please read NOTICE.txt
;;;; ==========================================================================

(in-package #:cl-3d)

(render
  <Scene>
    <Transform translation='-5 0 0'>
      <Shape DEF='DefaultShape'>
        <Box DEF='DefaultBox' size='2 2 2'/>
        <Appearance>
          <Material diffuseColor='1 0.2 0.2'/>
        </Appearance>
      </Shape>
    </Transform>
    <Transform translation='-2.5 0 0'>
      <Shape>
        <Cone DEF='DefaultCone' bottom='true' bottomRadius='1' height='2' side='true'/>
        <Appearance>
          <Material diffuseColor='0.2 1 0.2'/>
        </Appearance>
      </Shape>
    </Transform>
    <Transform translation='0 0 0'>
      <Shape>
        <Cylinder DEF='DefaultCylinder' bottom='true' height='2' radius='1' side='true' top='true'/>
        <Appearance>
          <Material diffuseColor='0.2 0.2 1'/>
        </Appearance>
      </Shape>
    </Transform>
    <Transform translation='2.5 0 0'>
      <Shape>
        <Sphere DEF='DefaultSphere' radius='1'/>
        <Appearance>
          <Material diffuseColor='1 1 0.2'/>
        </Appearance>
      </Shape>
    </Transform>
    <Transform translation='4 0 0'>
      <Shape>
        <Text DEF='DefaultText' string='"hello" "X3D!"'>
          <FontStyle DEF='DefaultFontStyle'/>
        </Text>
        <Appearance DEF='DefaultAppearance'>
          <Material DEF='DefaultMaterial'/>
        </Appearance>
      </Shape>
    </Transform>
  </Scene>
)
