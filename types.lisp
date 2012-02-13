;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; types.lisp --- Implementation of ISO/IEC 19775-1:2008:
;;;;                5. Field Types.
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

;; -----------------------------------------------------------------------class
(defclass  x3d-array-field ()
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.2.2 X3DArrayField

X3DArrayField is the abstract field type from which all field types that can
contain multiple values are derived. All fields derived from X3DArrayField have
names beginning with MF. MFxxxx fields may zero or more values, each of which
shall be of the type indicated by the corresponding SFxxxx field type. It is
illegal for any MFxxxx field to mix values of different SFxxxx field types.

EXAMPLE MFString is a field type that can contain zero or more character
strings."))

;; -----------------------------------------------------------------------class
(defclass  x3d-field ()
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.2.3 X3DField

X3DField is the abstract field type from which all single values field types are
derived. All fields derived from X3DField have names beginning with SF. SFxxxx
fields may only contain a single value of the type indicated by the name of the
field type.

EXAMPLE SFBool is a field type that can contain a single Boolean value.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-bool (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.1 SFBool and MFBool

The SFBool field specifies a single Boolean value. The MFBool field specifies
multiple Boolean values. Each Boolean value represents either TRUE or FALSE. How
these values are represented is encoding dependent.

The default value of an uninitialized SFBool field is FALSE. The default value
of an uninitialized MFBool field is the empty list
"))

;; -----------------------------------------------------------------------class
(defclass  mf-bool (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.1 SFBool and MFBool

The SFBool field specifies a single Boolean value. The MFBool field specifies
multiple Boolean values. Each Boolean value represents either TRUE or FALSE. How
these values are represented is encoding dependent.

The default value of an uninitialized SFBool field is FALSE. The default value
of an uninitialized MFBool field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-color (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.2 SFColor and MFColor

The SFColor field specifies one RGB (red-green-blue) colour triple. MFColor
specifies zero or more RGB triples. Each colour is written to the X3D file as an
RGB triple of floating point numbers in the range 0.0 to 1.0.

The default value of an uninitialized SFColor field is (0 0 0). The default
value of an uninitialized MFColor field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-color (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.2 SFColor and MFColor

The SFColor field specifies one RGB (red-green-blue) colour triple. MFColor
specifies zero or more RGB triples. Each colour is written to the X3D file as an
RGB triple of floating point numbers in the range 0.0 to 1.0.

The default value of an uninitialized SFColor field is (0 0 0). The default
value of an uninitialized MFColor field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-color-rgba (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.3 SFColorRGBA and MFColorRGBA

The SFColorRGBA field specifies one RGBA (red-green-blue-alpha) colour quadruple
that includes alpha (opacity) information. MFColorRGBA specifies zero or more
RGBA quadruples. Each colour is written to the X3D file as an RGBA quadruple of
floating point numbers in the range 0.0 to 1.0. Alpha values range from
0.0 (fully transparent) to 1.0 (fully opaque).

The default value of an uninitialized SFColorRGBA field is (0 0 0 0). The
default value of an uninitialized MFColorRGBA field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-color-rgba (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.3 SFColorRGBA and MFColorRGBA

The SFColorRGBA field specifies one RGBA (red-green-blue-alpha) colour quadruple
that includes alpha (opacity) information. MFColorRGBA specifies zero or more
RGBA quadruples. Each colour is written to the X3D file as an RGBA quadruple of
floating point numbers in the range 0.0 to 1.0. Alpha values range from
0.0 (fully transparent) to 1.0 (fully opaque).

The default value of an uninitialized SFColorRGBA field is (0 0 0 0). The
default value of an uninitialized MFColorRGBA field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-double (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.4 SFDouble and MFDouble

The SFDouble field specifies one double-precision floating point
number. MFDouble specifies zero or more double-precision floating point
numbers. SFDouble and MFDouble are represented in the X3D file as specified in
the respective encoding.

Implementation of these fields is targeted at the double precision floating
point capabilities of processors. However, it is allowable to implement this
field using fixed point numbering provided at least 14 decimal digits of
precision are maintained and that exponents have range of at least [-12, 12] for
both positive and negative numbers.

The default value of an uninitialized SFDouble field is 0.0. The default value
of an MFDouble field is the empty list.


"))

;; -----------------------------------------------------------------------class
(defclass  mf-double (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.4 SFDouble and MFDouble

The SFDouble field specifies one double-precision floating point
number. MFDouble specifies zero or more double-precision floating point
numbers. SFDouble and MFDouble are represented in the X3D file as specified in
the respective encoding.

Implementation of these fields is targeted at the double precision floating
point capabilities of processors. However, it is allowable to implement this
field using fixed point numbering provided at least 14 decimal digits of
precision are maintained and that exponents have range of at least [-12, 12] for
both positive and negative numbers.

The default value of an uninitialized SFDouble field is 0.0. The default value
of an MFDouble field is the empty list.


"))

;; -----------------------------------------------------------------------class
(defclass  sf-float (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.5 SFFloat and MFFloat

The SFFloat field specifies one single-precision floating point number. MFFloat
specifies zero or more single-precision floating point numbers. SFFloats and
MFFloats are represented in the X3D file as specified in the respective
encoding.

Implementation of these fields is targeted at the single precision floating
point capabilities of processors. However, it is allowable to implement this
field using fixed point numbering provided at least six decimal digits of
precision are maintained and that exponents have range of at least [-12, 12] for
both positive and negative numbers.

The default value of an uninitialized SFFloat field is 0.0. The default value of
an MFFloat field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-float (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.5 SFFloat and MFFloat

The SFFloat field specifies one single-precision floating point number. MFFloat
specifies zero or more single-precision floating point numbers. SFFloats and
MFFloats are represented in the X3D file as specified in the respective
encoding.

Implementation of these fields is targeted at the single precision floating
point capabilities of processors. However, it is allowable to implement this
field using fixed point numbering provided at least six decimal digits of
precision are maintained and that exponents have range of at least [-12, 12] for
both positive and negative numbers.

The default value of an uninitialized SFFloat field is 0.0. The default value of
an MFFloat field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-image (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.6 SFImage and MFImage

The SFImage field specifies a single uncompressed 2-dimensional pixel
image. SFImage fields contain three integers representing the width, height and
number of components in the image, followed by width×height hexadecimal or
integer values representing the pixels in the image. MFImage fields contain zero
or more SFImage fields. Each image in an MFImage field may contain different
values for the width, height, and number of components in the image and hence
may have a different number of hexadecimal or integer values.

Pixel values are limited to 256 levels of intensity (i.e., 0-255 decimal or
0x00-0xFF hexadecimal). A one-component image specifies one-byte hexadecimal or
integer values representing the intensity of the image. For example, 0xFF is
full intensity in hexadecimal (255 in decimal), 0x00 is no intensity (0 in
decimal). A two-component image specifies the intensity in the first (high) byte
and the alpha opacity in the second (low) byte. Pixels in a three-component
image specify the red component in the first (high) byte, followed by the green
and blue components (e.g., 0xFF0000 is red, 0x00FF00 is green, 0x0000FF is
blue). Four-component images specify the alpha opacity byte after
red/green/blue (e.g., 0x0000FF80 is semi-transparent blue). A value of 0x00 is
completely transparent, 0xFF is completely opaque. Note that alpha equals (1.0
-transparency), if alpha and transparency each range from 0.0 to 1.0.

Each pixel is read as a single unsigned number. For example, a 3-component pixel
with value 0x0000FF may also be written as 0xFF (hexadecimal) or
255 (decimal). Pixels are specified from left to right, bottom to top. The first
hexadecimal value is the lower left pixel and the last value is the upper right
pixel.

The default value of an SFImage outputOnly field is (0 0 0). The default value
of an MFImage field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-image (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.6 SFImage and MFImage

The SFImage field specifies a single uncompressed 2-dimensional pixel
image. SFImage fields contain three integers representing the width, height and
number of components in the image, followed by width×height hexadecimal or
integer values representing the pixels in the image. MFImage fields contain zero
or more SFImage fields. Each image in an MFImage field may contain different
values for the width, height, and number of components in the image and hence
may have a different number of hexadecimal or integer values.

Pixel values are limited to 256 levels of intensity (i.e., 0-255 decimal or
0x00-0xFF hexadecimal). A one-component image specifies one-byte hexadecimal or
integer values representing the intensity of the image. For example, 0xFF is
full intensity in hexadecimal (255 in decimal), 0x00 is no intensity (0 in
decimal). A two-component image specifies the intensity in the first (high) byte
and the alpha opacity in the second (low) byte. Pixels in a three-component
image specify the red component in the first (high) byte, followed by the green
and blue components (e.g., 0xFF0000 is red, 0x00FF00 is green, 0x0000FF is
blue). Four-component images specify the alpha opacity byte after
red/green/blue (e.g., 0x0000FF80 is semi-transparent blue). A value of 0x00 is
completely transparent, 0xFF is completely opaque. Note that alpha equals (1.0
-transparency), if alpha and transparency each range from 0.0 to 1.0.

Each pixel is read as a single unsigned number. For example, a 3-component pixel
with value 0x0000FF may also be written as 0xFF (hexadecimal) or
255 (decimal). Pixels are specified from left to right, bottom to top. The first
hexadecimal value is the lower left pixel and the last value is the upper right
pixel.

The default value of an SFImage outputOnly field is (0 0 0). The default value
of an MFImage field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-int32 (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.7 SFInt32 and MFInt32

The SFInt32 field specifies one 32-bit integer. The MFInt32 field specifies zero
or more 32-bit integers. SFInt32 and MFInt32 fields are signed integers.

The default value of an uninitialized SFInt32 field is 0. The default value of
an MFInt32 field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-int32 (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.7 SFInt32 and MFInt32

The SFInt32 field specifies one 32-bit integer. The MFInt32 field specifies zero
or more 32-bit integers. SFInt32 and MFInt32 fields are signed integers.

The default value of an uninitialized SFInt32 field is 0. The default value of
an MFInt32 field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-matrix3d (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.8 SFMatrix3d and MFMatrix3d

The SFMatrix3d field specifies a 3×3 matrix of double-precision floating point
numbers. MFMatrix3d specifies zero or more 3×3 matrices of double-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix3d matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, and the second for the y
dimension. Since these data types are commonly used for transformation matrices,
translation values are stored in the third row.

The default value of an uninitialized SFMatrix3d field is the identity matrix [1
0 0 0 1 0 0 0 1]. The default value of an uninitialized MFMatrix3d field is the
empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-matrix3d (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.8 SFMatrix3d and MFMatrix3d

The SFMatrix3d field specifies a 3×3 matrix of double-precision floating point
numbers. MFMatrix3d specifies zero or more 3×3 matrices of double-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix3d matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, and the second for the y
dimension. Since these data types are commonly used for transformation matrices,
translation values are stored in the third row.

The default value of an uninitialized SFMatrix3d field is the identity matrix [1
0 0 0 1 0 0 0 1]. The default value of an uninitialized MFMatrix3d field is the
empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-matrix3f (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.9 SFMatrix3f and MFMatrix3f

The SFMatrix3f field specifies a 3×3 matrix of single-precision floating point
numbers. MFMatrix3f specifies zero or more 3×3 matrices of single-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix3f matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, and the second for the y
dimension. Since these data types are commonly used for transformation matrices,
translation values are stored in the third row.

The default value of an uninitialized SFMatrix3f field is the identity matrix [1
0 0 0 1 0 0 0 1]. The default value of an uninitialized MFMatrix3f field is the
empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-matrix3f (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.10 SFMatrix4d and MFMatrix4d

The SFMatrix4d field specifies a 4×4 matrix of double-precision floating point
numbers. MFMatrix4d specifies zero or more 4×4 matrices of double-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix4d matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, the second for the y dimension,
and the third for the z dimension. Since these data types are commonly used for
transformation matrices, translation values are stored in the fourth row.

The default value of an uninitialized SFMatrix4d field is the identity matrix [1
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]. The default value of an uninitialized MFMatrix4d
field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-matrix4d (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.10 SFMatrix4d and MFMatrix4d

The SFMatrix4d field specifies a 4×4 matrix of double-precision floating point
numbers. MFMatrix4d specifies zero or more 4×4 matrices of double-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix4d matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, the second for the y dimension,
and the third for the z dimension. Since these data types are commonly used for
transformation matrices, translation values are stored in the fourth row.

The default value of an uninitialized SFMatrix4d field is the identity matrix [1
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]. The default value of an uninitialized MFMatrix4d
field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-matrix4d (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.10 SFMatrix4d and MFMatrix4d

The SFMatrix4d field specifies a 4×4 matrix of double-precision floating point
numbers. MFMatrix4d specifies zero or more 4×4 matrices of double-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix4d matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, the second for the y dimension,
and the third for the z dimension. Since these data types are commonly used for
transformation matrices, translation values are stored in the fourth row.

The default value of an uninitialized SFMatrix4d field is the identity matrix [1
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]. The default value of an uninitialized MFMatrix4d
field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-matrix4f (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.11 SFMatrix4f and MFMatrix4f

The SFMatrix4f field specifies a 4x4 matrix of single-precision floating point
numbers. MFMatrix4f specifies zero or more 4x4 matrices of single-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix4f matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, the second for the y dimension,
and the third for the z dimension. Since these data types are commonly used for
transformation matrices, translation values are stored in the fourth row.

The default value of an uninitialized SFMatrix4f field is the identity matrix [1
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]. The default value of an uninitialized MFMatrix4f
field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-matrix4f (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.11 SFMatrix4f and MFMatrix4f

The SFMatrix4f field specifies a 4x4 matrix of single-precision floating point
numbers. MFMatrix4f specifies zero or more 4x4 matrices of single-precision
floating point numbers. Each floating point number is represented in the X3D
file as specified in the respective encoding.

SFMatrix4f matrices are organized in row-major fashion. The first row of the
matrix stores information for the x dimension, the second for the y dimension,
and the third for the z dimension. Since these data types are commonly used for
transformation matrices, translation values are stored in the fourth row.

The default value of an uninitialized SFMatrix4f field is the identity matrix [1
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]. The default value of an uninitialized MFMatrix4f
field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-node (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.12 SFNode and MFNode

The SFNode field specifies an X3D node. The MFNode field specifies zero or more
nodes.

The default value of an uninitialized SFNode field is NULL. The default value of
an MFNode field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-node (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.12 SFNode and MFNode

The SFNode field specifies an X3D node. The MFNode field specifies zero or more
nodes.

The default value of an uninitialized SFNode field is NULL. The default value of
an MFNode field is the empty list.
"))

;; (defun mf-node (&rest rest)
;;   "This creates a array of nodes of sf-node type"
;;   (make-array (length rest)
;;               :adjustable t
;;               :element-type 'sf-node
;;               :initial-contents rest))

;; NOTE: for now implement mf-node as a list. Add and remove children also need
;; to change imlementation when this is changed to using an array
(defun mf-node (&rest rest)
  "This simply creates of list of nodes"
  rest)

;; -----------------------------------------------------------------------class
(defclass  sf-rotation (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.13 SFRotation and MFRotation

The SFRotation field specifies one arbitrary rotation. The MFRotation field
specifies zero or more arbitrary rotations. An SFRotation is written to the X3D
file as four floating point values. The allowable form for a floating point
number is defined in the specific encoding. The first three values specify a
normalized rotation axis vector about which the rotation takes place. The fourth
value specifies the amount of right-handed rotation about that axis in radians.

The 3x3 matrix representation of a rotation (x y z a) is

[ tx2+c  txy+sz txz-sy
  txy-sz ty2+c  tyz+sx
  txz+sy tyz-sx tz2+c  ]

where c = cos(a), s = sin(a), and t = 1-c.

The default value of an uninitialized SFRotation field is (0 0 1 0). The default
value of an MFRotation field is the empty list.
"))

(defmethod sf-rotation(x y z w)
  (let ((a (coerce x 'single-float))
        (b (coerce y 'single-float))
        (c (coerce z 'single-float))
        (angle (coerce w 'single-float)))
  (list (sb-cga:vec a b c) angle)))

;; -----------------------------------------------------------------------class
(defclass  mf-rotation (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.13 SFRotation and MFRotation

The SFRotation field specifies one arbitrary rotation. The MFRotation field
specifies zero or more arbitrary rotations. An SFRotation is written to the X3D
file as four floating point values. The allowable form for a floating point
number is defined in the specific encoding. The first three values specify a
normalized rotation axis vector about which the rotation takes place. The fourth
value specifies the amount of right-handed rotation about that axis in radians.

The 3x3 matrix representation of a rotation (x y z a) is

[ tx2+c  txy+sz txz-sy
  txy-sz ty2+c  tyz+sx
  txz+sy tyz-sx tz2+c  ]

where c = cos(a), s = sin(a), and t = 1-c.

The default value of an uninitialized SFRotation field is (0 0 1 0). The default
value of an MFRotation field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-string (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.14 SFString and MFString

The SFString and MFString fields contain strings encoded with the UTF-8
universal character set (see ISO/IEC 10646). SFString specifies a single
string. The MFString specifies zero or more strings. Strings are specified as a
sequence of UTF-8 octets.

Any characters (including linefeeds and '#') may appear within the string.

The default value of an uninitialized SFString outputOnly field is the empty
string. The default value of an MFString field is the empty list.

Characters in ISO/IEC 10646 are encoded in multiple octets. Code space is
divided into four units, as follows:

+-------------+-------------+-----------+------------+

| Group-octet | Plane-octet | Row-octet | Cell-octet |

+-------------+-------------+-----------+------------+
ISO/IEC 10646 allows two basic forms for characters:

a .UCS-2 (Universal Coded Character Set-2). This form is also known as the Basic
Multilingual Plane (BMP). Characters are encoded in the lower two octets (row
and cell).

b. UCS-4 (Universal Coded Character Set-4). Characters are encoded in the full
four octets.

In addition, two transformation formats (UCS Transformation Format or UTF) are
accepted: UTF-8 and UTF-16. Each represents the nature of the transformation:
8-bit or 16-bit. UTF-8 and UTF-16 are referenced in ISO/IEC 10646.

UTF-8 maintains transparency for all ASCII code values (0...127). It allows
ASCII text (0x0..0x7F) to appear without any changes and encodes all characters
from 0x80.. 0x7FFFFFFF into a series of six or fewer bytes.

If the most significant bit of the first character is 0, the remaining seven
bits are interpreted as an ASCII character. Otherwise, the number of leading 1
bits indicates the number of bytes following. There is always a zero bit between
the count bits and any data.

The first byte is one of the following. The X indicates bits available to encode
the character:

 0XXXXXXX only one byte   0..0x7F (ASCII)
 110XXXXX two bytes       Maximum character value is 0x7FF
 1110XXXX three bytes     Maximum character value is 0xFFFF
 11110XXX four bytes      Maximum character value is 0x1FFFFF
 111110XX five bytes      Maximum character value is 0x3FFFFFF
 1111110X six bytes       Maximum character value is 0x7FFFFFFF
All following bytes have the format 10XXXXXX.

As a two byte example, the symbol for a registered trade mark ®, encoded as
0x00AE in UCS-2 of ISO 10646, has the following two byte encoding in UTF-8:
0xC2, 0xAE.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-string (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.14 SFString and MFString

The SFString and MFString fields contain strings encoded with the UTF-8
universal character set (see ISO/IEC 10646). SFString specifies a single
string. The MFString specifies zero or more strings. Strings are specified as a
sequence of UTF-8 octets.

Any characters (including linefeeds and '#') may appear within the string.

The default value of an uninitialized SFString outputOnly field is the empty
string. The default value of an MFString field is the empty list.

Characters in ISO/IEC 10646 are encoded in multiple octets. Code space is
divided into four units, as follows:

+-------------+-------------+-----------+------------+

| Group-octet | Plane-octet | Row-octet | Cell-octet |

+-------------+-------------+-----------+------------+
ISO/IEC 10646 allows two basic forms for characters:

a .UCS-2 (Universal Coded Character Set-2). This form is also known as the Basic
Multilingual Plane (BMP). Characters are encoded in the lower two octets (row
and cell).

b. UCS-4 (Universal Coded Character Set-4). Characters are encoded in the full
four octets.

In addition, two transformation formats (UCS Transformation Format or UTF) are
accepted: UTF-8 and UTF-16. Each represents the nature of the transformation:
8-bit or 16-bit. UTF-8 and UTF-16 are referenced in ISO/IEC 10646.

UTF-8 maintains transparency for all ASCII code values (0...127). It allows
ASCII text (0x0..0x7F) to appear without any changes and encodes all characters
from 0x80.. 0x7FFFFFFF into a series of six or fewer bytes.

If the most significant bit of the first character is 0, the remaining seven
bits are interpreted as an ASCII character. Otherwise, the number of leading 1
bits indicates the number of bytes following. There is always a zero bit between
the count bits and any data.

The first byte is one of the following. The X indicates bits available to encode
the character:

 0XXXXXXX only one byte   0..0x7F (ASCII)
 110XXXXX two bytes       Maximum character value is 0x7FF
 1110XXXX three bytes     Maximum character value is 0xFFFF
 11110XXX four bytes      Maximum character value is 0x1FFFFF
 111110XX five bytes      Maximum character value is 0x3FFFFFF
 1111110X six bytes       Maximum character value is 0x7FFFFFFF
All following bytes have the format 10XXXXXX.

As a two byte example, the symbol for a registered trade mark ®, encoded as
0x00AE in UCS-2 of ISO 10646, has the following two byte encoding in UTF-8:
0xC2, 0xAE.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-time (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.15 SFTime and MFTime

The SFTime field specifies a single time value. The MFTime field specifies zero
or more time values. Time values are specified as a double-precision floating
point number. The allowable form for a double precision floating point number is
defined in the specific encoding. Time values are specified as the number of
seconds from a specific time origin. Typically, SFTime fields represent the
number of seconds since Jan 1, 1970, 00:00:00 GMT.

The default value of an uninitialized SFTime field is -1. The default value of
an MFTime field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-time (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.15 SFTime and MFTime

The SFTime field specifies a single time value. The MFTime field specifies zero
or more time values. Time values are specified as a double-precision floating
point number. The allowable form for a double precision floating point number is
defined in the specific encoding. Time values are specified as the number of
seconds from a specific time origin. Typically, SFTime fields represent the
number of seconds since Jan 1, 1970, 00:00:00 GMT.

The default value of an uninitialized SFTime field is -1. The default value of
an MFTime field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec2d (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.16 SFVec2d and MFVec2d

The SFVec2d field specifies a two-dimensional (2D) vector. An MFVec2d field
specifies zero or more 2D vectors. SFVec2d's and MFVec2d's are represented as a
pair of double-precision floating point values (see 5.3.4 SFDouble and
MFDouble). The allowable form for a double-precision floating point number is
defined in the specific encoding.

The default value of an uninitialized SFVec2d field is (0 0). The default value
of an MFVec2d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-vec2d (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)
5.3.16 SFVec2d and MFVec2d

The SFVec2d field specifies a two-dimensional (2D) vector. An MFVec2d field
specifies zero or more 2D vectors. SFVec2d's and MFVec2d's are represented as a
pair of double-precision floating point values (see 5.3.4 SFDouble and
MFDouble). The allowable form for a double-precision floating point number is
defined in the specific encoding.

The default value of an uninitialized SFVec2d field is (0 0). The default value
of an MFVec2d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec2f (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.17 SFVec2f and MFVec2f

The SFVec2f field specifies a two-dimensional (2D) vector. An MFVec2f field
specifies zero or more 2D vectors. SFVec2f's and MFVec2f's are represented as a
pair of single-precision floating point values (see 5.3.5 SFFloat and
MFFloat). The allowable form for a single-precision floating point number is
defined in the specific encoding.

The default value of an uninitialized SFVec2f field is (0 0). The default value
of an MFVec2f field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-vec2f (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.17 SFVec2f and MFVec2f

The SFVec2f field specifies a two-dimensional (2D) vector. An MFVec2f field
specifies zero or more 2D vectors. SFVec2f's and MFVec2f's are represented as a
pair of single-precision floating point values (see 5.3.5 SFFloat and
MFFloat). The allowable form for a single-precision floating point number is
defined in the specific encoding.

The default value of an uninitialized SFVec2f field is (0 0). The default value
of an MFVec2f field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec3d (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.18 SFVec3d and MFVec3d

The SFVec3d field or event specifies a three-dimensional (3D) vector. An MFVec3d
field or event specifies zero or more 3D vectors. SFVec3d's and MFVec3d's are
represented as a 3-tuple of double-precision floating point values (see 5.3.4
SFDouble and MFDouble). The allowable form for a double-precision floating point
number is defined in the specific encoding.

The default value of an uninitialized SFVec3d field is (0 0 0). The default
value of an MFVec3d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-vec3d (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.18 SFVec3d and MFVec3d

The SFVec3d field or event specifies a three-dimensional (3D) vector. An MFVec3d
field or event specifies zero or more 3D vectors. SFVec3d's and MFVec3d's are
represented as a 3-tuple of double-precision floating point values (see 5.3.4
SFDouble and MFDouble). The allowable form for a double-precision floating point
number is defined in the specific encoding.

The default value of an uninitialized SFVec3d field is (0 0 0). The default
value of an MFVec3d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec3f (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.19 SFVec3f and MFVec3f

The SFVec3f field or event specifies a three-dimensional (3D) vector. An MFVec3f
field or event specifies zero or more 3D vectors. SFVec3f's and MFVec3f's are
represented as a 3-tuple of single-precision floating point values (see 5.3.5
SFFloat and MFFloat). The allowable form for a single-precision floating point
number is defined in the specific encoding.

The default value of an uninitialized SFVec3f field is (0 0 0). The default
value of an MFVec3f field is the empty list.
"))

(defmethod sf-vec3f (x y z)
  (let ((a (coerce x 'single-float))
        (b (coerce y 'single-float))
        (c (coerce z 'single-float)))
    (sb-cga:vec a b c)))

;; -----------------------------------------------------------------------class
(defclass  mf-vec3f (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.19 SFVec3f and MFVec3f

The SFVec3f field or event specifies a three-dimensional (3D) vector. An MFVec3f
field or event specifies zero or more 3D vectors. SFVec3f's and MFVec3f's are
represented as a 3-tuple of single-precision floating point values (see 5.3.5
SFFloat and MFFloat). The allowable form for a single-precision floating point
number is defined in the specific encoding.

The default value of an uninitialized SFVec3f field is (0 0 0). The default
value of an MFVec3f field is the empty list.


"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec4d (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.20 SFVec4d and MFVec4d

The SFVec4d field or event specifies a three-dimensional (3D) homogeneous
vector. An MFVec4d field or event specifies zero or more 3D homogeneous
vectors. SFVec4d's and MFVec4d's are represented as a 4-tuple of
double-precision floating point values (see 5.3.4 SFDouble and MFDouble). The
allowable form for a double-precision floating point number is defined in the
specific encoding.

The default value of an uninitialized SFVec4d field is (0 0 0 1). The default
value of an MFVec4d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-vec4d (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.20 SFVec4d and MFVec4d

The SFVec4d field or event specifies a three-dimensional (3D) homogeneous
vector. An MFVec4d field or event specifies zero or more 3D homogeneous
vectors. SFVec4d's and MFVec4d's are represented as a 4-tuple of
double-precision floating point values (see 5.3.4 SFDouble and MFDouble). The
allowable form for a double-precision floating point number is defined in the
specific encoding.

The default value of an uninitialized SFVec4d field is (0 0 0 1). The default
value of an MFVec4d field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  sf-vec4f (x3d-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.21 SFVec4f and MFVec4f

The SFVec4f field or event specifies a three-dimensional (3D) homogeneous
vector. An MFVec4f field or event specifies zero or more 3D homogeneous
vectors. SFVec4f's and MFVec4f's are represented as a 4-tuple of
single-precision floating point values (see 5.3.5 SFFloat and MFFloat). The
allowable form for a single-precision floating point number is defined in the
specific encoding.

The default value of an uninitialized SFVec4f field is (0 0 0 1). The default
value of an MFVec4f field is the empty list.
"))

;; -----------------------------------------------------------------------class
(defclass  mf-vec4f (x3d-array-field)
  ()
  (:documentation "
ISO/IEC 19775-1:2008 (SEE NOTICE.TXT)

5.3.21 SFVec4f and MFVec4f

The SFVec4f field or event specifies a three-dimensional (3D) homogeneous
vector. An MFVec4f field or event specifies zero or more 3D homogeneous
vectors. SFVec4f's and MFVec4f's are represented as a 4-tuple of
single-precision floating point values (see 5.3.5 SFFloat and MFFloat). The
allowable form for a single-precision floating point number is defined in the
specific encoding.

The default value of an uninitialized SFVec4f field is (0 0 0 1). The default
value of an MFVec4f field is the empty list.
"))
