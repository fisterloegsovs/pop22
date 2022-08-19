//
// Library for Functional images
// Inspired by Conal Elliott's paper on the topic.
// Martin Elsman (c), MIT License
//

module Img

// read a bitmap file
val fromFile : string -> System.Drawing.Bitmap

// save a bitmap as a png file
val toPngFile : string -> System.Drawing.Bitmap -> unit

// Some type abbreviations
type frac = float                          // floats in interval [0;1]

// colors
type color = frac * frac * frac * frac     // alpha,red,green,blue

val greyifyColor : color -> color
val boolToColor  : bool -> color
val fracToColor  : frac -> color

// convert between colors and system colors
val toSColor   : color -> System.Drawing.Color
val fromSColor : System.Drawing.Color -> color

type point = float * float
type 'a image = point -> 'a
type region = bool image

val vstrip   : region
val checker  : region
val altRings : region

// polar points
type polar_point = float * float
val pi        : float
val fromPolar : polar_point -> point
val toPolar   : point -> polar_point

val polarChecker : int -> region
val wavDist      : frac image

// store a color image into a bitmap; the float
// specifies the with and height of the image domain
val imgToBitmap : System.Drawing.Bitmap -> float -> color image -> unit
