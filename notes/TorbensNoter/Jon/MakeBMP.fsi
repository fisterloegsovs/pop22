module MakeBMP

/// Functions for reading and writing BMP files.
/// For documentation about BMP files, see https://en.wikipedia.org/wiki/BMP_file_format

/// <summary> Create BMP file from size and colour function
/// <example>
/// The call <c>makeBMP "gradient" 128 256 (fun (i,j) -> (i+i,j,0))</c>
/// creates a file <c>gradient.bmp</c> containing a 128×256 pixel image
/// showing a red/yellow/green gradient.
/// </example>
/// <param name="fname"> File name (without .bmp extension)</param>
/// <param name="w"> Width of image in pixels</param>
/// <param name="h"> Height of image in pixels</param>
/// <param name="cols"> function from pixel position to RGB value.</param>
/// <returns> nothing</returns>
/// <remarks> Width and height are integers between 1 and 8192.
/// (0,0) is bottom-left corner</remarks>
val makeBMP : string -> int -> int -> (int*int -> int*int*int) -> unit

/// <summary> Create BMP file from a colour array
/// <example>
/// The call <c>makeBMParray "something"  colourArray</c>
/// creates a file <c>something.bmp</c> containing a pixel image
/// with size and colours as specified in the array.
/// </example>
/// <param name="fname"> File name (without .bmp extension)</param>
/// <param name="colsArray"> array containing bitmap</param>
/// <returns> nothing</returns>
/// <remarks> Width and height are integers between 1 and 8192.
/// (0,0) is bottom-left corner</remarks>
val makeBMParray : string -> (int*int*int) [,] -> unit

/// <summary> Reads a BMP file into a colour array
/// <example>
/// The call <c>readBMParray "something"</c>
/// reads a file <c>something.bmp</c> containing a pixel image.
/// </example>
/// <param name="fname"> File name (without .bmp extension)</param>
/// <returns> array containing bitmap</returns>
val readBMParray : string -> (int*int*int) [,]

/// <summary> Reads a BMP file and creates funtion
/// <example>
/// The call <c>readBMParray "something"</c>
/// reads a file <c>something.bmp</c> containing a pixel image.
/// </example>
/// <param name="fname"> File name (without .bmp extension)</param>
/// <returns> size of bitmap (w,h) and function from position to colour</returns>
val readBMP : string -> int*int*(int*int -> (int*int*int))
