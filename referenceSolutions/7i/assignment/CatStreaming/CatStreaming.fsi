module CatStreaming

open System.IO
// Required functions
val catWithBufferSize: int -> string[] -> int
val cat: (string[] -> int)
// Recommended functions
val readBytes: int -> byte[] -> FileStream -> int
val writeBytes: int -> byte[] -> FileStream -> unit
val readAndWriteBytes: int -> byte[] -> FileStream -> FileStream -> unit
val openFileRead: string -> FileStream
val openFilesRead: string list -> FileStream option list
val openFileWrite: string -> FileStream option
