module Cat
open System.IO

val readBytes: FileStream -> byte[]
val readFile: string -> byte[]
val readFiles: string list -> byte[] option list
val writeBytes: byte[] -> FileStream -> unit
val writeFile: byte[] -> string -> int
val cat: string[] -> int
