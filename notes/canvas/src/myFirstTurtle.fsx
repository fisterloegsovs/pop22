#r "nuget:DIKU.Canvas, 1.0.0-alpha2"

open Canvas

let cmd = [Move 100; Turn 90; Move 200]
let w = 600;
let h = 400;
do turtleDraw (w,h) "My First Canvas" cmd
