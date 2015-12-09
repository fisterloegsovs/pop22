#load "packages/FsLab/FsLab.fsx"
open System
open XPlot.GoogleCharts
let rnd = new System.Random()
let data = [for i in 1 .. 15 -> (rnd.Next(10),rnd.Next(10))]
//let data = [for i in 1 .. 15 -> (rnd.NextDouble (),rnd.NextDouble ())]
Chart.Scatter data
