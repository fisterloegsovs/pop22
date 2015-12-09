#load "packages/FsLab/FsLab.fsx"

open System
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.Plotly
open XPlot.GoogleCharts.Deedle

let wb = WorldBankData.GetDataContext()

let euroGDP = [
  let euro = wb.Regions.``Euro area``
  yield "EU","",euro.Indicators.``GDP (current US$)``.[2010]
  for c in euro.Countries do
    yield c.Name,"EU",c.Indicators.``GDP (current US$)``.[2010]
  ]

Chart.Treemap(euroGDP)
|> Chart.WithOptions (Options(minColor="#B24590", midColor="#449AB5", maxColor="#76B747", headerHeight=0, showScale=true))

let visible = set ["Greece";"Ireland";"Latvia";"Malta"]
let data = [
  for ct in wb.Regions.``Euro area``.Countries ->
    let data = ct.Indicators.``GDP growth (annual %)``
    let vis = if visible.Contains ct.Name then "" else "legendonly"
    Scatter(x=Seq.map fst data, y=Seq.map snd data, name=ct.Name, visible=vis) ]

Figure ( Data.From data, Layout(title="GDP growth (annual %) in Euro area") )
                      
