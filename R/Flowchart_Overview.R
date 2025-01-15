if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  install.packages("htmlwidgets")
}

#find.package("DiagrammeR")

library(DiagrammeR)
library(htmlwidgets)

dot_code <- "
digraph {
  rankdir=TB
  
  node [shape=rectangle, style=filled, color=lightblue, fontname=Helvetica, labelType=\"html\"]

  RAW [label=<<b>Raw Data</b><br/>ReproductionSuccessResults.xlsx<br/>MovementData.csv<br/>HuntingEvents_NEW.csv<br/>FCMStressData.csv>]
  DataFusion [label=<<b>DataFusion.R</b><br/>Merge and clean timestamps,<br/>project coordinates, etc.>]
  Calc [label=<<b>CalcSenderPosDist_new.R</b><br/>1. Interpolate linearly the positions of all deer<br/>2. Calculate Euclidean or other distance metrics>]
  Assign [label=<<b>Assign_FCMData_to_Stressors.R</b><br/>For each FCM sample, find the most relevant<br/>hunting event (distance, time diff, etc.)<br/>and create multiple versions of the dataset>]
  Datasets [label=\"5 Datasets\\nfor Modeling\"]
  Models [label=<<b>Models:</b><br/>XGBoost, GLM, GAM, etc.>]
  Evaluation [label=<<b>Evaluation:</b><br/>RMSE, Adjusted R^2, AIC,<br/>cross-validation, etc.>]
  Plots [label=\"Plots & Visualizations\"]

  # Flow
  RAW -> DataFusion
  DataFusion -> Calc
  Calc -> Assign
  Assign -> Datasets
  Datasets -> Models
  Models -> Evaluation
  Evaluation -> Plots
}
"

#Save as HTML
diagram <- DiagrammeR::grViz(dot_code)
saveWidget(diagram, file = "./Figures/Project_Overview.html", selfcontained = TRUE)



graphTimes <- grViz("
  digraph {
  layout = dot
    node [shape = rectangle, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    DefecTime[label = 'Defecation Time']
    SampleTime[label = 'Sample Time']
    SampleDelay[label = 'Sample Delay', color = magenta]
    HuntTime[label = 'Hunt Time']
    TimeDiffStress[label = 'Time Diff Stress', color = magenta]
    StressTime[label = 'temp: Stress Time', color=grey]
    Day[label = 'Day of Year', color = magenta]
    
    DefecTime -> SampleDelay
    DefecTime -> Day
    SampleTime -> SampleDelay
    DefecTime -> StressTime [label=<Gut Retention <b>low</b>>, fontname=Helvetica]
    HuntTime -> TimeDiffStress 
    StressTime -> TimeDiffStress
    edge [minlen = 2]
    rank=same {SampleTime, DefecTime, HuntTime}
    rank=same {SampleDelay, TimeDiffStress, Day}
  }
   ")

svg_Times <- DiagrammeRsvg::export_svg(graphTimes)
rsvg::rsvg_png(charToRaw(svg_Times), file = "./Figures/graphTimes.png", height = 1440)

graphInterX <- grViz("
  digraph {
  layout = dot
    node [shape = rectangle, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    
    HuntTime[label = 'Hunt Time']
    SenderTimeLow[label = 'Sender Timestamp t']
    SenderTimeHigh[label = 'Sender Timestamp t+1']
    SenderXLow[label = 'X Coordinate']
    SenderXHigh[label = 'X Coordinate']
    
    Xinter[label = 'temp: interpolated X Coordinate', color = grey]
    
    SenderTimeLow -> HuntTime
    HuntTime -> SenderTimeHigh

    SenderTimeLow -> SenderXLow

    SenderTimeHigh -> SenderXHigh
    
    SenderXLow -> Xinter
    SenderXHigh -> Xinter
    HuntTime -> Xinter
    
    edge [minlen = 2]
    rank=same {SenderTimeLow, HuntTime, SenderTimeHigh}
  }
   ")

svg_InterX <- DiagrammeRsvg::export_svg(graphInterX)
rsvg::rsvg_png(charToRaw(svg_InterX), file = "./Figures/graphInterX.png", height = 1440)


graphDistances <- grViz("
  digraph {
  layout = dot
    node [shape = rectangle, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    
    InterX[label = <temp:<br/> Interpolated X Coordinate <br/> of Deer>, color = grey]
    InterY[label = <temp:<br/> Interpolated Y Coordinate <br/> of Deer>, color = grey]
    HuntX[label = <X Coordinate <br/>of Hunt Event>]
    HuntY[label = <Y Coordinate <br/>of Hunt Event>]
    
    Distance[label = 'Euclidean Distance', color = magenta]
    XDist[label = 'Distance in X', color = magenta]
    YDist[label = 'Distance in Y', color = magenta]
    
    InterX -> XDist
    HuntX -> XDist
    
    InterY -> YDist
    HuntY -> YDist
    
    XDist -> Distance
    YDist -> Distance
    
    edge [minlen = 2]
    rank=same {HuntX, HuntY}
    rank=same {InterX, InterY}
    
  }
   ")

svg_Distances <- DiagrammeRsvg::export_svg(graphDistances)
rsvg::rsvg_png(charToRaw(svg_Distances), file = "./Figures/graphDistances.png", height = 1440)


graphPregnant <- grViz("
  digraph {
  layout = dot
    node [shape = rectangle, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    
    pregnancyYear[label = <Year of Pregnany <br/> accompanied by calf>]
    SenderTime[label = <Sender Timestamp t>]
    
    Pregnant[label = <Pregnant (Y/N) <br/> at t>, color = magenta]
    
    pregnancyYear -> Pregnant
    SenderTime -> Pregnant
    
    edge [minlen = 2]
    rank=same {pregnancyYear, SenderTime}
    
  }
   ")

svg_Pregnant <- DiagrammeRsvg::export_svg(graphPregnant)
rsvg::rsvg_png(charToRaw(svg_Pregnant), file = "./Figures/graphPregnant.png", height = 1440)

graphEvent <- grViz("
  digraph {
  layout = dot
    node [shape = rectangle, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    
    Distance[label = <Euclidean Distance <br/>to Hunting Evenent>, color = magenta]
    TimeDiffStress[label = 'Time Diff Stress', color = magenta]
    
    Events[label = <temp: <br/>relevant Events>, color = grey]
    
    Event[label = <considered Event>, color = magenta]
    
    NumOther[label = <Number of other <br/> potentially impactfull <br/> Hunting Events>, color = magenta]
    
    
    Distance -> Events[label=<Distance Threshold>, fontname=Helvetica]
    TimeDiffStress -> Events[label=<Gut Retention <br/>Time <b>high</b>>, fontname=Helvetica]
    Events -> NumOther[label=<#Events - 1>, fontname=Helvetica]
    Events -> Event[label=<Proximity Criterion>, fontname=Helvetica]
    
    edge [minlen = 2]
    rank=same {Distance, TimeDiffStress}
    rank=same{Events, NumOther}
    
  }
   ")

svg_Event <- DiagrammeRsvg::export_svg(graphEvent)
rsvg::rsvg_png(charToRaw(svg_Event), file = "./Figures/graphEvent.png", height = 1440)


