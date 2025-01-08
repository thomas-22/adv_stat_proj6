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
