# Install DiagrammeR if you don't have it already
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}

library(DiagrammeR)

# Create the Mermaid diagram
mermaid_code <- "
graph TD
    RAW[RAW DATA<br>Reproduction Success Results.xlsx] -->|Integrate Reproduction into Movement Data| DATA[RAW DATA<br>Movement - CRS -&gt; ETRS UTM 33N.csv<br>HuntingEvents_NEW.csv<br>FCM Stress - Collared Deer - CRS -&gt; ETRS UTM 33N.csv]
    DATA --> Plots[Plots]
    DATA --> Datafusion[Datafusion.R<br>Combine Date and Timestamp 1 Var,<br>Parse Time Var to same timeframe,<br>Transform Coordinate System to ETRS89]
    Datafusion --> Save[Save all as RDS files]
    Save --> Calc[CalcSenderPosDist_new.R<br>1. Interpolate most likely position of all deer<br>at the time of all hunting events<br>2. Calculate Euclidean distance between all deer<br>and all hunting events]
    Calc --> Assign[Assign_FCMData_to_Stressors.R<br>1. Assign all FCM samples to all possible<br>Hunt Event & DeerID combinations: 111k total<br>2. Every FCM sample is now associated<br>with Distance & TimeDiff variable.<br>3. Create different versions of data for modelling:<br>a. Scoring function based on assignment relevance<br>b. Keep assignments with lowest distance<br>c. Keep assignments with lowest time diff<br>d. Combine b & c<br>e. Combine b & c, include duplicates.]
    Assign --> Datasets[5 Datasets for Modelling]
    Datasets --> Models[Models:<br>XGBoost, GLM, GAM, etc.]
    Models --> Evaluation[Model Evaluation Criteria:<br>RMSE, Adjusted R², AIC, etc.<br>Model-specific metrics like hyperparameter values,<br>smoothness of curves<br>Use Cross Validation!]
    Evaluation --> Plots[Plots]
"


# Render the Mermaid diagram
DiagrammeR::mermaid(mermaid_code)
diagram <- DiagrammeR::mermaid(mermaid_code)
htmlwidgets::saveWidget(diagram, "./Figures/Project_Overview.html", selfcontained = TRUE)