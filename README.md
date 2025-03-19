# Hunting Effect on Individual Deer Stress Level
**Module P15.2 Fortgeschrittenes Praxisprojekt - Bachelor Statistik und Data Science (PO 2021)**

**authors:** Nikolai German, Thomas Witzani, Ziqi Xu, Zhengchen Yuan, Baisu Zhou

## General
**Please read before executing any file**
This is the repository to the advanced statistical project.
Apart from the working scripts in the R/ folder, you can find our experiments in R/archive/.
The final Report is called Report.pdf.

## Instructions

### Restoring the environment

  1.  Open the repo as a new R-project.\
  **You'll neeed R Version 4.4+ and an appropriate version of RTools!**\
  
  2.  Please execute the following command to install the necessary packages
  ```
  renv::restore()
  ```
  
### Run the models

It is enough to run
  ```
  source("R/main.R")
  ```
  to obtain the models.
Afterwards, you can find the fitted models in the objects fits (GAMM) and in fits_xg (XGBoost).
