# Hunting Effect on Individual Deer Stress Level
**Module P15.2 Fortgeschrittenes Praxisprojekt - Bachelor Statistik und Data Science (PO 2021)**

**authors:** Nikolai German, Thomas Witzani, Ziqi Xu, Zhengchen Yuan, Baisu Zhou

## General

**Please read this README before executing any file.**

This is the repository for the advanced statistical practical project.
Apart from the working scripts in the R/ folder, you can find our experiments in R/archive/.
The final Report is called Report.pdf.

## Instructions

### Restoring the environment

  1.  Open the repo as a new R-project.\
  **You'll neeed R version 4.4+ and a recent version of RTools!**\
  
  2.  Please execute the following command to install the necessary packages
  ```
  renv::restore()
  ```
  3. If ...
  
### Run the models

It is enough to run
  ```
  source("R/main.R")
  ```
  to reproduce the analysis we present in the report.
Afterwards, you can find the fitted models in the objects `fits` (GAMM) and in `fits_xg` (XGBoost).

## Folder structure

- Data
  - raw: raw data & documentation.
  - intermediate: "clean" datasets.
  - processed: processed data & summary tables.
- Figures: plots for report.
  - Models: effect and diagnostic plots for GAMMs, 2d and 3d plots for XGBoost.
- R: scripts for main analysis.
  - archive: legacy experiments.
- renv: locked R environment for reproducibility.

`Presentation_v10.html`: presentation slides.
`references.bib`: bibliography file for the report.
