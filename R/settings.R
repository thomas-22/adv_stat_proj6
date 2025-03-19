# -------------------------
# load neccessary libraries
# -------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(ggeffects)
library(ggspatial)
library(patchwork)
library(sf)
library(osmdata)
library(mgcv)

library(xgboost)
library(caTools)
library(reshape2)
library(grid)
library(gridExtra)
library(png)
library(magick)

# -------------------------
# the following libraries get accessed via the :: operator
# -------------------------
# library(readr)
# library(readxl)
# library(stringr)
# library(purrr)
# library(gamm4)
# library(gratia)

# -------------------------
# set paths to raw data
# -------------------------
path.FCMStress <- "Data/raw/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv"
path.ReproductionSuccess <- "Data/raw/Reproduction Success Results.xlsx"
path.Movement <- "Data/raw/Movement - CRS=ETRS UTM 33N.csv"
path.HuntEvents <- "Data/raw/HuntingEvents_NEW.csv"

# ----------------------
# construct theme for spatial plots
# ----------------------
theme_set(theme_light() + 
            theme(plot.title = element_text(hjust = 0.5, size = 14)))

theme_spatial <- function() {
  theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}
