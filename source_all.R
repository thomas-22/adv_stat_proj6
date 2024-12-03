source("settings.R")
lapply(rev(list.files("R/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)), source)
