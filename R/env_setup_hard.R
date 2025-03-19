# set options so, packages get installed without asking the user
options(install.packages.compile.from.source = "always")

# needed packages
pkgs <- c("dplyr",
          "tidyr",
          "ggplot2",
          "ggrepel",
          "patchwork",
          "lubridate",
          "ggrepel",
          "ggeffects",
          "ggspatial",
          "sf",
          "osmdata",
          "mgcv",
          "xgboost",
          "caTools",
          "reshape2",
          "grid",
          "gridExtra",
          "png",
          "magick",
          "readr",
          "readxl",
          "stringr",
          "purrr",
          "gamm4",
          "gratia",
          "checkmate",
          "knitr",
          "kableExtra",
          "quarto",
          "tinytex")


# already installed packages
installed <- rownames(installed.packages())

# install all
cat("Try to install the necessary packages directly:")
for (pkg in pkgs) {
  if(any(installed == pkg)) {
    next
  } else {
    cat("installing package <", pkg, "> ...\n", sep = "")
    install.packages(pkg, verbose = FALSE)
  }
}
