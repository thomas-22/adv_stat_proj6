# set options so, packages get installed without asking the user
options(install.packages.compile.from.source = "always")

# needed packages
pkgs <- c("dplyr",
          "tidyr",
          "ggplot2",
          "ggrepel",
          "patchwork",
          "forcats",
          "checkmate",
          "viridis",
          "readxl",
          "DiagrammeR",
          "DiagrammeRsvg",
          "rsvg",
          "kableExtra",
          "quarto",
          "tinytex")

# already installed packages
installed <- rownames(installed.packages())

# install all
cat("Versuche die benötigen Packages direkt zu installieren:")
for (pkg in pkgs) {
  if(any(installed == pkg)) {
    next
  } else {
    cat("Package <", pkg, "> wird installiert...\n", sep = "")
    install.packages(pkg, verbose = FALSE)
  }
}
