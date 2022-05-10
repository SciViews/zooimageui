# Run this script with `source("install.r")` to install all required R packages

install.packages(c("shiny", "golem", "DT", "zooimage", "mlearning", "shinyjs",
  "shinythemes", "digest", "rlang", "R6", "htmltools", "processx", "attempt",
  "config", "glue", "stringr", "fs"))

# Note: with R 4.0.5, a series of packages mut be updated:
#remotes::install_github("r-lib/rlang@v1.0.2")
#remotes::install_github("r-lib/testthat@v3.1.4")
#remotes::install_github("rstudio/rmarkdown@v2.14")
#remotes::install_github("yihui/xfun@v0.30")
#remotes::install_github("rstudio/htmltools@v0.5.2")
#remotes::install_github("rstudio/shiny@v1.7.1")
#remotes::install_github("rstudio/bslib@v0.3.1")
#remotes::install_github("r-lib/cli@v3.1.1")
#remotes::install_github("r-lib/withr@v2.5.0")
#remotes::install_github("r-lib/lifecycle@v1.0.1")
