#' ZooImageUI
#' 
#' It is a web app to make an interface to the program ZooImage.
#' 
#' @section Application Shiny:
#' 
#' - To launch the Shiny application : use [run_app()]
#' 
#' @docType package
#' @name ZooImageUI-package
#' 
#' @import zooimage
#' @importFrom ggplot2 ggplot
#' @import rlang
#' @importFrom utils read.csv
#' @importFrom fs path
#' @import shinythemes
#' 
NULL

.onLoad <- function(lib, pkg) {
  options(svDialogs.tmpfiles = TRUE)
}
