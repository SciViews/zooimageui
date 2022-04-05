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
#' 
#' @import fs
#' @importFrom ggplot2 ggplot
#' @import rlang
#' @import shinyjs
#' @import shinythemes
#' @importFrom stringr str_replace_all
#' @importFrom utils head read.csv
#' @import zooimage
#' 
NULL

.onLoad <- function(lib, pkg) {
  options(svDialogs.tmpfiles = TRUE)
}
