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
#' 
NULL

.onLoad <- function(lib, pkg) {
  options(svDialogs.tmpfiles = TRUE)
}

data_folder_path <- golem::get_golem_options("data_folder_path")
