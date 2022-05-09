#' zooimageui
#' 
#' A web app to make an interface for the {zooimage} package.
#' 
#' @section Application Shiny:
#' 
#' - To launch the Shiny application : use [run_app()]
#' 
#' @docType package
#' @name zooimageui-package
#' 
#' 
#' @importFrom fs path dir_create dir_ls
#' @import mlearning
#' @import rlang
#' @importFrom shinyjs disabled disable enable
#' @import shinythemes
#' @importFrom stats predict
#' @importFrom stringr str_replace_all
#' @importFrom utils head read.csv zip unzip capture.output
#' @import zooimage
#' 
NULL

.onLoad <- function(lib, pkg) {
  options(svDialogs.tmpfiles = TRUE)
}
