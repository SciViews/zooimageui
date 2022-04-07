#' Upload TS
#' 
#' Get the zipped training set from upload, unzip it, unlink the zip file.
#'
#' @param upload_input Shiny input file
#' @param ts_folder_path Path to folder to put ts in
#'
#' @return TRUE if it worked, FALSE if an error occured
#' @export
#'
#' @examples
#' # Need a reactive context
upload_ts <- function(upload_input, ts_folder_path, existing_ts) {
  
  if (is.null(upload_input)) {
    res <- FALSE
    attr(res, "error") <- "File is NULL"
    return(res)
  }
  
  if (!is.null(existing_ts)) {
    existing_ts <- paste0(existing_ts, ".zip")
  } else {
    existing_ts <- ""
  }
  
  op <- options(warn = 2)
  
  oldir <- try(setwd(ts_folder_path), silent = TRUE)
  
  if (grepl(".zip", upload_input$name) && !(upload_input$name %in% existing_ts)) {
    fc_res <- try(file.copy(upload_input$datapath, upload_input$name), silent = TRUE)
  } else {
    res <- FALSE
    if (!grepl(".zip", upload_input$name)) {
      attr(res, "error") <- "File doesn't have .zip ext"
    } else {
      attr(res, "error") <- "Training Set's name already used"
    }
    setwd(oldir)
    return(res)
  }
  
  zip_res <- try(unzip(upload_input$name), silent = TRUE)
  unlink_res <- try(unlink(upload_input$name), silent = TRUE)
  on.exit(setwd(oldir))
  
  options(op)
  
  oldir_inh <- inherits(oldir, "try-error")
  fc_inh <- inherits(fc_res, "try-error")
  zip_inh <- inherits(zip_res, "try-error")
  unlink_inh <- inherits(unlink_res, "try-error")
  
  if (oldir_inh || fc_inh || zip_inh || unlink_inh) {
    res <- FALSE
    if (oldir_inh) {
      attr(res, "error") <- attr(oldir, "condition")
    } else if (fc_inh) {
      attr(res, "error") <- attr(fc_res, "condition")
    } else if (zip_inh) {
      attr(res, "error") <- attr(zip_res, "condition")
    } else if (unlink_inh) {
      attr(res, "error") <- attr(unlink_res, "condition")
    }
    
    return(res)
  } else {
    return(TRUE)
  }
  
}

# test <- unzip_and_unlink("get.zip")
# if (!test) {
#   attr(test, "error")
# } else {
#   "Fait !"
# }