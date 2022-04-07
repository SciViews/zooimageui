# Tests de gestion d'erreurs. Lecocq Martin.

unzip_and_unlink <- function(file_path) {
  
  op <- options(warn = 2)
  zip_res <- try(unzip(file_path), silent = TRUE)
  unlink_res <- try(unlink(file_path), silent = TRUE)
  options(op)
  
  zip_inh <- inherits(zip_res, "try-error")
  unlink_inh <- inherits(unlink_res, "try-error")
  
  if (zip_inh || unlink_inh) {
    res <- FALSE
    if (zip_inh) {
      attr(res, "error") <- attr(zip_res, "condition")
    } else if (unlink_inh) {
      attr(res, "error") <- attr(unlink_res, "condition")
    }
    return(res)
  } else {
    return(TRUE)
  }
  
}

test <- unzip_and_unlink("get.zip")
if (!test) {
  attr(test, "error")
} else {
  "Fait !"
}
