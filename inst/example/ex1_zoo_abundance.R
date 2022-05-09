# Example of script for {zooimageui} results
#
# function's name and get_results' arguments cannot change
#
library(zooimage)
get_results <- function(data, data_folder_path = NULL, Classif = NULL,
zidb_files = NULL) {
  zoo <- levels(data$Predicted)
  zoo <- zoo[grepl("^[A-Z]", zoo)]
  # detail <- list of classes for details
  res <- processSample(data, keep = zoo) # detail = detail)
  return(res)
}
comment(get_results) <- "Calculation of ZooPlancton's Abundance for selected sample."
