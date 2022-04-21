# example of script for ZooImageUI Results
#
# function's name and get_results' arguments cannot change
#
library(zooimage)
get_results <- function(data, data_folder_path = NULL, Classif = NULL) {
  zoo <- levels(data$Predicted)
  zoo <- zoo[grepl("^[A-Z]", zoo)]
  detail <- zoo[zoo %in% levels(data$Predicted)[table(data$Predicted) >= 50]]
  res <- processSample(data, keep = zoo, detail = detail)
  return(res)
}
comment(get_results) <- "Calculation of ZooPlancton's abundance 
                        (for those with at least 50 individuals measured)."
