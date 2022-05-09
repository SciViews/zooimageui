#' Check Training Set Name
#'
#' Check if the name is already used, or null, and if not, take off the special chars from the name. If yes, return NULL.
#'
#' @param ts_name Training Set's name to correct
#' @param ts_list Vector listing the existing Training Sets
#'
#' @return Correct Training Set's name or NULL
#' @export
#'
#' @examples
#' ts_list_ex <- c("test1","test2")
#' ts_name1 <- "test1"
#' ts_name2 <- "test3"
#' ts_name3 <- "test_4"
#' cor_ts_name(ts_name1,ts_list_ex)
#' cor_ts_name(ts_name2,ts_list_ex)
#' cor_ts_name(ts_name3,ts_list_ex)
cor_ts_name <- function(ts_name,ts_list) {
  
  invalid_names <- append(ts_list, "")
  
  if (!(ts_name %in% invalid_names)) {
    stringr::str_replace_all( ts_name, "[^[:alnum:]]", "")
  }
}
