#' Samples Folders
#'
#' Find the names of Samples Folders in a given path
#'
#' @param sample_folder_content Vector of names of a folder's content
#'
#' @return The names of the samples' folders from a vector of names
#' @export
#'
#' @examples
#' sample_folder_content <- list.files(system.file("example","Samples", package = "zooimageui"))
#' find_samples(sample_folder_content)
find_samples <- function(sample_folder_content) {
  smps <- sample_folder_content[!grepl("Description.zis", sample_folder_content)]
  smps[!grepl(".zidb", smps)]
}
