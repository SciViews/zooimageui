#' Samples Folders
#'
#' @param Sample_folder_content Vector of names of a folder's content
#'
#' @return The names of the samples' folders from a vector of names
#' @export
#'
#' @examples
#' Sample_folder_content <- list.files("~/shared/data/zooimage_data/Sample")
#' samples(Sample_folder_content)
samples <- function(Sample_folder_content) {
  smps <- Sample_folder_content[!grepl("Description.zis", Sample_folder_content)]
  smps[!grepl(".zidb", smps)]
}
