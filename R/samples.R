#' Samples Folders
#'
#' Description
#'
#' @param Sample_folder_content Vector of names of a folder's content
#'
#' @return The names of the samples' folders from a vector of names
#' @export
#'
#' @examples
#' Sample_folder_content <- list.files("~/shared/data/zooimage_data/Sample") 
#' # Attention qu'ici, il vaudrait mieux utiliser un dossier du package, en 
#' # utilisant des données échantillon du package
#' samples(Sample_folder_content)
samples <- function(Sample_folder_content) {
  smps <- Sample_folder_content[!grepl("Description.zis", Sample_folder_content)]
  smps[!grepl(".zidb", smps)]
}
