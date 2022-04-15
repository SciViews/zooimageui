#' Zip the Results
#' 
#' Zip Results of ZooImageUI in .txt and .RData files.
#'
#' @param data_folder_path Path to data folder.
#' @param name Name of the files for downloading.
#' @param res Results to zip.
#' @param file Name of the zip file.
#'
#' @return A zip file containing the .txt and .RData files.
#' @export
#'
#' @examples
#' # To Do...
res_dl <- function(data_folder_path, name, res, file) {
  
  # Pas de tests car réalisés en amont
  
  # Enlèvement des caractères spéciaux
  name <- stringr::str_replace_all( name, "[^[:alnum:]]", "")
  name_txt <- paste0(name,".txt") # Nom pour le fichier .txt
  
  # Dossier de téléchargement création
  dl_folder <- fs::path(data_folder_path,name)
  fs::dir_create(dl_folder)
  
  # Nom pour le .RData
  name_rdata <- paste0(name,".RData")
  # Path pour le .RData
  dl_folder_rdata <- fs::path(dl_folder,name_rdata)
  save(res, file = dl_folder_rdata) # Création du .RData
  
  # Entre dans le dossier de téléchargement
  old_dir <- setwd(dl_folder)
  on.exit(unlink(name, recursive = TRUE)) # A la fin on retourne au bon dossier
  on.exit(setwd(old_dir), add = TRUE, after = TRUE) # A la fin on supprime le dossier
  capture.output(res, file = name_txt) # Création du résultat sous forme .txt
  setwd("../") # Je me mets dans le dossier parent du dossier à télécharger pour ziper
  
  # Renvoie le zip
  return(zip(zipfile = file, files = name))
  
}