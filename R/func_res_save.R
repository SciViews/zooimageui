#' Save the Results
#' 
#' Save Results of ZooImageUI in .txt and .RData files.
#'
#' @param data_folder_path Folder containing all the data for ZooImageUI.
#' @param name Name of the files for saving.
#' @param res Results to save.
#'
#' @return TRUE if the save worked, FALSE if it didn't. Plus a message if there is an error.
#' @export
#'
#' @examples
#' # To Do...
res_save <- function(data_folder_path, name, res) {
  
  # Enlèvement des caractères spéciaux
  name <- stringr::str_replace_all( name, "[^[:alnum:]]", "")
  name_txt <- paste0(name,".txt") # Nom pour le fichier .txt
  
  # Test que le résultat ne soit pas NULL
  if (is.null(res)) {
    res <- FALSE
    attr(res, "error") <- "No Results yet"
    return(res)
  }
  
  # Pas de test car le chemin est testé dans l'app via un req() de shiny
  # Création du chemin du dossier de sauvegardes
  save_folder_path <- fs::path(data_folder_path, "Saved_Results")
  # Création du dossier si il n'existe pas (pour sauvergade)
  if (!"Saved_Results" %in% list.files(data_folder_path)) {
    fs::dir_create(save_folder_path)
  }
  
  # Nom pour le .RData
  name <- paste0(name,".RData")
  # Test si le nom existe déjà
  if (!name %in% list.files(save_folder_path)) {
    # Chemin pour sauver le .RData
    rdata_path <- fs::path(save_folder_path,name)
    save(res, file = rdata_path)
    # Pour capturer en .txt
    old_dir <- setwd(save_folder_path)
    on.exit(setwd(old_dir))
    capture.output(res, file = name_txt)
    return(TRUE)
  } else {
    res <- FALSE
    attr(res, "error") <- "Name already used"
    return(res)
  }
  
}