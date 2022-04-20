#' Upload TS
#' 
#' Get the zipped training set from upload, unzip it, unlink the zip file.
#'
#' @param upload_input Shiny input file
#' @param ts_folder_path Path to folder to put ts in
#' @param existing_ts List of the existing training sets
#'
#' @return TRUE if it worked, FALSE if an error occured
#' @export
#'
#' @examples
#' # Need a reactive context
upload_ts <- function(upload_input, ts_folder_path, existing_ts) {
  
  # TEST : Input NULL ? oui -> stop et renvoie FALSE + message
  if (is.null(upload_input)) {
    res <- FALSE
    attr(res, "error") <- "No file uploaded, please retry."
    return(res)
  }
  
  # Variable : Training Sets existants
  # Si pas NULL : ajoute l'extension .zip pour vérifier par rapport à l'input
  if (!is.null(existing_ts)) {
    existing_ts <- paste0(existing_ts, ".zip")
  # Si NULL : met un élément vide qui permettra d'entrer n'importe quel fichier
  } else {
    existing_ts <- ""
  }
  
  # Option : utiliser les warnings comme erreur
  op <- options(warn = 2)
  
  # On essaie de se mettre dans le dossier de travail
  oldir <- try(setwd(ts_folder_path), silent = TRUE)
  # Si ça plante, on remet l'ancien dossier
  on.exit(setwd(oldir))
  
  # Remet l'option de warning
  on.exit(options(op), add = TRUE, after = TRUE)
  
  # TEST : Erreur ? Stop et renvoie FALSE + message
  if (inherits(oldir, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(oldir, "condition") # Pourrait être plus clair
    return(res)
  }
  
  # TEST : Input contient ".zip" ? Oui -> stop et renvoie FALSE + message
  if (!grepl("\\.zip$", upload_input$name)) {
    res <- FALSE
    attr(res, "error") <- "File doesn't have .zip ext. You have to compress your Training Set before uploading it."
    return(res)
  }
  
  # TEST : Nom Input déjà utilisé ? Oui -> stop et renvoie FALSE + message
  if (upload_input$name %in% existing_ts) {
    res <- FALSE
    attr(res, "error") <- "Training Set's name already used. If you want to update it, you should first delete it (button beneath), and then upload it again. Otherwise, name your training set differently."
    return(res)
  }
  
  # On essaie de récupérer l'Input sur le serveur
  fc_res <- try(file.copy(upload_input$datapath, upload_input$name), silent = TRUE)
  # TEST : Erreur ? Stop et renvoie FALSE + message
  if (inherits(fc_res, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(fc_res, "condition")
    return(res)
  }
  
  # On essaie de unziper le nouveau fichier sur le serveur
  unzip_res <- try(unzip(upload_input$name), silent = TRUE)
  # TEST : Erreur ? Stop et renvoie FALSE + message
  if (inherits(unzip_res, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(unzip_res, "condition")
    return(res)
  }
  
  # Enfin si tout s'est bien passé : On supprime le ZIP
  unlink(upload_input$name)
  
  # Renvoie TRUE si tout a fonctionné
  return(TRUE)
}
