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
    existing_ts_zip <- paste0(existing_ts, ".zip")
  # Si NULL : met un élément vide qui permettra d'entrer n'importe quel fichier
  } else {
    existing_ts <- ""
    existing_ts_zip <- ""
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
  
  # TEST : Input ne contient pas ".zip" ? Oui -> stop et renvoie FALSE + message
  if (!grepl("\\.zip$", upload_input$name)) {
    res <- FALSE
    attr(res, "error") <- "File doesn't have .zip ext. You have to compress your Training Set before uploading it."
    return(res)
  }
  
  # TEST : Nom Input déjà utilisé ? Oui -> stop et renvoie FALSE + message
  if (upload_input$name %in% existing_ts_zip) {
    res <- FALSE
    attr(res, "error") <- "Training Set's name already used. If you want to update it, you should first delete it (button beneath), and then upload it again. Otherwise, name your training set differently."
    return(res)
  }
  
  # On crée un dossier temporaire
  tmp_folder <- try(fs::dir_create("tmp"), silent = TRUE)
  if (inherits(tmp_folder, "try-error")) {
    res <- FALSE
    attr(res, "error") <- "Error by creating tmp folder. Data folder path may be wrong."
    return(res)
  }
  
  # On se met dans le dossier tmp
  goin_tmp <- try(setwd("tmp"), silent = TRUE)
  if (inherits(goin_tmp, "try-error")) {
    res <- FALSE
    attr(res, "error") <- "Error when trying to enter in tmp folder. Data folder path may be wrong."
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  
  # On essaie de récupérer l'Input sur le serveur dans le dossier tmp
  fc_res <- try(file.copy(upload_input$datapath, upload_input$name), silent = TRUE)
  # TEST : Erreur ? Stop et renvoie FALSE + message
  if (inherits(fc_res, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(fc_res, "condition")
    setwd(goin_tmp)
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  
  # On essaie de unziper le nouveau fichier sur le serveur dans le dossier tmp
  unzip_res <- try(unzip(upload_input$name), silent = TRUE)
  # TEST : Erreur ? Stop et renvoie FALSE + message
  if (inherits(unzip_res, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(unzip_res, "condition")
    setwd(goin_tmp)
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  
  # Variable liste des éléments dans le dossier
  all_files <- if (length(list.files()) > 0) {list.files()} else {""}
  
  # Vérification si ce qui est unzippé est correct
  # Premier test
  test1 <- length(all_files[!grepl("\\.zip$", all_files)]) == 1
  if (test1) {
    ts <- all_files[!grepl("\\.zip$", all_files)]
    ts_name <- all_files[!grepl("\\.zip$", all_files)]
  } else {
    res <- FALSE
    attr(res, "error") <- "Zip file's content is wrong. Please zip only the training set's folder."
    setwd(goin_tmp)
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  
  # Deuxième test
  inside_ts <- try(list.files(ts), silent = TRUE)
  if (inherits(inside_ts, "try-error")) {
    res <- FALSE
    attr(res, "error") <- "Zip file's content is wrong. Please zip only the training set's folder."
    setwd(goin_tmp)
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  test2 <- !any(!c("_", "alter", "Zooplankton other") %in% inside_ts)
  if (test2) {
    # Retour dans le bon dossier
    setwd(goin_tmp)
    # Supprime le dossier tmp
    unlink("tmp", recursive = TRUE)
    # Copie du zip dans le bon dossier
    file.copy(upload_input$datapath, upload_input$name)
    # Dézippage du zip
    unzip(upload_input$name)
    # Supprime le ZIP
    unlink(upload_input$name)
  } else {
    res <- FALSE
    attr(res, "error") <- "Zip file's content is wrong. Please zip only the training set's folder."
    setwd(goin_tmp)
    unlink("tmp", recursive = TRUE)
    return(res)
  }
  
  # Renvoie TRUE si tout a fonctionné
  res <- TRUE
  attr(res, "name") <- ts_name
  return(res)
}
