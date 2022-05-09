#' Is Script Good Model ?
#' 
#' Try different parameters of a script to check if it is correct to make a model.
#'
#' @param models_folder_path Path to scripts/models.
#' @param selected_script Selected script.
#'
#' @return TRUE if the script is good or FALSE if it isn't. Plus a message explaining what is the problem or not.
#' @export
#'
#' @examples
#' folder_path <- system.file("example", package = "zooimageui")
#' is_correct <- is_script_good_model(folder_path, "ex1_mlRforest.R")
#' 
is_script_good_model <- function(models_folder_path, selected_script) {
  # Tout d'abord, test du nom du fichier : si ne ressemble pas à un script R, on arrête avec message d'erreur
  if ( selected_script == "No Model yet" || !grepl(".R", selected_script)) {
    res <- FALSE
    attr(res, "message") <- "Selected Model isn't an R script"
    return(res)
  } else {
    # Ensuite, si jamais le nom est bon :
    
    # Supprime la possible ancienne fonction et source le nouveau script
    if (exists("get_classif")) { rm(get_classif) }
    script_path <- fs::path(models_folder_path, selected_script)
    try(source(script_path , local = TRUE), silent = TRUE)
    
    # Test si une fonction get_classif est récupérée / existe
    if (!exists("get_classif")) {
      res <- FALSE
      attr(res, "message") <- "Selected Model doesn't return a function called get_classif"
      return(res)
      # Test si la fonction a un argument training_set
      # formals récupère les arguments de la fonction, names pour avoir les noms
      # et on vérifie si "training_set" n'est pas dedans. (le any n'a pas d'utilité ici)
    } else if (any(!"training_set" %in% names(formals(get_classif)))) {
      res <- FALSE
      attr(res, "message") <- "Selected Model doesn't have a training_set argument"
      return(res)
      # Enfin, si ça passe, alors le modèle est bon
    } else {
      res <- exists("get_classif")
      attr(res, "message") <- "Model : Ok"
      return(res)
    }
    
  }
}