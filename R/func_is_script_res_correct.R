#' Is Script Good Results ?
#' 
#' Try different parameters of a script to check if it is correct to make results.
#'
#' @param results_folder_path Path to scripts for calculations.
#' @param selected_script Selected script.
#'
#' @return TRUE if the script is good or FALSE if it isn't. Plus a message explaining what is the problem or not.
#' @export
#'
#' @examples
#' folder_path <- system.file("example", package = "ZooImageUI")
#' is_correct <- is_script_good_results(folder_path, "ex1_zoo_abundance.R")
#' 
is_script_good_results <- function(results_folder_path, selected_script) {
  # Tout d'abord, test du nom du fichier : si ne ressemble pas à un script R, on arrête avec message d'erreur
  if ( selected_script == "No Calculations yet" || !grepl(".R", selected_script)) {
    res <- FALSE
    attr(res, "message") <- "Selected Calculation doesn't have .R extension"
    return(res)
  } else {
    # Ensuite, si jamais le nom est bon :
    
    # Supprime la possible ancienne fonction et source le nouveau script
    if (exists("get_results")) { rm(get_results) }
    script_path <- fs::path(results_folder_path, selected_script)
    try(source(script_path , local = TRUE), silent = TRUE)
    
    # Test si une fonction get_results est récupérée / existe
    if (!exists("get_results")) {
      res <- FALSE
      attr(res, "message") <- "Selected Calculation doesn't return a function called get_results"
      return(res)
      # Test si la fonction a un argument data
      # formals récupère les arguments de la fonction, names pour avoir les noms
      # et on vérifie si "data" n'est pas dedans. (le any n'a pas d'utilité ici)
    } else if (any(!c("data","data_folder_path", "Classif") %in% names(formals(get_results)))) {
      res <- FALSE
      attr(res, "message") <- "Selected Calculation doesn't have a data or data_folder_path or Classif argument"
      return(res)
      # Enfin, si ça passe, alors le modèle est bon
    } else {
      res <- exists("get_results")
      attr(res, "message") <- "Model : Ok"
      return(res)
    }
    
  }
}