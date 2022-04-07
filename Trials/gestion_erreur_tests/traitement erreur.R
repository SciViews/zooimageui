install_trainingset <- function(file) {
  # popotte
  # En cas d'erreur

  ret <- try(unzip(...), silent = TRUE)
  if (inherits(ret, "try-error")) {
    res <- FALSE
    attr(res, "error") <- attr(ret, "condition")
    return(res)
  } else {
    # Si tout va bien
    return(TRUE)
  }

}


# Server

res <- install_trainingset(file)
if (!res) {
  # Traitement pour afficher le message d'erreur
  # Afficher l'erreur au bon endroit
  # App shiny avec une zone de texte qui s'appelle error_msg
  output$error_msg <- attr(res, "error")
}
