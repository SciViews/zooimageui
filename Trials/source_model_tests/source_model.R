# Définition de la fonction dans le script
# Doit retourner un classifieur ou une erreur
my_model <- function(x, y, ...) {
  x + y
}
comment(my_model) <- "Description du modèle"

# Dans ton app Shiny
rm(my_model)
#source(<fichier>)
# Après source, tu dois avoir une fonction my_model avec les arguments voulus
if (!exists("my_model", mode = "function"))
  stop("my_model not found")
# Arguments
if (any(!c("x", "y") %in% names(formals(my_model))))
  stop("Incorrect arguments to my_model(), need 'x' and 'y'")

# Affichage des l'info sur le modèle
comment(my_model)

my_model_safe <- purrr::safely(my_model)
res <- my_model_safe(training_set, ...)

# Test de l'erreur:
if (!is.null(res$error))
  ....

# Classifieur dans res$result
my_classif <- res$result
