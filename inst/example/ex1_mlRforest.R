# example of script for ZooImageUI Models
#
# function's name and get_classif's arguments cannot change
#
library(mlearning)
get_classif <- function(training_set) {
  ZIClass(Class ~ ., training_set, method = "mlRforest", calc.vars = calcVars,
          ntree = 200, cv.k = 10)
}
comment(get_classif) <- "mlRforest 1 : Model using the mlRforest method and
                        default calculated variables of ZooImage."
