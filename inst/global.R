#
# File for global pre-processing for the shiny app
#

library(zooimageui)

if (exists("work_dirs")) { try(rm(work_dirs), silent = TRUE) }
if ("work_dirs.RData" %in% list.files()) {
  try(load(file = "work_dirs.RData"), silent = TRUE)
  worked <- TRUE
} else {
  worked <- FALSE
}

# Variables
if (!worked) {
  options(
    data_folder_path = Sys.getenv("ZOOIMAGE_DATA_DIR")
  )
} else if (exists("work_dirs")) {
  options(
    data_folder_path = work_dirs
  )
  try(rm(work_dirs))
}

# Commands
old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)