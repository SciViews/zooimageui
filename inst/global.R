#
# File for global pre-processing for the shiny app
#

# Libraries
library(ZooImageUI)

# Variables
data_folder_path <- Sys.getenv("ZOOIMAGE_DATA_DIR")
smpfiles <- list.files(paste0(data_folder_path,"/Samples"))
smps <- samples(smpfiles)

# Commands
old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)