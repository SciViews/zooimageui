#
# File for global pre-processing for the shiny app
#

# Libraries
library(ZooImageUI)

# Variables
options(
  data_folder_path = Sys.getenv("ZOOIMAGE_DATA_DIR")
)

# Commands
old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)