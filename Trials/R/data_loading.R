#
# Data loading for ZI_Test_App
# 

download.file(url = "https://filedn.com/lzGVgfOGxb6mHFQcRn9ueUb/zooimage/zooimage_examples.zip", destfile = "ZI_Test_App/zooimage_exemples.zip")
setwd("ZI_Test_App/")
unzip("zooimage_exemples.zip")
unlink("zooimage_exemples.zip")
setwd("../")

