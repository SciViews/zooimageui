#
# Test data of ZooImage downloading
#
# Once this script has been executed, you can move everything that is inside the www folder into a/the data folder
# 

download.file(url = "https://filedn.com/lzGVgfOGxb6mHFQcRn9ueUb/zooimage/zooimage_examples.zip", destfile = "./zooimage_exemples.zip")
unzip("zooimage_exemples.zip")
unlink("zooimage_exemples.zip")
