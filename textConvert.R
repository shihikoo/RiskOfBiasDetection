# This script is written to do:
# 1. read filenames from a file list
# 2. read all files (text file) of all files on file list
# 3. apply regular expression on it and flag it
# 4. out put the data id, filenames and all flags

#-----load library
source('core/textMining/regularExpressionMatching.R')

#-------- Set up file folders, different for different projects ----------
projectFolder <- "mcao/"

datafolder <- paste(projectFolder, "data/",sep ="")

outputFolder <- paste(projectFolder, "output/",sep ="")

fileList <- list.files(path = "./.", full.names = TRUE, recursive = TRUE)

txtFileName <- sapply(fileList, ConvertPdftoText, deletePdf = TRUE)


