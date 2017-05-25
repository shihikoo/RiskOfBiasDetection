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

fileListName <- "fileList.txt"

filefolder <- paste(datafolder,  "files/", sep="")
#-------- Read full information from the file ----------
myData <- read.delim(paste(datafolder, fileListName, sep=""),header = FALSE)

names(myData) <- "fileName"
#-------- Read file list and store text dataname in to myData$txtfilename ----------
# myData$txtfilename <- paste(filefolder, myData$fileName , ".txt",sep="")

myData$pdffilename <- paste(filefolder, myData$fileName , ".pdf",sep="")

myData$txtfilename <- sapply(myData$pdffilename, ConvertPdftoText, deletePdf = TRUE)

myData <- myData[which(myData$txtfilename != ""),] # only files with text 

myData$fulltext <-  readFullText(myData$txtfilename) # Read text from file into myData$fulltext 

myData$cleanText <- cleanText(myData$fulltext) # Clean up data text into myData$cleanText

#------- Risk Of Bias Identification  ------------
myData <- RiskOfBiasIdentification(myData)

#-------- output data -----------
outputData <- myData[,c('fileName', 'randomFlag','blindingFlag','sampleFlag')]

write.table(outputData, paste(outputFolder,  "DataFlagged.txt", sep=""), quote = F, sep = "\t", row.names = F)
