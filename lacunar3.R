# This script is written to do:
# 1. read filenames from a file list
# 2. read all files (text file) of all files on file list
# 3. apply regular expression on it and flag it
# 4. out put the data id, filenames and all flags

# ----- Load library ----------
source('core/textMining/regularExpressionMatching.R')
source('core/textMining/affiliationParser.R')
source('core/textMining/affiliationParserKeywords.R')

# ----- Functions ----------
#parse the first author address for author addresses downloaded from SyRF
parseFirstAuthorAddress <- function(authorAddresses)
{
  require(stringr)
  if(is.na(authorAddresses)) return(NA)
  if(grepl("\\\\r", authorAddresses))
  {
    authorAddresses <- str_match(authorAddresses,".+?(?=(\\\\r))")[1]
  }
  # if(grepl("[/;]", authorAddresses))
  # {
  #   authorAddresses <- str_match(authorAddresses,".+?(?=;)")[1]
  # }
  return(authorAddresses)
}

combine_emails <- function(emails)
{
  paste(emails, collapse = ", ")
}

# -------- Set up file folders, different for different projects ----------
projectFolder <- "lacunar3/"

datafolder <- paste(projectFolder, "data/",sep ="")

outputFolder <- paste(projectFolder, "output/",sep ="")

fileListName1 <- "fileList.txt"

fileListName2 <- "Lacunar quality update Studies.csv"

fileListName3 <- "lacunar3.xml"

filefolder <- paste(datafolder,  "files/", sep="")

# -------- Read full information from the file ----------
myData1 <- read.delim(paste(datafolder, fileListName1, sep=""))
myData1$StudyId <- tolower(myData1$StudyId)

myData2 <- read.csv(paste(datafolder, fileListName2, sep=""))
myData2$Study.Id <- tolower(myData2$Study.Id)

require(XML)
doc <- xmlTreeParse(paste(datafolder, fileListName3, sep=""), encoding="UTF-8")
myData3 <- list(
  endnoteid = xmlToDataFrame(nodes=getNodeSet(doc$doc$children$xml, "//rec-number"), stringsAsFactors = F),
  pmid = xmlToDataFrame(nodes=getNodeSet(doc$doc$children$xml, "//accession-num")),
  title = xmlToDataFrame(nodes=getNodeSet(doc$doc$children$xml, "//title/style"), stringsAsFactors = F),
  pubdate = xmlToDataFrame(nodes=getNodeSet(doc$doc$children$xml, "//edition/style"))
)

myData3 <- as.data.frame(myData3, stringsAsFactors=FALSE)

names(myData3) <- c("endnoteid","pmid","title","pubdate")

# -------- merge data from different files ----------
myData <- merge(myData1[,c("X_id" ,   "StudyId"   ,    "Abstract" ,       
                           "ProjectId"  ,  "AuthorAddress"  ,  "PdfRelativePath"  ,   "fileName",             
                           "Url"  ,  "SystematicSearchId"   , "Year"   ,  "PublicationName.Name"    ,     
                           "PublicationName.AlternateName" ,"ReferenceType"   ,   "DOI" )]
                , myData2[,c("Study.Id","Title","ScreeningInfo.Inclusion")]
                , by.x = "StudyId", by.y = "Study.Id", ignorein)

myData <- merge(myData, myData3, by.x = "Title", by.y = "title", ignorein)

# file.remove(paste(filefolder, myData$PdfRelativePath,".pdf",sep=""))

# -------- Read file list and store text dataname in to myData$txtfilename ----------
myData$pdffilename <- paste(filefolder, myData$fileName , ".pdf",sep="")

myData$txtfilename <- sapply(myData$pdffilename, ConvertPdftoText, deletePdf = TRUE)

index <- which(myData$txtfilename != "") # only files with txt file that has been converted from pdfs found from ENDNote 

myData$fulltext[index] <- readFullText(myData$txtfilename[index]) # Read text from file into myData$fulltext 

myData$cleanText[index] <- cleanText(myData$fulltext[index]) # Clean up data text into myData$cleanText

# ------- Risk Of Bias Identification  ------------
myData <- RiskOfBiasIdentification(myData)

myData[-index,c("randomFlag", "blindingFlag", "sampleFlag")] <- ""

# ------- Email, Country Identification  ------------
myData$AuthorAddress <- as.character(myData$AuthorAddress)

myData$Email <- sapply(myData$AuthorAddress, parse_email,USE.NAMES = F)

myData$Email <- sapply(myData$Email, paste, collapse = " ")

myData$FirstAuthorAddress <- as.character(sapply(myData$AuthorAddress, parseFirstAuthorAddress))

# myData[,c("affil_full_text","department","institution","email","zipcode","location","country")] <- ""

temp <- t(as.data.frame(sapply(myData$FirstAuthorAddress, parse_affil, USE.NAMES = F)))

myData$FirstAuthorInstitution <- temp[,3]
myData$FirstAuthorLocation <- temp[,6]
myData$FirstAuthorCountry <- temp[,7]

index <- which(myData$FirstAuthorCountry == "" & myData$Email != "")
myData$FirstAuthorCountry[index] <- identify_country_from_Email(myData$Email[index])

# -------- writes output data -----------
outputData <- myData[,c('X_id','StudyId','Title','AuthorAddress' ,'fileName','Year','Url'
                        ,"PublicationName.Name", "PublicationName.AlternateName" , "DOI"
                        , 'randomFlag','blindingFlag','sampleFlag','Email'
                        , 'FirstAuthorAddress', "FirstAuthorInstitution","FirstAuthorLocation","FirstAuthorCountry"
                        ,'ScreeningInfo.Inclusion','pubdate')]

write.table(outputData
            , paste(outputFolder,  "DataFlagged_lacunar3.txt", sep=""), quote = F, sep = "\t", row.names = F)

write.table(outputData[which(myData$ScreeningInfo.Inclusion == 1),]
            , paste(outputFolder,  "Included_DataFlagged_lacunar3.txt", sep=""), quote = F, sep = "\t", row.names = F)
