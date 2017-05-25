# Functions for regular expression Identification for data frame

# ConvertPdftoText
ConvertPdftoText <- function(pdfLink, ignoreExistingTextFile = FALSE, deletePdf = FALSE)
{
  if(grepl('.pdf', pdfLink)){ 
  
  txtLink <- sub('.pdf', '.txt', pdfLink)
  
  if(file.exists(txtLink) && file.exists(pdfLink) && deletePdf) 
  {
      file.remove(pdfLink)
  }
  
  if(file.exists(txtLink) && !ignoreExistingTextFile) 
  {
    return(txtLink)
  }
  
  exe <- '"pdftotext"'
  if(file.exists(pdfLink))
  {
    com <- paste(exe, paste('"',pdfLink,'"',sep=''))
    system(com, wait = T)
    if(file.exists(txtLink)) 
    {
      if(deletePdf) {
        file.remove(pdfLink)
      }
      return(txtLink)
    }
    else 
    {
      return("")
    }
  }
  }
  return("")
}

# Read full text
readFullText <- function(txtfilename){
  
  readinFulltext <- function(inputarray){  
    return (readChar(inputarray[1], inputarray[2])) 
  }
  
  #--------- Calculate file size  -------------------------------------------------------------------------------
  fulltextsize <- file.info(txtfilename)$size
  
  #--------- Read text from file into myData$fulltext -----------------------------------------------------------
  fulltext <- apply(cbind(txtfilename = txtfilename, fulltextsize = fulltextsize), 1, readinFulltext)
  
  return(fulltext)
}

# Identify any pattern (regular expression) in fullText (array)
regularExpressionIdentification <- function(fullText, pattern){
  
  greplFunction <- function(text, pattern=""){  
    return(grepl(pattern, text, ignore.case = T, perl = T))
  }
  
  return(sapply(fullText, greplFunction, pattern = pattern))
}

# Clean up text from pdfExtractor new lines, dashlines
cleanText <- function(text, pdfExtractor=T, newLine=T, dashLine=T){
  if(pdfExtractor==T) text <- gsub("[(][a-zA-Z0-9. ]*PDF Extractor SDK[a-zA-Z0-9 .]*[)]","", text, perl=T)
  
  if(newLine==T) text <- gsub("\r|\n|\f"," ", text, perl=T)
  
  if(dashLine==T) text <- gsub("-","", text, perl=T)
  
  return(text)
}

# Identify Risk of Bias in myData$cleanText
RiskOfBiasIdentification <- function(myData){
  
  # coreFolder <- "../core/textMining/"
  # 
  # ROBregularExpressions <- read.delim(paste(coreFolder, "randomizatinRegularExpression.txt", sep=""), header = TRUE)
  # 
  # for(regularExpression in ROBregularExpressions$regular.expression)
  # {
  #   if(exists("foo")) foo <- rbind(foo, regularExpressionIdentification(text, regularExpression))
  #   else foo <- regularExpressionIdentification(text, regularExpression)
  # }
  
  # Set regular expression  
  randomRegex <- ''
  blindingRegex <- ''
  sampleRegex <- ''
  
  # Apply regular expression to newdata$cleanText and then store into flag
  myData$randomFlag <- regularExpressionIdentification(myData$cleanText, randomRegex)
  
  myData$blindingFlag <- regularExpressionIdentification(myData$cleanText, blindingRegex)
  
  myData$sampleFlag <- regularExpressionIdentification(myData$cleanText, sampleRegex)
  
  return(myData)
}