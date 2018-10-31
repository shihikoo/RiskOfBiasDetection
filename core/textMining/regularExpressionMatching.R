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
  randomRegex <- '((?<!not )(\brandom(ly)?.*(assign|divid|treat|split|determin|receiv|alloc|subdiv|categor))|((?<!not )(assign|divid|treat|split|determin|receiv|alloc|subdiv|categor).(at)?.*random))|(?<!not )(\brandomi[sz]).*(in|to)|were randomi[sz]ed'
  blindingRegex <- '(?<!not )(((\bblind (as )?to)|blind(ed|ly)|blind.(manner|eval|observ|investigat|rate|rati|experiment|research|test|quantif|cod|with respect to|method|analy[s|z]?|condition)?|((perform|count|conduct|genotyp|cod|test)e?d?.(blind))|(\'blind\')|(\""blind\""))|((observer(s)?|experimenter(s)?|researcher(s)?|tester(s)?|rater(s)?|person(s)?|investigator(s)?|operator(s)?) (unaware|not aware|unaware|without awareness)?( of)?))|((unaware|not aware|unaware|without awareness)( of )(the )?(pre)?(treatment|genotyp|group|drug|experimental condition))|(naive to the identity of)'
  sampleRegex <- '(?<!not )(\bpower|would be needed if the null hypothesis|(to estimate the|used to determine the|based on these assumptions a) sample size|(the study|gave appropriate|increase the|descrease the|the planned|statistical) power|a power of|(?<!failed to )achieve statistical significance|was powered at|minimum number of (mice|rats|animals|subjects|patients) were used|\bchance of|\to detect a|minimum clinically worthwhile effect|power of more than|\to reject the null hypothesis|effectively powered|power and statistical analysis|are required per group|per group were required|minimum number required to give|required to give statistically valid results|through a priori calculation)'
  
  # Apply regular expression to newdata$cleanText and then store into flag
  myData$randomFlag <- regularExpressionIdentification(myData$cleanText, randomRegex)
  
  myData$blindingFlag <- regularExpressionIdentification(myData$cleanText, blindingRegex)
  
  myData$sampleFlag <- regularExpressionIdentification(myData$cleanText, sampleRegex)
  
  return(myData)
}
