# A "translation" of afflication parser repository on github from Python to R
# https://github.com/shihikoo/affiliation_parser/blob/master/affiliation_parser/parse.py
#------ Functions for parsing affiliation strings from PubMED---------
preprocess <- function(text)
{
  # require(tm)
  text_preprocess <- tolower(text)
  # text <- removePunctuation(text)
  # text_preprocess <- ' '.join(w_tokenizer.tokenize(text))
  return (text_preprocess)
}

replace_institution_abbr <- function(affil_text)
{
  #Replace abbreviation with full institution string
  for(university_list in UNIVERSITY_ABBR)
  {
    for(university in university_list)
    {
      if(grepl(university,affil_text))
      {
        affil_text <- gsub(university, university_list[[1]][1], affil_text)
        return(affil_text)
      }
    }  
  } 
  return(affil_text)
}

append_institution_city <- function(affil, location)
{ 
  # Append city to university that has multiple campuses if exist
  for(university_list in UNIVERSITY_MULTIPLE_CAMPUS)
  {
    if(grepl(university_list[1], tolower(affil)))
    {
      for(city in university_list[-1])
      {
        if(grepl(city,tolower(location)) && !grepl(city, tolower(affil)))
        {
          affil <- paste(affil , city, sep=" ")
          return(affil)
        }
      }
    }
  }
  return(affil)
}

clean_text <- function(affil_text){ 
  require(stringr)
  # Given affiliation text with abbreviation, clean that text
  affil_text <- gsub('Dept. ', 'Department ', affil_text)
  affil_text <- gsub('Surg. ', 'Sugery ', affil_text)
  affil_text <- gsub('Univ. ', 'University ', affil_text)
  affil_text <- gsub('2 ', ' ', affil_text)
  affil_text <- gsub('2. ', ' ', affil_text)
  affil_text <- gsub('[*]', ' ', affil_text)
  affil_text <- gsub(';', '', affil_text)
  affil_text <- gsub('E-mail:', '', affil_text)
  affil_text <- gsub('Electronic address:', '', affil_text)
  affil_text <- gsub('email:', '', affil_text)
  affil_text <- gsub('\t', ' ', affil_text)
  affil_text <- gsub('P.O. Box', '', affil_text)
  affil_text <- replace_institution_abbr(affil_text)
  return(str_trim(affil_text))
}

find_country <- function(location){
  # Find country from string
  location_lower <- tolower(location)
  for (country in COUNTRY){
    for(c in country){
      if(grepl(c,location_lower,perl=T))
      {
        return(country[1])
      }
    }
  }
  
  return("")
}

check_country <- function(affil_text){
  
  for(country in c('UK')){
    if (grepl(country, affil_text)){
      return('united kingdom')
    }
  }
  
  # Check if any states string from USA
  for(state in STATES){
    if(grepl(state,affil_text)){
      return ('united states of america')
    }
  }
  for(state in " USA."){
    if(grepl(state,affil_text)){
      return ('united states of america')
    }
  }
  return ('')
}

parse_email <- function(affil_text){
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  lastChar <- function(x){
    return(substrRight(x,1))
  }
  
  substrLeft <- function(x, n){
    substr(x, 0, n)
  }
  
  trimLastChar <- function(x){
    substrLeft(x,nchar(x)-1)
  }
  
  #   Find email from given string
  require(stringr)
  emails <- str_extract_all(affil_text,'[[:alnum:].-_]+@[[:alnum:].-]+') 
  
  if(length(emails[[1]]) > 0){ 
    
    for(ind in seq_along(emails)){ 
      email <- emails[ind]
      if(nchar(email) > 0) {  
        if(lastChar(email) == '.') {
          email <- trimLastChar(email)
        }
      }
      else{
        email <- ""
      }
      emails[ind] <- email
    }
  }
  else{   emails <- ""        
  }
  return(emails)
}

parse_zipcode <- function(affil_text){ 
  # Parse zip code from given affiliation text
  zip_code_group <- ''
  
  zip_code <- str_extract_all(affil_text,'([0-9]{5})([-])?([0-9]{4})?')
  
  if(length(zip_code[[1]]) == 0) zip_code <- str_extract_all(affil_text,'([0-9]{3})([-])?([0-9]{4})?')
  
  if(length(zip_code[[1]]) > 0){
    zip_code_group <- paste(zip_code, collapse = ' ')
  }
  return(zip_code_group)
}

parse_location <- function(location){ 
  require(stringr)
  # Parse location and country from affiliation string
  location <- sub('[/.]', '', location)
  country <- find_country(location)
  dict_location <- list('location'= str_trim(location),
                        'country'= str_trim(country))
  return(dict_location)
}

parse_domain <- function(email){
  require(stringr)
  
  email <- sub("[.]$","", email)
  
  domain <- str_match(email[1],".[[:alnum:]-]+$")
}

identify_country_from_Email <- function(email) {
  country <- list()
  
  if(length(email)==0) {return(country)} 
  
  domain <- parse_domain(email)
  
  if(is.na(domain)) {return("")}
  
  for(i in seq_along(COUNTRY_DOMAIN[,1]))
  { 
    countryDomain <- COUNTRY_DOMAIN[i,]
    
    if(str_trim(countryDomain$Name) == str_trim(domain)) {return(countryDomain$Entity)  }   
  }
  
  return("")
}

# main function for parse afflication. example:
# affil_text <- "Department of Health Science, Kochi Women's University, Kochi 780-8515, Japan. watanabe@cc.kochi-wu.ac.jp"
# parse_affil(affil_text)
parse_affil <- function(affil_full_text){ 
  # Parse affiliation string to institution and department
  if(is.na(affil_full_text) || length(affil_full_text)==0 || affil_full_text == ""|| affil_full_text == "NULL") return(NA)
  
  affil_full_text <- gsub('[(][^)]*[)]', '', affil_full_text)
  
  affil_text <- as.character(affil_full_text)
  
  require(stringi)
  # affil_text <- try(iconv(affil_text, stri_enc_mark(affil_text), "ASCII//TRANSLIT"))
  
  affil_text <- clean_text(affil_text)
  email <- parse_email(affil_text)
  zip_code <- parse_zipcode(affil_text)
  affil_text <- gsub(email, '', affil_text)
  affil_text <- gsub(zip_code, '', affil_text)
  
  affil_list <- strsplit(affil_text,', ')[[1]]
  affil <- list()
  location <- list()
  departments <- list()
  
  for(i in seq_along(affil_list)){ 
    a <- tolower(affil_list[i])
    for(ins in INSTITUTE){
      ins <- tolower(ins)
      if(grepl(ins, tolower(a),fixed=T) && (length(affil) == 0  || !grepl(a, affil,fixed=T))){
        affil<- rbind(affil,a)
        location <- affil_list[(i+1):length(affil_list)]
      }
    }
  }
  
  # remove unwanted from affliation list and location list
  pop_index <- list()
  for(i in affil){
    a <- affil[i]
    for(rm in REMOVE_INSTITUE){ 
      if(grepl(rm, tolower(a)) && !grepl('university', tolower(a))){ 
        pop_index <- rbind(pop_index,i)
      }
    }
  }
  if(length(pop_index) > 0){  
    affil <- affil[-pop_index]                              
  }
  
  pop_index <- list()
  for(i in location){
    l <- location[i]
    for(rm in DEPARMENT){
      if(grepl(rm,tolower(l))){ 
        pop_index<- rbind(pop_index,i)
      }
    }
  }
  if(length(pop_index) > 0){  
    location <- location[-pop_index]                              
  }
  
  affil <- paste(affil, collapse = ', ')
  location <- paste(location, collapse = ', ')
  if(location == ''){
    location <- tail(strsplit(affil_text,', ')[[1]], n= 1L)
  }
  location <- gsub('[(][^)]*[)]', '', location)
  
  for(i in seq_along(affil_list)){
    a <- affil_list[i]
    for(dep in DEPARMENT){ 
      if(grepl(dep,tolower(a),fixed=T) && (length(departments) == 0 || !grepl(a, departments,fixed=T)))
        departments <- rbind(departments, affil_list[i])
    }
  }
  department <- paste(departments, collapse = ', ')
  
  dict_location <- parse_location(location)
  
  affil <- append_institution_city(affil, dict_location['location'])
  
  dict_out <- data.frame(affil_full_text = as.character(affil_full_text),
                         department = department,
                         institution = affil,
                         email  = email,
                         zipcode = zip_code,
                         location = as.character(dict_location["location"]),
                         country =  as.character(dict_location["country"]), stringsAsFactors=FALSE
  )
  
  for(icountry in seq_along(dict_out$country[[1]])){ 
    country <- dict_out$country[[1]][icountry]

    if(country  == ""){
      country <- identify_country_from_Email(email[[1]][icountry])
    } #check email
    if(country == ""){
      country <- check_country(affil_text[[1]][icountry] )
    } # check country
    dict_out$country[[1]][icountry] <- country
  }
  
  dict_out$country[[1]] <- dict_out$country[[1]][1]
  
  return(as.vector(dict_out,mode="character"))
}

parse_country <- function(affil_full_text){ 
  # Parse affiliation string to institution and department
  if(affil_full_text == "") return("")
  affil_text <- as.character(affil_full_text)
  require(stringi)
  affil_text <- try(iconv(affil_text, stri_enc_mark(affil_text), "ASCII//TRANSLIT"))
  affil_text <- clean_text(affil_text)
  
  email <- parse_email(affil_text)
  
  affil_text <- gsub(email, '', affil_text)
  affil_text <- gsub(parse_zipcode(affil_text), '', affil_text)
  
  affil_list <- strsplit(affil_text,', ')[[1]]
  
  location <- list()
  
  for(i in seq_along(affil_list)){ 
    a <- affil_list[i]
    for(ins in INSTITUTE){     
      if(grepl(ins, tolower(a))){
        location <- affil_list[(i+1):length(affil_list)]
      }
    }
  }
  
  # remove unwanted from affliation list and location list
  pop_index <- list()
  for(i in location){
    l <- location[i]
    for(rm in DEPARMENT){
      if(grepl(rm,tolower(l))){ 
        pop_index<- rbind(pop_index,i)
      }
    }
  }
  if(length(pop_index) > 0){  
    location <- location[-pop_index]                              
  }
  
  location <- paste(location, collapse = ', ')
  if(location == ''){
    location <- tail(strsplit(affil_text,', ')[[1]], n= 1L)
  }
  location <- gsub('[(][^)]*[)]', '', location)
  location <- paste(" ", location, sep = '')
  
  dict_out <- parse_location(location) 
  if(dict_out['country'][[1]] == ''){
    dict_out['country'] <- identify_country_from_Email(email) 
  }
  if(dict_out['country'][[1]] == ''){
    dict_out['country'] <- check_country(affil_text) 
  }
  
  return(dict_out$country[1])
}



