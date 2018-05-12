fetchWikipediaIDs <- function(title){
  tryCatch({
    url <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=", URLencode(title), "&format=json")
    
    request <- httr::GET(url)
    
    data <- httr::content(request)
    cat(title, "\n")
    return(data$query$pages[[1]]$pageprops$wikibase_item)
  }, error = function(e){
    return(NULL)
  })
}

checkvalueExists <- function(value){
  if(is.null(value)){
    value <- NA
  } else {
    value <- value
  }
  cat(value, "\n")
  return(value)
}
#' Fetch Article Entities
#' 
#' @description This function is used to fetch one articles entities as seen when looking at WikiData. Example here:
#' https://www.wikidata.org/wiki/Q1299.
fetchArticleEntities <- function(articleId){
  url <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",articleId,"&format=json&languages=en")
  request <- httr::GET(url)
  data <- httr::content(request)
  statements <- data$entities[[1]]$claims
  
  tryCatch({
    nnid <- checkvalueExists(statements$P1263[[1]]$mainsnak$datavalue$value)
    
  }, error = function(e){
    return("Error::", e)
  })
  cat(nnid, "\n")
  return(nnid)
}

scrapePageCSS = function(url, css, table){
  webpage <- xml2::read_html(url)
  rank_data_html <- rvest::html_nodes(webpage, css)
  
  if(table){
    rank_data <- rvest::html_table(rank_data_html, fill = TRUE) 
  } else {
    rank_data <- rvest::html_text(rank_data_html) 
  }
  return(list(unlist(rank_data)))
}

fetchPersonDetails = function(nndbID){
  url = paste0("http://www.nndb.com/people/", nndbID)
  results = scrapePageCSS(url, "p", table = FALSE)
  data = head(results[[1]], 4)
  return(data)
}

ensureValuesForDataFrame = function(x){
  if(length(x) == 0){
    return("NA")
  } else {
    return(x)
  }
}

fetchAllPersonMetaData = function(uniquePeople){
  allPeopleData = NULL
  peopleWithNNID = filter(uniquePeople, nnddId != "NA")
  pb <- txtProgressBar(min = 0, max = nrow(peopleWithNNID), style = 3)
  for(i in 1:nrow(peopleWithNNID)){
    peopleData = NULL
    profileData = fetchPersonDetails(peopleWithNNID$nnddId[i])
    profileData = str_replace_all(profileData, "\\s\\[[0-9]\\]", "")
    profileData = str_replace_all(profileData, "\\[[0-9]\\]", "")
    profileData = str_split(profileData, "\n")
    profileData = lapply(profileData, function(e){str_split(e, "(?<=[a-z])(?=[A-Z])|(\n)")})
    profileData = flatten(profileData)
    profileData = flatten(profileData)
    profileData = lapply(profileData, function(x){str_split(x, "(?<=[0-9])(?=[A-Z])")})
    profileData = flatten(profileData)
    profileData = flatten(profileData)
    names = lapply(profileData, function(x){str_extract(x, "^(.*?):")})
    names = lapply(names, function(x){str_replace_na(x)})
    profileData = data.frame(profileData)
    names(profileData) = make.unique(as.character(names))
    profileData = select(profileData, -starts_with("NA", ignore.case = FALSE))
    
    peopleData$birthday = str_replace(profileData$`Born:`, "Born: ", "")
    peopleData$birthplace = str_replace(profileData$`Birthplace:`, "Birthplace: ", "")
    peopleData$gender = str_replace(profileData$`Gender:`, "Gender: ", "")
    peopleData$raceOrEthnicity = str_replace(profileData$`Race or Ethnicity:`, "Race or Ethnicity: ", "")
    peopleData$sexualOrienation = str_replace(profileData$`Sexual orientation:`, "Sexual orientation: ", "")
    peopleData$type = str_replace(profileData$`Occupation:`, "Occupation: ", "")
    
    peopleData$nnddId = peopleWithNNID$nnddId[i]
    peopleData = lapply(peopleData, ensureValuesForDataFrame)
    peopleData = data.frame(peopleData, stringsAsFactors = FALSE)
    allPeopleData = rbind(allPeopleData, peopleData)
    # cat(peopleData$nnddId, round(i/nrow(peopleWithNNID)*100,2), "%\n")
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(allPeopleData)
}

