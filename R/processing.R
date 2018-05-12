getAwardNames = function(url){
  awardsFirstAttempt = data.frame(awards = scrapePageCSS(urls$url[i], ".wikitable td b", table = FALSE)[[1]], stringsAsFactors = FALSE)
  awardsFirstAttempt_filtered = filter(awardsFirstAttempt, grepl("^Best", awards), awards != "Best Boy", awards != "Best Boy – Ira Wohl")
  awardsSecondAttempt = data.frame(awards = scrapePageCSS(urls$url[i], ".wikitable th a", table = FALSE)[[1]], stringsAsFactors = FALSE)
  awardsSecondAttempt_filtered = filter(awardsSecondAttempt, grepl("^Best", awards), awards != "Best Boy", awards != "Best Boy – Ira Wohl")
  awardsThirdAttempt = filter(awardsFirstAttempt, grepl("^Outstanding", awards))
  awards = rbind(awardsThirdAttempt, awardsFirstAttempt_filtered, awardsSecondAttempt_filtered)  
  return(awards)
}

getWinners = function(url, awards){
  winners = scrapePageCSS(url, ".wikitable td ul b", table = FALSE)
  winners = data.frame(person = winners[[1]], stringsAsFactors = FALSE)
  winners = cbind.fill(awards, winners)
  # winners = data.frame(awards = awards, person = winners)
  winners$result = "winner"
  return(winners)
}

getNominations = function(url, awards){
  nominations = scrapePageCSS(url, ".wikitable td ul ul", table = FALSE)
  nominations = nominations[[1]]
  nominations = unique(nominations)
  # [seq(1, length(nominations[[1]]), 2)]
  # nominations = cbind.fill(awards, nominations)
  # names(nominations) = c("awards", "person")
  if(url == "https://en.wikipedia.org/wiki/50th_Academy_Awards"){
    awards = filter(awards, awards != "Best Sound Effects Editing")
  } else if(url == "https://en.wikipedia.org/wiki/26th_Academy_Awards"){
    awards = filter(awards, awards != "Best Visual Effects")
  } else if(url == "https://en.wikipedia.org/wiki/25th_Academy_Awards"){
    awards = filter(awards, awards != "Best Visual Effects")
  } else if(url == "https://en.wikipedia.org/wiki/24th_Academy_Awards"){
    awards = filter(awards, awards != "Best Special Effects")
  } else if(url == "https://en.wikipedia.org/wiki/22nd_Academy_Awards"){
    nominations[26] = "Tulsa – Walter Wagner; Eagle Lion"
  } else if(url == "https://en.wikipedia.org/wiki/6th_Academy_Awards"){
    awards = filter(awards, awards != "Best Short Subject, Cartoon")
  }
  nominations = data.frame(awards = awards, person = nominations)
  nominations$person = str_split(nominations$person, "(\n)")
  nominations = unnest(nominations) 
  nominations = filter(nominations, person != "")
  nominations$result = "nominated"
  return(nominations)
}

getOrdinal = function(number){
  strtail = function(s, n=1) {
    if(n < 0) substring(s, 1-n)
    else substring(s, nchar(s)-n+1)
  }
  
  tmp <- strtail(as.character(number), 2)
  if (tmp %in% c('1', paste(c(0, 2:9), 1, sep=""))) tmp.suffix <- "st"
  if (tmp %in% c('2', paste(c(0, 2:9), 2, sep=""))) tmp.suffix <- "nd"
  if (tmp %in% c('3', paste(c(0, 2:9), 3, sep=""))) tmp.suffix <- "rd"
  if (tmp %in% c('11', '12', '13')) tmp.suffix <- "th"
  if (tmp %in% c('4', paste(0:9, 4, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('5', paste(0:9, 5, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('6', paste(0:9, 6, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('7', paste(0:9, 7, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('8', paste(0:9, 8, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('9', paste(0:9, 9, sep=""))) tmp.suffix <- "th"
  if (tmp %in% c('0', paste(0:9, 0, sep=""))) tmp.suffix <- "th"
  
  return(paste0(number, tmp.suffix))
}

generateURLs = function(){
  allAcademyAwardURLs = NULL
  firstYear = 1928
  for(i in 1:90){
    year = getOrdinal(i)
    academyAwardYear = data.frame(url = paste0("https://en.wikipedia.org/wiki/", year, "_Academy_Awards"), year = firstYear + i, stringsAsFactors = FALSE)
    allAcademyAwardURLs = rbind(academyAwardYear, allAcademyAwardURLs)
  }  
  return(allAcademyAwardURLs)
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

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
  data.frame(rbind(x, matrix(, n-nrow(x), ncol(x))))))
}
