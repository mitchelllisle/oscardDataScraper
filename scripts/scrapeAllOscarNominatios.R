suppressMessages(library(httr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(highcharter))
suppressMessages(library(dime))
suppressMessages(library(xml2))
suppressMessages(library(rvest))
suppressMessages(library(crayon))
suppressMessages(source("R/processing.R"))

cat(magenta("###### Generating URLs for Scraping ######\n"))
urls = generateURLs()

allMovieNominationsEver = NULL

cat(magenta("###### Scraping... ######\n"))
for(i in 1:nrow(urls)){
    tryCatch({
    awards = getAwardNames(urls$url[i])
    winners = getWinners(urls$url[i], awards)
    nominations = getNominations(urls$url[i], awards)
    ceremonyData = rbind(winners, nominations) %>% arrange(awards)
    ceremonyData$year = urls$year[i]
    ceremonyData$url = urls$url[i]
    allMovieNominationsEver = rbind(allMovieNominationsEver, ceremonyData)  
    cat(green(urls$url[i], round(i / nrow(urls) * 100,2), "%\n"))
    }, error = function(e){
      cat(red(urls$url[i], round(i / nrow(urls) * 100,2), "%\n"))
    })
}  
cat(magenta("###### Generating URLs for Scraping ######\n"))
allMovieNominationsEver = suppressWarnings(allMovieNominationsEver %>% separate(person, into = c("person", "detail"), "–", extra = "merge"))

cat(magenta("###### Joining with Movie Metadata ######\n"))
awardData = read.csv("../data/awards.csv", stringsAsFactors = FALSE)
allMovieNominationsEver = left_join(allMovieNominationsEver, awardData, by = c("awards" = "Award"))
allMovieNominationsEver$person = str_replace_all(allMovieNominationsEver$person, "\\s$", "")
write.csv(allMovieNominationsEver, "data/oscars.csv")

cat(green("###################\n"))
cat(green("###### Done! ######\n"))
cat(green("###################\n"))