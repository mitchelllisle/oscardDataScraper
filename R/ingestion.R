getAllMoviesIMDBInfo <- function(movies){
  allMovies <- NULL
  for(i in 1:nrow(movies)){
    cat(paste0(movies$movie[i], ": ", round(i/nrow(movies)*100,2), "%\n"))
    movieInfo <- data.frame(getIMDBInfo(movies$movie[i]))
    allMovies <- plyr::rbind.fill(allMovies, movieInfo)
  }
  return(allMovies)
}


getIMDBInfo <- function(movie){
  tryCatch({
    url <- paste0("http://www.omdbapi.com/?t=",URLencode(as.character(movie)),"&apikey=",Sys.getenv("omdbApiKey"))
    
    request <- GET(url)
    
    response <- content(request)
    
    data.frame(response)
  }, error = function(e){
    return(NULL)
  })
}
