require(rvest)
require(magrittr)
require(stringi)
#Getting list of NBA seasons
url <- 'https://www.basketball-reference.com/leagues/'
webpage <- read_html(url)
seasons <- webpage %>% html_nodes('ul') %>%
  extract2(5) %>%
  html_nodes('a')
#Getting link names for all the months per seasons since they B-R started collecting play-by-play data
pbpindex <- which(grepl('NBA_2001', seasons))
months <- c("october", "november", "december", "january", "february", "march", "april", "may", "june")
#Function below adds to the play-by-play url for each game to pbpurls
pbpurls <- c()
errors <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(errors) <- c("Error", "Link")
for (i in 1:pbpindex) {
  year = substr(seasons[[i]], 10, 26)
  for (j in 1:length(months)) {
    seasonurl = paste("https://www.basketball-reference.com", year, "_games-", months[j], ".html", sep="")
    #Refer to https://www.basketball-reference.com/leagues/NBA_2012_games-october.html - if it starts with '404' you can skip
    monthgames <- tryCatch(read_html(seasonurl),
             error = function(e) {
               e #One day put some good error handling here
             })
    if(inherits(monthgames, "error")) {
      errors[nrow(errors) + 1, 1] = as.character(monthgames)
      errors[nrow(errors), 2] = seasonurl
      next
    }
    gamescores <- monthgames %>% html_nodes('table a')
    #Will be concatenated to pbpurls after k-loop
    for (k in 1:length(gamescores)) {
      if (k %% 4 == 0) {
        gameurl <- stringi::stri_extract_all_regex(as.character(gamescores[[k]]), '(?<=").*?(?=")')
        pbpurls[length(pbpurls) + 1] = paste("https://www.basketball-reference.com", substr(gameurl, 1, 11), "pbp", substr(gameurl, 11, nchar(gameurl) + 1), sep="")
      }
    }
  }
}  

write.csv(pbpurls, "pbpurls.csv")

#Getting the actual play-by-play data  
url <- 'https://www.basketball-reference.com/boxscores/pbp/201710170CLE.html'
webpage <- read_html(url)
  
pbp <- html_nodes(webpage, '.overthrow table')
pbptable <- html_table(pbp)[[1]] #The whole table is its own entry in a list

