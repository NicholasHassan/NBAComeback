require(rvest)
require(magrittr)
require(stringi)
setwd("~/Downloads/NBA Comeback data")

#Here's an example of the kind of page we'll be scraping from: https://www.basketball-reference.com/boxscores/200106150PHI.html
#basketball-reference has one for each game. We gotta get the URL for each game from their game list for each season, and scrape that big list of URLs

#Getting list of NBA seasons
url <- 'https://www.basketball-reference.com/leagues/'
webpage <- read_html(url)
seasons <- webpage %>% html_nodes('ul') %>%
  extract2(5) %>%
  html_nodes('a')
#B-R only started collecting PBP data in the 2000-2001 season
pbpindex <- which(grepl('NBA_2001', seasons))
months <- c("october", "november", "december", "january", "february", "march", "april", "may", "june")
#Code below adds the play-by-play url for each game to pbpurls
#Not well optimized, but only takes 1-2 minutes
pbpurls <- c()
errors <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(errors) <- c("Error", "Link")
start = Sys.time()
for (i in 1:pbpindex) {
  year = substr(seasons[[i]], 10, 26)
  for (j in 1:length(months)) {
    seasonurl = paste("https://www.basketball-reference.com", year, "_games-", months[j], ".html", sep="")
    #Some seasons don't have games in certain months, so you get an error unless run it in a tryCatch
    monthgames <- tryCatch(read_html(seasonurl),
             error = function(e) {
               e #One day put some good error handling here
             })
    if (inherits(monthgames, "error")) {
      errors[nrow(errors) + 1, 1] = as.character(monthgames)
      errors[nrow(errors), 2] = seasonurl
      next
    }
    gamescores <- monthgames %>% html_nodes('table a')
    #Will be concatenated to pbpurls with each k-loop
    for (k in 1:length(gamescores)) {
      if (k %% 4 == 0) {
        #This gets the URL between the quotation marks
        gameurl <- stringi::stri_extract_all_regex(as.character(gamescores[[k]]), '(?<=").*?(?=")')
        pbpurls[length(pbpurls) + 1] = paste("https://www.basketball-reference.com", substr(gameurl, 1, 11), "pbp", substr(gameurl, 11, nchar(gameurl) + 1), sep="")
      }
    }
  }
}  
end = Sys.time()
print(start - end)

#You can save it for later
write.csv(pbpurls, "pbpurls.csv")
pbpurls <- read.csv("pbpurls.csv", stringsAsFactors = FALSE)
pbpurls <- pbpurls[,2]

#Getting the actual play-by-play data  
#NOTE: I have not fully run this code yet. It takes >15 hours....

#We're gonna fill this list with select entries from each play-by-play table and then do.call(rbind, prebindgames) to make the full table
#I'm vectorizing! Creating an empty list and then changing values is apparently way faster than appending
#5M is a safe number based on there being ~22k games that usually don't go above 150 score changes
prebindgames <- rep(list(NA), 5000000)

#Will keep track of the list index we're replacing at
count <- 0
testcount <- 0

#Here we go! 15 hours!
start_time <- Sys.time()
for (n in 1:10) {
  url <- pbpurls[n]
  
  #Random shit just to see progress
  print(url)
  testcount = testcount + 1
  print(testcount)
  
  webpage <- read_html(url)
  pbp <- html_nodes(webpage, '.overthrow table')
  
  #https://www.basketball-reference.com/boxscores/pbp/201803040SAC.html has a random blank cell at 1 second remaining in the fourth!? So we need to do fill = TRUE
  #This html_table function appears to be the longest step, taking >2 seconds per loop
  #html_table returns a list of all the text between <table> tags. [[1]] gets the actual table
  #Hometeam is column 5
  pbptable <- html_table(pbp, header = FALSE, fill = TRUE)[[1]] 
  
  #We're only going to record an entry if someone scores, otherwise we'll get repeats of different times with the same score
  scorecheck <- c("", "")
  
  #Time resets to 12:00 after every quarter, maxtime will be used to keep track of what quarter it is
  maxtime = as.difftime(0, units = "secs")

  for (i in 1:nrow(pbptable)) {
    #Score is kept in "score-score" format in column 4, so we split by "-" to get each team's score
    #Note: Some players have a hypen in their name! Gonna have to clean this up later
    splitscore <- strsplit(pbptable$X4[i], "-")
    #If it only has length 1, then it didn't have a hyphen. It might mark the start of a new quarter though.
    if (length(splitscore[[1]]) == 1) {
      if (grepl("^\\w{3} Q", splitscore)) {
        maxtime = maxtime + as.difftime(720, units = "secs")
      }
      if (grepl("^\\w{3} OT", splitscore)) {
        maxtime = maxtime + as.difftime(300, units = "secs")
      }
    }
    else {
      #Checks if the score is the same as the previous one
      if (!all(splitscore[[1]] == scorecheck)) {
        count = count + 1
        scorecheck = splitscore[[1]]
        #Creating a data.frame in the format we want the final table to be. It's only one row but we'll do a huge rbind at the end
        prebindgames[[count]] = data.frame(Time = maxtime - as.difftime(pbptable$X1[i], format = "%M:%OS"), 
                                            HScore = splitscore[[1]][2], 
                                            AScore = splitscore[[1]][1],
                                            HTeam = pbptable$X6[2],
                                            ATeam = pbptable$X2[2],
                                            GameID = substr(url, 52, 63))
      }
    }
  }
}
end_time <- Sys.time()
print(end_time - start_time)

#Binding all the list entries from prebindgames into one data frame
start_time <- Sys.time()
allgamespbp <- do.call(rbind, prebindgames[!is.na(prebindgames)])
end_time <- Sys.time()
print(end_time - start_time)
