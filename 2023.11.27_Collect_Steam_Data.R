rm(list = ls())

### load all necessary packages ###
library(httr)
library(jsonlite)
library(lubridate)

### TASK 2 ###
### Task 2.1: Collect data about the top 100 games played on Steam###

url <- "https://steamspy.com/api.php?request=top100forever"

### Perform a HTTP request ###
top100.request <- GET(url)

### Convert the binary content of top100.request into string ### 
### Store it in an object called top100.raw ###
top100.raw <- content(top100.request, as = "text", encoding = "UTF-8")

### Transform the string into a list by using the function "fromJSON"### 
### Save the list in an object, top100.list ###
top100.list <- fromJSON(top100.raw)

### Obtain the data frame for the top 100 Steam video games ###
top100.list1 <- lapply(top100.list, as.data.frame)
top100 <- do.call ("rbind",top100.list1)

### Transform the variables price, initialprice, and discount into numeric variables ### 
### Multiply price and initialprice by 0.01 ###
top100$price <- as.integer(top100$price)*0.1
top100$initialprice <- as.integer(top100$initialprice)*0.1
top100$discount <- as.integer(top100$discount)

### Save the data frame top100 in 01_data ###
save(top100, file = "C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/top100.RData")


# -------------------------------------------------------------------------------------------------##
### Task 2.2: Download the game details of Steam video game assigned to team_02 ###
### Team_02 was assigned the game_id 271590 ###

### Store the string in an object called details.url ###
details.url <- "http://store.steampowered.com/api/appdetails/?"

### Use the Team_02 Steam Application ID / game_id to extract game details###
query.list = list(appids = 271590)

### Now perform the GET request using the variables "details.url" and "query.list" ###
### Store the results in an object called details.request ###
detail.request <- GET(url = details.url, query = query.list)

### Convert the binary content of detail.request into string ###
### Store it in an object => details.raw ###
details.raw <- content(detail.request, as = "text", encoding = "UTF-8")

### Apply the function "fromJSON" to transform the string into a list ###
### Store the list details.list ####
details.list <- fromJSON(details.raw)

### Create a list that captures all the details of the video game. Name the list details ###
details <- details.list[[1]]$data

### Save captured data in details into 01_data ###
save(details, file = "C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/gamedetails.RData")

# ------------------------------------------------------------------------------------------------###
### Task 2.3: Collect the most recent user 100 reviews for game_id = 271590 ###

gameid = 271590
n = 1000

### create an empty data frame ###
game.rev <- data.frame() 

### Establish the base path URL ###
rev.url1 = "https://store.steampowered.com/appreviews/"

### Establish the URL extension ###
rev.url2 = "?json=1&filter=recent&num_per_page=100&cursor="

for (i in 1:n) {
  
  # create a conditional statement to assign initial query cursor to cursor=* only for i == 1
  if(i == 1){cursor = "*"} 
  
  # run the API query for batch i.
  rev.request <- GET(url = paste0(rev.url1, gameid, rev.url2, cursor))
  
  # convert query from Unicode to a string, save as tibble.
  reviews.raw <- content(rev.request, as = "text", encoding = "UTF-8")
  reviews.list <- fromJSON(reviews.raw)
  reviews <- reviews.list$reviews
  
  # unnest data frame
  reviews <- do.call("data.frame", reviews)
  
  ## Design an if/else statement to check if 'recommendationid' exists. If TRUE, continue with loop,
  ## If false, then break.
  
  if (('recommendationid' %in% names(reviews)) == TRUE) {
    
    reviews$recommendationid <- as.character(reviews$recommendationid)
    reviews$weighted_vote_score <- as.numeric(reviews$weighted_vote_score)
    
    # convert Unix timestamp to date.time format.
    reviews$timestamp_created  <- as_datetime(reviews$timestamp_created)
    reviews$timestamp_updated  <- as_datetime(reviews$timestamp_updated)
    reviews$author.last_played <- as_datetime(reviews$author.last_played)
    
    
    # to request the next n reviews, need to extract a custom cursor that is provided within the previous request
    cursor <- fromJSON(content(rev.request, as = "text", encoding = "UTF-8"))$cursor
    
    
    # some cursors return characters that are not URL encoded. 
    # Must replace problem pagination "+" with correct character "%2B.
    cursor <- gsub('\\+','%2B', cursor)
    
    # row bind the query to previous queries in an aggregated data frame.
    # use distinct to keep only unique rows.
    game.rev <- rbind(game.rev, reviews)
    
    # if recommendation does not exist in the data frame, then break.
  }else {
    
    
    
    print(paste0("No further recent reviews are available for App ID #",
                 as.character(gameid), ". Query ", as.character(i), " of ",
                 as.character(n), " aborted. No further queries possible."))
    
    break}
  
  # Printing status code.
  message(".", appendLF = FALSE) 
  Sys.sleep(time = 0.5)
  message(".", appendLF = FALSE) 
  Sys.sleep(time = 0.5)
  message(".", appendLF = FALSE) 
  Sys.sleep(time = 0.5)
  
}
### print game review collected
game.rev

# ----------------------------------------------------------------------------------------###
### Task 2.4: Save the resulting data frame###
save(game.rev, file = "C:/Users/oluwa/Documents/DS_projects/git/IntroADS_ES6/01_data/raw/gamereviews_new.RData")

# ----------------------------------------------------------------------------------------####
### Task 2.5: Create a data frame that contains review summary ####
rev.request <- GET(url = paste0(rev.url1, gameid, "?json=1&filter=recent&num_per_page=100&purchase_type=all&cursor=*"))
reviews.raw <- content(rev.request, as = "text", encoding = "UTF-8")
reviews.list <- fromJSON(reviews.raw)
rev.summary <- do.call ("cbind", reviews.list$query_summary)

# --------------------------------------------------------------------------------------- ####
### Task 2.6: Save the resulting dataframe in 01_data
save(rev.summary, file = "C:/Users/oluwa/Documents/DS_projects/git/IntroADS_ES6/01_data/raw/reviewsummary_new.RData")

