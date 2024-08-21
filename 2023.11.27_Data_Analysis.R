# ------------------------------------------------------------------- ####
### Clear workspace ####
rm(list = ls())

### Load libraries needed for data analysis #######
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------- ####
###Steam Application ID for team 02
gameid = 271590

# ------------------------------------------------------------------- ####
### Load Steam Datasets ####

gamedetails <- load("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/gamedetails.RData")
top100s <- load("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/top100.RData")

# ------------------------------------------------------------------- ####
### Additional Queries #####################
### Test loaded data for missing and duplicated values ###
is.na(top100)
top100[!complete.cases(top100), ]

duplicated(top100)
sum(duplicated(top100))

is.na(gamedetails)
top100[!complete.cases(gamedetails), ]

duplicated(gamedetails)
sum(duplicated(gamedetails))

# ------------------------------------------------------------------- ####
### TASK 3: Video Game Description ##################
### Question 3.1: What is the name of your video game? ####
game.name = details$name
game.name

# ------------------------------------------------------------------- ####
### Question 3.2: What is the game about?
game.about = details$about_the_game
game.about

# ------------------------------------------------------------------- ####
### Question 3.3: Who developed the game?
game.developer = details$developers
game.developer

# ------------------------------------------------------------------- ####
### Question 3.4.0: Is the game free? (Additional question)
game.free = details$is_free
game.free

# ------------------------------------------------------------------- ####
### Question 3.4.1: How much does the game cost?
game.cost <- top100 %>%
  filter(appid == 271590) %>%
  select(price)
game.cost

# ------------------------------------------------------------------- ####
### Question 3.5: Which age is required to play the game?
game.required_age = paste0(details$required_age, " ", "years")
game.required_age

# ------------------------------------------------------------------- ####
### Question 3.6: When was it released?
game.release_date = details$release_date
game.release_date

# ------------------------------------------------------------------- ####
### Question 3.7: To which genre does it belong?
game.genre = details$genres
game.genre

# ------------------------------------------------------------------- ####
### Question 3.8: How many people own the game?
game.owners <- top100 %>%
  filter(appid == 271590) %>%
  select(owners)
game.owners


# ------------------------------------------------------------------- ####
### TASK 4: Summary of the User Reviews #############
### Task 4.1: Load revsummary datasets

rev_summary <- load("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/revsummary.RData")
game_reviews <- load("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/01_data/gamereviews.RData")

# ----------------------------------------------------------------- ####
### Additional Queries #####################
### Test loaded data for missing and duplicated values ###
is.na(game.rev)
game.rev[!complete.cases(game.rev), ]

duplicated(game.rev)
sum(duplicated(game.rev))

# ------------------------------------------------------------------- ####
### Question 4.2: How many reviews did you collect?
num_reviews = rev.summary[, "total_reviews"]
num_reviews

# ------------------------------------------------------------------- ####
### Question 4.3: When was the first and the last review in your dataset written? ###
first_review_data <- min(game.rev$timestamp_created)
first_review_data

last_review_data <- max(game.rev$timestamp_created)
last_review_data

# ------------------------------------------------------------------- ####
### Question 4.4: For how long have users who wrote a review played the game?
min_playtime<-min(game.rev$author.playtime_forever)
max_playtime<-max(game.rev$author.playtime_forever)
cat("The minimum playtime among users who wrote a review:", min_playtime, "minutes")
cat("The maximum playtime among users who wrote a review:", max_playtime, "minutes")

# ------------------------------------------------------------------- ####
### Question 4.4: How long did the users play the game before writing their review?
min_playtime_before_review<-min(game.rev$author.playtime_at_review)
max_playtime_before_review<-max(game.rev$author.playtime_at_review)
cat("The minimum playtime before users write a review:", min_playtime_before_review)
cat("The maximum playtime before users write a review:", max_playtime_before_review)


# ------------------------------------------------------------------- ####
### Question 4.5: How many reviews were written during early access?
rev_during_early_access<-sum(game.rev$written_during_early_access == TRUE, na.rm = TRUE)
cat("There is",rev_during_early_access,"review was written during early access.")

# ------------------------------------------------------------------- ####
### Question 4.5.1 Plot the number of reviews written per day separated for positive and negative reviews ###

# Extract column data needed for the plot and store in "df"
df <- game.rev %>%
  select(author.num_reviews, timestamp_created, voted_up) %>%
  group_by(voted_up)

# Filter the data for positive and negative reviews
positive_reviews <- df[df$voted_up == TRUE, ]
negative_reviews <- df[df$voted_up == FALSE, ]

# Aggregate the number of reviews per day
positive_reviews_per_day <- aggregate(positive_reviews$author.num_reviews, by = list(positive_reviews$timestamp_created), FUN = sum)
negative_reviews_per_day <- aggregate(negative_reviews$author.num_reviews, by = list(negative_reviews$timestamp_created), FUN = sum)

# Plot the number of reviews per day and save as plot1
p <- ggplot() +
  geom_line(data = positive_reviews_per_day, aes(x = Group.1, y = x, color = "Positive")) +
  geom_line(data = negative_reviews_per_day, aes(x = Group.1, y = x, color = "Negative")) +
  labs(x = "Months of the Year, 2023", y = "Number of Reviews") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red"))

ggsave("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/03_report/graphs/plot1.png", plot = p, width = 7, height = 7)

# ------------------------------------------------------------------- ####
### Question 4.5.2 Plot using line graph, the cumulative percent of positive reviews and negative reviews

positive_reviews_per_day$cumulative_percent <- cumsum(positive_reviews_per_day$x) / sum(positive_reviews_per_day$x)
negative_reviews_per_day$cumulative_percent <- cumsum(negative_reviews_per_day$x) / sum(negative_reviews_per_day$x)

# Plot the cumulative percent of positive and negative reviews and save as Plot2
p2 <- ggplot() +
  geom_line(data = positive_reviews_per_day, aes(x = Group.1, y = cumulative_percent, color = "Positive")) +
  geom_line(data = negative_reviews_per_day, aes(x = Group.1, y = cumulative_percent, color = "Negative")) +
  labs(x = "Months of the Year, 2023", y = "Cumulative Percent") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red"))
  
ggsave("C:/Users/oluwa/Documents/DS_projects/git/IntroADS_Ass1_Team02/03_report/graphs/plot2.png", plot = p2, width = 7, height = 7)


# ------------------------------------------------------------------- ####
### 4.2.3 How many reviews in total are positive reviews ####
total_positive_review = sum(count(positive_reviews))
total_positive_review

# ------------------------------------------------------------------- ####
### 4.2.3 How many reviews in total are positive reviews ####
total_negative_review = sum(count(negative_reviews))
total_negative_review