rm(list = ls())

#install packages
library(httr)
library(stringr)
library(hunspell)
library(tm)
library(wordcloud2)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(lubridate)

#load the dataset
path <- "C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/"
load("C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/cleaningdata.RData")

# Using the Bing sentiment word list
#########################
# Read the positive word 
bing_pos_df <- read.table("C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/positive-words.txt", sep = "\n", stringsAsFactors = FALSE)

bing_pos_df <- bing_pos_df[!grepl("^;", bing_pos_df$V1), , drop = FALSE]

# Reading the negative word 
bing_neg_df <- read.table("C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/negative-words.txt", sep = "\n", stringsAsFactors = FALSE)

bing_neg_df <- bing_neg_df[!grepl("^;", bing_neg_df$V1), , drop = FALSE]

#Split the reviews into words
reviews.words <- str_split(reviews.final$review, "\\s+")

# Calculate sentiment score (positive word count - negative word count)
sentiment_scores <- numeric(length(reviews.words))

for (i in seq_along(reviews.words)) {
  words <- strsplit(reviews.words[[i]], "\\s+")
  positive_count <- sum(unlist(words) %in% bing_pos_df$V1)
  negative_count <- sum(unlist(words) %in% bing_neg_df$V1)
  sentiment_scores[i] <- positive_count - negative_count
}

reviews.score <- cbind(reviews.final, Sentiment_Score_manually = sentiment_scores)

#Analysis using the syuzhet package
######################################################################
sentiment_scores_syuzhet <- get_sentiment(reviews.final$review, method = "bing")
sentiment_scores_syuzhet

reviews.score <- cbind(reviews.score, Sentiment_Score_syuzhet = sentiment_scores_syuzhet)

#Compare the result
##########################################################################
reviews.score$comparison_bing <- ifelse(reviews.score$Sentiment_Score_manually == 
                                          reviews.score$Sentiment_Score_syuzhet, "T", "F")
count_F <- sum(reviews.score$comparison_bing == "F")

sentiment_scores_syuzhet <- get_sentiment(reviews.final$review, method = "bing")
sentiment_scores_syuzhet

#Plot the line chart : manually vs syuzhet package
ggplot(data = reviews.score, aes(x = author.steamid, group = 1)) +
  geom_line(aes(y = Sentiment_Score_manually, color = "manually"))+
  geom_line(aes(y = sentiment_scores_syuzhet, color = "syuzhet"))+
  labs(x = "User ID", y = "Sentiment Score", title = "Figure 1. Comparison of Sentiment Score")+
  scale_color_manual(values = c("manually" = "red", "syuzhet" = "green")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

#Caculate the socre differences between by manually and by using syuzhet package
score_dif <- reviews.score$Sentiment_Score_manually - reviews.score$Sentiment_Score_syuzhet
reviews.score <- cbind(reviews.score, Sentiment_Score_dif = score_dif)

score_dif_freq <- table(reviews.score$Sentiment_Score_dif)
score_dif_freq <- as.data.frame(score_dif_freq)
colnames(score_dif_freq) <- c("Socre_dif", "Frequency")

score_dif_freq_trim <- score_dif_freq[score_dif_freq$Socre_dif != 0, ]


######################Another two dictionaris##############################
sentiment_scores_nrc <- get_sentiment(reviews.final$review, method = "nrc")
sentiment_scores_syu <- get_sentiment(reviews.final$review, method = "syuzhet")

reviews.score <- cbind(reviews.score, Sentiment_Score_nrc = sentiment_scores_nrc)
reviews.score <- cbind(reviews.score, Sentiment_Score_syu = sentiment_scores_syu)

df <- reviews.score %>%
  select(timestamp_created, Sentiment_Score_syuzhet,Sentiment_Score_nrc,Sentiment_Score_syu)

df$timestamp_created <- as.Date(df$timestamp_created)

score_per_month <- df %>%
  mutate(month = floor_date(timestamp_created, unit = "month")) %>%
  group_by(month) %>%
  summarise(
    mean.score.bing = round(mean(Sentiment_Score_syuzhet, na.rm = TRUE), 4),
    mean.score.nrc = round(mean(Sentiment_Score_nrc, na.rm = TRUE), 4),
    mean.score.syu = round(mean(Sentiment_Score_syu, na.rm = TRUE), 4)
  )

ggplot(data = score_per_month, aes(x = month)) +
  geom_line(aes(y = mean.score.nrc, color = "nrc"))+
  geom_line(aes(y = mean.score.bing, color = "bing"))+
  geom_line(aes(y = mean.score.syu, color = "syuzhet"))+
  labs(x = "Date", y = "Sentiment Score", title = "Figure 2. Sentiment scores based on three different
dictionaries")+
  scale_color_manual(values = c("syuzhet"="skyblue","bing" = "purple", "nrc"="orange")) +
  theme_minimal()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels = " %Y.%m")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45))


#Compare the sentiment score and voted_up result
reviews.score <- reviews.score %>%
  mutate(logical_score = ifelse(Sentiment_Score_syuzhet >= 0, "TRUE", "FALSE"))

reviews.score <- reviews.score %>%
  mutate(logical_score_check = ifelse(logical_score == voted_up, "1", "0"))

score_vs_votedup <- table(reviews.score$logical_score_check)
score_vs_votedup <- as.data.frame(score_vs_votedup)
colnames(score_vs_votedup) <- c("logical_score_check", "Frequency")

ggplot(score_vs_votedup, aes(x = logical_score_check, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.3) +
  labs(x = "", y = "Frequency", 
       title = "Figure 4. Consistency Between Sentiment Score and Voted Up Status")+
  theme_minimal()+
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3)+
  scale_x_discrete(labels = c("0" = "Inconsistent", "1" = "Consistent"))

#Does the average sentiment score resemble the over all review score of your game?
reviews.score$logical_score <- as.logical(reviews.score$logical_score)
reviews.score <- reviews.score[complete.cases(reviews.score$logical_score), ]

true_percentage <- round(mean(reviews.score$logical_score) * 100, 2)
print(true_percentage)

positive_percentage <- round(mean(reviews.score$voted_up) * 100, 2)
print(positive_percentage)


#Does the sentiment vary over time?
df2 <- reviews.score %>%
  select(timestamp_created, Sentiment_Score_syuzhet)
df2$timestamp_created <- as.Date(df$timestamp_created)

score_per_day <- df2 %>%
  group_by(timestamp_created) %>%
  summarise(mean.score.bing = round(mean(Sentiment_Score_syuzhet, na.rm = TRUE), 4))

ggplot(data = score_per_day, aes(x = timestamp_created, y = mean.score.bing)) +
  geom_line(color = "blue")+
  labs(x = "Date", y = "Daily Average Score", title = "Figure 3. Sentiment Scores Over Time")+
  theme_minimal()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = " %Y.%m")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45),
        panel.grid = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_blank())

#################create a Word cloud#############################

#create a vector with all word from the reviews final 

all.words<- NULL
for (i in 1:nrow(reviews.final)){
  tmp<- str_split(reviews.final$review[i], " ", simplify=T)
  all.words<- c(all.words, tmp)
}

all.words<- all.words[nchar(all.words)>2]

word.freq<- data.frame(table(all.words))

wordcloud2(data=word.freq, size=1.2, minSize= 3,
           color="random-light", background = "white")
stopwords()


