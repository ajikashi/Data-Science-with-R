
# clear workspace
rm(list = ls())

# load necessary packages
library(wordcloud2)
library(stringr)
library(syuzhet)
library(hunspell)
install.packages(c("tm", "slam"))
library(tm)
library(slam)

#load data
load("C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/raw/gamereviews.RData")


###############Task 2: Clean your Game Reviews##################
##################################################################

#Create the data frame.
reviews.clean <- game.rev
options(max.print = 10000)


#remove all non-ASCII characters
reviews.clean$review <- gsub("[^\x01-\x7F]", "", reviews.clean$review)
reviews.clean$review[1:40]

#remove links
reviews.clean$review<-gsub('https-?:+', "", reviews.clean$review)
reviews.clean$review[1:40]

#replace letter that appear three times with the letter only once
reviews.clean$review<-gsub("(\\w)\\1{2,}", "\\1", reviews.clean$review)
reviews.clean$review[1:40]

#replace all abbreviation of “n’t” into the actual word “not”.
reviews.clean$review<-gsub("(\\b)(\\w+)n't\\b", "\\1 not", reviews.clean$review)
reviews.clean$review[1:40]

#remove numbers
reviews.clean$review <- gsub("\\b\\d+\\w*\\d*\\b", " ", reviews.clean$review)
reviews.clean$review[1:40]

#remove punctuations
reviews.clean$review<-gsub("[[:punct:]]", "", reviews.clean$review)
reviews.clean$review[1:40]

#remove new lines
reviews.clean$review<-gsub("\n", " ", reviews.clean$review)
reviews.clean$review[1:40]

#remove unnecessary space at the begining
reviews.clean$review<-gsub("^\\s+", " ", reviews.clean$review)
reviews.clean$review[1:40]

#remove unnecessary space at the end
reviews.clean$review<-gsub("\n", " ", reviews.clean$review)
reviews.clean$review[1:40]

#remove unnecessary empty spaces
reviews.clean$review <-gsub("[ |\t]+", " ", reviews.clean$review)
reviews.clean$review[1:40]

#Exclude all observations with empty reviews from your dataset.
reviews.clean$review[nchar(reviews.clean$review) < 2] <- ""
reviews.clean$review[1:40]


#Spelling check
#-------------------------------

#split the reviews title into all the words
review.checked <- reviews.clean
reviewl <- review.checked$review[1]
words <- str_split(reviewl, " ", simplify = T)

check.spell<-sapply(words, function(word) hunspell_check(word, dict="en_US"))

review.checked$review[1:10000] <- sapply(review.checked$review[1:10000],
                                         function(rev){
                                           words <- str_split(rev, " ", simplify = TRUE)
                                           check.spell.us <- sapply(words,
                                                                    function(word) hunspell_check(word, dict = c("en_US")))
                                           correct_words <- words[check.spell.us]
                                           str_c(correct_words, collapse = " ")
                                         })

#Convert all reviews to lowercase
review.checked$review<- tolower(review.checked$review)
review.checked$review[1:40]

#Delete all stopwords except for the word “not”.
###################################################
# Get the English stopwords list
stopwords <- stopwords("en")

# Remove "not" from the stopwords list
stopwords <- setdiff(stopwords, "not")

# Keep only words not in the stopwords list for each review
reviews.clean$review <- sapply(review.checked$review, function(text) {
  words <- str_split(text, " ", simplify = T)  # Split into words
  remaining_words <- words[!words %in% stopwords]  # Keep words not in the stopwords list
  paste(remaining_words, collapse = " ")  # Reconstruct words into a string
})

reviews.clean$review[1:40]

#delete reviews with ""
reviews.final <- reviews.clean[reviews.clean$review != "", ]


# Save the cleaned dataset
save(reviews.final, file ="C:/Users/rkhan/git/IntroADS_Ass2_Team02/01_data/cleaningdata.RData")



