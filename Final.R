install.packages("syuzhet")
library(textcat)
library("NLP")
library(tokenizers)
library(textstem)
library(tm)
library(googleLanguageR)
library(cld2)
library("quanteda")
library(tidyverse)
library(quanteda)
library(dplyr)
library(plyr)
library(syuzhet)




load(file = "hotels_hackathon.rda")


#Reviews texts are in different languages so we need to detect the language of each review
## Function to return the language when given a text
reviewLanguage <- function(text) {
  lang <- textcat(text, p = ECIMCI_profiles)
  return(lang)
}

## Adding a new column with the review language
Hotel_reviews$lang <- lapply(Hotel_reviews$reviews.text, reviewLanguage)
Hotel_reviews$lang <- as.character(Hotel_reviews$lang)
Hotel_reviews$reviews.text <- as.character(Hotel_reviews$reviews.text)
Hotel_reviews$reviews.title <- as.character(Hotel_reviews$reviews.title)

#barplot(table(Hotel_reviews$lang)) #language distribution graph

#export Hotel_reviews and apply a function on google sheets to translate the detected languages to english
write.csv(Hotel_reviews, "Hotel_reviews_notranslation.csv")


# List files in the current working directory
list.files()

# Check the current working directory
getwd()

# Use a relative path
file_path <- "Hotel_reviews_translated.csv"
Hotel_reviews_translated <- read.csv(file_path)

# function to apply all the cleaning functions
cleanText <- function(text){
  cleaned_text <- as.list(text)
  
  cleaned_text <- lemmatize_strings(cleaned_text)
  
  cleaned_text <- tolower(cleaned_text)
  cleaned_text <- removeNumbers(cleaned_text)
  cleaned_text <- removePunctuation(cleaned_text)
  cleaned_text <- stripWhitespace(cleaned_text)
  return (cleaned_text)
}
## cleaned_text <- tokenize_words(cleaned_text)

#adding cleaned text column and apply function
Hotel_reviews_translated$cleaned_text <- lapply(Hotel_reviews_translated$translated_text, cleanText)
Hotel_reviews_translated$cleaned_text <- as.character(Hotel_reviews_translated$cleaned_text)

#convert to corpus to remove stopwords and strip whitspace again
cleaned_text_vector <- Hotel_reviews_translated$cleaned_text
cleaned_text_corpus <- Corpus(VectorSource(cleaned_text_vector))
cleaned_text_corpus <- tm_map(cleaned_text_corpus, removeWords, stopwords("en"))
cleaned_text_vector <- unlist(cleaned_text_corpus)
#cleaned_text_vector <- trimws(cleaned_text_vector)
cleaned_text_vector <- stripWhitespace(cleaned_text_vector)

#create corpus from the final cleaned text to create document feature matrix
cleaned_text_corpus2 <- Corpus(VectorSource(cleaned_text_vector))

#remove the last character from cleaned vector
cleaned_text_vector <- cleaned_text_vector[-1400]

Hotel_reviews_translated$cleaned_text <- cleaned_text_vector

#adding the final cleaned text to the hotel_reviews_translated
Hotel_reviews_translated$cleaned_text <- cleaned_text_vector


## 1st approach: import custom dictionary from moodle and apply tf & tf-idf
mycorpus <- corpus(Hotel_reviews_translated$cleaned_text)
dict <- read.csv2("custom_dictionary.csv", stringsAsFactors = F)
dict_positive <- dict$ï..positive
dict_positive <- dict_positive[dict_positive != ""]
dict_negative <- dict$negative

mydict <- dictionary(list(positive = dict_positive, negative = dict_negative))

#document feature matrix with custom dictionary terms
myDfm <- dfm(mycorpus, dictionary = mydict)

# CREATE DATA-FRAME WITH TEXTS AND DICTIONARY-VALUES
df_sentiment <- cbind(Hotel_reviews_translated$cleaned_text, convert(myDfm, to = "data.frame"))

#term frequency
df_sentiment$label <- NA
for (i in 1:nrow(df_sentiment)) {
  if (df_sentiment$positive[i] > df_sentiment$negative[i]) {
    df_sentiment$label[i] <- "positive"
  }
  
  if (df_sentiment$negative[i] > df_sentiment$positive[i]) {
    df_sentiment$label[i] <- "negative"
  }
  
  if (df_sentiment$positive[i] == df_sentiment$negative[i]) {
    df_sentiment$label[i] <- "neutral"
  }
} 


#barplot(table(df_sentiment$label))

# CREATE TF-IDF MATRIX
myDfm_tfidf <- dfm_tfidf(myDfm, scheme_tf = "count", scheme_df = "inverse")
myDfm_tfidf_matrix <- as.matrix(myDfm_tfidf)  
df_sentiment <- cbind(df_sentiment, myDfm_tfidf_matrix)

df_sentiment$label_tf_idf <- NA

for (i in 1:nrow(df_sentiment)) {
  if (df_sentiment[i,6] > df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "positive"
  }
  
  if (df_sentiment[i,6] < df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "negative"
  }
  
  if (df_sentiment[i,6] == df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "neutral"
  }
} 

barplot(table(df_sentiment$label_tf_idf))

# CREATE NEW COMPARISON-COLUMN
df_sentiment$comparison <- NA

# COMPARE LABELS
for (i in 1:nrow(df_sentiment)) {
  if(df_sentiment$label[i] == df_sentiment$label_tf_idf[i]) {
    df_sentiment$comparison[i] <- "same"
  } else {
    df_sentiment$comparison[i] <- "different"
  }
}
 print(df_sentiment)


# CREATE NEW SUBSET, WHERE LABELS ARE DIFFERENT
df_sentiment_different <- subset(df_sentiment, df_sentiment$comparison == "same")
df_sentiment_different$whos_right <- NA
print(df_sentiment_different)


# VALIDATE LABELS AND DECIDE WHO IS RIGHT
df_sentiment_different$whos_right[1] <- "tf"
df_sentiment_different$whos_right[2] <- "tf"
df_sentiment_different$whos_right[3] <- "tf-idf"
df_sentiment_different$whos_right[4] <- "tf-idf"
df_sentiment_different$whos_right[5] <- "tf"
df_sentiment_different$whos_right[6] <- "tf"
df_sentiment_different$whos_right[7] <- "tf"
df_sentiment_different$whos_right[8] <- "tf-idf" 
df_sentiment_different$whos_right[9] <- "tf"
df_sentiment_different$whos_right[10] <- "tf"

# BARPLOT COMPARISON
barplot(table(df_sentiment_different$whos_right))

# calculating review score based on tf
df_sentiment$percentage <- NA

for (i in 1:nrow(df_sentiment)){
  if(df_sentiment$positive[i] == df_sentiment$negative[i]){
    df_sentiment$percentage[i] <- 0.5
  }else {
    df_sentiment$percentage[i] <- df_sentiment$positive[i] /
      (df_sentiment$positive[i] + df_sentiment$negative[i])
  }
}

df_sentiment$rating_tf <- NA

for(i in 1:nrow(df_sentiment)){
  if(df_sentiment$percentage[i] < 0.2){
    df_sentiment$rating_tf[i] <- 1
  }else if(df_sentiment$percentage[i] < 0.4){
    df_sentiment$rating_tf[i] <- 2
  }else if(df_sentiment$percentage[i] < 0.6){
    df_sentiment$rating_tf[i] <- 3
  }else if(df_sentiment$percentage[i] < 0.8){
    df_sentiment$rating_tf[i] <- 4
  }else if(df_sentiment$percentage[i] <= 1){
    df_sentiment$rating_tf[i] <- 5
  }
}

#barplot(table(df_sentiment$rating_tf))

#adding the rating column to the hotel reviews_translated
ratings_tf <- df_sentiment$rating
Hotel_reviews_translated$ratings_tf <- ratings_tf


## 2nd approach for sentiment analysis using nrc package
##

#subset revews
reviews_text <- Hotel_reviews_translated$reviews.text

#create corpus for sentiment analysis
corpus_reviews <- iconv(reviews_text)
corpus_reviews <- Corpus(VectorSource(corpus_reviews))

inspect(corpus_reviews[1:30]) #check corpus

corpus_reviews <- tm_map(corpus_reviews, tolower) #convert into lowercase
corpus_reviews <- tm_map(corpus_reviews, removePunctuation) #remove punctuation symbols
corpus_reviews <- tm_map(corpus_reviews, removeNumbers) #remove numbers
corpus_reviews <- tm_map(corpus_reviews, removeWords, stopwords("english")) #remove stopwords
corpus_reviews <- tm_map(corpus_reviews, removeWords, stopwords("french")) 
corpus_reviews <- tm_map(corpus_reviews, removeWords, stopwords("german")) 
corpus_reviews <- tm_map(corpus_reviews, stripWhitespace) #remove whitespace
corpus_reviews <- tm_map(corpus_reviews, removeWords, c("bed", "back", "bathroom", "sleep", 
                                                        "day", "desk", "staff","however", "didnt", "one", "way", "really", "next",
                                                        "stayed", "coffee", "much", "two", "service", "food", "dont", "old", "went", "water",
                                                        "people", "front", "sleep", "needed", "hotels", "morning", "day", "even", "check", "inn", "hotel", "room",
                                                        "location", "breakfast", "rooms", "get", "time", "night", "stay", "area", "just",
                                                        "pool", "play", "place")) 


#create a term document
tdm_reviews <- TermDocumentMatrix(corpus_reviews)
tdm_reviews <- as.matrix(tdm_reviews) 

#create bar plot of words
w <- rowSums(tdm_reviews)
w <- subset(w, w>=100)
barplot(w, las = 3, col = "blue")

#create word cloud
#w <- sort(rowSums(tdm_reviews), decreasing = T)
#set.seed(2000)
#wordcloud(words = names(w), freq = w, max.words = 50, 
 #         random.order = T, min.freq = 5, colors = brewer.pal(25, "Dark2"),
 #         scale = c(3, 0.3))

#get sentiment scores
sentiment_data <- iconv(reviews_text)
sentiment_analysis2 <- get_nrc_sentiment(sentiment_data)
sentiment_analysis2[1:10]

#score for each review
sentiment_analysis2$score <- sentiment_analysis2$positive - sentiment_analysis2$negative
sentiment_analysis2[1:20,]


##


#Sentiment_analysis <- read.csv("D:/MME TIME/Text Mining/Hackathon_final/Sentiment_analysis.csv")
#calculate ratings based on score
recencyBins=quantile(sentiment_analysis2$score, probs = seq(0, 1, by=0.2))
recencyBins
sentiment_analysis2$rating = cut(sentiment_analysis2$score, breaks = recencyBins, labels = c(1,2,3,4,5), include.lowest = TRUE, right=FALSE)

Hotel_reviews_translated$rating <- NA
rating <- sentiment_analysis2$rating
Hotel_reviews_translated$rating <- rating

barplot(table(Hotel_reviews_translated$rating))


## 1st approach to calculate the average ratings
# counting the no of reviews per hotel and calculating the average rating per hotel
average_ratings <- ddply(Hotel_reviews_translated, .(name,city,province), summarize,  hotel_rating=mean(rating), absolute=length(rating))
Hotel_reviews_translated$rating <- as.integer(Hotel_reviews_translated$rating)

average_ratings$hotel_rating <- format(round(average_ratings$hotel_rating, 2), nsmall = 2)

# creating a subset of hotels with more than 4 average rating and have more than 5 reviews OR above 3 rating and have more than 15 reviews
Hotels_reviews_high <- subset(average_ratings, average_ratings$absolute >= 5)
Hotels_final_list <- subset(Hotels_reviews_high, Hotels_reviews_high$hotel_rating >= 3)
Hotels_final_list2 <- subset(Hotels_final_list, Hotels_final_list$hotel_rating >= 4 | Hotels_final_list$absolute >= 15)

## 2nd approach to calculate the average rating
# Code for aggregration of ratings by hotel
hotel_ratings <- data.frame(Hotel_reviews_translated$name, Hotel_reviews_translated$city, sentiment_analysis2$score ,sentiment_analysis2$rating)

# Table with list of hotel names and their number of reviews
ratings_count = aggregate(hotel_ratings$Hotel_reviews_translated.name , list(Name = hotel_ratings$Hotel_reviews_translated.name), length)
print(ratings_count)

# Save these to csv
write.csv(x = ratings_count,file = "ratings_count_2nd.csv")

# Code to get separated ratings for hotels and recommend based on more number of reviews with higher (4 & 5 star) ratings
hotel_names = unique(Hotel_reviews_translated$name)


ratings_negative = subset(hotel_ratings, sentiment_analysis2.rating == 1 | sentiment_analysis2.rating == 2)
ratings_positive = subset(hotel_ratings, sentiment_analysis2.rating == 4 | sentiment_analysis2.rating == 5)

hotel_negative_counts = aggregate(ratings_negative$sentiment_analysis2.rating, list(Name = ratings_negative$Hotel_reviews_translated.name), length)
hotel_positive_counts = aggregate(ratings_positive$sentiment_analysis2.rating, list(Name = ratings_positive$Hotel_reviews_translated.name), length)

hotel_recommendations = merge(hotel_positive_counts, hotel_negative_counts, by = "Name")
names(hotel_recommendations)[2] = "Number of Positive Reviews"
names(hotel_recommendations)[3] = "Number of Negative Reviews"

hotel_recommendations$recommendation_score = hotel_recommendations$`Number of Positive Reviews`- hotel_recommendations$`Number of Negative Reviews`

results = c()

for (i in 1:length(hotel_recommendations$recommendation_score)){
  value = hotel_recommendations$recommendation_score[i];
  
  if(value>0){
    results[i]= "Recommended"
  }  else if(value<0){
    results[i]= "Not Recommended"
  }  else{
    results[i] = "Undecided"
  }
}

hotel_recommendations$recommendation_result = results

Recommended_hotels <- subset(hotel_recommendations, hotel_recommendations$recommendation_result == "Recommended")
Recommended_hotels_highscore <- subset(Recommended_hotels,Recommended_hotels$recommendation_score >= 5)



barplot(table(Hotel_reviews$province))
unique(Hotel_reviews$province)
unique(Hotel_reviews$city)
unique(Hotel_reviews$name)




languages <- ddply(Hotel_reviews, .(lang), summarize, occurence=length(lang))
average_ratings <- ddply(Hotel_reviews_translated, .(name,city,province), summarize,  hotel_rating=mean(rating), absolute=length(rating))

# Specify the file path within the /cloud/project directory
output_file <- "/cloud/project/average_ratings.csv"

# Write the data frame to a CSV file
write.csv(average_ratings, file = output_file, row.names = FALSE)

#write.csv(average_ratings, "average_ratings.csv")
write.csv(Hotel_reviews_translated, "reviews_ratings.csv")
        