Hotel Reviews Sentiment Analysis
Introduction
This R script performs sentiment analysis on a dataset of hotel reviews. The analysis includes language detection, text cleaning, and two approaches for sentiment analysis using different methods.

Prerequisites
Make sure to install the required R packages before running the script. You can install them using the following commands:

R
Copy code
install.packages("syuzhet")
install.packages("textcat")
install.packages("NLP")
install.packages("tokenizers")
install.packages("textstem")
install.packages("tm")
install.packages("googleLanguageR")
install.packages("cld2")
install.packages("quanteda")
install.packages("tidyverse")
Usage

Load the hotel reviews dataset from the "hotels_hackathon.rda" file.
load(file = "hotels_hackathon.rda")

Detect the language of each review and export the dataset to a CSV file for translation.
Hotel_reviews$lang <- lapply(Hotel_reviews$reviews.text, reviewLanguage)
write.csv(Hotel_reviews, "Hotel_reviews_notranslation.csv")
Translate the reviews using Google Sheets or any preferred translation method.

Load the translated dataset and perform text cleaning.
file_path <- "Hotel_reviews_translated.csv"
Hotel_reviews_translated <- read.csv(file_path)
Hotel_reviews_translated$cleaned_text <- lapply(Hotel_reviews_translated$translated_text, cleanText)

Perform sentiment analysis using two approaches: custom dictionary and the NRC package.

# Custom Dictionary Approach

# NRC Package Approach
Calculate ratings and analyze the results.

# Rating calculation based on the custom dictionary approach

# Hotel recommendations based on the NRC package approach

# Additional analysis, e.g., plotting bar charts

Output
The script generates several output files, including "Hotel_reviews_notranslation.csv," "Hotel_reviews_translated.csv," and others. Review the results and insights in these files.
