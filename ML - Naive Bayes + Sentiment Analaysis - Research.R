#Naive Bayes + Sentiment Analaysis - Research

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(gmodels)
library(lubridate)
library(class)
library(e1071)
library(tm)
library(wordcloud)

#DATA COLLECTION
news <- read.csv("~/Downloads/Sentiment_Analaysis_for_Financial_News.csv")
names(news) <- c('labels', 'headlines')

#DATA EXPLORATION
str(news)
table(news$labels)

#randomize data if not evenly spread out 
#(don't if its time series data because you will mess with time!)
news <- news[sample(nrow(news)),]

#DATA CLEANING
#create corpus to store word documents
headlines_corpus <- Corpus(VectorSource(news$headlines))
headlines_corpus
inspect((headlines_corpus[1:5]))

#clean data to make it computationally readable
corpus_clean <- tm_map(headlines_corpus, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, tolower)

inspect(corpus_clean[1:5])

#create 'dtm' for 'tokenization'
headlines_dtm <- DocumentTermMatrix(corpus_clean)


#DATA PREPARATION

#1. split raw dataframe
train_news <- news[1:4000, ]
test_news <- news[4001:4845, ]

#2. split document term matrix
train_dtm <- headlines_dtm[1:4000, ]
test_dtm <- headlines_dtm[4001:4845, ]

#3. split corpus clean data
train_cc <- corpus_clean[1:4000]
test_cc <- corpus_clean[4001:4845]

#check data is evenly spread
prop.table(table(train_news$labels))
prop.table(table(test_news$labels))

#check which words appear the most
wordcloud(train_cc, min.freq = 30, random.order = FALSE)

#check which words appear the most within each label
positive <- subset(train_news, train_news$labels == "positive")
negative <- subset(train_news, train_news$labels == "negative")
neutral <- subset(train_news, train_news$labels == "neutral")

wordcloud(positive$headlines, max.words = 30, scale = c(3, 0.5))
wordcloud(negative$headlines, max.words = 30, scale = c(3, 0.5))
wordcloud(neutral$headlines, max.words = 30, scale = c(3, 0.5))

#get the most frquent terms to feature in the training and testing sets
findFreqTerms(train_dtm, 5)
freq_terms_dict <- findFreqTerms(train_dtm, 5)
train <- DocumentTermMatrix(train_cc,
                            list(dictionary = freq_terms_dict))
test  <- DocumentTermMatrix(test_cc,
                            list(dictionary = freq_terms_dict))

#write 'yes' or 'no' depending one whether these words feature in the headlines
convert_counts <- function(x) {
x <- ifelse(x > 0, 1, 0)
x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
return(x)
}

train <- apply(train, MARGIN = 2, convert_counts)
test  <- apply(test, MARGIN = 2, convert_counts)

#MODEL CREATION
headline_classifier <- naiveBayes(train, train_news$labels)
headline_prediction <- predict(headline_classifier, test)

#DATA VISUALIZATION
CrossTable(headline_prediction, test_news$labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))

#74% accuracy

#IMPROVING THE MODEL
headline_classifier2 <- naiveBayes(train, train_news$labels, laplace = 2)
headline_prediction2 <- predict(headline_classifier2, test)

CrossTable(headline_prediction2, test_news$labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#75% accuracy
