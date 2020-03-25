library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(scales)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(reshape2)
library(gridExtra)
library(corrplot)
library(ggmap)
library(igraph)
library(leaflet)
library(knitr)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)
#install.packages("devtools")
library(devtools)
#devtools::install_github("lchiffon/wordcloud2", force = TRUE)
library(wordcloud2)
#library(KoNLP)



setwd("/Users/leeyeji/Desktop/MSBA_20Winter/Customer&Social Analytics /final project/tweets-during-cavaliers-vs-warriors/")
peak <- read.csv("PEAK.csv")
#peak <- read_csv("/Users/leeyeji/Desktop/MSBA_20Winter/Customer&Social Analytics /final project/tweets-during-cavaliers-vs-warriors/PEAK.csv")

# Most frequent terms 
frequentTerms <- function(text){
  
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}

# Text transformations
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(removeURL))
  v_stopwords <- c(stopwords("english"), stopwords("spanish"),
                   "thats","weve","hes","theres","ive", "im","will","can","cant",
                   "dont","youve","us","youre","youll","theyre","whats","didnt")
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
  
}

# peak Tweets
peak_tweets <- peak %>%
  filter(peak_time=="0")

# Wordcloud
peak_wc <- frequentTerms(peak_tweets$text)
head(peak_wc)
wordcloud(peak_wc$word, peak_wc$freq, min.freq=30, colors=brewer.pal(8,"Dark2"))
#export 
write.csv(peak_wc,"/Users/leeyeji/Desktop/MSBA_20Winter/Customer&Social Analytics /final project/tweets-during-cavaliers-vs-warriors/final_pick.csv", row.names = FALSE)


#from here please see python jupyter notebook for the further visualizaiton. 

