
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(scales)
library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(gridExtra)
library(corrplot)
library(ggmap)
library(igraph)
library(leaflet)
library(knitr)

twitter <- read_csv("/Users/leeyeji/Desktop/MSBA_20Winter/Customer&Social Analytics /final project/tweets-during-cavaliers-vs-warriors/TweetsNBA.csv")
twitter <- twitter %>%
  mutate_at(vars(text), as.character) %>%
  mutate_at(vars(lang), factor) %>%
  mutate(lang=recode(lang, en="English", es="Spanish"))


# View first 6 rows
head(twitter)
# View last 6 rows
tail(twitter)

# Summary
summary(twitter)


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
  


twitter %>%
  # User attributes
  select(friends_count, followers_count,
         favourites_count, statuses_count) %>%
  # Variables as values of a new column (facet_wrap)
  gather(Attribute, Num, 1:4) %>%
  mutate_at(vars(Attribute), factor) %>%
  ggplot(aes(x=Num, fill=Attribute)) +
  geom_histogram(bins=20, show.legend=FALSE) +
  xlim(c(0,2000)) +
  facet_wrap(~Attribute) +
  theme_bw() +
  labs(y="Frequency") +
  theme(axis.title.x=element_blank())



# Correlation between number of followers and number of friends
ggplot(data=twitter, aes(x=followers_count, y=friends_count)) +
  geom_point(alpha=0.1) + 
  xlim(0, quantile(twitter$followers_count, 0.95, na.rm=TRUE)) +
  ylim(0, quantile(twitter$friends_count, 0.95, na.rm=TRUE)) + 
  geom_smooth(method="lm", color="red") +
  theme_bw() +
  labs(x="Number of followers", y="Number of friends") 



twitter %>%
  mutate(user_created_at=substr(user_created_at, 27, 30)) %>%
  count(user_created_at) %>%
  ggplot(aes(x=user_created_at, y=n, group=1)) +
  geom_bar(stat="identity", fill="thistle2", colour="black") +
  theme_bw() +
  labs(x="Year", y="Frequency") 

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


# Wordcloudtwitter
dm <- frequentTerms(twitter$description)
wordcloud2(dm, minRotation=-pi/6, maxRotation=-pi/6, rotateRatio=1)

ggplot(dm %>% arrange(desc(freq)) %>% head(n=10),
       aes(x=reorder(word, -freq), y=freq)) +
  geom_bar(stat="identity", fill="darkviolet", colour="orange") +
  labs(y="Frequency") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x=element_blank()) 


#correlation
attributes <- twitter %>%
  # User attributes
  select(friends_count, followers_count,
         favourites_count, statuses_count) 
attributes

pm <- ggpairs(twitter, columns = 5:8 , ggplot2::aes(colour = 'r'))
p_ <- GGally::print_if_interactive
pm
library(GGally)


# English Tweets
en_tweets <- twitter %>%
  filter(lang=="English")

# Wordcloud
dm2 <- frequentTerms(en_tweets$text)
wordcloud(dm2$word, dm$freq, min.freq=30, colors=brewer.pal(8,"Dark2"), max.words=200)

# Top 20 frequent words in English 
ggplot(dm2 %>% arrange(desc(freq)) %>% head(n=5),
       aes(x=reorder(word, -freq), y=freq)) +
  geom_bar(stat="identity", fill="dodgerblue3", colour="dodgerblue3") +
  labs(y="Frequency") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x=element_blank()) 
 
# Spanish Tweets
es_tweets <- twitter %>%
  filter(lang=="Spanish")

# Wordcloud
dm3 <- frequentTerms(es_tweets$text)
wordcloud(dm3$word, dm3$freq, min.freq=30, colors=brewer.pal(8,"Dark2"))

# Top 20 frequent words in Spanish 
ggplot(dm3 %>% arrange(desc(freq)) %>% head(n=5),
       aes(x=reorder(word, -freq), y=freq)) +
  geom_bar(stat="identity", fill="orange", colour="purple") +
  labs(y="Frequency") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x=element_blank()) 

