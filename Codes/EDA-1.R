setwd('/Users/yangz/Desktop/UCI MSBA winter 2020/BANA 277 - Customer & Social Analytics/Final Project')
data = read.csv('TweetsNBA.csv',stringsAsFactors = FALSE)
View(data)

library(ggplot2)
library(dplyr)

created_at=substr(data$created_at, 12, 16)
x=as.numeric(as.factor(created_at))
data %>%
  mutate(created_at=substr(created_at, 12, 16)) %>%
  count(created_at) %>%
  ggplot(aes(x=as.numeric(as.factor(created_at)), y=n, group=1)) +
  geom_line(size=2, show.legend=FALSE) +
  geom_area(fill="coral")+
  geom_vline(size=1,xintercept=5,linetype="dashed",colour="red") +
  geom_vline(size=1,xintercept=7,linetype="dashed",colour="red") +
  labs(x="UCT time (hh:mm)", y="Number of Tweets",title='number of tweets in every minute') + 
  theme_set(theme_bw())+
  theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour='black'))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=c(1,5,10,15,20,
                              25,30,35,40,45),
                     labels=c("01:13","01:17","01:22","01:27","01:32",
                              "01:37","01:42","01:47","01:52","01:57")) 


#language
# barplot
data %>%
  count(lang) %>%
  arrange(desc(n)) %>%
  head(n=8) %>%
  ggplot(aes(x=reorder(lang,-n),y=n,fill=lang))+
  geom_bar(stat="identity")+
  labs(x='language',y='number',title="predominate languages among NBA fans")+
  theme(plot.title = element_text(hjust = 0.5)) 
#The barplot shows the most frequently used language among NBA fans is English, Spanish and Arabic

#number of words
#violin plot
data %>%
  mutate(words_per_tweet= sapply(strsplit(text, " "), length)) %>%
  filter(lang=="en" | lang=="es") %>%
  ggplot(aes(x=lang, y=words_per_tweet, fill=lang)) +
  geom_violin() +
#  geom_boxplot(show.legend=FALSE,notch=TRUE) +
  theme_bw() +
  labs(x="language",y='words',title='# of words in tweets of different languages') +
  theme(axis.title.y=element_blank())+
  theme(plot.title = element_text(hjust = 0.5)) 
#Twitter in spanish has the most number of words.

#Density plot
data %>%
  filter(lang=="en" | lang=="es") %>%
  ggplot(aes(x=nchar(text), fill=lang)) +
  geom_density(alpha=0.5) +
  xlim(c(0, 150)) +
  theme_bw() +
  labs(x="characters", y="density",title='tweets length of different languages') +
  guides(fill=guide_legend(title="Language"))+
  theme(plot.title = element_text(hjust = 0.5)) 
#Most twitter in English have 80 characters.
#Most twitter in Spanish have 130 characters.





data %>%
  # User attributes
  select(friends_count) %>%
  mutate_at(vars(friends_count), factor) %>%
  ggplot(aes(x=, fill=friends_count)) +
  geom_histogram(bins=20, show.legend=FALSE) +
  xlim(c(0,2000)) +
  theme_bw() +
  labs(y="Frequency") +
  theme(axis.title.x=element_blank())

#correlation
network = data[,c('statuses_count','followers_count','favourites_count','friends_count')]
install.packages("corrgram")
library(corrgram)
corrgram(network, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="social influence")

#correlation coefficient
install.packages("corrplot")
library(corrplot)
mf_cor<-cor(network)#calculate corr matrix
col3 <- colorRampPalette(c("blue", "white", "red")) 
cor.plot <- corrplot(corr = mf_cor,col=col3(10),type="upper",tl.pos="d",tl.cex = 0.5) #???????????? ????????????"??????"
cor.plot <- corrplot(corr = mf_cor,add=TRUE, type="lower",col=col3(10),method="color",addCoef.col="black",diag=FALSE,tl.pos="n", cl.pos="n",number.cex = 0.7)

#Years when twitter accounts were created
data %>%
  mutate(user_created_at=substr(user_created_at, 27, 30)) %>%
  count(user_created_at) %>%
  ggplot(aes(x=user_created_at, y=n, group=1)) +
  geom_point() +
  geom_smooth()+
  theme_bw() +
  labs(x="Year", y="Frequency") 
#2011 has most people enrolled in twitter.
#We guess it is mainly because twitter launched 5 new languages.
