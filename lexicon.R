library(tidyverse)
library(tidytext)


reviews <- read_csv("C:/Users/Prarthana/OneDrive - General Sir John Kotelawala Defence University/5th sem/Marketing/lexicons/Outscraper-cinnamon.csv")
names(reviews)
review_words<-reviews%>%
  unnest_tokens(word,review_text)

head(stop_words)
review_sentiment<-review_words%>%
  anti_join(stop_words,by="word")

review_sentiment%>%count(word,sort=T)%>%top_n(20)



library(ggplot2)

ggplot(review_sentiment%>%count(word,sort=T)%>%top_n(20),aes(reorder(word,n),n))+
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = n),color="#0f190f", hjust = -0.05, size = 2)+
  theme_bw()+
  coord_flip()+
  xlab("Number of Occurences")+
  ylab("Words used")+
  ggtitle("Number of Occurences of each word")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#------------------Doing sentiment analysis using Bing Lexicon.--------------------

review_bing<-review_sentiment%>%
  inner_join(get_sentiments("bing"),by="word")%>%
  ungroup()

head(review_bing, 10)

review_bing_sentiment_freq <- review_bing %>%
  count(sentiment)
review_bing_sentiment_freq
review_bing %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = "Number of Occurrences", y = "Words", title = "Top 10 Positive and Negative Sentiment Words Used in Headlines (using Bing lexicon)") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  geom_text(aes(label = n), hjust = --1.3, size = 3, color = "black")


#-----------------sentiment analysis using AFINN lexicon---------------------------

library(textdata)
review_afinn <- review_sentiment %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(sentiment = ifelse(value < 0, "Negative", "Positive"))




table(review_afinn$value,review_afinn$sentiment)


#Top 10 negative and positive words in the captions


review_afinn%>%
  group_by(sentiment)%>%
  count(word,sentiment)%>%
  top_n(10,n)%>%
  ungroup()%>%
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment))+
  geom_col(show.legend=F)+
  geom_text(aes(label = n), hjust = --1.3, size = 3, color = "black") +
  facet_wrap(~sentiment,scales="free")+
  coord_flip()+
  labs(x="number of occurences",y="Words",title="Top 10 words for each sentiment used captions using afinn lexicon")+
  theme(plot.title = element_text(size = 8, face = "bold"))

