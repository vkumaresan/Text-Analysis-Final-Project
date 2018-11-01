# Reddit Data Analysis
# Text Analysis Final Project
# Viggy Kumaresan

# total keyword search database: depression_reddit_total_final

# Load data
depression_reddit_total_sub_final <- read.csv('SubredditDepression.csv')
depression_reddit_total_final <- read.csv('SearchDepression.csv')

### Dictionary Based Analysis
# Count Top Words
library(tidyverse)
library(tm)
library(tidytext)

tidy_total <- depression_reddit_total_final %>%
  select(post_date,post_text) %>%
  unnest_tokens("word", post_text)

top_words_total<-
  tidy_total %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

top_20_total<-top_words_total[1:20,]
#create factor variable to sort by frequency
top_words_total$word <- factor(top_words_total$word, levels = top_words_total$word[order(top_words_total$n,decreasing=TRUE)])

library(ggplot2)
ggplot(top_20_total, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Depression Search")+
  xlab("")+
  guides(fill=FALSE)

# tf-idf
tidy_total_tfidf<- depression_reddit_total_final %>%
  select(post_date,post_text) %>%
  unnest_tokens("word", post_text) %>%
  anti_join(stop_words) %>%
  count(word, post_date) %>%
  bind_tf_idf(word, post_date, n)

top_tfidf_total<-tidy_total_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf_total$word[1]

# Create our own dictionary

depression_dictionary <- c('prescription', 'lonely', 'sad')
library(stringr)
detected <-depression_reddit_total_final[str_detect(depression_reddit_total_final$post_text, depression_dictionary),]
head(detected$post_text)

# Sentiment Analysis

depression_total_sentiment <- tidy_total %>%
  inner_join(get_sentiments("bing")) %>%
  count(post_date, sentiment) 

head(depression_total_sentiment)

tidy_total$date<-as.Date(tidy_total$post_date, 
                                format="%Y-%m-%d %x")

depression_sentiment_plot <-
  tidy_total%>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)


ggplot(depression_sentiment_plot, aes(x=date, y=n))+
  geom_line(color="red")+
  theme_minimal()+
  ylab("Frequency of Negative Words in Depression Search")+
  xlab("Date")


# Topic Modeling
# Create DTM 
library(tm)
depression_total_corpus <- Corpus(VectorSource(as.vector(depression_reddit_total_final$post_text)))
depression_total_corpus
depression_total_corpus = tm_map(depression_corpus, removeWords, stopwords('english')) #remove stopwords
depression_total_DTM <- DocumentTermMatrix(depression_total_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_total_DTM , 1, sum) #Find the sum of words in each Document
depression_total_DTM   <- depression_total_DTM[rowTotals> 0, ]           #remove all docs without words

# Create topic model
library(topicmodels)
library(tidytext)
library(tidyverse)

topic_model_total<-LDA(depression_total_DTM, k=5, control = list(seed = 321)) # k = # of topics in corpus

DEP_topics_total <- tidy(topic_model_total, matrix = "beta")

dep_top_terms <- 
  DEP_topics_total %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


dep_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()






## subreddit specific search database: depression_reddit_total_sub_final
library(topicmodels)
library(tidytext)
library(tidyverse)

# Post Text

# Create unique posts and unique comments dataframes

sub_unique_posts <- depression_reddit_total_sub_final[!duplicated(depression_reddit_total_sub_final[,c('post_text')]),]
dim(sub_unique_posts)
str(sub_unique_posts)

sub_unique_comments <- depression_reddit_total_sub_final[!duplicated(depression_reddit_total_sub_final[,c('comment')]),]
dim(sub_unique_comments)
str(sub_unique_comments)

# Change post text and comments to character format
sub_unique_posts$post_text <- as.character(sub_unique_posts$post_text)
sub_unique_comments$comment <- as.character(sub_unique_comments$comment)


## Post Text Analysis

# Count Top Words

tidy_sub_post <- sub_unique_posts %>%
  select(post_text) %>%
  unnest_tokens("word", post_text)

top_words_sub_post<-
  tidy_sub_post %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

top_20_sub_post<-top_words_sub_post[1:20,]
#create factor variable to sort by frequency
top_words_sub_post$word <- factor(top_words_sub_post$word, levels = top_words_sub_post$word[order(top_words_sub_post$n,decreasing=TRUE)])

library(ggplot2)
ggplot(top_20_sub_post, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Depression Subreddit Posts")+
  xlab("")+
  guides(fill=FALSE)

# tf-idf
tidy_sub_tfidf_post<- sub_unique_posts %>%
  select(post_date,post_text) %>%
  unnest_tokens("word", post_text) %>%
  anti_join(stop_words) %>%
  count(word, post_date) %>%
  bind_tf_idf(word, post_date, n)

top_tfidf_sub_post<-tidy_sub_tfidf_post %>%
  arrange(desc(tf_idf))

top_tfidf_sub_post$word[1]

# Topic Modeling
library(tm)

# Make corpus of Post Text
sub_corpus_post <- Corpus(VectorSource(as.vector(sub_unique_posts$post_text)))
sub_corpus_post
sub_corpus_post = tm_map(sub_corpus_post, removeWords, stopwords('english')) #remove stopwords
sub_DTM_post <- DocumentTermMatrix(sub_corpus_post, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(sub_DTM_post , 1, sum) #Find the sum of words in each Document
sub_DTM_post <- sub_DTM_post[rowTotals> 0, ]           #remove all docs without words

# Create topic model
topic_model_sub_post<-LDA(sub_DTM_post, k=7, control = list(seed = 321)) # k = # of topics in corpus

DEP_topics_sub_post <- tidy(topic_model_sub_post, matrix = "beta")

dep_top_terms_sub_post <- 
  DEP_topics_sub_post %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


dep_top_terms_sub_post %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



## Comment Analysis

# Count Top Words

tidy_sub_comment <- sub_unique_comments %>%
  select(comment) %>%
  unnest_tokens("word", comment)

top_words_sub_comment<-
  tidy_sub_comment %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

top_20_sub_comment <- top_words_sub_comment[1:20,]
#create factor variable to sort by frequency
top_words_sub_comment$word <- factor(top_words_sub_comment$word, levels = top_words_sub_comment$word[order(top_words_sub_comment$n,decreasing=TRUE)])

library(ggplot2)
ggplot(top_20_sub_comment, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Depression Subreddit Comments")+
  xlab("")+
  guides(fill=FALSE)

# tf-idf
tidy_sub_tfidf_comment<- sub_unique_comments %>%
  select(comm_date,comment) %>%
  unnest_tokens("word", comment) %>%
  anti_join(stop_words) %>%
  count(word, comm_date) %>%
  bind_tf_idf(word, comm_date, n)

top_tfidf_sub_comment<-tidy_sub_tfidf_comment %>%
  arrange(desc(tf_idf))

top_tfidf_sub_comment$word[1]

# Topic Modeling
library(tm)

# Make corpus of Post Text
sub_corpus_comment <- Corpus(VectorSource(as.vector(sub_unique_comments$comment)))
sub_corpus_comment
sub_corpus_comment = tm_map(sub_corpus_comment, removeWords, stopwords('english')) #remove stopwords
sub_DTM_comment <- DocumentTermMatrix(sub_corpus_comment, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(sub_DTM_comment , 1, sum) #Find the sum of words in each Document
sub_DTM_comment <- sub_DTM_comment[rowTotals> 0, ]           #remove all docs without words

# Create topic model
topic_model_sub_comment<-LDA(sub_DTM_comment, k=7, control = list(seed = 321)) # k = # of topics in corpus

DEP_topics_sub_comment <- tidy(topic_model_sub_comment, matrix = "beta")

dep_top_terms_sub_comment <- 
  DEP_topics_sub_comment %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


dep_top_terms_sub_comment %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
