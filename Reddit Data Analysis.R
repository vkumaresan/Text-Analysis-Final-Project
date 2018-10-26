# Reddit Data Analysis
# Text Analysis Final Project
# Viggy Kumaresan

# total keyword search database: depression_reddit_total_final

# Create DTM 
library(tm)
depression_total_corpus <- Corpus(VectorSource(as.vector(depression_reddit_total_final$post_text)))
depression_total_corpus
depression_total_corpus = tm_map(depression_corpus, removeWords, stopwords('english')) #remove stopwords
depression_total_DTM <- DocumentTermMatrix(depression_total_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_total_DTM , 1, sum) #Find the sum of words in each Document
depression_total_DTM   <- depression_total_DTM[rowTotals> 0, ]           #remove all docs without words

# Topic Modeling
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


# subreddit specific search database: depression_reddit_total_sub_final

# Create DTM 
library(tm)
depression_total_sub_corpus <- Corpus(VectorSource(as.vector(depression_reddit_total_sub_final$post_text)))
depression_total_sub_corpus
depression_total_sub_corpus = tm_map(depression_total_sub_corpus, removeWords, stopwords('english')) #remove stopwords
depression_total_sub_DTM <- DocumentTermMatrix(depression_total_sub_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_total_sub_DTM , 1, sum) #Find the sum of words in each Document
depression_total_sub_DTM   <- depression_total_sub_DTM[rowTotals> 0, ]           #remove all docs without words

# Topic Modeling
library(topicmodels)
library(tidytext)
library(tidyverse)

topic_model_total_sub<-LDA(depression_total_sub_DTM, k=7, control = list(seed = 321)) # k = # of topics in corpus

DEP_topics_total_sub <- tidy(topic_model_total_sub, matrix = "beta")

dep_top_terms_sub <- 
  DEP_topics_total_sub %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


dep_top_terms_sub %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
