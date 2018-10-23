# Reddit Data Collection
# Viggy Kumaresan

# Reddit API
# install.packages('RedditExtractoR')

library(RedditExtractoR)

# Get reddit data from search query 
mental_health <- get_reddit(search_terms = NA, regex_filter = "", subreddit = 'mentalhealth',
                            cn_threshold = 1, page_threshold = 1, sort_by = "comments",
                            wait_time = 2)

depression <- get_reddit(subreddit = 'depression', wait_time = 2)
search_data = get_reddit(search_terms="depression")

depression_corpus <- Corpus(VectorSource(as.vector(depression$post_text)))
corpus = unlist(sapply(depression_corpus, function(x) x['content']))



depression_urls = reddit_urls(search_terms = 'depression')

depression_urls = list(depression_urls$URL)

example_urls = reddit_urls(search_terms="science")
example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
example_data = get_reddit(search_terms="economy")



# Would have to create a for loop to go through tags (keywords) and create corpus for each

depression_tags_reddit <- c('depression', 'sad', 'upset', 'lonely')

depression_reddit = data.frame()

for (item in depression_tags_reddit){
  posts = get_reddit(search_terms = item)
  depression_reddit <- rbind(depression_reddit, posts)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

depression_reddit2 = data.frame()

for (item in depression_tags_reddit){
  posts2 = get_reddit(search_terms = item)
  depression_reddit2 <- rbind(depression_reddit2, posts2)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

depression_reddit_total <- rbind(depression_reddit, depression_reddit2)
depression_reddit_total <- depression_reddit_total[!duplicated(depression_reddit_total), ]


depression_reddit3 = data.frame()

for (item in depression_tags_reddit){
  posts3 = get_reddit(search_terms = item)
  depression_reddit3 <- rbind(depression_reddit3, posts3)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

# Search by subreddit

depression_subreddits_list <- c('depression', 'depression_help')

depression_subreddits = data.frame()

for (item in depression_subreddits_list){
  posts_sub = get_reddit(subreddit = item)
  depression_subreddits <- rbind(depression_subreddits, posts_sub)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

depression_subreddits2 = data.frame()

for (item in depression_subreddits_list){
  posts_sub2 = get_reddit(subreddit = item)
  depression_subreddits2 <- rbind(depression_subreddits2, posts_sub2)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

depression_reddit_total_sub <- rbind(depression_subreddits, depression_subreddits2)

depression_subreddits3 = data.frame()

for (item in depression_subreddits_list){
  posts_sub3 = get_reddit(subreddit = item)
  depression_subreddits3 <- rbind(depression_subreddits3, posts_sub3)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}




# Create DTM 
library(tm)
depression <- get_reddit(search_terms="depression")
depression_corpus <- Corpus(VectorSource(as.vector(depression$post_text)))
depression_corpus
depression_corpus = tm_map(depression_corpus, removeWords, stopwords('english')) #remove stopwords
depression_DTM <- DocumentTermMatrix(depression_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_DTM , 1, sum) #Find the sum of words in each Document
depression_DTM   <- depression_DTM[rowTotals> 0, ]           #remove all docs without words

# Topic Modeling
library(topicmodels)
topic_model<-LDA(depression_DTM, k=10, control = list(seed = 321)) # k = # of topics in corpus

DEP_topics <- tidy(topic_model, matrix = "beta")

dep_top_terms <- 
  DEP_topics %>%
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
