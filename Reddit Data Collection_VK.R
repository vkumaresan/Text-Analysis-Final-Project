# Reddit Data Collection
# Text Analysis Final Project
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

# depression_reddit = data.frame()

for (item in depression_tags_reddit){
  posts = get_reddit(search_terms = item)
  depression_reddit <- rbind(depression_reddit_total, posts)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

# depression_reddit2 = data.frame()

# for (item in depression_tags_reddit){
  # posts2 = get_reddit(search_terms = item)
  # depression_reddit2 <- rbind(depression_reddit2, posts2)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
# }

# depression_reddit_total <- rbind(depression_reddit, depression_reddit2)

# Remove duplicates
depression_reddit_total_final <- depression_reddit_total[!duplicated(depression_reddit_total), ]
dim(depression_reddit_total_final)







# Search by subreddit

depression_subreddits_list <- c('depression', 'depression_help')

# depression_subreddits = data.frame()

for (item in depression_subreddits_list){
  posts_sub = get_reddit(subreddit = item)
  depression_subreddits_total_sub <- rbind(depression_reddit_total_sub, posts_sub)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
}

# depression_subreddits2 = data.frame()

# for (item in depression_subreddits_list){
  # posts_sub2 = get_reddit(subreddit = item)
  # depression_subreddits2 <- rbind(depression_subreddits2, posts_sub2)
  # corpus = unlist(sapply(posts, function(x) x$tags))
  # depression_reddit = list.append(depression_reddit, corpus)
# }

# depression_reddit_total_sub <- rbind(depression_subreddits, depression_subreddits2)

# Remove duplicates
depression_reddit_total_sub_final <- depression_subreddits_total_sub[!duplicated(depression_subreddits_total_sub), ]
dim(depression_reddit_total_sub_final)

# In the future, add to one dataframe for search and one for subreddit, and then remove duplicates in each iteration.


