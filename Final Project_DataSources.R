# Final Project: Mental Health
# Viggy Kumaresan
# Exploring Data Sources

# Reddit API
# install.packages('RedditExtractoR')

library(RedditExtractoR)

# Graph file for reddit thread
my_url = "https://www.reddit.com/r/mentalhealth/comments/9mb2es/im_a_therapist_for_anyone_looking_for_a_free/"
url_data = reddit_content(my_url)
graph_object = construct_graph(url_data)

# Get reddit data from search query 
mental_health <- get_reddit(search_terms = NA, regex_filter = "", subreddit = 'mentalhealth',
           cn_threshold = 1, page_threshold = 1, sort_by = "comments",
           wait_time = 2)

depression <- get_reddit(subreddit = 'depression', wait_time = 2)
search_data = get_reddit(search_terms="depression")

depression_corpus <- Corpus(VectorSource(as.vector(depression$post_text)))
corpus = unlist(sapply(depression_corpus, function(x) x['content']))



test <- get_reddit(subreddit = 'mildlyinteresting', wait_time = 2)

depression_urls = reddit_urls(search_terms = 'depression')

depression_urls = list(depression_urls$URL)

example_urls = reddit_urls(search_terms="science")
example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
example_data = get_reddit(search_terms="economy")



# Would have to create a for loop to go through tags (keywords) and create corpus for each

depression_tags_reddit <- c('depression', 'sad', 'upset', 'lonely')

depression_reddit = list()

for (item in depression_tags_reddit){
  posts = get_reddit(search_terms = item)
  corpus = unlist(sapply(posts, function(x) x$tags))
  depression_reddit = list.append(depression_reddit, corpus)
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




### Tumblr API
# library(devtools)
# install_github('klapaukh/tumblR')
library(tumblR)
# install.packages('wordcloud')
library(wordcloud)
consumer_key <- 'AXmWBmjPnNKqh5VNguL58fSG12ExgERY4YL0kwbCDKLWKwwzVo'

setup_tumblr_apikey(consumer_key)
depressionPosts <- get_tagged("depression")
info <- get_info("todayifeltproud.tumblr.com")

all <- get_posts("todayifeltproud.tumblr.com")
posts = all$posts
corpus =  unlist(sapply(posts, function(x) x$tags))
wordcloud(corpus,colors=brewer.pal(8, "Dark2"))

# Search through posts that were tagged 'depression'
depression <- get_tagged('depression')
corpus = unlist(sapply(depression, function(x) x$body))
depression_corpus <- Corpus(VectorSource(as.vector(corpus)))
depression_corpus
depression_corpus = tm_map(depression_corpus, removeWords, stopwords('english')) #remove stopwords
depression_DTM <- DocumentTermMatrix(depression_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_DTM , 1, sum) #Find the sum of words in each Document
depression_DTM   <- depression_DTM[rowTotals> 0, ]           #remove all docs without words



depression_tags <- c('depression', 'sad', 'upset', 'lonely')

depression = list()

# Would have to create a for loop to go through tags (keywords) and create corpus for each
library(rlist)
for (item in depression_tags){
  posts = get_tagged(item)
  corpus = unlist(sapply(posts, function(x) x$body))
  depression = list.append(depression, corpus)
}
  
# Then create DTM of depression
library(tm)
depression_corpus <- Corpus(VectorSource(as.vector(depression)))
depression_corpus
depression_corpus = tm_map(depression_corpus, removeWords, stopwords('english')) #remove stopwords
depression_DTM <- DocumentTermMatrix(depression_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(depression_DTM , 1, sum) #Find the sum of words in each Document
depression_DTM   <- depression_DTM[rowTotals> 0, ]           #remove all docs without words


# Topic Modeling
library(topicmodels)
topic_model<-LDA(depression_DTM, k=10, control = list(seed = 321)) # k = # of topics in corpus

AP_topics <- tidy(topic_model, matrix = "beta")

ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





### Twitter API
library(rtweet)

# Set up credentials
app_name <- "TextAnalysisClass"
consumer_key <- 'b8zwnOshvar3pzxB7oE5QmC9i'
consumer_secret <- 'O6rjT5QstBymHHOEPlWUppsHY6i7IoAElT8T6AwKGG8g0BOso7'
access_token <- '850073991322771457-AtShABZ7yHT4STTYhcZPhhMz15mrMwU'
access_secret <- 'nQ0YnPJB6wAD5X71DUVu2siUPTlJUgPAfaVmUuvVGU1H6'

# Create token
create_token(app = app_name, consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token,
             access_secret = access_secret, set_renv = TRUE)

# Search #Korea tweets
depression_tweets <- search_tweets("#depression", n = 50, include_rts = FALSE)

head(depression_tweets$text)

# Plot frequency of tweets

ts_plot(depression_tweets, "3 hours") + 
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face="bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Depression from the Past Day",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Seach_tweets function
depression_tweets <- search_tweets("depression",
                           "lang:en", geocode = lookup_coords("usa"),
                           n = 1000, type = "recent", include_rts = FALSE
)

# Geocode tweets
geocoded <- lat_lng(depression_tweets)

# Plot on a map of US
library(maps)
par(mar = c(0,0,0,0))
maps::map("state", lwd = .25)
with(geocoded, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))



# Create DTM 
library(tm)
depression_corpus <- Corpus(VectorSource(as.vector(depression_tweets$text)))
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

### Webscraping

library(rvest)
library(tidytext)
library(dplyr)
library(SnowballC)

page <-'http://www.depression-understood.org/forum/'
topics <- c("viewtopic.php?f=5&t=35527",
            "viewtopic.php?f=5&t=35092",
            "viewtopic.php?f=5&t=34648",
            "viewtopic.php?f=5&t=35025")
bb_text <-list()
for(i in seq_along(topics)){
  depunderstood_page <- read_html(paste0(page,topics[i]))
  path <- '.content'
  bb_posts <- html_nodes(depunderstood_page,path)
  bb_text[[i]] <- html_text(bb_posts) %>%
    tolower() %>%
    gsub("(\\n)|(\\s{2,})","",.) %>%
    gsub("(.*underwood)|(.*reply)","",.) %>%
    gsub("[a-z]\\.[a-z]","\\1 \\3",.)
  
}
names(bb_text) <- topics

data(stop_words)

bb_tidy <- bb_text %>%
  unlist() %>%
  data_frame(text = .) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n))

bb_stem <- bb_tidy %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#select only top 20 words
top_20<-bb_stem[1:20,]

#create factor variable to sort by frequency
bb_stem$word <- factor(bb_stem$word, levels = bb_stem$word[order(bb_stem$n,decreasing=TRUE)])


library(ggplot2)
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Depression_Understood Forum Posts")+
  xlab("")+
  guides(fill=FALSE)

# Create DTM 
library(tm)
depression_corpus <- Corpus(VectorSource(as.vector(bb_stem$word)))
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

