# Twitter Data Collection
# Viggy Kumaresan

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

# Search #depression tweets
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
                                   n = 18000, type = "recent", include_rts = FALSE)

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
library(tidyverse)
library(tidytext)
library(rvest)

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

## stream depression tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "depression, sad, depressed, lonely",
  timeout = 60 * 60 * 24 * 1,
  file_name = "tweetsaboutdepression.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
depression_tidy <- parse_stream("tweetsaboutdepression.json")
