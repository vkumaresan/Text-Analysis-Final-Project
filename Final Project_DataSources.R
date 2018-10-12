# Final Project: Mental Health

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


example_urls = reddit_urls(search_terms="science")
example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
example_data = get_reddit(search_terms="economy")


# Tumblr API
library(devtools)
install_github('klapaukh/tumblR')
library(tumblR)
install.packages('wordcloud')
library(wordcloud)
consumer_key <- 'AXmWBmjPnNKqh5VNguL58fSG12ExgERY4YL0kwbCDKLWKwwzVo'

setup_tumblr_apikey(consumer_key)
depressionPosts <- get_tagged("depression")
info <- get_info("todayifeltproud.tumblr.com")

all <- get_posts("todayifeltproud.tumblr.com")
posts = all$posts
corpus =  unlist(sapply(posts, function(x) x$tags))
wordcloud(corpus,colors=brewer.pal(8, "Dark2"))


