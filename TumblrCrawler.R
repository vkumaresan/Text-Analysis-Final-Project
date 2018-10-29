# Part1 Browser Automation
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(RSelenium)
library(rvest)
library(urltools)
library(devtools)
library(tumblR)
library(wordcloud)
```
# Initiate a chrome auto control browser.
```{r}
rD <- rsDriver()
remDr <- rD$client
```
# Go through the alert webpage.
```{r}
remDr$navigate("https://www.tumblr.com/psa/search/depression")
webElem <- remDr$findElement(using = 'css selector',"#psa_actions_display > div.basic_message_container.fill_container.center_wrap > div > a.footer_link")
webElem$clickElement()
webElem2 <- remDr$findElement("css", "body")
webElem2$sendKeysToElement(list(key = "end"))
```

# Autimatically scroll down to the end of the page. The code id functioning, but tryCatch does not work. Need to take a close look at it.  
```{r}
tryCatch({
    for (i in c(1:100)) {
      Sys.sleep(1)
      webElem2$sendKeysToElement(list(key = "end"))
    }
  },
  error = function(e) {an.error.occured <- 'Reached the end of the page'; print(an.error.occured); break;}
)
```

# Extract all post urls from html node
```{r}
postElement <- remDr$findElements(using = 'css', 'div[class = post-info-tumblelog] a')
tumblrs = list()
tumblrs=sapply(postElement, function(x){x$getElementAttribute('href')})
for(tumblr in tumblrs)
  print(tumblr)
```

# Authorize Tumblr Developer account
```{r}
consumer_key <- 'AXmWBmjPnNKqh5VNguL58fSG12ExgERY4YL0kwbCDKLWKwwzVo'
setup_tumblr_apikey(consumer_key)
```

# Authorize Tumblr.
```{r}
# tumblr_blog <- list()
# i <- 0
# for (domain in domains[,1]) {
#   print(domain)
#   i <- i + 1
#   print(i)
#   allpost <- get_posts(domain)
#   tumblr_blog[[i]] <- allpost$posts
# }
```

# Get domain url and craw each blog.
```{r}
# Extract all domain url.
tumblr_blog_url <- list()
for (i in 1:length(tumblrs)) {
  tumblr_blog_url[[i]] <- suffix_extract(domain(tumblrs[[i]]))[1]
}

# Craw all the data from each post.
tumblr_blog <- list()
i <- 0
for (domain in tumblr_blog_url) {
  i <- i + 1
  Sys.sleep(1)
  tryCatch(
    {
      print(i)
      allpost <- get_posts(domain)
      tumblr_blog[[i]] <- allpost$posts
    },
    error = function(e) {an.error.occured <- 'Extra authorization required.'; print(an.error.occured) }
  )
}

```

# Transform text into corpus.
```{r}
for (post in tumblr_blog) {
  corpus =  unlist(sapply(post, function(x) x$tags))
  wordcloud(corpus,colors=brewer.pal(8, "Dark2"))
}
```

# Store data into a dataframe.
```{r}
# Firstly, lets initiate our dataframe
tumblr.df <- data.frame(
  Keyword = character(),
  Platform = character(),
  Created_time = character(),
  Title = character(),
  Content = character(),
  Author = character(),
  URL = character(),
  Likes = integer(),
  Views = character(),
  #Comments = list(),
  stringsAsFactors=FALSE
)
```

```{r}
# Secondly, parse data and store into dataframe.
length(tumblr_blog)
for (tumblrs in tumblr_blog) {
  for (tumblrs_one_blog in tumblrs) {
    tryCatch({
    if(length(tumblrs_one_blog[["type"]] == "text")) {
      Content <- tumblrs_one_blog[["body"]]
      Platform <- 'Tumblr'
      Created_time <- tumblrs_one_blog[["timestamp"]]
      Title <- tumblrs_one_blog[["title"]]
      if (is.null(Title)) {Title = 'no title'}
      Author <- tumblrs_one_blog[["blog"]][["name"]]
      URL <- tumblrs_one_blog[["post_url"]]
      Likes <- tumblrs_one_blog[["note_count"]]
      Views <- '0'
      Keyword <- 'Depression'
      tumblr.df[nrow(tumblr.df) + 1, ] = 
      c(Keyword, Platform, Created_time, Title, Content, Author, URL, Likes, Views)
    }
    },
  error = function(e) {an.error.occured <- 'Data insertion error'; print(e);}
)
  } 
  
  
  #print(variable[["posts"]][[1]][["caption"]])
}
```


