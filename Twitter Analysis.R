# SOCIAL MEDIA HANDLE : TWITTER - Tweets Analytics

# Importing Libraries

library(tidyverse)
library(NLP)
library(tm)
library(twitteR)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(sentimentr)

# Setting Up Twitter Account in R using API Keys

ckey <- "xxxxxxxxxxxxxxxx"
skey <- "xxxxxxxxxxxxxxxxxxxxxxxx"
access <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
accesssecret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(ckey, skey, access, accesssecret)

# Trends Location ID

trend <- availableTrendLocations()

# Getting Trends

world <- getTrends(1)
delhi <- getTrends(20070458)
mumbai <- getTrends(2295411)

# Top 10 World-Wide Trending Tweets
head(world,10)

# Importing tweets from Twitter basis keyword, language, geo-code & date filters

tweets = searchTwitter(searchString = "keyword", lang = "en", n = 20000, since = '2019-05-25', 
                       until = '2019-05-31', geocode = '20.5937, 78.9629, 1000mi', resultType = "recent")

# Search has generated a list of tweets but the properties of tweets as well
# Our job is to pull the Text from all the tweets

# Stripping Retweets, Conversion to a Data Frame & Exporting the data

pure_tweets <- strip_retweets(tweets)
pure_tweetsdf <- twListToDF(pure_tweets)
write.csv(x = pure_tweetsdf, file = "Clean Tweets.csv", row.names = F)

# Extracting Text

tweets.text = sapply(pure_tweets, function(x) x$getText())

# Cleaning the tweets - Unstructured Text - Using tm package

tweets.text = iconv(tweets.text, "UTF-8", "ASCII")

# Creating a Corpus of all the tweets

document.matrix <- Corpus(VectorSource(tweets.text))

# Creating a Term Document Matrix - Removing URL's, Punctuations, Numbers & Stopwords

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
doc.matrix <- tm_map(document.matrix, content_transformer(removeURL))

term.document.matrix <- TermDocumentMatrix(doc.matrix,
                                           control = list(removePunctuation = T, stopwords = c("keyword",stopwords("english")),
                                                          removeNumbers = T, tolower = T))

# Convert Object to Matrix Type

term.document.matrix = as.matrix(term.document.matrix)

# Word Frequency Distribution

word.freq = sort(rowSums(term.document.matrix), decreasing = T)

# Creating Data Frame

df <- data.frame(Word = names(word.freq), Frequency = word.freq)

# Word Cloud Visualization

wordcloud(df$Word, df$Frequency, random.order = F,
          colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 200)

# Alternative Word Cloud (3-D)

wordcloud2(df, size = 2)

# Extracting Cleaned Data

data <- tweets.text[!is.na(tweets.text)] %>% as.data.frame()
colnames(data) <- c("Comments")
comments <- iconv(data$Comments, "UTF-8")

# Performing Sentiment Analysis using BING Method

sentiments <- get_sentiment(comments, method = "bing") %>% as.data.frame()
colnames(sentiments) <- "Score"
sentiments$Category <- ifelse(sentiments$Score<0, "Negative",
                              ifelse(sentiments$Score>0, "Positive", "Neutral"))

# Extracting Key-Words from the Algorithm

key_words <- extract_sentiment_terms(comments)

# Barplot of Sentiments

s <- sentiments %>% group_by(Category) %>% summarise(Count = n())
s$percentage <- round(100*s$Count/sum(s$Count), 2)
s %>% ggplot(aes(Category, percentage)) + geom_bar(stat = "identity", fill = c("red","blue","darkgreen"))+
  labs(title = "Sentiment Analysis - Twitter Data", subtitle = "Keyword Searched : Keyword", x = "Category", y = "Percentage") + 
  geom_label(aes(Category, percentage, label = percentage)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic()
