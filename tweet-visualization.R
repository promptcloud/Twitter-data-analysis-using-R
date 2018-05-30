# Downloading data from Twitter

library("rtweet")
library("httpuv")

# whatever name you assigned to your created app
appname <- "Data-visualization"

# api key (example below is temporary one; don't use it)
key <- "2jif2VhWYd2Q3XFafn97Y8zO1"

# api secret (example below is temporary one; don't use it)
secret <- "2jif2VhWYd2Q3XFafn97Y8zO12jif2VhWYd2Q3XFafn97Y8zO1"

# create token named "twitter_token"
twitter_token <- create_token(app = appname,
                              consumer_key = key,
                              consumer_secret = secret)

em_tweets <- get_timeline("elonmusk", n = 3200)

#Exploratory analysis based on time

library("ggplot2")
library("lubridate")

em_tweets$created_at <- ymd_hms(em_tweets$created_at)

#Histogram of the tweets

ggplot(data = em_tweets, aes(x = created_at)) +
  geom_histogram(bins=36,aes(fill = ..count..)) +
  xlab("Year") + ylab("Number of tweets") + theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

#Year-wise tweet count

ggplot(data = em_tweets, aes(x = year(created_at))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Year") + ylab("Number of tweets") + 
  scale_x_continuous (breaks = c(2015:2018)) +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")

#Tweets on the days of a week

ggplot(data = em_tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Day of the week") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

#Month-wise tweet count

ggplot(data = em_tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Month") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")

#Month-wise tweets with yearly breakup

ggplot(data = em_tweets,
       aes(month(created_at, label=TRUE, abbr=TRUE), 
           group=factor(year(created_at)), color=factor(year(created_at))))+
  geom_line(stat="count") +
  geom_point(stat="count") +
  labs(x="Month", colour="Year") +
  xlab("Month") + ylab("Number of tweets") +
  theme_minimal()
  
  #Comparison of retweets and original tweets
  
  ggplot(data = em_tweets, aes(x = created_at, fill = is_retweet)) +
  geom_histogram(bins=36) +
  xlab("Time") + ylab("Number of tweets") + theme_minimal() +
  scale_fill_manual(values = c("chartreuse4", "chartreuse3"),
                    name = "Retweet")
  
  #Exploratory analysis based on time of the day                                 
  
  library("hms")
  library("scales")

  #Extract hour and minutes
  em_tweets$time <- hms::hms(second(em_tweets$created_at), 
                           minute(em_tweets$created_at), 
                           hour(em_tweets$created_at))

  #Convert to POSIXct since ggplot does not work with class hms
  
  em_tweets$time <- as.POSIXct(em_tweets$time)

  ggplot(data = em_tweets)+
    geom_density(aes(x = time, y = ..scaled..),
                 fill="red", alpha=0.5) + 
    xlab("Time") + ylab("Density") +
    scale_x_datetime(breaks = date_breaks("2 hours"), 
                     labels = date_format("%H:%M")) +
    theme_minimal()
    
  #Visualizing hashtags
  
  # Frequency of the hashtags
library("dplyr")

# Getting the hashtags from the list format
em_tags_split <- unlist(strsplit(as.character
                                 (unlist(em_tweets$hashtags)), 
                                 '^c\\(|,|"|\\)'))

# Formatting by removing the white spaces
em_tags <- sapply(em_tags_split, 
                  function(y) nchar(trimws(y)) > 0 & !is.na(y))

em_tag_df <- as_data_frame(table(tolower(em_tags_split[em_tags])))
em_tag_df <- em_tag_df[with(em_tag_df,order(-n)),]
em_tag_df <- em_tag_df[1:10,]

ggplot(em_tag_df, aes(x = reorder(Var1, -n), y=n)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags") + ylab("Count")

#Word cloud of tweet text
library("tm")
library("wordcloud")

em_original_tweets <- em_tweets[ which(em_tweets$is_retweet =='FALSE'),] 
                        
tweet_text <- em_original_tweets$text
#Removing numbers, punctations, links and alphanumeric content
tweet_text<- gsub('[[:digit:]]+', '', tweet_text)
tweet_text<- gsub('[[:punct:]]+', '', tweet_text)
tweet_text<- gsub("http[[:alnum:]]*", "", tweet_text)
tweet_text<- gsub("([[:alpha:]])\1+", "", tweet_text)
#creating a text corpus
docs <- Corpus(VectorSource(tweet_text))
# coverting the encoding to UTF-8 to handle funny characters 
docs <- tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
# Converting the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Removing english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Removing stopwords specified by us as a character vector
docs <- tm_map(docs, removeWords, c("amp"))
# creating term document matrix 
tdm <- TermDocumentMatrix(docs)
# defining tdm as matrix
m <- as.matrix(tdm)
# getting word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# creating a data frame with words and their frequencies
em_wf <- data.frame(word=names(word_freqs), freq=word_freqs)

em_wf <- em_wf[1:200,]

# plotting wordcloud

set.seed(1234)
wordcloud(words = em_wf$word, freq = em_wf$freq, 
          min.freq = 1,scale=c(1.8,.5),
          max.words=200, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
          
# Plotting the top 15 most frequently used words
ggplot(data=em_wf[1:15,], aes(x = reorder(word, freq), 
                              y=freq)) +
      geom_bar(stat="identity",fill="steelblue") +
      coord_flip() +
      xlab("Words") + ylab("Frequency count") +
      theme_minimal()
      
# Sentiment analysis
library(tidyr)
library(syuzhet)

# Converting tweets to ASCII to trackle strange characters
em_original_tweets$text <- iconv(em_original_tweets$text, from="UTF-8", to="ASCII", sub="")

# removing mentions
em_original_tweets$text <- gsub("@\\w+","",em_original_tweets$text)

# removing numbers, puncuations, links and alhpanumeric content
em_original_tweets$text<- gsub('[[:digit:]]+', '', em_original_tweets$text)
em_original_tweets$text<- gsub('[[:punct:]]+', '', em_original_tweets$text)
em_original_tweets$text<- gsub("http[[:alnum:]]*", "", em_original_tweets$text)
em_original_tweets$text<- gsub("([[:alpha:]])\1+", "", em_original_tweets$text)

# Getting the sentiment value for the tweets
em_sentiment <- get_nrc_sentiment((em_original_tweets$text))

# Merging the sentiment value with original dataframe
em_original_tweets <- cbind(em_original_tweets, em_sentiment)

# Creating dataframe with date and sentiment
sentiment_dataset <- em_original_tweets[c(2,44:53)]

# Transforming into year, month, date, hour, minute and second format
sentiment_dataset$created_at <- ymd_hms(em_original_tweets$created_at)

# Dataframe with mean value of sentiments with 2-month breaks
sentiment_time <- sentiment_dataset %>%
  mutate(created_at = cut(created_at, breaks="2 months")) %>%
  group_by(created_at) %>%
  summarize(negative = mean(negative),
            positive = mean(positive)) %>%
  gather('sentiment', 'mean_value', negative, positive)

# Plot average sentiment score over time  
ggplot(data = sentiment_time, aes(x = as.Date(created_at), y = mean_value, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("firebrick3", "springgreen4")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_date(breaks = date_breaks("6 months"), 
               labels = date_format("%Y-%b")) +
  ylab("Average sentiment score") + xlab ("Time") +
  ggtitle("Sentiment Over Time") + theme_minimal()

# Dataframe with average sentiment value for days of a week
sentiment_wday <- sentiment_dataset %>%
  group_by(weekday = lubridate::wday(created_at, label = TRUE)) %>%
  summarize(negative = mean(negative),
            positive = mean(positive)) %>%
  gather('sentiment', 'mean_value', negative, positive)

# Plot for Sentiment During the Week 
ggplot(data = sentiment_wday, aes(x = weekday, y = mean_value, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Week") + theme_minimal()

# Dataframe with average sentiment value for months of a year
sentiment_month <- sentiment_dataset %>%
  group_by(month = lubridate::month(created_at, label = TRUE)) %>%
  summarize(negative = mean(negative),
            positive = mean(positive)) %>%
  gather('sentiment', 'mean_value', negative, positive)

# Plot for Sentiment During the Year
ggplot(data = sentiment_month, aes(x = month, y = mean_value, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year") + theme_minimal()

# Dataframe with cumulative value of the sentiments
sentimentscores<-data.frame(colSums(em_sentiment[,]))

# Dataframe with sentiment and score as columns
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

# Plot for the cumulative sentiments
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()       
