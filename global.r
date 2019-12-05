if (!require("rtweet")) install.packages("rtweet")
if (!require("tm")) install.packages("tm")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("wordcloud2")) install.packages("wordcloud2")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("tidytext")) install.packages("tidytext")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")
if (!require("remotes")) install.packages("remotes")
if (!require("ggplot2")) install.packages("ggplot2")


library("rtweet")
library("tm") 
library("SnowballC") 
library("wordcloud") 
library("wordcloud2")
library("RColorBrewer")
library("tidytext")
library("dplyr")
library("scales")
library("remotes")
install_github("EmilHvitfeldt/textdata")
install_github("juliasilge/tidytext")
library("ggplot2")


# token <- create_token(app = "twitter_analysis_hkbu", 
#                       consumer_key = "GIDTwKtSL6zmalcKNuxziO3t5",
#                       consumer_secret = "jbgCGxuJGK1sLN1V9Qi2InX2LYHsauD8vFcMTSRKwTyWYsTJXw",
#                       access_token = "1184726061742821376-WzaQqrotIUlCquPO9V9jmnzr7FeFY1",
#                       access_secret = "3sfdNVGnetoHPrcfVZthhEL689FtUlee4YYzT5lhyxoBs",
#                       set_renv = TRUE)

# abstract function
abstract <- function(amount, hashtag){
  marvel <- search_tweets(hashtag, n = amount, include_rts = FALSE, lang = "en")
  df <- as.data.frame(marvel)
  return(df)
}


# clean function
clean <- function(df){
  df.v <- VectorSource(df$text) 
  df.c <- SimpleCorpus(df.v)
  df.c.p <- tm_map(df.c, content_transformer(tolower))
  df.c.p <- tm_map(df.c.p, removeNumbers)
  df.c.p <- tm_map(df.c.p, removeWords, stopwords("english"))
  df.c.p <- tm_map(df.c.p, removeWords, c("marvel"))
  df.c.p <- tm_map(df.c.p, removePunctuation)
  df.c.p <- tm_map(df.c.p, stripWhitespace)
  return(df.c.p)
}


# word cloud function
cloud <- function(df.c.p){
  dtm <- TermDocumentMatrix(df.c.p)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  wordcloud(words = d$word, freq = d$freq,
            max.words=100, random.order=FALSE, rot.per=0.0,
            colors=brewer.pal(8, "Dark2")) 
  #min.freq = 1, ordered.colors = TRUE
  #letterCloud(data = d, word = hashtag, color='random-light' )
  # wordcloud2(data = d, color='random-dark')#, figPath = "twitter.png"
}

# go further, wordfrequency function
wordfrequency <- function(df.c.p){
  dtm <- TermDocumentMatrix(df.c.p)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col = "lightgreen", main = "Most Frequent Words",
          ylab = "Word Frenquency")
}

#Histogram

tweet_len_his <- function(df){
  options(scipen = 200)
  hist(df$display_text_width, main = "Tweet Length",xlab = "Number of Word", col = "lightblue")
}

follower_num_his <- function(df){
  options(scipen = 200)
  hist(df$followers_count,main = "How Many Followers Do They Have?", xlab = "Number of Followers", col = "yellow")
}

friend_num_his <- function(df){
  options(scipen = 200)
  hist(df$friends_count, main = "How Many Friends Do They Have?", xlab = "Number of Friends", col = "pink")
}

like_num_his <- function(df){
  options(scipen = 200)
  hist(df$favourites_count, main = "Popularity", xlab = "Number of 'Like'", col = "orange")
}

# histogram <- function(df.c.p){
#   options(scipen = 200)
#   hist(df$display_text_width, main = "Tweet Length",xlab = "Number of Word", col = "lightblue", ) #, xlab = "???"
#   hist(df$followers_count,main = "How Many Followers Do They Have?", xlab = "Number of Followers", col = "steelblue") 
#   hist(df$friends_count, main = "How Many Friends Do They Have?", xlab = "Number of Friends", col = "pink") 
#   hist(df$favourites_count, main = "Popularity", xlab = "Number of 'Like'", col = "lightsteelblue") 
# }


# sentiment counts
sentiment_counts <- function(df, amount){
  text <- df$text
  text_df <- tibble(line = 1:amount, text = text)
  
  text_df_tidy <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  sentiment_counts <- text_df_tidy %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  sentiment_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution of Top Sentiments",
         x = NULL) +
    coord_flip()
}
