# Read file
apple <- read.csv(file.choose(), header = T)
head(apple)
tail(apple)
dim(apple)
str(apple)

# Build corpus
library(tm)
corpus <- iconv(apple$text, to = 'UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:1000])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:1000])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:1000])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:1000])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:1000])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:1000])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:1000])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = 'UTF-8', sub = "byte")

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[33]
get_nrc_sentiment('breakout')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')

# Bar Plot
counts <- table(apple$isRetweet)
barplot(counts, main="Bar Plot of Retweets",
        ylim = c(0,600),
        xlab="Retweets")

# Scatter Plot
plot(apple$retweetCount,apple$id,
     main = "Scatterplot",
     xlab = "Retweets Count",
     ylab = "ID",
     xlim = c(0,100),
     las= 2,
     col="darkorchid")

# Pie Chart
count<-table(apple$isRetweet)
percent<- round(count/sum(count)*100)
texts<- paste(names(count),"\n",percent,"%")
pie(count,
    labels = texts,
    main = "Retweets",
    col=c("brown1","darkolivegreen1"),
    border = "blue1",
    clockwise=TRUE)

    
