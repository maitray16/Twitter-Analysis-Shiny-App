library(shiny)
library(ggplot2)
library(lubridate)
library(scales)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(reshape2)
library(dplyr )

function(input, output) {
  output$contents <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    tweets <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    tweets$timestamp <- ymd_hms(tweets$timestamp)
    tweets$timestamp <- with_tz(tweets$timestamp, "America")
    
    nohandles <- str_replace_all(tweets$text, "@\\+", "")
    wordCorpus <- Corpus(VectorSource(nohandles))
    wordCorpus <- tm_map(wordCorpus, removePunctuation)
    wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
    wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
    wordCorpus <- tm_map(wordCorpus, removeWords, c("amp", "2yo", "3yo", "4yo"))
    wordCorpus <- tm_map(wordCorpus, stripWhitespace)
    
    mySentiment <- get_nrc_sentiment(tweets$text)
    
    
    p <- ggplot(data = tweets, aes(x = timestamp)) +
        geom_histogram(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Time") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "#3DD2CC", high = "#2657C1")
      print(p)
    
    output$wday <- renderPlot({
    p1 <- ggplot(data = tweets, aes(x = wday(timestamp, label = TRUE))) +
        geom_bar(breaks = seq(0.5, 7.5, by =1), aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Day of the Week") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "#3DD2CC", high = "#2657C1")
    print(p1)
    })
    
    output$month <- renderPlot({
      p2 <- ggplot(data = tweets, aes(x = month(timestamp, label = TRUE))) +
        geom_bar(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Month") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "#3DD2CC", high = "#2657C1")
      print(p2)
    })
    
    output$hashtag <- renderPlot({
      p3 <- ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
      geom_bar(fill = "#3DD2CC") + 
      theme(legend.position="none", axis.title.x = element_blank()) +
      ylab("Number of tweets") + 
      ggtitle("Tweets with Hashtags") +
      scale_x_discrete(labels=c("No hashtags", "Tweets with hashtags"))
    print(p3)
    })
    
    output$retweet <- renderPlot({
      p4 <-ggplot(tweets, aes(factor(!is.na(retweeted_status_id)))) +
        geom_bar(fill = "#2657C1") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Retweeted Tweets") +
        scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))
      print(p4)
    })
    
    output$reply <- renderPlot({
      p5 <-ggplot(tweets, aes(factor(!is.na(in_reply_to_status_id)))) +
        geom_bar(fill = "#3DD2CC") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Replied Tweets") +
        scale_x_discrete(labels=c("Not in reply", "Replied tweets"))
      print(p5)
    })
    
    output$chars <- renderPlot({
      tweets$charsintweet <- sapply(tweets$text, function(x) nchar(x))
      p6 <-ggplot(data = tweets, aes(x = charsintweet)) +
        geom_histogram(aes(fill = ..count..), binwidth = 8) +
        theme(legend.position = "none") +
        scale_fill_gradient(low = "#3DD2CC", high = "#2657C1")
      print(p6)
    })
    
    output$wordCloud <- renderPlot({
      pal <- brewer.pal(9,"YlGnBu")
      pal <- pal[-(1:4)]
      set.seed(123)
      text(x=0.5, y=0.5, "Title of my first plot")
      p6 <- wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE, 
                rot.per=0.35, use.r.layout=FALSE, colors=pal,main="Most common words")
      print(p6)
      
    })
    
    output$sentiment <- renderPlot({
      tweets <- cbind(tweets, mySentiment)
      sentimentTotals <- data.frame(colSums(tweets[,c(11:18)]))
      names(sentimentTotals) <- "count"
      sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
      rownames(sentimentTotals) <- NULL
      p7 <- ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "none") +
        xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
      print(p7)
      
    })

    
  })
}