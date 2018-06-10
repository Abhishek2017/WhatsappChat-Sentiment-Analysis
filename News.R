Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_91")

library(readxl)
library(tm)
library(rJava)
library(qdap)
library(wordcloud)
library(syuzhet)
library(data.table)
library(text2vec)
library(topic)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(DT)
dat <- read_excel("KR_news_TH.xlsx")
dat_news <- dat[,3]
dat1 <- read_excel("rk_news_all.xlsx")
News <- rbind.fill(dat, dat1)
dat_news <- dat_news[complete.cases(dat_news),]

#Data Cleaning
corpus <- VCorpus(VectorSource(dat_news))

removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
removeapo<-function(x) gsub("'","",x)
removeSpace<-function(x) gsub("\\s+"," ",x)
removeNonASCII<-function(x) iconv(x, "latin1", "ASCII", sub="")
toLowerCase <- function(x) sapply(x,tolower)

corpus<-tm_map(corpus,content_transformer(removeURL)) #remove web url
corpus<-tm_map(corpus,content_transformer(removeapo))#remove apostrophe
corpus<-tm_map(corpus,content_transformer(removeSpace)) #remove multiple space
corpus<-tm_map(corpus,content_transformer(removeNonASCII)) #remove non-ASCII
corpus<-tm_map(corpus,content_transformer(toLowerCase))# convert uppercase to lowercase

corpus <- tm_map(corpus,removeWords,stop_words$word)
corpus <- tm_map(corpus, removePunctuation)#remove punctuation
corpus <- tm_map(corpus, stripWhitespace)#striwhitespace
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, PlainTextDocument)#map-t0-plain-text-doc

dtm <- TermDocumentMatrix(corpus)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing = TRUE)

d <- data.frame(word = names(v), freq = v)
head(d, 10)

#generate the wordcloud
set.seed(1056)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 700, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#fetch sentiment word from texts 

texts_news <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

sentiment_news <- get_nrc_sentiment(texts_news$character.0.)
text_news <- cbind(dat_news, sentiment_news)

final_news <- merge(dat,text_news, by = "News")
df <- final_news[, c(2,3)]
setnames(df, c("URL", "Headline"), c("url", "heading"))
df$link <- paste0("<a href='", df$url, "' target='_blank'>", df$heading, "</a>")

##########################################################################################



#count the sentiments word by category
TotalSentiment_news <- data.frame(colSums(final_news[, c(6:15)]))
names(TotalSentiment_news) <- "count"
TotalSentiment_news <- cbind("sentiment" = rownames(TotalSentiment_news), 
                        TotalSentiment_news)
rownames(TotalSentiment_news) <- NULL

library(ggplot2)
#totalsentiment score of all texts
ggplot(data = TotalSentiment_news, aes(x = sentiment, y = count)) + 
   geom_bar(aes(fill = sentiment), stat = "identity") + 
   theme(legend.position = "none") + xlab("Sentiment") + 
   ylab("Total Count") + ggtitle("News Sentiment Score")

