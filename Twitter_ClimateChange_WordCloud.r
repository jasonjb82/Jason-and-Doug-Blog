# Load required libraries
library(RCurl)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(twitteR)
library(streamR)
library(grid)
library(ggplot2)
library(wesanderson)
 
# Load credentials
load("D:/ClimData/Twitter/twitter authentification.Rdata")
registerTwitterOAuth(twitCred)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
 
tweets <- searchTwitter("Climate Change Bangladesh", n=1500, lang="en") 
 
tweets.text <- sapply(tweets, function(x) x$getText())
 
# Remove non alphanumeric characters
tweets.text <- gsub("[^a-zA-Z0-9 ]","",tweets.text)
 
# Convert all text to lower case
tweets.text <- tolower(tweets.text)
 
# Replace blank space (“rt”)
tweets.text <- gsub("rt", "", tweets.text)
 
# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)
 
# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
 
# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)
 
# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
 
# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)
 
# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)
 
# remove word amp
tweet.text <- gsub("amp","",tweets.text)
 
# Create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
 
# Clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
 
# Create document term matrix applying some transformations
tdm = TermDocumentMatrix(tweets.text.corpus,
      control = list(removePunctuation = TRUE,
      stopwords = c("climate change","bangladesh","climate", "change"), stopwords("english"),
      removeNumbers = TRUE, tolower = TRUE))
 
# Define tdm as matrix
m = as.matrix(tdm)
 
# Get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
 
# Create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
 
# Plot and save the image in png format
png("BGD_ClimateChange_Dec2014.png", width=5, height=5, units="in", res=500)
 
wordcloud(dm$word, dm$freq, random.order=FALSE, min.freq = 2,scale=c(4,0.5),max.words = 100,colors=wes.palette(5,"Darjeeling"))
 
dev.off()