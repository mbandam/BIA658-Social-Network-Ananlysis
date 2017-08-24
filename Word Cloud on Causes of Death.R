# Install and load the required packages
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer")# color palettes
install.packages('readr')
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("readr")

#Load the data as a corpus
Death_records = read_csv("Deaths_in_US_2014.csv")
Cause <- Corpus(VectorSource(Death_records$Cause))

#Build a term-document matrix
dtm <- TermDocumentMatrix(Cause, control = list(tolower = FALSE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
         random.order=FALSE, random.color = TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

names(v)
  