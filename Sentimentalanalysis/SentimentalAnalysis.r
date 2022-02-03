#Used Amazon reviews Exporter in google chrome to extract reviews
#Importing file into R
#load packages into R

#install.packages("tm")
#install.packages("wordcloud")
#install.packages("syuzhet")


library(tm)         # text analytics- text mining.
library(wordcloud)  # create wordcloud
library(syuzhet)

#import data in R
reviews<-read.csv(file.choose(), header = T )

#check the structure of file
str(reviews)

#Creating Corpus
#This function uses the base package function iconv to translate value labels into a specified value
corpus <- iconv(reviews$review.text)
corpus<-Corpus(VectorSource(corpus))

#To see the corpus
inspect(corpus[1:5])

#Cleaning Corpus
corpus <- tm_map(corpus, tolower)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, stopwords("english"))
#inspect(corpus[1:5])

#Remove some common words not to used in text analysis - replace word 1, word 2 by actual words
corpus <- tm_map(corpus, removeWords, c("read","life"))
#inspect(corpus[1:5])

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

reviews_final <- corpus

#Create a term document
tdm <- TermDocumentMatrix(reviews_final)
tdm<-as.matrix(tdm)
tdm[1:10, 1:5]


#Bar PLots
w <- rowSums(tdm)
w<-subset(w, w>=25)
barplot(w, las = 2, col = "blue")

#create word cloud
w<- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))

#obtain sentiment scores
sentiment_data <- iconv(reviews$review.text)
s<-get_nrc_sentiment(sentiment_data)
s[1:10,]

#calculate review wise score
s$score <- s$positive - s$negative
s[1:10,]

#write scores into a csv file
write.csv(x = s, file = "Downloads/reviews (1).csv")

#check product sentiment 

#check overall sentiment of the product
review_score <- colSums(s[,])
print(review_score)

#Plot product sentiment
#Bar plot
barplot(colSums(s),
        las = 2,
        col= rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')

