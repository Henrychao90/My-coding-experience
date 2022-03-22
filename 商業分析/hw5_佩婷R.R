#https://www.kaggle.com/nicapotato/womens-ecommerce-clothing-reviews/home

##read in data##
data <- read.csv("Womens Clothing E-Commerce Reviews.csv",encoding = "UTF-8")


##reviewrs who are recommended##
c1 <- data[which(data$Recommended.IND==1),] 

## Make a vector source and a corpus
library(tm)
x1=Corpus(VectorSource(c1$Review.Text))

##Clean text
x1 <- tm_map(x1, content_transformer(tolower))
x1 <- tm_map(x1, removePunctuation) #remove punctuation

#Remove stopwords
x1 <-  tm_map(x1, removeWords, stopwords("english"))
x1 <- tm_map(x1, stemDocument)
tdm1 <- TermDocumentMatrix(x1)
inspect(tdm1)

# Convert TDM to matrix
review_m1 <- as.matrix(tdm1)

# Sum rows and frequency data frame
freq_df1 <- rowSums(review_m1)

# Sort term_frequency in descending order
freq_df1 <- sort(freq_df1, decreasing = T)

# View the top 15 most common words
freq_df1[1:15]
barplot(freq_df1[1:15], col = "royalblue", las = 2)

summary1 <- data.frame(word = names(freq_df1),
                               num = freq_df1)

library(wordcloud2)
wordcloud2(summary1,size=0.5)
tdm2 <- removeSparseTerms(tdm1, sparse = 0.9)
mydata1 <- as.data.frame(as.matrix(tdm2))
library("proxy")
hc <- hclust(d = dist(mydata1, method = "cosine"), method = "complete")
plot(hc)

##-------------------------------------------------------------------##

##reviewrs who are not recommended##
c2 <- data[which(data$Recommended.IND==0),] 

## Make a vector source and a corpus
library(tm)
x2=Corpus(VectorSource(c2$Review.Text))

##Clean text
x2 <- tm_map(x2, content_transformer(tolower))
x2 <- tm_map(x2, removePunctuation) #remove punctuation

#Remove stopwords
x2 <-  tm_map(x2, removeWords, stopwords("english"))
x2 <- tm_map(x2, stemDocument)
tdm3 <- TermDocumentMatrix(x2)
inspect(tdm3)

# Convert TDM to matrix
review_m2 <- as.matrix(tdm3)

# Sum rows and frequency data frame
freq_df2 <- rowSums(review_m2)

# Sort term_frequency in descending order
freq_df2 <- sort(freq_df2, decreasing = T)

# View the top 15 most common words
freq_df2[1:15]
barplot(freq_df2[1:15], col = "yellow", las = 2)

summary2 <- data.frame(word = names(freq_df2),
                       num = freq_df2)

library(wordcloud2)
wordcloud2(summary2,size=0.5)
tdm4 <- removeSparseTerms(tdm3, sparse = 0.9)
mydata2 <- as.data.frame(as.matrix(tdm4))
library("proxy")
hc2 <- hclust(d = dist(mydata2, method = "cosine"), method = "complete")
plot(hc2)


##Find the co-words##
good <- c(names(freq_df1[1:10]))
bad <- c(names(freq_df2[1:10]))
intersect(good,bad)
mystopwords <- c(intersect(good,bad))



##Dropping these co-words##
x1 <- tm_map(x1, removeWords,mystopwords)
tdm1 <- TermDocumentMatrix(x1)
inspect(tdm1)

# Convert TDM to matrix
review_m1 <- as.matrix(tdm1)

# Sum rows and frequency data frame
freq_df1 <- rowSums(review_m1)

# Sort term_frequency in descending order
freq_df1 <- sort(freq_df1, decreasing = T)

# View the top 15 most common words
freq_df1[1:15]
barplot(freq_df1[1:15], col = "royalblue", las = 2)

summary1 <- data.frame(word = names(freq_df1),
                       num = freq_df1)
wordcloud2(summary1,size=0.5)

##-----------------------------------------##

##Dropping these co-words##
x2 <- tm_map(x2, removeWords,mystopwords)

##Doing everthing again##
tdm3 <- TermDocumentMatrix(x2)
inspect(tdm3)

# Convert TDM to matrix
review_m2 <- as.matrix(tdm3)

# Sum rows and frequency data frame
freq_df2 <- rowSums(review_m2)

# Sort term_frequency in descending order
freq_df2 <- sort(freq_df2, decreasing = T)

# View the top 15 most common words
freq_df2[1:15]
barplot(freq_df2[1:15], col = "yellow", las = 2)

summary2 <- data.frame(word = names(freq_df2),
                       num = freq_df2)

wordcloud2(summary2,size=0.5)
