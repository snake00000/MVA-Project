
##necessary packages
library(tm)
library(SnowballC)
library(ggplot2)
library(lsa)
library(cluster)
library(proxy)
library(wordcloud)
library(MASS)

##data reading
lyrics_data_all = read.csv("lyrics.csv",stringsAsFactors = F)
#omitting those with no lyrics
to_omit=which(lyrics_data_all$lyrics=="")
#working data set
lyrics_data_all=lyrics_data_all[setdiff(1:339277,to_omit),]
n=dim(lyrics_data_all)[1] ##no of lyrics we will take 
subset=sample(1:n,1000,replace = F)
lyrics_data=lyrics_data_all[subset,]

#str(text_data)
colnames(lyrics_data)=c("doc_id",names(lyrics_data)[2:5],"text")
df=as.data.frame(lyrics_data)

#replacing \n with white spaces
lyrics_data$text=gsub("[\r\n]", " ", lyrics_data$text)

##making the corpus
df_source=DataframeSource(lyrics_data)
df_corpus = VCorpus(df_source)
print(df_corpus)

##pre-processing steps
clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  #removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  #stemming (can ignore for now)
  #corpus <- tm_map(corpus, stemDocument, language = "english")
  return(corpus)
}
cleaned_corpus=clean_corpus(df_corpus)
cleaned_corpus

#save(cleaned_corpus,file = "lyrics_corpus.rds")
#load("lyrics_corpus.rds")

#pictorial visualisation of important words
wordcloud(cleaned_corpus,max.words = 100)


#Term document matrix based on Tf-idf weighting
initial_td.mat<-TermDocumentMatrix(cleaned_corpus)
initial_td.mat.tfidf<-as.matrix(weightTfIdf(initial_td.mat))

##lsa
lsa_space=lsa(initial_td.mat.tfidf,dims=100)
#td.mat.tfidf=lsa_space$tk%*%diag(lsa_space$sk)%*%t(lsa_space$dk)
doc_vec=as.matrix(lsa_space$dk)


#cosine distance between feature vectors
dist.mat.tfidf <- dist(doc_vec,method = "cosine")
dist.mat.tfidf  # check distance matrix

#Multi-dimensional Scaling
fit <- cmdscale(dist.mat.tfidf, eig = TRUE, k = 10)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y),color=df$genre) + geom_point(data = points, aes(x = x, y = y,color = df$genre))

k=length(unique(df$genre))

#clustering(hierarchical) using tf_idf value
groups <- hclust(dist.mat.tfidf,method="ward.D")
plot(groups, cex=0.9, hang=-1)
#visualisation of clusters
rect.hclust(groups, k)


#clustering (k means) using tf_idf values
cluster<-kmeans(dist.mat.tfidf,k)
cluster #inspection of cluster
cluster$cluster
table(cluster$cluster)

#visualisation of clusters using first 2 principal components
ggplot(points,aes(x = x, y = y,color=cluster$cluster)) + geom_point(data = points, aes(x = x, y = y,color = cluster$cluster)) 

#elbow methhod for optimal k choice
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(dist.mat.tfidf, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

