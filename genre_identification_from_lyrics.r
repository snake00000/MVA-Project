
##packages to load
library(tm)
library(SnowballC)
library(ggplot2)
library(lsa)
library(cluster)
library(proxy)
library(wordcloud)
library(MASS)
library(svs)
library(topicmodels)
library(igraph)
library(ldatuning)

##lyrics data reading
lyrics_data_all = read.csv("lyrics.csv",stringsAsFactors = F)

#omitting those with no lyrics
to_omit=which(lyrics_data_all$lyrics=="")

#working full data set
lyrics_data_all=lyrics_data_all[setdiff(1:339277,to_omit),]

##omitting instances with genre not available
to_omit=which(lyrics_data_all$genre=="Not Available")
lyrics_data_all=lyrics_data_all[setdiff(1:dim(lyrics_data_all)[1],to_omit),]

##stratified sampling
n=dim(lyrics_data_all)[1] ##no of lyrics we will take 
genre_names=names(table(lyrics_data_all$genre))
genre_size=as.numeric(table(lyrics_data_all$genre))
subset=numeric(0)
sampling_sizes=numeric(length(genre_names))
for (j in 1:length(genre_names)){
  sampling_set=lyrics_data_all[lyrics_data_all$genre==genre_names[j],]
  sampling_sizes[j]=sample(80:120,1) #change this according to the size of data
  subset=rbind(subset,sampling_set[sample(1:genre_size[j],sampling_sizes[j],rep=F),])
}
lyrics_data=subset

#str(text_data)
colnames(lyrics_data)=c("doc_id",names(lyrics_data)[2:5],"text")
df=as.data.frame(lyrics_data)

##for aaplying the code on kobita data comment above part and uncomment the below lines
#kobita_data
#df=as.data.frame(read.csv("C:/Users/lenovo/Downloads/Kobita_Data/kobita_and_porjaay.csv",header = T))
#colnames(df)=c("doc_id","text","genre")


#plotting representations of each genres in the data
to.plot=as.data.frame(table(df$genre))
ggplot(data=to.plot, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

#replacing \n with white spaces
df$text=gsub("[\r\n]", " ", df$text)

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

##genre wise wordcloud
for(id in unique(df$genre)){
  cat(id)
  dummy_df=df[df$genre==id,]
  dummy_df_corpus=VCorpus(DataframeSource(dummy_df))
  dummy_df_corpus=clean_corpus(dummy_df_corpus)
  wordcloud(dummy_df_corpus,max.words = 20)
}

#save(cleaned_corpus,file = "lyrics_corpus.rds")
#load("lyrics_corpus.rds")

#pictorial visualisation of important words
wordcloud(cleaned_corpus,max.words = 100)

#term document matrix
initial_td.mat<-TermDocumentMatrix(cleaned_corpus)

#removing empty documents after all the preprocessing
col_totals=apply(initial_td.mat,2,sum)
initial_td.mat=initial_td.mat[,col_totals>0]

#document_term_matrix
initial_dt.mat=DocumentTermMatrix(cleaned_corpus)

#removing empty documents
row_totals=apply(initial_dt.mat,1,sum)
to.omit=which(row_totals==0)
initial_dt.mat=initial_dt.mat[row_totals>0,]

#updating the data frame to work with
if(length(to.omit)!=0){
df=df[-(to.omit),]
}

#Term document matrix based on Tf-idf weighting
initial_td.mat.tfidf<-as.matrix(weightTfIdf(initial_td.mat))

##latent semantic analysis
lsa_space=lsa(initial_td.mat.tfidf,dims=100)
doc_vec=as.matrix(lsa_space$dk)

#cosine distance between feature vectors
dist.mat.tfidf <- dist((as.matrix(doc_vec)),method = "cosine")
dist.mat.tfidf  # check distance matrix

##original points with 2-D representation
#Multi-dimensional Scaling
fit <- cmdscale(dist.mat.tfidf, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y),color=df$genre) + geom_point(data = points, aes(x = x, y = y,color = df$genre)) 

#no of actual genres in the data we are using
k=length(unique(df$genre))

#clustering(hierarchical) using tf_idf value
groups <- hclust(dist.mat.tfidf,method="ward.D")
plot(groups, cex=0.9, hang=-1)

#visualisation of clusters
rect.hclust(groups, k)

#clustering (k means) using tf_idf values
cluster<-kmeans(dist.mat.tfidf,k)
#cluster #inspection of cluster

table(cluster$cluster)

#visualisation of clusters using first 2 principal components
col=as.factor(cluster$cluster)
ggplot(points,aes(x = x, y = y,color=col)) + geom_point(data = points, aes(x = x, y = y,color = col)) 

#elbow methhod for optimal k choice
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(dist.mat.tfidf, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#lda_model with no of topics=no of genres
model_lda=LDA(initial_dt.mat,k,method = "Gibbs",control = list(iter=1000,seed=33))

#matrix containing P(z|d) z being topic,d being documents
theta_topic=posterior(model_lda)$topics

#prediction of category of the documents with maximum posterior probability
pred_category=apply(theta_topic,1,function(x) as.numeric(which.max(x)))

#category wise mean theta
theta_topic_by=by(theta_topic,df$genre,colMeans)
theta_means=do.call("rbind",theta_topic_by)

##LDA K means with symmetrized KL divergence and one-column out k means
model_lda=LDA(initial_dt.mat,25,method = "Gibbs",control = list(iter=1000,seed=33))

#matrix containing P(z|d) z being topic,d being documents
theta_topic=posterior(model_lda)$topics

obs_no=nrow(df)
#Jensen-Shannon Divergence
FUN = function(i,j) {
  p=theta_topic[i,]
  q=theta_topic[j,]
  m=0.5*(p+q)
  JS <- 0.5 * (sum(p * log(p / m)) + sum(q * log(q / m)))
  return(JS)
}

#distance measures
dist_lda_1=outer(1:obs_no,1:obs_no,Vectorize(FUN))
dist_lda_2=dist(as.matrix(theta_topic[,-1]),method="euclidean")

#doing k means
lda_kmeans_1=kmeans(dist_lda_1,k)
lda_kmeans_2=kmeans(dist_lda_2,k)

#prediction
pred_category_1=lda_kmeans_1$cluster
pred_category_2=lda_kmeans_2$cluster

##evaluation
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category),method="rand")
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category),method="nmi")
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category_1),method="rand")
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category_1),method="nmi")
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category_2),method="rand")
compare(as.numeric(as.factor(df$genre)),as.numeric(pred_category_2),method="nmi")
compare(as.numeric(as.factor(df$genre)),as.numeric(cluster$cluster),method="rand")
compare(as.numeric(as.factor(df$genre)),as.numeric(cluster$cluster),method="nmi")

##topic wise most diagnostic words
most_diagnostic=function(n,vec){
  return(as.vector(order(vec,decreasing = T)[1:n]))
}
pw_z=posterior(model_lda)$terms
for(i in 1:k){
  index=most_diagnostic(10,pw_z[i,])
  print(initial_dt.mat$dimnames$Terms[index])
}

result <- FindTopicsNumber(
  initial_dt.mat,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 33),
  mc.cores = 2L,
  verbose = TRUE
)

#visualisation of clusters using first 2 principal components
col=as.factor(pred_category_1)
ggplot(points,aes(x = x, y = y,color=col)) + geom_point(data = points, aes(x = x, y = y,color = col)) 
