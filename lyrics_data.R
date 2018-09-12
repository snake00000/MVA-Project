setwd("C:/Users/HP/Documents/Project")
#install.packages("tm")
library(tm)
text_data = read.csv("lyrics.csv",stringsAsFactors = F)
str(text_data)

lyrics_data=text_data
colnames(lyrics_data)=c("doc_id",names(text_data)[2:5],"text")

#new lines \n were a problem so they are removed
lyrics_data$text=gsub("[\r\n]", " ", lyrics_data$text)


#coffee_tweets = text_data$text
df_source=DataframeSource(lyrics_data)
df_corpus = VCorpus(df_source)
# Make a volatile corpus: coffee_corpus


# Print out coffee_corpus
print(df_corpus)
# Print the 15th tweet in coffee_corpus
print(df_corpus[[15]])
# Print the contents of the 15th tweet in coffee_corpus
df_corpus[[15]][1]
# Now use content to review plain text
content(df_corpus[[10]])

clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
clean_corp=clean_corpus(df_corpus)
#save(clean_corp,file = "lyrics_corpus.rds")
load("lyrics_corpus.rds")

#install.packages("wordcloud")
library(wordcloud)
wordcloud(clean_corp,max.words = 100)


# Create the tdm from the corpus: coffee_dtm
coffee_tdm = TermDocumentMatrix(clean_corp)

# Print out coffee_dtm data
coffee_tdm

# Convert coffee_dtm to a matrix: coffee_m
coffee_m = as.matrix(coffee_tdm)

# Print the dimensions of coffee_m
dim(coffee_m)

# Review a portion of the matrix to get some Starbucks
coffee_m[2593:2594,475:478]
