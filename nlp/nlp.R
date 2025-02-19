library("tm")
library("wordcloud")
data("crude")

second_document <- content(crude[[2]])

# task 1
corpus_b <- Corpus(VectorSource(second_document))
dtm_b <- TermDocumentMatrix(corpus_b, control = list(
  removePunctuation = FALSE,
  removeNumbers = FALSE,
  stopwords = FALSE))
freq_b <- head(sort(rowSums(as.matrix(dtm_b)), decreasing = TRUE), 20)
names(freq_b)
wordcloud(names(freq_b), freq_b, max.words = 20, main = "Part B")


# task 2
corpus_c <- Corpus(VectorSource(second_document))
corpus_c <- tm_map(corpus_c, content_transformer(tolower))
corpus_c <- tm_map(corpus_c, removePunctuation)
corpus_c <- tm_map(corpus_c, removeNumbers)
corpus_c <- tm_map(corpus_c, removeWords, stopwords("english"))
corpus_c <- tm_map(corpus_c, stripWhitespace)
dtm_c <- TermDocumentMatrix(corpus_c)
freq_c <- head(sort(rowSums(as.matrix(dtm_c)), decreasing = TRUE), 20)
names(freq_c)
wordcloud(names(freq_c), freq_c, max.words = 20, main = "Part C")


# task 3
dtm_d <- DocumentTermMatrix(crude, control = list(
  weighting = function(x) weightTfIdf(x, normalize = FALSE),
  removePunctuation = FALSE,
  removeNumbers = FALSE,
  stopwords = FALSE)
)
freq_d <- head(sort(as.matrix(dtm_d)[2,], decreasing = TRUE), 20)
names(freq_d)
wordcloud(names(freq_d), freq_d, max.words = 20, main = "Part D")


# task 4
corpus_clean <- tm_map(crude, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
dtm_e <- DocumentTermMatrix(corpus_clean, control = list(
  weighting = function(x) weightTfIdf(x, normalize = FALSE),
  removePunctuation = FALSE,
  removeNumbers = FALSE,
  stopwords = FALSE)
)
freq_e <- head(sort(as.matrix(dtm_e)[2,], decreasing = TRUE), 20)
names(freq_e)
wordcloud(names(freq_e), freq_e, max.words = 20, main = "Part E")
