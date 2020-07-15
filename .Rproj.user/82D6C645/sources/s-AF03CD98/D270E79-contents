filename <- 'data/crime_and_punishment_chapter1.txt'
crime_punishment_1 <- c(readChar(filename, file.info(filename)$size))

# to lower case
crime_punishment_1 <- stringr::str_to_lower(crime_punishment_1)

# remove contractions
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = " i’m ",
  replacement = " i am "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "’re ",
  replacement = " are "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "’t ",
  replacement = " not "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "’ve ",
  replacement = " have "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "’ll ",
  replacement = " will "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = " doesn’t ",
  replacement = " does not "
)
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = " that’s ",
  replacement = " that is "
)

# remove punctuation
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "[:punct:]",
  replacement = " "
)

# remove numbers
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "[:digit:]",
  replacement = " "
)

# replace symbols
crime_punishment_1 <- stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "\\W",
  replacement = " "
)

# remove stopwords
crime_punishment_1 <- tm::removeWords(
  x = crime_punishment_1, 
  words = tm::stopwords(kind='SMART')
)
crime_punishment_1 <- tm::removeWords(
  x = crime_punishment_1,
  words = tm::stopwords(kind = "english")
)
crime_punishment_1 <- tm::removeWords(
  x = crime_punishment_1,
  words = qdapDictionaries::Top200Words
)

# remove redundant, unnecessary white space
crime_punishment_1 <- trimws(stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "\\s+",
  replacement = " "
))

# stem words
strsplit_crime_punishment_1 <- strsplit(crime_punishment_1," ")
dictionary_tweets <- sort(unique(unlist(strsplit_crime_punishment_1)))
strsplit_crime_punishment_1 <- lapply(
  X = strsplit_crime_punishment_1,
  FUN = tm::stemDocument
)

# complete words
strsplit_crime_punishment_1 <- lapply(
  X = strsplit_crime_punishment_1,
  FUN = tm::stemCompletion,
  dictionary = dictionary_tweets
)
strsplit_crime_punishment_1 <- lapply(
  X = strsplit_crime_punishment_1,
  FUN = paste,
  collapse = " "
)
crime_punishment_1 <- unlist(strsplit_crime_punishment_1)

# remove redundant, unnecessary white space
crime_punishment_1 <- trimws(stringr::str_replace_all(
  string = crime_punishment_1,
  pattern = "\\s+",
  replacement = " "
))

# convert the tweets to a corpus
Corpus_crime_punishment_1 <- tm::VCorpus(tm::VectorSource(crime_punishment_1))

# create a document-term matrix
DocumentTermMatrix_crime_punishment_1 <- tm::DocumentTermMatrix(Corpus_crime_punishment_1)

# remove sparse terms
DocumentTermMatrix_crime_punishment_1 <- tm::removeSparseTerms(
  DocumentTermMatrix_crime_punishment_1,
  0.995
)

# create a integer matrix
M <- as.matrix(DocumentTermMatrix_crime_punishment_1)
dim(M)

# word cloud
term_frequency <- data.frame(
  Term = colnames(M),
  Frequency = colSums(M),
  stringsAsFactors = FALSE
)
term_frequency <- term_frequency[order(term_frequency$Frequency),]
# tail(term_frequency)
wordcloud::wordcloud(
  words = term_frequency$Term,
  freq = term_frequency$Frequency,
  max.words = 25,
  random.order = FALSE,
  colors = viridis::viridis(100)
)

# create term-document matrix (transpose of document-term matrix).
TermDocumentMatrix_crime_punishment_1 <- tm::TermDocumentMatrix(Corpus_crime_punishment_1)
TermDocumentMatrix_crime_punishment_1 <- tm::removeSparseTerms(
  TermDocumentMatrix_crime_punishment_1,
  0.995
)
