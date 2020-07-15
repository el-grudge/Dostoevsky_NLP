read.delim('data/crime_and_punishment_chapter1.txt')

tm::removePunctuation(tm::tm_map(read.delim('data/crime_and_punishment_chapter1.txt')))

stringr::str_to_lower(read.delim('data/crime_and_punishment_chapter1.txt'))

class(read.delim('data/crime_and_punishment_chapter1.txt'))

View(read.delim('data/crime_and_punishment_chapter1.txt',delim='\nl'))

filename <- 'data/crime_and_punishment_chapter1.txt'
View(readChar(filename, file.info(filename)$size))

View(strsplit(readChar(filename, file.info(filename)$size),'\\n\\n'))

class(strsplit(readChar(filename, file.info(filename)$size),'\\n\\n'))

tm::removeWords(strsplit(readChar(filename, file.info(filename)$size),'\\n\\n'))

tm::tm_map(strsplit(readChar(filename, file.info(filename)$size),'\\n\\n'), removePunctuation)

crime_punish_corp <- tm::as.VCorpus(strsplit(readChar(filename, file.info(filename)$size),'\\n\\n'))

tm::removeWords(
  x = crime_punish_corp,
  words = tm::stopwords(kind = "SMART")
  )


tm::Corpus(readChar(filename, file.info(filename)$size))

tm::removeWords(
  x = c(readChar(filename, file.info(filename)$size)), 
  words = tm::stopwords(kind='SMART')
  )

stringr::str_to_lower(c(readChar(filename, file.info(filename)$size)))

##############################################################################################################

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

################################################################################################

# 2302 Poor Folk
pf <- toString(gutenberg_download(2302)$text)
pf_Parts <- data.frame(text=stringi::stri_split_regex(pf,", , PART [A-Z]*, ,")[[1]])
pf_Parts$part <- nrow(pf_Parts)
pf_M <- data.frame()
for (part in pf_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(pf_Parts[pf_Parts$part==part,'text'],", , [A-Za-z0-9]*, ,"))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(1:nrow(chapters))
  chapters$part <- part
  chapters$title <- 'poor folk'
  chapters$year <- 1845
  pf_M <- rbind(pf_M, chapters)
}
pf_M$text <- as.character(pf_M$text)

# 600 Notes from the Underground
nftu <- toString(gutenberg_download(600)$text)
nftu_Parts <- data.frame(text=stringi::stri_split_regex(nftu,", , PART [A-Z]*, ,")[[1]])
nftu_Parts$part <- c(0:(nrow(nftu_Parts)-1))
nftu_Parts <- nftu_Parts[nftu_Parts$part!=0,]
nftu_M <- data.frame()
for (part in nftu_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(nftu_Parts[nftu_Parts$part==part,'text'],", , [IVX]*, , "))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(0:(nrow(chapters)-1))
  chapters$part <- part
  chapters$title <- 'notes from the underground'
  chapters$year <- 1864
  nftu_M <- rbind(nftu_M, chapters)
}
nftu_M <- nftu_M[nftu_M$chapter!=0,]
nftu_M$text <- as.character(nftu_M$text)

# 2554 Crime and Punishment
cap <- toString(gutenberg_download(2554)$text)
cap_Parts <- data.frame(text=stringi::stri_split_regex(cap,", , PART [A-Z]*, ,|EPILOGUE")[[1]])
cap_Parts$part <- c(0:(nrow(cap_Parts)-1))
cap_Parts <- cap_Parts[cap_Parts$part!=0,]
cap_M <- data.frame()
for (part in cap_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(cap_Parts[cap_Parts$part==part,'text'],", , , , CHAPTER [A-Z]*, ,|, , , , [A-Z]*, , "))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(1:nrow(chapters))
  chapters$part <- part
  chapters$title <- 'crime and punishment'
  chapters$year <- 1866
  cap_M <- rbind(cap_M, chapters)
}
cap_M$text <- as.character(cap_M$text)
# cap_M <- rbind(cap_M, data.frame(text=stringi::stri_split_regex(cap_M[cap_M$chapter==8 & cap_M$part==6,'text'],", , , , [A-Z]*, , ")[[1]][-1], chapter=c(1:2), part=7, title='crime and punishment', year=1866))
# cap_M[cap_M$chapter==8 & cap_M$part==6,]$text <- stringi::stri_split_regex(cap_M[cap_M$chapter==8 & cap_M$part==6,'text'],", , , , [A-Z]*, , ")[[1]][1]
# View(data.frame(stringi::stri_split_regex(cap_Parts[cap_Parts$part==6,'text'],", , , , CHAPTER [A-Z]*, , |EPILOGUE")))

# 28054 The Brothers Karamazov
brothers <- toString(gutenberg_download(28054)$text)
brothers_Parts <- data.frame(text=stringi::stri_split_regex(brothers,", , Book [A-Z. A-Za-z ]*|, , EPILOGUE")[[1]])
brothers_Parts$part <- c(0:(nrow(brothers_Parts)-1))
brothers_Parts <- brothers_Parts[brothers_Parts$part!=0,]
brothers_M <- data.frame()
for (part in brothers_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(brothers_Parts[brothers_Parts$part==part,'text'],", , , , Chapter [A-Z. A-Za-z' ]*")[[1]][-1])
  colnames(chapters) <- 'text'
  chapters$chapter <- c(1:nrow(chapters))
  chapters$part <- part
  chapters$title <- 'the brothers karamazov'
  chapters$year <- 1866
  brothers_M <- rbind(brothers_M, chapters)
}
brothers_M$text <- as.character(brothers_M$text)

# View(brothers_M)

#################################################################################

# 2638 The Idiot	
idiot_M <- data.frame(text=stringi::stri_split_regex(toString(gutenberg_download(2638)$text),", , [A-Z]*., , ")[[1]])
idiot_M$chapter <- c(0:(nrow(idiot_M)-1))
idiot_M$title <- 'the idiot'
idiot_M$year <- 1869
idiot_M <- idiot_M[idiot_M$chapter!=0,]
View(idiot_M)

# 600 Notes from the Underground
nftu_M <- data.frame(text=stringi::stri_split_regex(toString(gutenberg_download(600)$text),", , [A-Z]*., , ")[[1]])
nftu_M$chapter <- c(0:(nrow(nftu_M)-1))
nftu_M$title <- 'notes from the underground'
nftu_M$year <- 1864
nftu_M <- nftu_M[nftu_M$chapter!=0,]
View(nftu_M)

# 2554 Crime and Punishment
cap_M <- data.frame(text=stringi::stri_split_regex(toString(gutenberg_download(2554)$text),", , , , CHAPTER [A-Z]*, , ")[[1]])
cap_M$chapter <- c(0:(nrow(cap_M)-1))
cap_M$title <- 'crime and punishment'
cap_M$year <- 1866
cap_M <- cap_M[cap_M$chapter!=0,]
View(cap_M)


###################################################################################
idiot_Parts <- data.frame(text=stringi::stri_split_regex(idiot,", , PART [A-Z]*, ,")[[1]])
idiot_Parts$part <- c(0:(nrow(idiot_Parts)-1))
idiot_Parts <- idiot_Parts[idiot_Parts$part!=0,]
idiot_M <- data.frame()
for (part in idiot_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(idiot_Parts[idiot_Parts$part==part,'text'],", , [A-Z]*., , "))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(1:nrow(chapters))
  chapters$part <- part
  chapters$title <- 'the idiot'
  chapters$year <- 1869
  idiot_M <- rbind(idiot_M, chapters)
}


########################################################################################################################################


# 2638 The Idiot
idiot <- toString(gutenberg_download(2638)$text)
idiot_Parts <- data.frame(text=stringi::stri_split_regex(idiot,", , PART [A-Z]*, ,")[[1]])
idiot_Parts$part <- c(0:(nrow(idiot_Parts)-1))
idiot_Parts <- idiot_Parts[idiot_Parts$part!=0,]
idiot_M <- data.frame()
for (part in idiot_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(idiot_Parts[idiot_Parts$part==part,'text'],", , [A-Z]*., , "))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(1:nrow(chapters))
  chapters$part <- part
  chapters$title <- 'the idiot'
  chapters$year <- 1869
  idiot_M <- rbind(idiot_M, chapters)
}

dostoevsky <- rbind(nftu_M, cap_M, idiot_M)
dostoevsky_M <- data.frame()
for (title in unique(dostoevsky$title)){
  for (part in unique(dostoevsky[dostoevsky$title==title,'part'])){
    for (chapter in dostoevsky[dostoevsky$title==title & dostoevsky$part==part,'chapter']){
      year = dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'year']
      text = dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'text']
      dostoevsky_M <- rbind(dostoevsky_M, data.frame(text=get_sentences(as.character(text)), part, chapter, title, year))
    }
  }
}

dostoevsky_M[dostoevsky_M$title=='Notes from the Underground',]

dostoevsky_M %>%
  group_by(title, part) %>%
  summarise(chapters=n_distinct(chapter))

dostoevsky_DCT <- data.frame()
dostoevsky_DCTPart <- data.frame()
# dostoevsky_DCTChapter <- data.frame()
for (title in unique(dostoevsky_M$title)){
  year = unique(dostoevsky_M[dostoevsky_M$title==title,'year'])
  df_part <- data.frame()
  df_chapter <- data.frame()
  for (part in unique(dostoevsky_M[dostoevsky_M$title==title,'part'])){
    sentiment_Part <- dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$part==part, 'sentiment']
    dct_part <- get_dct_transform(
      sentiment_Part,
      low_pass_size = 7,
      x_reverse_len = 100,
      scale_vals = F,
      scale_range = F
    )
    df_1 <- data.frame(dct_part, part=rep(part,length(dct_part)), title=rep(title,length(dct_part)), year=rep(year,length(dct_part)))
    df_part <- rbind(df_part, df_1)
    #   for (chapter in unique(dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$part==part, 'chapter'])){
    #     sentiment_Chapter <- dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$part==part & dostoevsky_M$chapter==chapter,'sentiment']
    #     dct_chapter <- get_dct_transform(
    #       sentiment_Chapter,
    #       low_pass_size = 7,
    #       x_reverse_len = 100,
    #       scale_vals = F,
    #       scale_range = F
    #     )
    #     df_2 <- data.frame(dct_chapter, chapter=rep(chapter,length(dct_chapter)), part=rep(part,length(dct_chapter)), title=rep(title,length(dct_chapter)), year=rep(year,length(dct_chapter)))
    #     df_chapter <- rbind(df_chapter, df_2)
    #   }
  }
  df_part$n <- seq(1:nrow(df_part))
  dostoevsky_DCTPart <- rbind(dostoevsky_DCTPart,df_part)
  # df_chapter$n <- seq(1:nrow(df_chapter))
  # dostoevsky_DCTChapter <- rbind(dostoevsky_DCTChapter,df_chapter)
  # for (chapter in unique(dostoevsky_M[dostoevsky_M$title==title,'chapter'])){
  #   year = dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$chapter==chapter,'year']
  #   sentiment = dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$chapter==chapter,'sentiment']
  #   dct_values <- get_dct_transform(
  #     sentiment,
  #     low_pass_size = 7,
  #     x_reverse_len = 100,
  #     scale_vals = F,
  #     scale_range = F
  #   )
  #   df_1 <- data.frame(dct_values, chapter=rep(chapter,length(dct_values)), title=rep(title,length(dct_values)), year=rep(unique(year),length(dct_values)))
  #   df_2 <- rbind(df_2, df_1)
  # }
  # df_2$n <- seq(1:nrow(df_2))
  # dostoevsky_DCT <- rbind(dostoevsky_DCT, df_2)
}
# seq <- seq(1, nrow(dostoevsky_DCT[dostoevsky_DCT$title=='crime and punishment',]), by=100)
# ggplot(dostoevsky_DCT[dostoevsky_DCT$title=='crime and punishment',]) +
#   aes(x=n, y=dct_values) +
#   geom_line() +
#   geom_vline(xintercept = seq)