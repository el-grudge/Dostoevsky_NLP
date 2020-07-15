# 2302 Poor Folk
pf <- toString(gutenberg_download(2302)$text)
pf_Parts <- data.frame(text=stringi::stri_split_regex(pf,", , PART [A-Z]*, ,")[[1]])
pf_Parts$part <- nrow(pf_Parts)
pf_M <- data.frame()
for (part in pf_Parts$part){
  chapters <- data.frame(stringi::stri_split_regex(pf_Parts[pf_Parts$part==part,'text'],", , , , , "))
  colnames(chapters) <- 'text'
  chapters$chapter <- c(0:(nrow(chapters)-1))
  chapters$part <- part
  chapters$title <- 'Poor Folk'
  chapters$year <- 1845
  pf_M <- rbind(pf_M, chapters)
}
pf_M <- pf_M[pf_M$chapter!=0,]
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
  chapters$title <- 'Notes from the Underground'
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
  chapters$title <- 'Crime and Punishment'
  chapters$year <- 1866
  cap_M <- rbind(cap_M, chapters)
}
cap_M$text <- as.character(cap_M$text)

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
  chapters$title <- 'The Brothers Karamazov'
  chapters$year <- 1880
  brothers_M <- rbind(brothers_M, chapters)
}
brothers_M$text <- as.character(brothers_M$text)

dostoevsky <- rbind(pf_M, nftu_M, cap_M, brothers_M)
dostoevsky_M <- data.frame()
for (title in unique(dostoevsky$title)){
  for (part in unique(dostoevsky[dostoevsky$title==title,'part'])){
    for (chapter in dostoevsky[dostoevsky$title==title & dostoevsky$part==part,'chapter']){
      year = unique(dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'year'])
      text = dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'text']
      dostoevsky_M <- rbind(dostoevsky_M, data.frame(text=get_sentences(text), part, chapter, title, year))
    }
  }
}
dostoevsky_M$text <- as.character(dostoevsky_M$text)
dostoevsky_M$text <- stringr::str_to_lower(dostoevsky_M$text)

remove.contraction <- function(doc){
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
}

# dostoevsky_M$text <- sapply(dostoevsky_M$text, remove.contraction)

# Basic cleaning
basic.cleaning <- function(doc) {
  # to lower case
  # doc <- stringr::str_to_lower(doc)
  
  # # "won't" is a special case as it does not expand to "wo not"
  # doc <- gsub("won't", "will not", doc)
  # doc <- gsub("can't", "can not", doc)
  # doc <- gsub("n't", " not", doc)
  # doc <- gsub("'ll", " will", doc)
  # doc <- gsub("'re", " are", doc)
  # doc <- gsub("'ve", " have", doc)
  # doc <- gsub("'m", " am", doc)
  # doc <- gsub("'d", " would", doc)
  # # 's could be 'is' or could be possessive: it has no expansion
  # doc <- gsub("'s", "", doc)
  
  # remove stopwords
  doc <- tm::removeWords(
    x = doc, 
    words = tm::stopwords(kind='SMART')
  )
  doc <- tm::removeWords(
    x = doc,
    words = tm::stopwords(kind = "english")
  )
  doc <- tm::removeWords(
    x = doc,
    words = qdapDictionaries::Top200Words
  )
  
  # function to remove special characters
  removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
  # remove special characters
  doc <- sapply(doc, removeSpecialChars)
  
  # remove redundant, unnecessary white space
  doc <- trimws(stringr::str_replace_all(
    string = doc,
    pattern = "\\s+",
    replacement = " "
  ))
  
  return(doc)
}

# sentiment
dostoevsky_M$sentiment <- get_sentiment(dostoevsky_M$text)

# Basic cleaning
dostoevsky_Cleaned <- dostoevsky_M
dostoevsky_Cleaned$text <- sapply(dostoevsky_Cleaned$text, basic.cleaning)
dostoevsky_Cleaned <- dostoevsky_Cleaned[nzchar(dostoevsky_Cleaned$text),]
n <- c()
for (title in unique(dostoevsky_Cleaned$title)){
  n <- c(n,seq(1:nrow(dostoevsky_Cleaned[dostoevsky_Cleaned$title==title,])))
}
dostoevsky_Cleaned$n <- n # dostoevsky_M is used for plotting the syuzhet
dostoevsky_Cleaned$title <- as.character(dostoevsky_M$title)

# unnest and remove stop, undesirable and short words
dostoevsky_Words <- unnest_tokens(dostoevsky_Cleaned, word, text)

# stem words
dictionary <- sort(unique(dostoevsky_Words$word))
dostoevsky_Words$word <- sapply(dostoevsky_Words$word, tm::stemDocument)

# complete words
dostoevsky_Words$word <- sapply(dostoevsky_Words$word, tm::stemCompletion, dictionary = dictionary)
# remove NA
dostoevsky_Words <- dostoevsky_Words[!is.na(dostoevsky_Words$word),]

# transformed DCT data
dostoevsky_DCT <- data.frame()
dostoevsky_DCTPart <- data.frame()
for (title in unique(dostoevsky_M$title)){
  year = unique(dostoevsky_M[dostoevsky_M$title==title,'year'])
  df_part <- data.frame()
  df_chapter <- data.frame()
  sentiment <- dostoevsky_M[dostoevsky_M$title==title, 'sentiment']
  dct_values <- get_dct_transform(
    sentiment,
    low_pass_size = 7,
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = F
  )
  df_1 <- data.frame(dct_values, title=rep(title,length(dct_values)), year=rep(year,length(dct_values)))
  df_1$n <- seq(1:nrow(df_1))
  dostoevsky_DCT <- rbind(dostoevsky_DCT,df_1)
  for (part in unique(dostoevsky_M[dostoevsky_M$title==title,'part'])){
    sentiment_Part <- dostoevsky_M[dostoevsky_M$title==title & dostoevsky_M$part==part, 'sentiment']
    dct_part <- get_dct_transform(
      sentiment_Part,
      low_pass_size = 7,
      x_reverse_len = 100,
      scale_vals = F,
      scale_range = F
    )
    df_2 <- data.frame(dct_part, part=rep(part,length(dct_part)), title=rep(title,length(dct_part)), year=rep(year,length(dct_part)))
    df_part <- rbind(df_part, df_2)
  }
  df_part$n <- seq(1:nrow(df_part))
  dostoevsky_DCTPart <- rbind(dostoevsky_DCTPart,df_part)
}

# dostoevsky
# dostoevsky_M
# dostoevsky_Cleaned
# dostoevsky_Words
# dostoevsky_DCT
# dostoevsky_DCTPart
