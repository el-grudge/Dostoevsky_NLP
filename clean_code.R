options(scipen=999)

# 1. Load the libraries
library(gutenbergr) # DATA SOURCE
library(dplyr) # Data manipulation (also included in the tidyverse package)
library(tidytext) #  #text mining, unnesting
library(tidyr) # Spread, separate, unite, text mining (also included in the tidyverse package)  #gather()
library(widyr) # Use for pairwise correlation
library(textdata) # ??
library(syuzhet) # ??

# Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`  #text and label geoms for ggplot2
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables  # for dynamic reporting
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function  #color tile and color bar in `kables`
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(grid) #to manipulate grid text

library(formattable) # for the color_tile function

library(wordcloud2) #creative visualizations

# Libraries and Functions
library(topicmodels) #the LDA algorithm
library(tm) #text mining
library(plotly) #interactive ggplot graphs
library(cleanNLP)

# 2. Data Loading
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

# dostoevsky_M
dostoevsky <- rbind(pf_M, nftu_M, cap_M, brothers_M)
dostoevsky_M <- data.frame()
for (title in unique(dostoevsky$title)){
  df <- data.frame()
  for (part in unique(dostoevsky[dostoevsky$title==title,'part'])){
    for (chapter in dostoevsky[dostoevsky$title==title & dostoevsky$part==part,'chapter']){
      year = unique(dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'year'])
      text = get_sentences(dostoevsky[dostoevsky$title==title & dostoevsky$part==part & dostoevsky$chapter==chapter,'text'])
      # dostoevsky_M <- rbind(dostoevsky_M, data.frame(text, part, chapter, title, year))
      df <- rbind(df, data.frame(text, part, chapter, title, year))
    }
  }
  df$n <- seq(1:nrow(df))
  dostoevsky_M <- rbind(dostoevsky_M, df)
}

dostoevsky_M$text <- as.character(dostoevsky_M$text)
dostoevsky_M$sentiment <- get_sentiment(dostoevsky_M$text)
dostoevsky_M$segment <- paste(dostoevsky_M$part, dostoevsky_M$chapter, sep=".")
dostoevsky_M$doc_id <- c(1:nrow(dostoevsky_M))

# pos tagging
cnlp_init_udpipe(model_name = "english")

dostoevsky_annotated <- cnlp_annotate(
  input=dostoevsky_M,
  verbose=10,
  text_name="text",
  doc_name="doc_id"
)

dostoevsky_Words <- dostoevsky_annotated$token[,c('doc_id','token','lemma')] %>% 
  right_join(dostoevsky_annotated$document, by=c('doc_id'))
dostoevsky_Words <- data.frame(dostoevsky_Words)
colnames(dostoevsky_Words)[2] <- 'word'

# to lower
dostoevsky_Words$word <- stringr::str_to_lower(dostoevsky_Words$word)
dostoevsky_Words$lemma <- stringr::str_to_lower(dostoevsky_Words$lemma)

# remove redundant, unnecessary white space
remove.space <- function(doc){
  doc <- trimws(stringr::str_replace_all(
    string = doc,
    pattern = "\\s+",
    replacement = " "
  ))
}

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# cleaning
dostoevsky_Words$word <- sapply(dostoevsky_Words$word, removeSpecialChars)
dostoevsky_Words$lemma <- sapply(dostoevsky_Words$lemma, removeSpecialChars)
dostoevsky_Words$word <- sapply(dostoevsky_Words$word, remove.space)
dostoevsky_Words$lemma <- sapply(dostoevsky_Words$lemma, remove.space)

# remove stopwords
dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$word %in% tm::stopwords(kind='SMART'),]
dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$word %in% qdapDictionaries::Top200Words,]

dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$lemma %in% tm::stopwords(kind='SMART'),]
dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$lemma %in% qdapDictionaries::Top200Words,]

# remove names
pf_names <- c('makar', 'alexievitch', 'dievushkin',
              'barbara', 'dobroselova', 'alexievna',
              'hospodin', 'bwikov',
              'anna', 'thedorovna',
              'pokrovski', 'petinka',
              'rataziaev', 
              'thedora',
              'gorshkov',
              'emelia', 'ilyitch',
              'sasha',
              'tchinovnik',
              'theresa',
              'samson', 'virin',
              'evstafi', 'ivanovitch',
              'phaldoni',
              'markov')

nftu_names <- c('liza', 'simonov', 'zverkov', 'trudolyubov', 'apollon', 'ferfitchkin', 'anton', 'antonitch')

cap_names <- c('rodion', 'romanovitch', 'raskolnikov', 'rodya',
               'sofya', 'semyonovna', 'marmeladov', 'sonia',
               'avdotya', 'romanovna', 'dounia',
               'arkady', 'ivanovitch', 'svidrigailov',
               'dmitri', 'prokofitch', 'razumihin',
               'katerina', 'ivanovna',
               'porfiry', 'petrovitch',
               'semyon', 'zaharovitch',
               'pulcheria', 'alexandrovna',
               'pyotr', 'luzhin',
               'andrey', 'semyonovitch', 'lebeziatnikov',
               'alyona',
               'lizaveta',
               'zossimov',
               'praskovya', 'pavlovna', 'pashenka',
               'nastasya', 'petrovna',
               'ilya',
               'alexandr', 'grigorievitch', 'zametov',
               'nikolay', 'dementyev', 'mikolka',
               'polenka',
               'amalia',
               'marfa',
               'lida',
               'nikodim',
               'koch')

brothers_names <- c('alexey', 'fyodorovitch', 'karamazov', 'alyosha',
                    'anna', 'fyodorovna', 'madame', 'krassotkin',
                    'dmitri', 'mitya',
                    'fenya',
                    'ferapont',
                    'fetyukovitch',
                    'fyodor', 'pavlovitch',
                    'grigory', 'vassilyevitch',
                    'ilusha',
                    'ilyitch', 'perhotin',
                    'ippolit', 'kirillovitch',
                    'ivan', 'vanya',
                    'katerina', 'ivanovna', 'katya',
                    'kuzma', 'kuzmitch', 'samsonov',
                    'lise',
                    'lizaveta', 'smerdyastchaya',
                    'hohlakov',
                    'mihail', 'osipovitch', 'rakitin',
                    'nikolay', 'ivanovitch', 'kolya',
                    'pavel', 'smerdyakov',
                    'pyotr', 'alexandrovitch', 'miuesov',
                    'snegiryov',
                    'zossima',
                    'agrafena', 'alexandrovna', 'svyetlov', 'grushenka', 'grusha',
                    'adelaida',
                    'andrey',
                    'fomitch', 'kalganov',
                    'paissy',
                    'gorstkin', 'lyagavy',
                    'herzenstube',
                    'makarovitch', 'makarov',
                    'marfa', 'ignatyevna',
                    'marya', 'kondratyevna',
                    'maximov',
                    'mussyalovitch',
                    'pan', 'vrublevsky',
                    'sofya',
                    'trifon', 'borissovitch',
                    'varvinsky',
                    'parfenovitch', 'nelyudov',
                    'yefim', 'petrovitch', 'polenov',
                    'von', 'sohn',
                    'richard',
                    'perezvon',
                    'smurov',
                    'zhutchka',
                    'nina',
                    'iosif')

dostoevsky_names <- c(pf_names, nftu_names, cap_names, brothers_names)

dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$word %in% dostoevsky_names,]
dostoevsky_Words <- dostoevsky_Words[!dostoevsky_Words$lemma %in% dostoevsky_names,]
dim(dostoevsky_Words) # [1] 376676      9

# remove NA
dostoevsky_Words <- dostoevsky_Words[nzchar(dostoevsky_Words$word),]
dostoevsky_Words <- dostoevsky_Words[nzchar(dostoevsky_Words$lemma),]
dim(dostoevsky_Words) # [1] 175779      9

# NRC sentiments
dostoevsky_nrc <- data.frame()
for (title in unique(dostoevsky_M$title)){
  nrc_df <- get_nrc_sentiment(dostoevsky_M[dostoevsky_M$title==title,'text'])
  dwc <- round(nrow(nrc_df)*.1)
  nrc_rolled <- zoo::rollmean(nrc_df, k=dwc)
  nrc_rolled <- data.frame(nrc_rolled) %>% select(-negative, -positive)
  for (column in colnames(nrc_rolled)){
    nrc_list <- rescale_x_2(nrc_rolled[,column])
    nrc_sentiment <- data.frame(title=rep(title, length(nrc_list$x)), sentiment=rep(column, length(nrc_list$x)), x=nrc_list$x, y=nrc_list$y)
    dostoevsky_nrc <- rbind(dostoevsky_nrc, nrc_sentiment)
  }
}

# transformed DCT data
dostoevsky_DCT <- data.frame()
for (title in unique(dostoevsky_M$title)){
  year <- unique(dostoevsky_M[dostoevsky_M$title==title,'year'])
  len <- nrow(dostoevsky_M[dostoevsky_M$title==title,])
  part <- dostoevsky_M[dostoevsky_M$title==title,] %>%
    group_by(part) %>%
    summarise(index=min(n),
              percetage=round(index/len*100)) %>%
    ungroup()
  part <- part %>% mutate(Diff = lead(percetage) - percetage)
  part$Diff[length(part$Diff)] <- 100-part$percetage[length(part$percetage)]
  seq <- c()
  for (i in c(1:nrow(part))){
    n <- rep(i,part$Diff[i])
    seq <- c(seq, n)
  }
  sentiment <- dostoevsky_M[dostoevsky_M$title==title, 'sentiment']
  dct_values <- get_dct_transform(
    sentiment,
    low_pass_size = 7,
    # x_reverse_len = length(sentiment),
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = F
  )
  df_1 <- data.frame(dct_values, title=rep(title,length(dct_values)), part=seq, year=rep(year,length(dct_values)))
  df_1$n <- seq(1:nrow(df_1))
  dostoevsky_DCT <- rbind(dostoevsky_DCT,df_1)
}
sentiment <- dostoevsky_M[dostoevsky_M$title=='Crime and Punishment' & dostoevsky_M$part==1,'sentiment']
dct_part <- get_dct_transform(
  sentiment,
  low_pass_size = 7,
  x_reverse_len = length(sentiment),
  scale_vals = F,
  scale_range = F
)
dostoevsky_DCTPart <- data.frame(cbind(dostoevsky_M[dostoevsky_M$title=='Crime and Punishment' & dostoevsky_M$part==1,], dct_part))
dostoevsky_DCTPart$doc_id <- seq(1:nrow(dostoevsky_DCTPart))
rownames(dostoevsky_DCTPart) <- seq(1:nrow(dostoevsky_DCTPart))

# Basic cleaning
basic.cleaning <- function(doc) {
  # to lower case
  doc <- stringr::str_to_lower(doc)
  
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
  
  return(doc)
}

# remove stop words
remove.stopwords <- function(doc){
  doc <- tm::removeWords(
    x = doc, 
    words = tm::stopwords(kind='SMART')
  )
  doc <- tm::removeWords(
    x = doc, 
    words = qdapDictionaries::Top200Words
  )
  return(doc)
}

# remove names
remove.names <- function(doc){
  doc <- tm::removeWords(
    x = doc, 
    words = dostoevsky_names
  )
  return(doc)
}

# remove punctuation
remove.punctuation <- function(doc) {
  doc <- stringr::str_replace_all(
    string = doc,
    pattern = "[:punct:]",
    replacement = " "
  )
  return(doc)
}

# remove numbers
remove.numbers <- function(doc) {
  doc <- stringr::str_replace_all(
    string = doc,
    pattern = "[:digit:]",
    replacement = " "
  )
  return(doc)
}

# replace symbols
remove.symbols <- function(doc){
  doc <- stringr::str_replace_all(
    string = doc,
    pattern = "\\W",
    replacement = " "
  )
  return(doc)
}

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# remove redundant, unnecessary white space
remove.space <- function(doc){
  doc <- trimws(stringr::str_replace_all(
    string = doc,
    pattern = "\\s+",
    replacement = " "
  ))
  return(doc)
}

dostoevsky_clean <- select(dostoevsky_M, text, title)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, basic.cleaning)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.punctuation)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.numbers)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.symbols)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, removeSpecialChars)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.space)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.stopwords)
dostoevsky_clean$text <- sapply(dostoevsky_clean$text, remove.names)
dostoevsky_clean <- dostoevsky_clean[nzchar(dostoevsky_clean$text),]

dostoevsky_bigrams <- dostoevsky_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# 3. Word clouds
# word clouds
dostoevsky_Words_counts <- dostoevsky_Words %>%
  count(lemma, sort = TRUE) 
wordcloud2(dostoevsky_Words_counts[1:300, ], size = .5)

dostoevsky_Words_counts_pf <- dostoevsky_Words %>%
  filter(title=='Poor Folk') %>%
  count(lemma, sort = TRUE)
wordcloud2(dostoevsky_Words_counts_pf[1:300, ], size = .5)

dostoevsky_Words_counts_nftu <- dostoevsky_Words %>%
  filter(title=='Notes from the Underground') %>%
  count(lemma, sort = TRUE)
wordcloud2(dostoevsky_Words_counts_nftu[1:300, ], size = .5)

dostoevsky_Words_counts_cap <- dostoevsky_Words %>%
  filter(title=='Crime and Punishment') %>%
  count(lemma, sort = TRUE)
wordcloud2(dostoevsky_Words_counts_cap[1:300, ], size = .5)

dostoevsky_Words_counts_brothers <- dostoevsky_Words %>%
  filter(title=='The Brothers Karamazov') %>%
  count(lemma, sort = TRUE)
wordcloud2(dostoevsky_Words_counts_brothers[1:300, ], size = .5)

# 4. replace wordclouds with word count tables
dostoevsky_wordcount <- list()
for (title in unique(dostoevsky_Words$title)){
  top_words <- dostoevsky_Words[dostoevsky_Words$title==title,] %>% 
    group_by(lemma) %>% 
    summarise(countx = n()) %>% 
    arrange(desc(countx)) %>% 
    slice(seq_len(10)) %>% 
    rename(word=lemma) %>%
    mutate(row=row_number())
  
  dostoevsky_wordcount[[title]] <- ggplot(top_words) +
    aes(x=row, y=rev(countx)) +
    geom_col(fill=my_colors[title], show.legend = NULL) +
    labs(x = NULL, y = NULL) +
    theme_lyrics() +
    theme(axis.text.x = element_text()) +
    scale_x_continuous(  # This handles replacement of row
      breaks = top_words$row, # notice need to reuse data frame
      labels = top_words$word) +
    ggtitle(title) +
    coord_flip()
}

grid.arrange(
  dostoevsky_wordcount$`Poor Folk`, 
  dostoevsky_wordcount$`Notes from the Underground`, 
  dostoevsky_wordcount$`Crime and Punishment`, 
  dostoevsky_wordcount$`The Brothers Karamazov`,
  nrow = 2, ncol = 2, 
  top = textGrob("Top Words",gp=gpar(fontsize=20)), 
  bottom = textGrob("Word Count",gp=gpar(fontsize=20)))

# 5. topics per part
dostoevsky_tfidf <- dostoevsky_Words %>%
  distinct() %>%
  mutate(title_part=paste(title,part,sep='_')) %>% 
  select(title_part, lemma) %>%
  count(title_part, lemma, sort = TRUE) %>% 
  bind_tf_idf(lemma, title_part, n) %>%
  mutate(title=unlist(lapply(strsplit(title_part,"_"), `[[`, 1)),
         part=unlist(lapply(strsplit(title_part,"_"), `[[`, 2))) %>%
  group_by(title, part) %>%
  mutate(word=lemma,row=row_number()) %>% 
  select(-title_part, -lemma)

dostoevsky_top_tfidf <- dostoevsky_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(title,part) %>% 
  slice(seq_len(3)) %>%
  ungroup() %>%
  arrange(part, tf_idf) %>%
  group_by(title) %>%
  mutate(row=row_number())

# topic part plot
topic_part <- list()
for (title in unique(dostoevsky_top_tfidf$title)){
  topic_part[[title]] <- ggplot(dostoevsky_top_tfidf[dostoevsky_top_tfidf$title==title,]) +
    aes(x=row, y=tf_idf) +
    geom_col(fill=my_colors[title], show.legend = NULL) +
    labs(x = NULL, y = NULL) +
    theme_lyrics() +  
    facet_wrap(~as.integer(part),scales = "free") +
    scale_x_continuous(  # This handles replacement of row 
      breaks = dostoevsky_top_tfidf[dostoevsky_top_tfidf$title==title,]$row, # notice need to reuse data frame
      labels = dostoevsky_top_tfidf[dostoevsky_top_tfidf$title==title,]$word) +
    coord_flip()
}

grid.arrange(
  topic_part$`Poor Folk`, 
  topic_part$`Notes from the Underground`, 
  topic_part$`Crime and Punishment`, 
  topic_part$`The Brothers Karamazov`,
  nrow = 2, ncol = 2, 
  top = textGrob("Topics per Part",gp=gpar(fontsize=20)), 
  bottom = textGrob("TF-IDF",gp=gpar(fontsize=20)))

# 6. Topic analysis
# plotting utilities (color scheme and theme)
my_colors <- c(
  "Poor Folk"="deepskyblue3", 
  "Notes from the Underground" = "darkred", 
  "Crime and Punishment" = "goldenrod", 
  "The Brothers Karamazov" = "darkgreen"
)

theme_lyrics <- function() 
{
  theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 4,
                     color='black') +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    scale_fill_manual(values=c("dodgerblue", "firebrick1", "yellow", "limegreen")) +
    coord_flip()
}

# Document term Matrix
dostoevsky_dtm_balanced <- dostoevsky_Words %>%
  count(title, lemma, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(title, lemma, n)

seed <- 823
num_words <- 15
k <- 4

lda_dostoevsky <- LDA(dostoevsky_dtm_balanced, k = 4, method = "GIBBS", control = list(seed = seed))

top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}

top_terms_per_topic(lda_dostoevsky, num_words)

title_topic_relationship <- tidy(lda_dostoevsky, matrix='gamma') %>%
  inner_join(dostoevsky_Words, by = c("document"="title")) %>%
  select(document, topic, gamma) %>%
  group_by(document, topic) %>%
  mutate(mean = mean(gamma)) %>%
  select(-gamma) %>%
  distinct()

title_topic_relationship$topic = paste("Topic", title_topic_relationship$topic, sep = " ")  

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
grid.col = c('Poor Folk' = my_colors[['Poor Folk']],
             "Notes from the Underground" = my_colors[['Notes from the Underground']],
             "Crime and Punishment" = my_colors[['Crime and Punishment']],
             "The Brothers Karamazov" = my_colors[['The Brothers Karamazov']],
             "Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey", "Topic 4" = "grey")

circos.par(gap.after = c(rep(5, length(unique(title_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(title_topic_relationship[[2]])) - 1), 15))
#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(title_topic_relationship, grid.col = grid.col, transparency = .2)
title("Relationship Between Topic and Source")

# Word Network
dostoevsky_Maps <- dostoevsky_Words[,c('title','lemma')] %>% 
  count(title, lemma, sort=TRUE) %>% 
  group_by(title) %>% 
  summarise(lemma,
            wordcount=n,
            totalcount=sum(n),
            wordpercent=wordcount/totalcount*100) %>% 
  select(title, lemma, wordpercent) %>%
  rename(n=wordpercent) %>%
  slice(seq_len(20)) 

maps_graph <- dostoevsky_Maps %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")

mcolor <- maps_graph$data %>% mutate(mcolor = if_else(name%in%unique(dostoevsky_Maps$title), "blue", "black")) %>% select(mcolor)

maps_graph +
  ggraph::geom_edge_link(alpha = .25) +
  ggraph::geom_edge_density(aes(fill = n)) +
  ggraph::geom_node_point(color = "blue3", size = 1) + #Purple for Prince!
  ggraph::geom_node_text(aes(label = name),  repel = TRUE, color=mcolor$mcolor) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5))

# 7. networks
desired.words <- c('father','god','room','poor','murder','love','door','money','heart')

desired_bigrams <- dostoevsky_bigrams[dostoevsky_bigrams$word1 %in% desired.words | dostoevsky_bigrams$word2 %in% desired.words,] %>%
  count(word1, word2, sort = TRUE) %>%
  slice(seq_len(40)) 

bigram_graph <- desired_bigrams %>% graph_from_data_frame() #From `igraph`

set.seed(823)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_edge_density(aes(fill = n)) +
  geom_node_point(color = "blue3", size = 1) + #Purple for Prince!
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5))

# 8. Plot analysis
seq <- c(dostoevsky_DCTPart[dostoevsky_DCTPart$title=='Crime and Punishment' & dostoevsky_DCTPart$part==1,] %>%
           group_by(chapter) %>%
           summarise(index=min(doc_id)) %>%
           ungroup() %>%
           select(index))$index

rescale <- function(x){
  2 * (x - min(x))/( max(x) - min(x)) -1
}

raw_values <- dostoevsky_DCTPart$sentiment
lps <- 10
window <- 0.1
wdw <- round(length(raw_values) * window)
rolled <- rescale(zoo::rollmean(raw_values, k = wdw, fill = 0))
half <- round(wdw/2)
rolled[1:half] <- NA
end <- length(rolled) - half
rolled[end:length(rolled)] <- NA
trans <- get_dct_transform(raw_values, low_pass_size = lps, x_reverse_len = length(raw_values), scale_range = T)
x <- 1:length(raw_values)
y <- raw_values
raw_lo <- stats::loess(y ~ x, span = 0.5)
low_line <- rescale(stats::predict(raw_lo))
normed_trans <- get_dct_transform(raw_values, x_reverse_len = length(raw_values), scale_range = T, low_pass_size = 5)
cap_part <- data.frame(n=seq(1:length(low_line)), low_line, rolled, trans, normed_trans)

sentiment_raw <- ggplot(dostoevsky_DCTPart) +
  aes(x=doc_id, y=sentiment) +
  geom_line(color='goldenrod', size=1) +
  geom_hline(yintercept = 0, lty=2, alpha=0.3) +
  geom_vline(xintercept = seq, lty=2, alpha=0.3) +
  theme_lyrics() +
  labs(x=NULL, y=NULL, title='Sentiment (raw values)') +
  theme(axis.text.x = element_text()) +
  scale_x_continuous(
    breaks=seq, 
    labels=seq(1:length(seq)))

sentiment_rolled <- cap_part %>% 
  select(-normed_trans) %>%
  gather(key='n', value='line_type') %>% 
  group_by(n) %>% 
  summarise(row_num = row_number(), 
            line_type) %>%
  ungroup() %>%
  ggplot() +
  aes(x=row_num, y=line_type, color=n) +
  geom_line() +
  geom_hline(yintercept = 0, lty=2, alpha=0.3) +
  geom_vline(xintercept = seq, lty=2, alpha=0.3) +
  theme_lyrics() +
  labs(x=NULL, y=NULL, title='Sentiment (normalized values)') +
  theme(axis.text.x = element_text(),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(
    breaks=seq, 
    labels=seq(1:length(seq))) +
  scale_color_manual(values = c('black', 'grey60', 'goldenrod'), name='Line Type', 
                     labels = c("Loess Smooth", "Rolling Mean", "Syuzhet DCT (lps = 10)"))

sentiment_normal <- ggplot(cap_part) +
  aes(x=n, y=normed_trans, color='goldenrod') +
  geom_line(size=1) +
  geom_hline(yintercept = 0, lty=2, alpha=0.3) +
  geom_vline(xintercept = seq, lty=2, alpha=0.3) +
  theme_lyrics() +
  labs(x=NULL, y=NULL, title='Sentiment (normalized values)') +
  theme(axis.text.x = element_text(),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(
    breaks=seq,
    labels=seq(1:length(seq))) +
  scale_color_manual(values = 'goldenrod', name='Line Type',
                     labels = "Syuzhet DCT (lps = 5)") +
  ggplot2::annotate("segment", x=65, xend = 65, y=-0.75, yend = -0.27, arrow=arrow()) +
  geom_text(x=65, y=-0.8, label='a young man living in poor conditions', color='gray45', size=3, alpha=4, show.legend = FALSE) +
  ggplot2::annotate("segment", x=170, xend = 170, y=0.27, yend = -0.16, arrow=arrow()) +
  geom_text(x=170, y=0.3, label='meets a drunkard', color='gray45', size=3, alpha=4, show.legend = FALSE) + 
  ggplot2::annotate("segment", x=570, xend = 570, y=0.27, yend = 0.78, arrow=arrow()) +
  geom_text(x=570, y=0.2, label='a letter from his mother', color='gray45', size=3, alpha=4, show.legend = FALSE) +
  ggplot2::annotate("segment", x=750, xend = 750, y=-0.15, yend = 0.95, arrow=arrow()) +
  geom_text(x=750, y=-0.18, label='rescues a litte girl', color='gray45', size=3, alpha=4, show.legend = FALSE) +
  ggplot2::annotate("segment", x=1150, xend = 1150, y=0.3, yend = -0.55, arrow=arrow()) +
  geom_text(x=1150, y=0.32, label='nightmare of the old mare', color='gray45', size=3, alpha=4, show.legend = FALSE) +
  ggplot2::annotate("segment", x=1320, xend = 1320, y=0.6, yend = -0.96, arrow=arrow()) +
  geom_text(x=1320, y=0.62, label='a vision and a fateful coincidence', color='gray45', size=3, alpha=4, show.legend = FALSE) +
  ggplot2::annotate("segment", x=1550, xend = 1550, y=0, yend = -0.78, arrow=arrow()) +
  geom_text(x=1550, y=0.05, label='committing the crime', color='gray45', size=3, alpha=4, show.legend = FALSE)

grid.arrange(
  sentiment_raw, 
  sentiment_rolled, 
  sentiment_normal,
  nrow = 3, ncol = 1, 
  top = textGrob("Crime and Punishment - Part I",gp=gpar(fontsize=20)),
  bottom = 'Chapters')

# dct transform plot
plot_syuzhet <- list()
for (title in unique(dostoevsky_DCT$title)){
  seq <- c(dostoevsky_DCT[dostoevsky_DCT$title==title,] %>%
             group_by(part) %>%
             summarise(index=min(n)) %>%
             ungroup() %>%
             select(index))$index
  
  plot_syuzhet[[title]] <- ggplot(dostoevsky_DCT[dostoevsky_DCT$title==title,]) +
    aes(x=n, y=dct_values) +
    geom_bar(stat = "identity", alpha = 0.8, color = my_colors[title], fill = my_colors[title]) +
    geom_bar(stat = "identity", alpha = 0.8, color = my_colors[title], fill = my_colors[title]) +
    geom_hline(yintercept = 0, lty=2) +
    geom_vline(xintercept = seq, lty=2, alpha=0.5) +
    labs(x=NULL, y=NULL, title=title) +
    theme_lyrics() +
    theme(axis.text.x = element_text(), axis.text.y = element_blank()) +
    scale_x_continuous(
      breaks=seq,
      labels=dostoevsky_DCT[dostoevsky_DCT$title==title & dostoevsky_DCT$n %in% seq,'part'])
}

grid.arrange(
  plot_syuzhet$`Poor Folk`, 
  plot_syuzhet$`Notes from the Underground`, 
  plot_syuzhet$`Crime and Punishment`, 
  plot_syuzhet$`The Brothers Karamazov`,
  nrow = 2, ncol = 2, 
  top = textGrob("Story Arc",gp=gpar(fontsize=20)),
  bottom = textGrob("Part",gp=gpar(fontsize=15))
)

# 8. Sentiment plot
ggplot(dostoevsky_nrc) +
  aes(x=x, y=y, color=title) +
  geom_smooth(size=2) +
  facet_wrap(~sentiment) +
  scale_color_manual(values=my_colors) +
  theme_lyrics() +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# 9. word-sentiment matrix
sentiment_matrix <- list()
for (title in unique(dostoevsky_Words$title)){
  sentiment_matrix[[title]] <- dostoevsky_Words[dostoevsky_Words$title==title,] %>% 
    group_by(lemma) %>% 
    summarise(countx = n()) %>% 
    arrange(desc(countx)) %>% 
    slice(seq_len(80)) %>% 
    rename(word=lemma) %>%
    inner_join(get_sentiments("nrc")) %>% 
    filter(!sentiment %in% c('negative', 'positive')) %>%
    ggplot(aes(x = word, fill = sentiment)) +
    facet_grid(~sentiment) +
    geom_bar() + #Create a bar for each word per sentiment
    theme_lyrics() +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_blank()) + #Place the words on the y-axis
    xlab(NULL) + ylab(NULL) +
    ggtitle(title) +
    coord_flip()
}

grid.arrange(
  sentiment_matrix$`Poor Folk`,
  sentiment_matrix$`Notes from the Underground`,
  sentiment_matrix$`Crime and Punishment`,
  sentiment_matrix$`The Brothers Karamazov`,
  nrow = 2, ncol = 2, 
  top = textGrob("Story Arc",gp=gpar(fontsize=20)),
  bottom = textGrob("Part",gp=gpar(fontsize=15))
)

# 10. Character Arc
raskolnikov <- dostoevsky_M[dostoevsky_M$title=='Crime and Punishment' & 
                              (grepl('rodion', tolower(dostoevsky_M$text)) |
                                 grepl('raskolnikov', tolower(dostoevsky_M$text)) |
                                 grepl('rodya', tolower(dostoevsky_M$text))),]

sonia <- dostoevsky_M[dostoevsky_M$title=='Crime and Punishment' & 
                        (grepl('sofya', tolower(dostoevsky_M$text)) |
                           grepl('sonia', tolower(dostoevsky_M$text))),]

svidrigailov <- dostoevsky_M[dostoevsky_M$title=='Crime and Punishment' & 
                               (grepl('svidrigailov', tolower(dostoevsky_M$text)) |
                                  grepl('arkady', tolower(dostoevsky_M$text))),]

dct_values_raskolnikov <- get_dct_transform(
  raskolnikov$sentiment,
  low_pass_size = 5,
  x_reverse_len = 100,
  scale_vals = T,
  scale_range = F
)
dct_values_sonia <- get_dct_transform(
  sonia$sentiment,
  low_pass_size = 5,
  x_reverse_len = 100,
  scale_vals = T,
  scale_range = F
)
dct_values_svidrigailov <- get_dct_transform(
  svidrigailov$sentiment,
  low_pass_size = 5,
  x_reverse_len = 100,
  scale_vals = T,
  scale_range = F
)

character_dct <- data.frame(
  dct_values_raskolnikov=dct_values_raskolnikov,
  dct_values_sonia=dct_values_sonia,
  dct_values_svidrigailov=dct_values_svidrigailov,
  row=seq(1:length(dct_values_raskolnikov))
)

character_dct %>% 
  gather(row, dct_value) %>%
  rename(dct_type = row) %>% 
  group_by(dct_type) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x=row, y=dct_value, color=dct_type) +
  geom_line(size=2) +
  geom_hline(yintercept = 0, lty=2, alpha=0.3) +
  theme_lyrics() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(1,1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c('red',"midnightblue","honeydew3"), name='Character', 
                     labels = c("Raskolnikov", "Sonia", "Svidrigailov")) +
  ggtitle("Character Arc")
