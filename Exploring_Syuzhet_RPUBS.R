library("syuzhet")
library("magrittr")
library("rvest")
library("NLP")
# library("rlist")
library("tidyr")
library("ggplot2")
library("stringr")

if(!file.exists("books.rda")) {
  moby_dick <- "mobydick.txt" %>% 
    get_text_as_string()
  theron_ware <- "theronware.txt" %>% 
    get_text_as_string()
  norwood <- "https://archive.org/stream/norwood00beecgoog/norwood00beecgoog_djvu.txt" %>% 
    html() %>% 
    html_node("pre") %>% 
    html_text() %>% 
    as.String()
  wooing <- "wooing.txt" %>% 
    get_text_as_string()
  uncle_tom <- "uncletom.txt" %>%
    get_text_as_string()
  
  books <- list(moby_dick = moby_dick, 
                theron_ware = theron_ware, 
                norwood = norwood, 
                wooing = wooing, 
                uncle_tom = uncle_tom) %>% 
    lapply(get_sentences)
  
  save(books, file = "books.rda")
} else {
  load("books.rda")
}

multi_sentiment <- function(sentences) {
  list(bing  = get_sentiment(sentences, method = "bing"),
       afinn = get_sentiment(sentences, method = "afinn"),
       nrc   = get_sentiment(sentences, method = "nrc")
       #        stanford = get_sentiment(sentences, method = "stanford", 
       #                     path_to_tagger = "/Applications/stanford-corenlp")
  )
}

sentiment <- books %>% 
  lapply(multi_sentiment)

sum_up_sentiment <- function(x) {
  apply_sentiment <- function(vec) {
    list(sum = sum(vec),
         mean = mean(vec),
         summary = summary(vec))
  }
  
  if(is.list(x))
    lapply(x, apply_sentiment)
  else
    apply_sentiment(x)
}

# sentiment %>% 
#   lapply(sum_up_sentiment) %>% 
#   list.unzip()

bind_pos <- function(df) {
  pos <- data.frame(position = 1:nrow(df))
  cbind(df, pos)
}

plot_nrc <- function(df, title) {
  ggplot(df,
         aes(x = position, y = value, color = emotion)) +
    geom_smooth(size = 2, se = FALSE) +
    xlab("Narrative position") +
    ylab("Prevalence") +
    theme_classic() +
    ggtitle(title)
}

books %>% 
  lapply(get_nrc_sentiment) %>% 
  lapply(bind_pos) %>% 
  lapply(gather, emotion, value, -position, -negative, -positive) %>% 
  Map(plot_nrc, ., names(.))