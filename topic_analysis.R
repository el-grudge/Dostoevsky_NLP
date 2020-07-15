dostoevsky_dtm_balanced <- dostoevsky_Words %>%
  select(-n) %>%
  count(title, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(title, word, n)

lda <- LDA(dostoevsky_dtm_balanced, k = 4, method = "GIBBS", control = list(seed = seed))
