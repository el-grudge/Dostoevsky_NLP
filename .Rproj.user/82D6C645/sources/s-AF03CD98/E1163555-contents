library(gutenbergr)

gutenberg_authors

gutenberg_metadata

gutenbergr::gutenberg_metadata("Doyle, Arthur Conan")

class(gutenberg_authors)

gutenberg_authors[
grepl(
  'dost', 
  stringr::str_to_lower(gutenberg_authors$author), 
  fixed = TRUE),]



remove(gutenberg_metadata)

class(gutenberg_metadata$author)

View(gutenberg_metadata[
!is.na(gutenberg_metadata$author) &
gutenberg_metadata$author==gutenberg_authors[gutenberg_authors$gutenberg_author_id==314,'author']$author,]
)

# 600 Notes from the Underground	
# 2554 Crime and Punishment
# 2638 The Idiot	
# 8117 The Possessed (The Devils)	
# 28054 The Brothers Karamazov	

View(toString(gutenberg_download(2554)$text)[1])

View(gutenberg_download(2554)$text)

View(strsplit(toString(gutenberg_download(2554)$text)[1],split=', , , , '))

strsplit(toString(gutenberg_download(2554)$text)[1],split=', , , , ')
chapters <- strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]]
View(strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]])

strsplit(toString(gutenberg_download(2638)$text)[1],split=', , , , ')
View(strsplit(toString(gutenberg_download(2638)$text)[1], split = ", , , , ")[[1]])


BiocManager::install("Biobase", version = "3.8")
BiocManager::install("Biobase")
View(c(strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]]))
class(list(strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]]))
Biobase::listLen(list(strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]]))
View(list(strsplit(toString(gutenberg_download(2554)$text)[1], split = ", , , , ")[[1]]))

sapply(strsplit(toString(gutenberg_download(2554)$text), split = ", , , , ")[[1]],nchar)

chapters <- strsplit(toString(gutenberg_download(2554)$text), split = ", , , , ")[[1]]
View(data.frame(chapters, sapply(chapters, nchar)))
View(data.frame(chapters, sapply(chapters, nchar), row.names = nrow(chapters)))

# 2638 The Idiot	
View(strsplit(toString(gutenberg_download(2638)$text), split = ", , , , ")[[1]])
View(strsplit(toString(gutenberg_download(2638)$text), split = ", , ")[[1]])
View(toString(gutenberg_download(2638)$text))

stringi::stri_split_regex(c("ab,c", "d,ef  ,  g", ",  h", ""),"\\p{WHITE_SPACE}*,\\p{WHITE_SPACE}*", omit_empty=NA, simplify=TRUE)

stringi::stri_split_regex(c("PART I, , I., , Towards the end of November"),"\\p*, ,\\p*, , ")

stringi::stri_split_regex(c("PART I, , I., , Towards the end of November"),",{WHITE_SPACE},{WHITE_SPACE}\\p,{WHITE_SPACE},{WHITE_SPACE}")

stringi::stri_split_regex(c("PART I, , I., , Towards the end of November"),", , [I]., ,")

stringi::stri_split_regex(c("THE IDIOT, , By Fyodor Dostoyevsky, , , Translated by Eva Martin, , , , , PART I, , I., , Towards the end of November, windows., , Some of"),", , [A-Z]., , ")

View(stringi::stri_split_regex(toString(gutenberg_download(2638)$text),", , [A-Z]., , "))

View(stringi::stri_split_regex(toString(gutenberg_download(2638)$text),", , [A-Z]*., , ")[[1]])

# 8117 The Possessed (The Devils)	
View(toString(gutenberg_download(8117)$text))
View(stringi::stri_split_regex(toString(gutenberg_download(8117)$text),", , CHAPTER")[[1]])

# 28054 The Brothers Karamazov	
View(toString(gutenberg_download(28054)$text))
View(stringi::stri_split_regex(toString(gutenberg_download(28054)$text),", , Chapter")[[1]])
