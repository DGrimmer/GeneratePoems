library(magrittr)
library(stringr)
library(tidytext)
library(tidyr)
library(plyr) #count()
library(Momocs) #
library(stylo) # stopwords
library(tm)
library(Hmisc)
library(foreach)

fileName <- "all.csv"
path_wd <- getwd()
raw_data <- data_love_poems

# Clean the data by removing white space
clean_data <- raw_data[,2] %>% 
  str_squish() %>%
  as.data.frame()

t <- apply(clean_data, 1, function(x){if(grepl("copyright", x,ignore.case = T))T else F})


clean_data <- clean_data[!grepl("copyright", clean_data,ignore.case = T), ]

names(clean_data) = c("Poems")

# Tokenize, split 
trigrams_data <-
  unnest_tokens(clean_data, 
                Output, 
                Poems, 
                token = "ngrams",
                to_lower = TRUE, 
                n= 3) %>%
  separate(Output, c("word1", "word2", "word3"), sep = " ") %>%
  count()

find_third_word <- function( w1, w2){
  words <- trigrams_data %>% filter(word1 == w1 & word2 == w2)
  
  # Check if words has been found, Handle this
  if(nrow(words) == 0){
    temp <- trigrams_data %>% filter(word1 == w2) %>% sample_n(size = 1)
    
    warning("third word not found, adding ", temp %>% .[["word3"]], " to ", w1, " " , w2)
    w <- temp
  }
  else{
    #use freq as weights for the sampling
    w <- words %>% sample_n(size = 1, weight=words[,4]) %>% .[["word3"]]
  }
  w
}

generate_sentence = function(word1, word2, sentencelength =10, debug =FALSE){
  #check input so it's not too short
  if(sentencelength <3)stop("I need more to work with")
  
  # substract '2' to make the total sentence size = sentencelength
  sentencelength <- sentencelength -2
  # add the two first words
  sentence <- c(word1, word2)
  # temp variables
  w1 <- word1
  w2 <- word2
  
  for(i in seq_len(sentencelength)){
    wNew <- find_third_word( w1, w2)
    sentence <- c(sentence, wNew)
    w1 <- w2
    w2 <- wNew
  }
  output <-paste(sentence, collapse = " ")
  output
}
generate_sentence("i","want",20)
generate_sentence( "is", "copyright", 15 )
generate_sentence(trigrams_data[10,1], 
                  trigrams_data[10,2])


fix_Sentence <- function(s){
  s <- s %>% as.String()
  ss <- strsplit(s, " ")
  sLength <- lengths(ss)
  
  remove <- 0
  while (TRUE) {
    if(!is.element(ss[[1]] %>% tail(remove +1) %>% head(1), stopwords()))break
    remove <- remove + 1
  }
  # remove the found stopwords from the back
  if(remove > 0){ 
    ss <- ss[[1]] %>% head(-remove)}
  else ss <- ss[[1]]
  
  # Rebuild the string
  ss <- paste(ss, collapse = " ")
  ss <- capitalize(ss)
  ss <- paste0(ss, ".")
  ss
  }
fix_Sentence("hello my name is david")

generate_poem <- function(sentences =3  ){
  if(sentences < 1)stop("a poem need to have a minimum of 1 sentence")
  #create the first sentence manualy
  sentences <- sentences -1
  random_index <- sample(1:nrow(trigrams_data),1)
  poem <- generate_sentence(trigrams_data[random_index,1], 
                            trigrams_data[random_index,2], 
                            sample(12:18,1))
  
  for (i in seq_len(sentences)) {
    random_index <- sample(1:nrow(trigrams_data),1)
    newSentence <- generate_sentence(trigrams_data[random_index,1], 
                                     trigrams_data[random_index,2], 
                                     sample(12:18,1) )
    # TODO
    # fix upper case letter in the begining of a sentence.
    # newSentence <- fix_Sentence(newSentence)
    poem <- c(poem, newSentence)
  }
  
  poem <- foreach(p = poem) %do%
    fix_Sentence(p)
  
  
  output <- paste(poem, collapse = " ")
  output
}
generate_poem(2)

