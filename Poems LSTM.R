library(keras)
library(magrittr)
library(stringr)

# Functions
uniqchars <- function(x) unique(strsplit(x, "")[[1]]) 

charToInt <- function(x) match(x,mapChar)
intToChar <- function(x) mapChar[x]

info <- function(){
  print(paste0("Length of raw text: ", nchar(text)))
  print(paste0("Total number of Vocab: " ,length(mapChar)))
  print(paste0("Number of sequences from text: ", length(input_Output[[1]])))
  print("**Different Chars in text**")
  print(mapChar)
}

charToCategorical <- function(x){
  categorical <- replicate(length(mapChar), 0)
  categorical[charToInt(x)] <- 1
  categorical
}

normInput <- function(x){x/length(mapChar)}

# Extact sentences from text 
get_sequences_and_predictions <- function(){
  x <- vector("list",(nchar(text) - sequence_len))
  y <- vector("list",(nchar(text) - sequence_len))
  
  start_time <- Sys.time()
  for(i in 1:(nchar(text) - sequence_len)){
    # Take substring trom text
    temp <- substring(text, i, i + sequence_len -1)
    temp <- sentence_transform(temp)
    # Insert vector of integers in list at index "i" 
    x[[i]] <- temp
    
    # Take first character after sequence of x and convert to integer 
    y[[i]] <- substring(text, i + sequence_len, i + sequence_len) %>% charToCategorical()
  }
  end_time <- Sys.time()
  print(end_time - start_time)
  list(x,y)
}

# Transform a sentence to vector of integers
sentence_transform <- function(x) {
  # Split string into single characters and convert them to integers
  substring(x, 1:nchar(x),1:nchar(x)) %>% charToInt()
}
sentence_transform("hello")

# Reshape list of vectors (of integers) to array for network
reshapeX <- function(listX){
  x <- array(0,dim = c(length(listX),sequence_len,1))
  
  for(i in 1:length(listX)){
    x[i,,] <- sapply(listX[i], FUN = function(x){x})
  }
  x
}
reshapeY <- function(listY){
  y <- array(0, dim = c(length(listY),length(mapChar)))
  for(i in 1:length(listY)){
    y[i,] <- sapply(listY[i], FUN = function(x){x})
  }
  y
}

# build model
build_model <- function(){
  model <- keras_model_sequential()
  model %>%
    layer_lstm(256, input_shape = c(sequence_len,1), name = "LSTM") %>% 
    layer_dropout(0.2) %>%
    layer_dense(length(mapChar), activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = "adam")
  model
}
model <- build_model()

generate_Poem <- function() {
  
  # choose random sequence to start with
  s = input_Output[[1]][20000]
  s <- s %>% reshapeX()
  output <- ""
  for(i in 1:100){
    pred <- predict(model, s) %>% which.max()
    output <- paste0(output, pred %>% intToChar())
    s[1,,] <- c(s[,-1,], pred %>% normInput())
  }
  output
}
generate_Poem()
stop()
#**** Flow *******
# Variables
sequence_len <- 100

#Load raw data
data_raw <- read.csv(paste0(getwd(), "/Poems/all.csv"))
data_love <- data_raw %>% subset(data_raw$type == "Love")
data_target <- data_raw

# Clean and preprocess, (data_target)
data <- data_target[,2] %>% 
  str_squish() %>%
  sapply(tolower) %>%
  as.data.frame()
names(data) = c("Poems")

# Shuffle poems
data <- data[sample(nrow(data),nrow(data)),]
# Combine poems to a long single text.
text <- paste(unlist(data), collapse =" ")

# Create dictionary for unique characters, access with "charToInt()"
mapChar <- text %>% uniqchars()

sequence_len <- 70
input_Output <- get_sequences_and_predictions()

# Normalize X
input_Output[[1]] <- lapply(input_Output[[1]] , FUN = normInput)

model <- build_model()

x <- reshapeX(input_Output[[1]])
y <- reshapeY(input_Output[[2]])

model_history <- model %>% fit(
  x, y,
  batch_size = sequence_len,
  epochs = 40,
  verbose =2,
  callback_model_checkpoint("model.h5")
)

