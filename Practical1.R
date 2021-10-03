## Students:Muxing Wang (S2201749) Karla Vega (s2126801) Nutsa Tokhadze (s1778736) Work Group 3. 
## set the working directory
setwd("../Statistical-Programming")

## scan the file in to object a
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

## define a function to split the punctuation.
split_punct <- function(words,punc){
  index = grep(punc,words,fixed = TRUE)
  words_new <-rep(0,length(words)+length(index))
  new_index = index+1:length(index)
  words_new[new_index] = punc
  words_new[-new_index] = gsub(punc,"",words, fixed = TRUE)
  words_new
}

## split the bible for each punctuation in the list of punctuations
punc_list = c(",", ".", ";", "!", ":", "?")
for (punc in punc_list){
  a = split_punct(words = a, punc)
}

## load the library 'mgcv'
library(mgcv)

## get the 
unique_words = uniquecombs(tolower(a))
ind = attr(unique_words,"index")
freq = tabulate(ind)

## search for the top 1000 frequent words
lower_bound = min(freq) #hduhdsd
upper_bound = max(freq)
threshold = 0
median = 0
m = 1000
while(abs(sum(freq >= median)-m) > 0){
  if (upper_bound == lower_bound + 1){
    if (abs(sum(freq >= upper_bound)-1000) <= abs(sum(freq >= lower_bound)-1000)){
      threshold = upper_bound
    }else{
      threshold = lower_bound
    }
    break()
  }
  median =floor((lower_bound + upper_bound)/2)
  if(sum(freq >= median) > 1000){
    lower_bound = median
  }else{
    upper_bound = median
  }
  print(c(lower_bound, upper_bound, sum(freq >= lower_bound),sum(freq >= upper_bound)))
}

filtered_index = which(freq>=threshold)
unique_words_vec = as.vector(unique_words[[1]])

## b is the most common word vector
b = unique_words_vec[filtered_index]

## Question 7a

col1 = match(tolower(a),b)

index_matrix = cbind(col1[1:length(col1)-1],col1[2:length(col1)])

pair_matrix = index_matrix[!is.na(rowSums(index_matrix)),]


## create a matrix A
max_ind = max(pair_matrix) # get the 
A = matrix(0, max_ind, max_ind)
for (row in 1:nrow(pair_matrix)){
  i <- pair_matrix[row,1] # get the target index of column
  j <- pair_matrix[row,2] # get the target index of rol
  A[i,j] = A[i,j] + 1
}

for (row in 1:nrow(A)){
  A[row,] = A[row,] / sum(A[row,])
}

## Question 8
## simulating 50 words

simulation <- function (number_of_words){
  starting_index = sample(1:1004,size = 1) # randomly pick an entry index
  simulated_text_indices = 1:number_of_words # initialize the indices vector to store all the word indices during simulation
  simulated_text_indices[1] = starting_index # initialize the first index to the starting_index we just got
  cursor = starting_index # initialize a cursor as the index
  
  ##
  ## simulation begins
  for (i in 2:number_of_words){
    next_word_index = sample(1:1004,size = 1,prob = A[cursor,]) # random sampling the next word's index
    simulated_text_indices[i] = next_word_index # update the corresponding word index in our vector simulated_text_indices
    cursor = next_word_index # move cursor to the next index and continue generating new index from there
  }
  
  for (i in simulated_text_indices){
    cat(paste(b[i],""))
  }
}

number_of_words = 50 # number of words that we want to simulate
simulation(number_of_words)
