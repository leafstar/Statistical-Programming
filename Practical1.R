#start_time <- Sys.time()
## set the working directory
setwd("../Statistical-Programming")

## scan the file in to object a
a <- scan("1581-0.txt",what="character",skip=156)                 ## scans the file
n <- length(a)                                                    ## counts number of words 
a <- a[-((n-2909):n)]                                             ## strip license

## define a function to split the punctuation.
split_punct <- function(words,punc){                              ## defines function with the inputs: words and punc
  index = grep(punc,words,fixed = TRUE)                           ## finds the indices of the words containing punctuation marks
  words_new <-rep(0,length(words)+length(index))                  ## creates vector of zeroes to store the words and punctuation marks
  new_index = index+1:length(index)                               ## computes the locations for punctuation marks
  words_new[new_index] = punc                                     ## inserts punctuation marks in words_new vector
  words_new[-new_index] = gsub(punc,"",words, fixed = TRUE)       ## strippes out punctuation marks from the words in words_new vector 
  words_new                                                       ## prints words_new
}

## separate the punctuation marks
punc_list = c(",", ".", ";", "!", ":", "?")                       ## creates a list of punctuation marks 
for (punc in punc_list){                                          ## loop that separates the punctuation marks from the words in the bible text-a
  a = split_punct(words = a, punc) 
}

## get the 
a_lower = tolower(a)                                              ## replaces the capital letters in words with lower case letters in the text-a
unique_words = unique(a_lower)                                    ## finds the vector of unique words
ind = match(a_lower,unique_words)                                 ## finds the indicies of common elements of uniq words and the lower case bible text
freq = tabulate(ind)                                              ## counts how many times each unique word occurs in the text

## search for the top 1000 frequent words
lower_bound = min(freq)                                           ## creates lower bound for minimum frequency
upper_bound = max(freq)                                           ## creates upper bound for maximum frequency
threshold = 0                                                     ## sets treshhold
median = 0                                                        ## sets median
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
}

filtered_index = which(freq>=threshold)                                              

## b is the most common word vector
b = unique_words[filtered_index]                                   ## vector of 1004 most commonly occurring uniq words

## Question 7a

col1 = match(a_lower,b)                                            ## matches elements from the most common word vector -b and lower case text-a                       

index_matrix = cbind(col1[1:length(col1)-1],col1[2:length(col1)])  ## matrix with first column -the index of common words;the next column-the index for following word

pair_matrix = index_matrix[!is.na(rowSums(index_matrix)),]         ## drops word pairs containing na 


## create a matrix A
max_ind = max(pair_matrix) # get the 
A = matrix(0, max_ind, max_ind)
for (row in 1:nrow(pair_matrix)){
  i <- pair_matrix[row,1] # get the target index of column
  j <- pair_matrix[row,2] # get the target index of rol
  A[i,j] = A[i,j] + 1
}

## Normalizing the rows of matrix A.
for (row in 1:nrow(A)){                                           ## loop that standardizes the rows of Matrix A   
  A[row,] = A[row,] / sum(A[row,])
}                                                                 ## calculates conditional distribution for A[i,j]                                                        

## Question 8&9
## simulating 50 words

upper_match = match(a,b)
indices_modi = which((tabulate(col1) - tabulate(upper_match))/tabulate(col1)>=0.5)


simulation <- function (number_of_words){
  starting_index = sample(1:1004,size = 1)                        ## randomly pick an entry index from b
  simulated_text_indices = 1:number_of_words                      ## initialize the indices vector to store all the word indices during simulation
  simulated_text_indices[1] = starting_index                      ## initialize the first index to the starting_index we just got
  cursor = starting_index                                         ## initialize a cursor as the index
  ##
  ## simulation begins
  for (i in 2:number_of_words){
    next_word_index = sample(1:1004, size = 1, prob = A[cursor,]) ## random sampling the next word's index
    simulated_text_indices[i] = next_word_index                   ## update the corresponding word index in our vector simulated_text_indices
    cursor = next_word_index                                      ## move cursor to the next index and continue generating new index from there
  }
  
  for (i in simulated_text_indices){
    if (i %in% indices_modi){
      cat(toupper(substring(b[i], 1, 1)))
      cat(substring(b[i], 2, nchar(b[i])))
    }
    else{
      cat(b[i])
    }
    cat(" ")
  }
}

number_of_words = 50                                               ## number of words that we want to simulate
simulation(number_of_words)
