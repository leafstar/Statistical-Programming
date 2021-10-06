## set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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
threshold = 0                                                     ## Inicialized treshhold
median = 0                                                        ## Inicialized median
m = 1000
while(abs(sum(freq >= median)-m) > 0){                            ## while to find the upper and lower bounds 
  if (upper_bound == lower_bound + 1){
    if (abs(sum(freq >= upper_bound) - m) <= abs(sum(freq >= lower_bound) - m)){  ##Specification of the limit of the bounces
      threshold = upper_bound                                     ##Update the bounds
    }else{
      threshold = lower_bound                                     ##Update the bounds
    }
    break()
  }
  median =floor((lower_bound + upper_bound)/2)                    ##rounds the value obtained from the calculation of bounds
  if(sum(freq >= median) > m){
    lower_bound = median
  }else{
    upper_bound = median
  }
}

filtered_index = which(freq>=threshold)                                              

## b is the most common word vector
b = unique_words[filtered_index]                                   ## vector of aprox 1000 most commonly occurring uniq words

## Question 7a

col1 = match(a_lower,b)                                            ## matches elements from the most common word vector -b and lower case text (complete text)-a                       

index_matrix = cbind(col1[1:length(col1)-1],col1[2:length(col1)])  ## matrix created with de col1 vector, the second column is also de col1 but traversed one position up 

pair_matrix = index_matrix[!is.na(rowSums(index_matrix)),]         ## drops word pairs containing na from the matrix created in line 64


## create a matrix A
max_ind = max(pair_matrix)                                         ## get the size of the clean matrix
A = matrix(0, max_ind, max_ind)                                    ## creates de matrix A with 0 values and size max_ind x max_ind. Contains the frequency of the common words
for (row in 1:nrow(pair_matrix)){
  i <- pair_matrix[row,1]                                          ## Creation of the target index of column
  j <- pair_matrix[row,2]                                          ## Creation of the target index of rol
  A[i,j] = A[i,j] + 1
}

## Normalizing the rows of matrix A.
for (row in 1:nrow(A)){                                           ## loop created for the calculation of distributions of Matrix A   
  A[row,] = A[row,] / sum(A[row,])
}                                                                 ## calculates conditional distribution for A[i,j]                                                        

## Question 8&9
## simulating 50 words

upper_match = match(a,b)                                          ## Match the complete vector with Unique words
indices_modi = which((tabulate(col1) - tabulate(upper_match))/tabulate(col1)>=0.5)   #This step analyze the words that may be change to uppercase


simulation <- function (number_of_words){
  starting_index = sample(1:length(b),size = 1)                        ## randomly pick an entry index from b
  simulated_text_indices = 1:number_of_words                      ## Establish the length of the text to be printed
  simulated_text_indices[1] = starting_index                      ## Put the word selected in the first position
  cursor = starting_index                                         ## initialize a cursor as the index
  ##
  ## simulation begins
  for (i in 2:number_of_words){                                   ##Starts to pick the following words, in the position 2, as the first is already choose
    next_word_index = sample(1:length(b), size = 1, prob = A[cursor,]) ## random sampling the next word's index
    simulated_text_indices[i] = next_word_index                   ## update the corresponding word index in our vector simulated_text_indices
    cursor = next_word_index                                      ## move cursor to the next index and continue generating new index from there
  }
  
  for (i in simulated_text_indices){                              ##Selects the final chosen words
    if (i %in% indices_modi){                                     
      cat(toupper(substring(b[i], 1, 1)))                         ##Chose the first index of the word which may be change and update it to capital letter
      cat(substring(b[i], 2, nchar(b[i])))                        ##Completes the word with its following letter 
    }
    else{
      cat(b[i])                                                   ##Prints the original word in case it was no in the most common word list
    }
    cat(" ")                                                      ##Use for format. It generate spaces
  }
}

number_of_words = 50                                              ## number of words that we want to simulate
simulation(number_of_words)                                       ##Needed to run the function 
    