##                Group 3               ##
## Muxing Wang (s2201749)               ##
## Karla Itzel Vega Ortega (s2126801)   ##
## Nutsa Tokhadze (s1778736)            ##
##########################################

## set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## scan the file in to object a
a <- scan("1581-0.txt",what="character",skip=156)                 ## scans the file
n <- length(a)                                                    ## counts number of words 
a <- a[-((n-2909):n)]                                             ## strip license

## "split_punc" splits the punctuation from the words.
## input arguments:
##    words: the text to be split, type: a vector of words
##    punc: the punctuation, type:  character 
## output: a vector contains the original text and the split punctuations.
split_punct <- function(words,punc){                              
  index = grep(punc,words,fixed = TRUE)                           ## finds the indices of the words containing the punctuation mark "punc"
  words_new <-rep(0,length(words)+length(index))                  ## creates vector of zeroes to store the words and punctuation marks
  new_index = index+1:length(index)                               ## computes the locations for punctuation marks
  words_new[new_index] = punc                                     ## inserts punctuation marks in words_new vector
  words_new[-new_index] = gsub(punc,"",words, fixed = TRUE)       ## stripes out punctuation marks from the words in words_new vector 
  words_new                                                       ## returns words_new
}

## separate the punctuation marks from the main text by calling "split_punct"
punc_list = c(",", ".", ";", "!", ":", "?")                       ## a list of punctuation marks which need to be split
for (punc in punc_list){                                          ## loop that separates the punctuation marks from the words in the bible text "a"
  a = split_punct(words = a, punc) 
}

a_lower = tolower(a)                                              ## replaces the capital letters in words with lower case letters in the text-a
unique_words = unique(a_lower)                                    ## finds the vector of unique words
ind = match(a_lower,unique_words)                                 ## finds the indices of common elements of unique words and the lower case bible text
freq = tabulate(ind)                                              ## counts how many times each unique word occurs in the text

## this while loop uses binary search to determine the threshold which gives us roughly 1000 most common words.
lower_bound = min(freq)                                           ## creates lower bound for minimum frequency
upper_bound = max(freq)                                           ## creates upper bound for maximum frequency
threshold = 0                                                     ## Initialized threshold
median = 0                                                        ## Initialized median
m = 1000
while(abs(sum(freq >= median)-m) > 0){                            ## the loop stops when the number of most common words is m=1000
  
  ## when reaching the edge case finally, the threshold will be set to the number 
  ## which gives us the closest number of words compared to 1000.
  if (upper_bound == lower_bound + 1){                            
    if (abs(sum(freq >= upper_bound) - m) <= abs(sum(freq >= lower_bound) - m)){  
      threshold = upper_bound                                     
    }else{
      threshold = lower_bound                                     
    }
    break()
  }
  
  ## binary search
  median =floor((lower_bound + upper_bound)/2)                    
  if(sum(freq >= median) > m){
    lower_bound = median
  }else{
    upper_bound = median
  }
}

filtered_index = which(freq>=threshold)                            ## extract the indices of the most common words                   

b = unique_words[filtered_index]                                   ## vector of roughly 1000 most commonly occurring unique words

col1 = match(a_lower,b)                                            ## matches elements from the most common word vector "b" and lower case text (complete text)"a"                       

index_matrix = cbind(col1[1:length(col1)-1],col1[2:length(col1)])  ## matrix created with de col1 vector, the second column is also the col1 but traversed one position up 

pair_matrix = index_matrix[!is.na(rowSums(index_matrix)),]         ## drops word pairs containing NA from the matrix created in line 64


## create a matrix A whose entry A[i,j] will be the probability of word b[i] followed by b[j].
dim = length(b)                                                    ## get the dimension of vector "b"
A = matrix(0, dim, dim)                                            ## initialize matrix A with 0 values 
for (row in 1:nrow(pair_matrix)){
  i <- pair_matrix[row,1]                                         
  j <- pair_matrix[row,2]                                         
  A[i,j] = A[i,j] + 1
}

## Normalizing the rows of matrix A so that each row sums up to 1.
for (row in 1:nrow(A)){                                           ## loop created for the calculation of distributions of Matrix A   
  A[row,] = A[row,] / sum(A[row,])
}                                                                 ## calculates conditional distribution for A[i,j]                                                        

## Question 8&9 Simulation
## simulating 50 words

## this part is to determine which words should start with a capital letter.
upper_match = match(a,b)                                          ## Match the main text(has upper case words) with Unique words
indices_modi = which((tabulate(col1) - tabulate(upper_match))/tabulate(col1)>=0.5)   # a vector contains indices of words should start with capital letters.

## "simulation" takes the number of words we want to simulate, 
##and returns the simulated text based on the probabilty matrix "A".
simulation <- function (number_of_words){
  starting_index = sample(1:length(b),size = 1)                   ## randomly pick an entry index from b
  simulated_text_indices = 1:number_of_words                      ## a vector to store the indices of simulated words.
  simulated_text_indices[1] = starting_index                      ## Put the word selected in the first position
  cursor = starting_index                                         ## initialize a cursor as the index

  ## simulation begins
  for (i in 2:number_of_words){                                   ##Starts to pick the following words, in the position 2, as the first is already choose
    next_word_index = sample(1:length(b), size = 1, prob = A[cursor,]) ## random sampling the next word's index
    simulated_text_indices[i] = next_word_index                   ## update the corresponding word index in our vector "simulated_text_indices"
    cursor = next_word_index                                      ## move cursor to the next index and continue generating new index from there
  }
  
  ## generate the text based on the simulated indices.
  for (i in simulated_text_indices){                              
    ## if the word should start with a capital letter, we should change the lowercase word to what is desired.
    if (i %in% indices_modi){                                     
      cat(toupper(substring(b[i], 1, 1)))                         
      cat(substring(b[i], 2, nchar(b[i])))                        
    }
    ## else, just print the lowercase word.
    else{
      cat(b[i])                                                   ##Prints the original word in case it was no in the most common word list
    }
    cat(" ")                                                      ##Use for format. It generate spaces
  }
}

number_of_words = 50                                              ## number of words that we want to simulate
simulation(number_of_words)                                       ## call the simulation function to generate text.
    