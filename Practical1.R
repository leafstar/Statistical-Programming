## Students: Muxing Wang (S2201749) Karla Vega (s2126801) Nutsa Tokhadze (s1778736) Work Group 3. 
## set the working directory
setwd("/Users/muxingwang/OneDrive - University of Edinburgh/StatisticalProgramming/git-repo/Statistical-Programming")

## scan the file in to object a
a <- scan("1581-0.txt",what="character",skip=156)           ## Read items
n <- length(a)                                              ## Measuring length of  "a"
a <- a[-((n-2909):n)]                                       ## strip license
a<-                                                         ## Check out what is in "a"                                                        

## define a function to split the punctuation.
split_punct <- function(words,punc){   
  index = grep(punc,words,fixed = TRUE)                     ## finds the indices of the words containing punctuation marks in words
  words_new <-rep(0,length(words)+length(index))            ## creates a vector of zeroes
  new_index = index+1:length(index)                         ## finds location for the words containing the punctuation marks in words_new
  words_new[new_index] = punc                               ## inserts words with punctuation marks in words_new
  words_new[-new_index] = gsub(punc,"",words, fixed = TRUE) ## removes punctuation marks from the word and inserts in ""
  words_new }                                               ## print words_new 
(## If we run it we will have zeros besides the words with punctuation marks, but we also want all other words to be in the string??)


## Use split_punct function to separate the punctuation marks from words.
punc_list = c(",", ".", ";", "!", ":", "?")         ## create a vector of the punctuations
for (punc in punc_list){
  a = split_punct(words = a, punc)                  ## This function splits a string based on various options.
} ## Why do we need loop here? can't we separate the punctuation marks without it? 

## load the library 'mgcv'
library(mgcv)

## get the 
unique_words = uniquecombs(tolower(a))  #Convert texts to lower case
ind = attr(unique_words,"index")      #to know the attributes of the variable unique words
freq = tabulate(ind) ## it says error in tabulation. 

## search for the top 1000 frequent words
lower_bound = min(freq) 
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


#end_time <- Sys.time()
#end_time - start_time
