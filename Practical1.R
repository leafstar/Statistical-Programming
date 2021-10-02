#start_time <- Sys.time()
## set the working directory
setwd("/Users/muxingwang/OneDrive - University of Edinburgh/StatisticalProgramming/git-repo/Statistical-Programming")

## scan the file in to object a
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

## define a function to split the punctuations.
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
bible_text = as.vector(unique_words[[1]])
word_vector = bible_text[filtered_index]

## Question 7a
match(bible_text,word_vector)




#end_time <- Sys.time()
#end_time - start_time