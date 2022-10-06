#setwd("put/your/local/repo/location/here")
a <- scan("D:/Rscript/SP/SP/pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers 

split_punct<-function(a){
  ia<- grep("[[:punct:]]",a,fixed=FALSE) # which element of a has punctuation
  xia<- rep("",length(ia)+length(a)) # vector to store the elements without punctuation
  iis <- ia+1:length(ia) #where should punctuation go
  xs<-c()
  xs[iis]<- substr(a[ia],nchar(a[ia]),nchar(a[ia]))
  xs[-iis]<- gsub("[[:punct:]]","",a) #without punctuation
  return(xs)
}
a<-split_punct(a)#separate the punctuation marks

#find the vector of unique words in a (whether the first letter to be capital or not 
#will make two different words)
uniq<-unique(a)

#find the vector of indices indicating which element in the unique word vector
idx_vector<-match(a,uniq)

#count up how many time each unique word occurs in the text
count<-tabulate(idx_vector)

m<-500 #decide the threshold number of occurrence
idx_m<-order(count,decreasing = TRUE)[1:m] #sort the index of occurrence
b<-c() #create a vector b
for (i in idx_m) { b<-append(b,uniq[i])} #b contains m(500) most commonly occurring words

#The following steps aim to separate b into two vectors, 'b_cap' contains all words start 
#with capital letter in b, and 'b_noncap' contains all words start without capital letter
#in b. 
idx_cap<-grep('^[[:upper:]].*',b,fixed=FALSE) #get indices of all words start with capital letter in b  
b_cap<-c()
for (i in idx_cap) { b_cap<-append(b_cap,b[i])} #get b_cap

idx_noncap<-grep('^[[:lower:]]|[[:punct:]].*',b,fixed=FALSE)#indices of all words start without capital letter in b
b_noncap<-c()
for (i in idx_noncap) { b_noncap<-append(b_noncap,b[i])}#get b_noncap

#get the number of words in b_cap and b_noncap
m_cap<-length(b_cap)
m_noncap<-length(b_noncap)

idx_common<-match(a,b)#first column
secondc<-append(idx_common[2:length(idx_common)],NA)#second column
thirdc<-append(secondc[2:length(secondc)],NA)#third column
mat<-cbind(idx_common,secondc,thirdc)#form a matrix

triplet_all<-rowSums(is.na(mat))
idx_triplet_com<-which(triplet_all==0)#identify the common word triplets
triplet_com<-mat[idx_triplet_com,]#drop the other word triplets those that contain an NA

T<-array(0,dim=c(m,m,m))#initialize matrix T with 500x500x500
#loop through the common word triplets adding a 1 to T[i,k,j] every time the jth common word follows the pair i,k
for (i in 1:nrow(triplet_com)) {
  T[triplet_com[i,1],triplet_com[i,2],triplet_com[i,3]]<-T[triplet_com[i,1],triplet_com[i,2],triplet_com[i,3]]+1
}

A<-array(0,dim=c(m,m))#initialize matrix A with 500x500
#loop through the common word triplets adding a 1 to A[i,k] every time the kth common word follows the word i
for (i in 1:nrow(triplet_com)) {
  A[triplet_com[i,1],triplet_com[i,2]]<-A[triplet_com[i,1],triplet_com[i,2]]+1
}

#The following steps aim to get two vectors, one is to count up the number of times 
#that each word in 'b_cap' occurs in the text, the other one is to count up the number
#of times that each word in 'b_noncap' occurs in the text
S_cap<-rep(0,m_cap)
S_noncap<-rep(0,m_noncap)
for (i in 1:nrow(triplet_com)) {
  #loop through the common word triplets adding a 1 to S_cap[i] every time the ith common word occur
  if ((b[triplet_com[i,1]] %in% b_cap) == TRUE) {
    S_cap[which(idx_cap==triplet_com[i,1])]<-S_cap[which(idx_cap==triplet_com[i,1])]+1
  }
  
  #loop through the common word triplets adding a 1 to S_noncap[i] every time the ith common word occur
  if ((b[triplet_com[i,1]] %in% b_noncap ==TRUE)) {
    S_noncap[which(idx_noncap==triplet_com[i,1])]<-S_noncap[which(idx_noncap==triplet_com[i,1])]+1
  }
}

#Now simulate_modify is a modified function used to simulate 50-word sections from the model.
#It takes 6 arguments:
#b: a vector of the m most commonly words
#b_cap: a vector collected from b, which contains all words start with capital letter in b
#S_cap: a vector that counts up the number of times that each word in b_cap occurs in the text
#A: a matrix that counts up each time bj follows bi in the text.
#T: a matrix that counts up the number of times bj follows sequential word pairs bi, bk for all words
#n: the number of words in a given word-section
simulate_modify<-function(b,b_cap,S_cap,A,T,n){
  generate_string='' #this string will be the final result to generate
  
  #first randomly pick a word from b_cap, based on the probabilities in S_cap,
  #because the first word in text must start with a capital letter
  sample_S<- sample(b_cap,1,prob = S_cap) 
  
  #then randomly pick a word from b again, based on the first word and the probabilities in A
  sample_A<-sample(b,1,prob=A[which(b==sample_S),])
  generate_string=paste(generate_string,sample_S,sample_A)
  
  #since the initial pair of words have been picked, loop n-2 times to randomly pick a word from b,
  #each time based on the previous two generated words and the the probabilities in T, until all words 
  #have been generated. However, when sample_A is a punctuation mark that can indicate the end of a sentence,
  #the next word sample_T should be randomly picked from b_cap, because the first word of a sentence must be 
  #capitalized at the beginning
  for (i in 1:n-2) {
    if ((sample_A=='.')||(sample_A=='?')||(sample_A=='!')) {
      sample_T<-sample(b_cap,1,prob = S_cap)
    } else {
      sample_T<-sample(b,1,prob=T[which(b==sample_S),which(b==sample_A),])
    }
    generate_string=paste(generate_string,sample_T)
    
    #update sample_S and sample_A
    sample_S<-sample_A
    sample_A<-sample_T
  }
  cat(generate_string)
}
simulate_modify(b,b_cap,S_cap,A,T,50)

