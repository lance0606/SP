#3
setwd("put/your/local/repo/location/here")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers 

#4
split_punct<-function(a){
  ia<- grep("[[:punct:]]",a,fixed=FALSE) # which element of a has punctuation
  xia<- rep("",length(ia)+length(a)) # vector to store the elements without punctuation
  iis <- ia+1:length(ia) #where should punctuation go
  xs<-c()
  xs[iis]<- substr(a[ia],nchar(a[ia]),nchar(a[ia]))
  xs[-iis]<- gsub("[[:punct:]]","",a) #without punctuation
  return(xs)
}

#5
a<-split_punct(a)#separate the punctuation marks

#6(a)
a<-tolower(a)#replaced the capital letters in words with lower case letters
uniq<-unique(a)#find the vector of unique words in a

#6(b)
#find the vector of indices indicating which element 
#in the unique word vector
idx_vector<-match(a,uniq)

#6(c)
count<-tabulate(idx_vector)#count up how many time each unique word occurs in the text

#6(d)(e)
m<-500#decide the threshold number of occurrence
idx_m<-order(count,decreasing = TRUE)[1:m]#sort the index of occurrence
b<-c()#create a vector b
#b contains m(500) most commonly occurring words
for (i in idx_m) { b<-append(b,uniq[i])}

#7a
idx_common<-match(a,b)#first column

#7b
secondc<-append(idx_common[2:length(idx_common)],NA)#second column
thirdc<-append(secondc[2:length(secondc)],NA)#third column
mat<-cbind(idx_common,secondc,thirdc)#form a matrix

#7c
triplet_all<-rowSums(is.na(mat))
idx_triplet_com<-which(triplet_all==0)#identify the common word triplets
triplet_com<-mat[idx_triplet_com,]#drop the other word triplets those that contain an NA

#7d
T<-array(0,dim=c(m,m,m))#initialize matrix T with 500x500x500
#loop through the common word triplets adding a 1 to T[i,k,j] every time the jth common word follows the pair i,k
for (i in 1:nrow(triplet_com)) {
  T[triplet_com[i,1],triplet_com[i,2],triplet_com[i,3]]<-T[triplet_com[i,1],triplet_com[i,2],triplet_com[i,3]]+1
}


#7f
A<-array(0,dim=c(m,m))#initialize matrix A with 500x500
#loop through the common word triplets adding a 1 to A[i,k] every time the kth common word follows the word i
for (i in 1:nrow(triplet_com)) {
  A[triplet_com[i,1],triplet_com[i,2]]<-A[triplet_com[i,1],triplet_com[i,2]]+1
}

S<-rep(0,m)##initialize vector s with 1x500
#loop through the common word triplets adding a 1 to S[i] every time the ith common word occur
for (i in 1:nrow(triplet_com)) {
  S[triplet_com[i,1]]<-S[triplet_com[i,1]]+1
}
