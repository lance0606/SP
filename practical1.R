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


