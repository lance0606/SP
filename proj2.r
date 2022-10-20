#Content of strategy1-Each prisoner starts from the box with the same number 
#as himself, and then opens the next box based on the card number in that box.

#Loop in turn until the prisoner finds a card with the same number as himself 
#or ends after looping n times.


##function "strategy1" is the code implementation of strategy1.

# The input to this function is:

#n: The prisoner can open the box at most n times, and the number of prisoners, 
#boxes and cards is 2n.

#k: Prisoner's number.

#box: distribution of 2n cards randomly assigned in 2n boxes.

##The output of this function is:

# Whether the prisoner found the correct card, if found, return the number "1", 
#otherwise return the number "0".

##Variable explanation in function:

#"found" indicates whether the prisoner found the correct card, the initial value 
#of 0 indicates that it was not found.

#"count" is used to count the number of times the prisoner opens the box, 
#the initial value is 1.

#"card_num" indicates the card number in the box opened each time, if card_num==k, 
#it means the prisoner finds a card with the same number as himself.

strategy1<-function(n,k,box){
  found<-0        
  count<-1        
  card_num<-box[k]        
  while (count<=n) {
    if (card_num==k) {
      found<-1
      break
    } else {
      card_num<-box[card_num]
    }
    count<-count+1
  }
  return(found)
}

################################################################################

#Content of strategy2: Each prisoner starts with a randomly numbered box 
#and then opens the next box based on the number of cards in that box.

#Loop in turn until the prisoner finds a card with the same number as himself 
#or ends after looping n times.

##function "strategy2" is the code implementation of strategy2.

# The input to this function is:

#n: The prisoner can open the box at most n times, and the number of prisoners, 
#boxes and cards is 2n.

#k: Prisoner's number.

#box: distribution of 2n cards randomly assigned in 2n boxes.

#The output of this function is:

# Whether the prisoner found the correct card, if found, return the number "1", 
#otherwise return the number "0".

#Explanation of variables in the function:

#found indicates whether the prisoner found the correct card, the initial value 
#of 0 indicates that it was not found.

#count is used to count the number of times the prisoner opens the box, 
#the initial value is 1.

#"random" means starting from a randomly selected box.

#"card_num" indicates the card number in the box opened each time, if card_num==k,
#it means the prisoner finds a card with the same number as himself.

strategy2<-function(n,k,box){
  found<-0
  count<-1
  random<-sample(1:(2*n),1)
  card_num<-box[random]
  while (count<=n) {
    if (card_num==k) {
      found<-1
      break
    } else {
      card_num<-box[card_num]
    }
    count<-count+1
  }
  return(found)
}

################################################################################

#Content of strategy3: Prisoners open n boxes at random and check if they 
#have their own numbered cards in them.

##function "strategy3" is the code implementation of strategy3.

#The input of this function is:

#n: The prisoner can open the box at most n times, and the number of prisoners, 
#boxes and cards is 2n.

#k: Prisoner's number.

#box: distribution of 2n cards randomly assigned in 2n boxes.

#The output of this function is:

# Whether the prisoner found the correct card, if found, return the number "1", 
#otherwise return the number "0".

#Explanation of variables in the function:

#"found“ indicates whether the prisoner found the correct card, the initial value 
#of 0 indicates that it was not found.

#"random" means to open n boxes randomly.

strategy3<-function(n,k,box){
  found<-0
  random<-sample(1:(2*n),n)
  if (k %in% box[random]){
    found<-1
  }
  return(found)
}
################################################################################

##Pone simulates 10,000 times the process of a given prisoner trying to find 
#the same card number as his own through three strategies.

##The input of this function:

#n: The prisoner can open the box at most n times, and the number of prisoners, 
#boxes and cards is 2n.

#k: Prisoner's number.

#strategy: The strategy used by the prisoner, the value can only be '1', '2', '3'.

#nreps: Number of simulations (default 10000).

##The output of this function:

#return the probability estimate.

##Variable explanation in function:

#"found" indicates whether the prisoner found the correct card, the initial value 
#of 0 indicates that it was not found.

#"boxes" means 2n cards are randomly distributed in 2n boxes.

#"prob" represents the frequency of successful discovery across all simulations.

Pone<-function(n,k,strategy,nreps=10000){
  found<-0
  for (i in 1:nreps){
    boxes<-sample(1:(2*n),2*n)
    
    #since the return value of strategy1/2/3 is 1 or 0,
    #it's now easy to calculate 'found' with simple addition
    if(strategy==1){
      found<-found+strategy1(n,k,boxes)
    }else if(strategy==2){
      found<-found+strategy2(n,k,boxes)
    }else if(strategy==3){
      found<-found+strategy3(n,k,boxes)
    }
  }
  prob<-found/nreps
  return(prob)
}

################################################################################


##Pall simulates the process of 10,000 times that all prisoners try to find 
#the same card number as themselves through three strategies in turn.

#The input of this function is:

#n: The prisoner can open the box at most n times, and the number of prisoners, 
#boxes and cards is 2n.

#strategy: The strategy used by the prisoner, the value can only be '1', '2', '3'.

#nreps: Number of simulations (default 10000).

##The output of this function is:

#Return a vector that records the results of each simulation 
#( the number of all prisoners that were successfully found).

##Variable explanation in function:

#"result" is a vector of length nreps, each element represents the result of the 
#current simulation.

#"boxes" means 2n cards are randomly distributed in 2n boxes.

#"prisoner" is the number of all prisoners.

#"found" indicates whether the prisoner found the correct card, the initial value 
#of 0 indicates that it was not found.

Pall<-function(n,strategy,nreps=10000){
  result<-rep(0,nreps)
  for (i in 1:nreps) {
    boxes<-sample(1:(2*n),2*n)
    prisoner<-1:(2*n)
    found<-0
    
    #since the return value of strategy1/2/3 is 1 or 0,
    #it's now easy to calculate 'found' with simple addition
    for (p in prisoner) {
      if(strategy==1){
        found<-found+strategy1(n,p,boxes)
      }else if(strategy==2){
        found<-found+strategy2(n,p,boxes)
      }else if(strategy==3){
        found<-found+strategy3(n,p,boxes)
      }
    }
    result[i]<-found
  }
  return(result)
}

################################################################################

##examples to show the results when n and strategy change.

#First simulate the individual probability when n=5 and 50.

sprintf('When n=5, the individual probability for choosing strategy1 is %f',Pone(5,10,1))
sprintf('When n=5, the individual probability for choosing strategy2 is %f',Pone(5,10,2))
sprintf('When n=5, the individual probability for choosing strategy3 is %f',Pone(5,10,3))
sprintf('When n=50, the individual probability for choosing strategy1 is %f',Pone(50,10,1))
sprintf('When n=50, the individual probability for choosing strategy2 is %f',Pone(50,10,2))
sprintf('When n=50, the individual probability for choosing strategy3 is %f',Pone(50,10,3))

#Then simulate the joint success probability distribution when n=5.

hist(Pall(5,1),col='lightgrey',main='Probability distribution when n=5 and choose strategy1',
     freq=F,breaks=10,xlab='numbers of prisoners who succeed in finding their number')
hist(Pall(5,2),col='lightgrey',main='Probability distribution when n=5 and choose strategy2',
     freq=F,breaks=10,xlab='numbers of prisoners who succeed in finding their number')
hist(Pall(5,3),col='lightgrey',main='Probability distribution when n=5 and choose strategy3',
     freq=F,breaks=10,xlab='numbers of prisoners who succeed in finding their number')

#simulate the joint success probability distribution when n=50.

hist(Pall(50,1),col='lightgrey',main='Probability distribution when n=50 and choose strategy1',
     xlab='numbers of prisoners who succeed in finding their number')
hist(Pall(50,2),col='lightgrey',main='Probability distribution when n=50 and choose strategy2',
     xlab='numbers of prisoners who succeed in finding their number')
hist(Pall(50,3),col='lightgrey',main='Probability distribution when n=50 and choose strategy3',
     xlab='numbers of prisoners who succeed in finding their number')

################################################################################

##Remarks about the surprising results:

#According to the results above, we can find that the individual probability for
#choosing 3 different strategies are close to be same, but the joint success
#probability distribution for choosing 3 strategies are very different. When
#prisoners choose strategy1, there are about 30% probability that they can all
#freed, while there is no chance for choosing strategy 2 and 3. 

################################################################################

##find_one_loop is used to find the loop starting with a certain number in the box.

##The input of the function:

#k: start number of loop.

#box: Represents the distribution of 2n cards randomly assigned in 2n boxes.

##The output of this function:

#Return a vector representing the found loop.

##Variable explanation in the function:

#"temp_k": Temporary variable for loop, initial assignment is k.

#"loop:" record the found loop.

find_one_loop<-function(k,box){
  temp_k<-k
  loop<-c(k)
  while(box[temp_k]!=k){
    temp_k<-box[temp_k]
    loop<-append(loop,temp_k)
  }
  return(loop)
}

################################################################################

##find_all_loop is used to find all loops that exist in the box.

##The input of this function:

#n: The number of prisoners, boxes and cards is 2n.

#box: Represents the distribution of 2n cards randomly assigned in 2n boxes.

##The output of this function:

#Return a vector that records the unique length of all loops that exist in the box.

##Variable explanation in the function:

#all_loop: record all loops, initially empty.

#idx: Indicates the number of the box.

find_all_loop<-function(n,box){
  all_loop<-c()
  idx<-1:(2*n)
  
  #Randomly select one of the remaining numbers to start to find a loop, 
  #and select a loop from all the numbers.
  
  #delete the number in the loop.
  
  while (length(idx)!=0) {
    k<-sample(idx,1)
    loop<-find_one_loop(k,box)
    all_loop<-append(all_loop,length(loop))
    idx<-idx[!idx %in% loop]
  }
  return(unique(all_loop))
}

################################################################################

##"dloop" is used to estimate the probability of each loop length from 1 to 2n 
#occurring at least once in a random shuffling of cards to boxes.

##The input of this function:

#n: The number of prisoners, boxes and cards is 2n.

#nreps: Number of simulations (default 10000).

##The output of this function:

#return a 2n-vector of probabilities.

##Variable explanation in the function:

#"prob": a vector of length 2n, counting the length counts of all loops 
#found in each simulation.

#"all_loop": record the unique length of all loops in each simulation.

dloop<-function(n,nreps=10000){
  prob<-rep(0,2*n)
  for (i in 1:nreps) {
    u<-sample(1:(2*n),2*n)
    all_loop<-find_all_loop(n,u)
    for (loop in all_loop) {
      prob[loop]<-prob[loop]+1
    }
  }
  prob<-prob/nreps
  return(prob)
}

################################################################################
#"dloop_plus" is used to count the length of the longest loop in each simulation; 
#in all simulations, count the number of occurrences of each longest loop, 
#and finally calculate the probability of each longest loop.

##The input of this function:

#n: The number of prisoners, boxes and cards is 2n.

#nreps: Number of simulations (default 10000).

##The output of this function:

#return a 2n-vector of probabilities.

##Variable explanation in the function:

#"prob": a vector of length 2n, count the maximum length of all loops found 
#in each simulation and count.

#"all_loop": record the unique length of all loops in each simulation.

dloop_plus<-function(n,nreps=10000){
  prob<-rep(0,2*n)
  for (i in 1:nreps) {
    u<-sample(1:(2*n),2*n)
    all_loop<-find_all_loop(n,u)
    
    prob[max(all_loop)]<-prob[max(all_loop)]+1
  }
  prob<-prob/nreps
  return(prob)
}

################################################################################

##probabilities visualization using dloop for n=50
prob<-dloop(50)
barplot(prob,
        main='The probability distribution of each loop length from 1 to 2n 
        occurring at least once in a random shuffling of cards to boxes',
        xlab='100 prisoners',ylab='Probabilities')

##probabilities visualization using dloop for n=50
prob1<-dloop_plus(50)
barplot(prob1,
        main='The probability distribution of each longest loop occuring 
        in a random shuffling of cards to boxes',
        xlab='100 prisoners',ylab='Probabilities')
sprintf('The probability that there is no loop longer than 50 is %f',sum(prob1[1:50]))