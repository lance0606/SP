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