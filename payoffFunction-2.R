profit <-function(classSize,oddsMoney,numberOfRepeats)
  # classSize:  number of students in the class
  # oddsMoney:  How much Brownlow pays to a $1 bet if he loses
  # numberOfRepeats: number of times the bet was made to a class
  {
  
  days=1:365 #days of the year 1 = Jan 1, 365 = Dec 31
  brownlowBank = 0 # start off even
  
  for (i in 1:numberOfRepeats) {
    classSample=sample(days,size=classSize,replace = T)
    d=duplicated(classSample)
    if (sum(d)>0) brownlowBank=brownlowBank+1 #brownlow wins $1
    else brownlowBank = brownlowBank - oddsMoney #brownlow loses, payoff
  }
  return(brownlowBank)
}
