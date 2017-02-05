######### PROBLEM 1 ##########

set.seed(424)
ratings <- rev(sort(rgamma(64, 3, 3)))
n <- 100000
winners <- vector()

tourney <- function(ratings){
  myratings <- ratings
  for(j in 1:6){
    for (i in 1:(length(myratings)/2)){
      probWin <- 1/(1+exp(-(myratings[i]-myratings[length(myratings)-i+1])))
      ifelse(rbinom(1,1,probWin) == 0 , myratings[i] <-  0, myratings[length(myratings)-i+1] <- 0)
    }
    myratings <- myratings[myratings!=0] # get rid of teams that lost
  }
  return(match(myratings,ratings)) # return the team number (1 through 64)
}

for (w in 1:n){                    # simulate 100000 times
 winners[w] <- tourney(ratings)
}

myTable <- data.frame(table(winners))
myTable$Freq <- myTable$Freq/n   # This is all to make pretty output
    #return the probabilities for all the winners
Problem1 <- data.frame(ratings,myTable$Freq)
names(Problem1)<-c("Ratings","Probability")
Problem1



######## Problem 2 ##########

getTfromGamma <- function(sampleSize1,sampleSize2,shape1=1,rate1=1,shape2=2,rate2=0.5){
  r1 <- rgamma(sampleSize1,shape1,rate1)
  r2 <- rgamma(sampleSize2,shape2,rate2)
  as.numeric(t.test(r1,r2)[3]) # returns p-value
}

#now run this 1000 times and store all p-values
n=10000
my.probs <- rep(NA,n)
for (i in 1:n){
  my.probs[i] <- getTfromGamma(7,7,15,10,15,10) # mean alpha/beta = 1.5 for both
}
mean(my.probs < 0.05) # the power is as expected right around 0.05


my.probs <- rep(NA,n)
for (i in 1:n){
  my.probs[i] <- getTfromGamma(15,3,2,10,3,10) # mean alpha/beta = 1.5 for both
}
mean(my.probs < 0.05)
#power is the number of times that you would reject the Nul Hypothesis - for these two distributions, the
#power is right around 85%


######## Problem 3 ##########
#done in SAS, the file is attached to the email.

# location on my machine: /Users/Marc/SASUniversityEdition/myfolders

######## Problem 4 ##########

my.square <- function(x) {
  x*(48-2*x)*(36-2*x)
}
optimize( f=my.square, c(-2, 20), maximum=TRUE)

  # This was my own version of optimization. It goes through the mathematical approach
  # and it gives the proper answer.
  find.square.size <- function(len1 = 48, len2 = 36){
    minSize = 0
    maxSize <- ifelse( len1 < len2 , (len1 / 2), (len2 / 2))
    x <- seq(minSize,maxSize,0.01)
    y <- vector()
    for (i in 1:length(x)){
      y[i] = x[i]*(len1 - 2*x[i])*(len2 - 2*x[i])
    }
    s <- data.frame(x,y)
    names(s) <- c("Size","Volume")
    return(s[s$Volume==max(s$Volume),]$Size)
  }
    find.square.size()

