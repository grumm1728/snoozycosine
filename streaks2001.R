# import a data visualization library
library(ggplot2)

######################################################################################
### PREP

# load the csv data into R as a dataframe
oak2001 <- read.csv("~/snoozycosine/2001OAK.csv")
oak2002 <- read.csv("~/snoozycosine/2002OAK.csv")
windicator2001 <- c(oak2001[,6])
windicator2002 <- c(oak2002[,7]) #eww I edited the 2001 csv first :( so the column numbers are different

# substitute numeric values for the hardcoded wins and loss letters from the data
loss = 0
win = 1
windicator2001 <- as.numeric(gsub(1,loss,windicator2001))
windicator2001 <- as.numeric(gsub(2,loss,windicator2001))
windicator2001 <- as.numeric(gsub(3,win,windicator2001))
windicator2001 <- as.numeric(gsub(4,win,windicator2001))

windicator2002 <- as.numeric(gsub(1,loss,windicator2002))
windicator2002 <- as.numeric(gsub(2,loss,windicator2002))
windicator2002 <- as.numeric(gsub(3,win,windicator2002))
windicator2002 <- as.numeric(gsub(4,win,windicator2002))

######################################################################################
### SUM UP WINS

# this will be the main matrix
# values will be the total of wins by stretchs of length "rownumber" and column "beginning with game #"
# for example stretches2001[20,41] will equal the total wins in a 20 game stretch beginning with game number 41.   
# this matrix will be upper left triangular:
# as all values where (stretches2001 > games remaining) should be NA

stretches2001 = matrix(nrow=length(windicator2001), ncol=length(windicator2001)) # normally 162 by 162
stretches2002 = matrix(nrow=length(windicator2001), ncol=length(windicator2001)) # normally 162 by 162

# b = beginning with game number
# s = stretch length
b <- 1
while (b <= length(windicator2001)) {
  s <- 1
  while (s <= (length(windicator2001)-b+1)) {
    winT <- sum(windicator2001[b:(b+s-1)])
    stretches2001[s,b] <- winT
    
    #sprintf("Stretch of %f with %f wins",s,stretches2001[s,b])
    #print("ding")
    s <- s+1
  }
  b <- b+1
}

b <- 1
while (b <= length(windicator2002)) {
  s <- 1
  while (s <= (length(windicator2002)-b+1)) {
    stretches2002[s,b] <- sum(windicator2002[b:(b+s-1)])
    #sprintf("Stretch of %f with %f wins",s,stretches2001[s,b])
    #print("ding")
    s <- s+1
  }
  b <- b+1
}

######################################################################################
### FIND COOL STUFF

bestStretch2001 <- apply(stretches2001,1,max, na.rm = TRUE) # most wins in a [position] game stretch
bestStretch2002 <- apply(stretches2002,1,max, na.rm = TRUE) # most wins in a [position] game stretch
#whenBest <- apply(stretches2001,1,which.max) # this only gets the first time such a stretch occured


###########  Plots of wins in a stretch vs. win% in a stretch
allStretches2001 <- c(rep(1,162))
i <- 1
while (i<=161) {
  i <- i+1
  allStretches2001 <- c(allStretches2001,c(rep(i,162)))
}
allWins2001 <- as.vector(t(stretches2001))
plot(allStretches2001,allWins2001)
plot(allWins2001,allWins2001/allStretches2001)
plot(allStretches2001,allWins2001/allStretches2001)

allStretches2002 <- c(rep(1,162))
i <- 1
while (i<=161) {
  i <- i+1
  allStretches2002 <- c(allStretches2002,c(rep(i,162)))
}
allWins2002 <- as.vector(t(stretches2002))
plot(allStretches2002,allWins2002)
plot(allWins2002,allWins2002/allStretches2002)
plot(allStretches2002,allWins2002/allStretches2002)
########################################################


# find the best winning percentage per length
bestStretch2001Pct <- vector(mode="double",length = length(bestStretch2001))
i <- 1
while(i<=length(bestStretch2001)){
  bestStretch2001Pct[i] <- (bestStretch2001[i]*1.0)/i
  i <- i+1
}

bestStretch2002Pct <- vector(mode="double",length = length(bestStretch2002))
i <- 1
while(i<=length(bestStretch2002)){
  bestStretch2002Pct[i] <- (bestStretch2002[i]*1.0)/i
  i <- i+1
}

x <- c(1:162)
plot(x,bestStretch2001Pct)
plot(x,bestStretch2002Pct)
#plot(bestStretch2001Pct,bestStretch2002Pct) # what is this even
