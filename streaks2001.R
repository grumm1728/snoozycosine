# import a data visualization library
library(ggplot2)

######################################################################################
### PREP

# load the csv data into R as a dataframe
oak2001 <- read.csv("~/snoozycosine/2001OAK.csv")
windicator <- c(oak2001[,6])

# substitute numeric values for the hardcoded wins and loss letters from the data
loss = 0
win = 1
windicator <- as.numeric(gsub(1,loss,windicator))
windicator <- as.numeric(gsub(2,loss,windicator))
windicator <- as.numeric(gsub(3,win,windicator))
windicator <- as.numeric(gsub(4,win,windicator))

######################################################################################
### SUM UP WINS

# this will be the main matrix
# values will be the total of wins by stretchs of length "rownumber" and column "beginning with game #"
# for example stretches[20,41] will equal the total wins in a 20 game stretch beginning with game number 41.   
# this matrix will be upper left triangular:
# as all values where (stretches > games remaining) should be NA

stretches = matrix(nrow=length(windicator), ncol=length(windicator)) # normally 162 by 162

# b = beginning with game number
# s = stretch length
b <- 1
while (b <= length(windicator)) {
  s <- 1
  while (s <= (length(windicator)-b+1)) {
    stretches[s,b] <- sum(windicator[b:(b+s-1)])
    #sprintf("Stretch of %f with %f wins",s,stretches[s,b])
    #print("ding")
    s <- s+1
  }
  b <- b+1
}

######################################################################################
### FIND COOL STUFF

bestStretch <- apply(stretches,1,max, na.rm = TRUE) # most wins in a [position] game stretch
whenBest <- apply(stretches,1,which.max) # this only gets the first time such a stretch occured

# find the best winning percentage per length
bestStretchPct <- vector(mode="double",length = length(bestStretch))
i <- 1
while(i<=length(bestStretch)){
  bestStretchPct[i] <- (bestStretch[i]*1.0)/i
  i <- i+1
}

x <- c(1:162)
plot(x,bestStretchPct)
