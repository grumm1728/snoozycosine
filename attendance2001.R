# import a data visualization library
library(ggplot2)

# load the csv data into R as a dataframe
OAK2001 <- read.csv("~/snoozycosine/2001OAK.csv")

# create a vector, attnd, from the the 17th column of the dataframe
# at the same time, remove the thousand-marker commas in this column of data
attend <- as.numeric(gsub(",","",OAK2001[,17]))

# show summary statistics
summary(attend)

