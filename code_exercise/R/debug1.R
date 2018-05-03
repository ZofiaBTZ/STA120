# This is not a working code. 
# For debugging purpose only.
# STA 120, UZH, FS2018

rm(list = ls())
library(readr)
water <- read_csv("../Data/water_transfer.csv")
 
perm_test <- function(setA, setB) {
  n = 1000
  T = median(setA) - median(setB)  #computing the test statisctics
  set_whole <- c(setA, setB) # all data 
  Tsim <- array (0,n) #making an array from 0 to 1000
  for (i in 1:n) {
    len = length(set_whole)
    lenA = length(setA)
    index <- sample(1:len, lenA, replace = F) #assigne each value of pd randomly to either xA or xB
    medianA1 <- median(set_whole[index]) #median of the data with rows  in index
    medianA2 <- median(set_whole[-index]) #median of the data with rows NOT in index
    Tsim[i] <- medianA1 - medianA2
  }
  return (sum(abs(Tsim)) >= abs(T))/n
}

at_term <- water[water$age == "At term",1]
weeks_12_26 <- water[water$age == "12-26 Weeks",1]
p_value <- perm_test (at_term, weeks_12_26)
print(p_value)