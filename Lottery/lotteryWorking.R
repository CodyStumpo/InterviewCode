# Purpose: Match the published probabilities of team X getting pick Y in the NBA draft.
# https://en.wikipedia.org/wiki/NBA_draft_lottery
# Given new rules, produce those probabilities too.

library(RcppAlgos)
library(dplyr)


lottery2005Rules <- c(250,199,156,119,88,63,43,28,17,11,8,7,6,5)
lottery2005prob <- lottery2005Rules / sum(lottery2005Rules) 
numTeams <- length(lottery2005Rules)


#there's only 2184 possible drafts - just assign the probability of each
allperm <- data.frame(permuteGeneral(numTeams,3))
allperm[paste0("X",4:14)] <- 0

for(i in 1:nrow(allperm)) {
  allperm[i,4:numTeams] <- setdiff(seq(numTeams),as.numeric(allperm[i,]))
}


#what is the prob of 1,2,3?  1,2,4? ....
#P(1) * P(2 without 1) * P(3 without 1 or 2)
#lottery2005prob[1] * lottery2005prob[2]/sum(lottery2005prob[-1]) * lottery2005prob[3] / sum(lottery2005prob[-c(1,2)])

allperm$prob =0

for(i in 1:nrow(allperm)){
  first=allperm[i,1]
  second=allperm[i,2]
  third=allperm[i,3]
  allperm[i,"prob"] <- lottery2005prob[first] * 
    lottery2005prob[second]/sum(lottery2005prob[-first]) * 
    lottery2005prob[third] / sum(lottery2005prob[-c(first,second)])
}


result2005 = data.frame(seed=seq(numTeams), 
                        chances=lottery2005Rules, 
                        first=0, 
                        second=0, 
                        third=0,
                        fourth=0,
                        fifth=0,
                        sixth=0,
                        seventh=0,
                        eighth=0,
                        ninth=0,
                        tenth=0,
                        eleventh=0,
                        twelfth=0,
                        thirteenth=0,
                        fourteenth=0)

for(i in 1:numTeams){
x <- allperm %>% group_by_at(i) %>% summarise(prob=sum(prob))
result2005[unlist(x[1]),i+2] <- x[2]
}

#This matches published results
source("~/Documents/Code/Personal/BLA/lottery test.R")
difference <- wiki2005-round(result2005,3)
max(abs(difference), na.rm=TRUE) == 0 #TRUE

########## OK NEW RULES

lotteryNewRules <- c(114,113,112,111,99,89,79,69,59,49,39,29,19,9,6,4)
lotteryNewprob <- lotteryNewRules / sum(lotteryNewRules) 
numTeamsNew <- length(lotteryNewRules)

# and five picks
allpermNew <- data.frame(permuteGeneral(numTeamsNew,5))  #524,160 possible drafts now.  :-(
allpermNew[paste0("X",6:16)] <- 0

#this needed to be optimized vs. the 2005 version, which would be very slow on a matrix this big. 
#still takes about a minute
nr=nrow(allpermNew)
allpermNew[1:nr,6:numTeamsNew] <- data.frame(matrix(unlist( 
  lapply(1:nr, function(n) setdiff(seq(16), allpermNew[n,])) ),
  nrow=nr, byrow=T))

#what is the prob of 1,2,3,4,5?  1,2,3,4,6? ....
#P(1) * P(2 without 1) * P(3 without 1 or 2) * P(4 without 1 or 2 or 3) * P(5 without 1 or 2 or 3 or 4)
#lottery2005prob[1] * lottery2005prob[2]/sum(lottery2005prob[-1]) * lottery2005prob[3] / sum(lottery2005prob[-c(1,2)]) *
# lottery2005prob[4] / sum(lottery2005prob[-c(1,2,3)]) * lottery2005prob[3] / sum(lottery2005prob[-c(1,2,3,4)]) *

allpermNew$prob =0

#Write result to a vector, then add it to dataframe later.  Very slow to update big dataframe in place.
#This takes ~15 seconds
prob <- allpermNew$prob
for(i in 1:nrow(allpermNew)){
  first=allpermNew[i,1]
  second=allpermNew[i,2]
  third=allpermNew[i,3]
  fourth=allpermNew[i,4]
  fifth=allpermNew[i,5]
  #allpermNew[i,"prob"]
  prob[i] <- lotteryNewprob[first] * 
    lotteryNewprob[second]/sum(lotteryNewprob[-first]) * 
    lotteryNewprob[third] / sum(lotteryNewprob[-c(first,second)]) * 
    lotteryNewprob[fourth] / sum(lotteryNewprob[-c(first,second,third)]) *
    lotteryNewprob[fifth] / sum(lotteryNewprob[-c(first,second,third,fourth)]) 
}

allpermNew$prob <- prob

resultNew = data.frame(seed=seq(numTeamsNew), 
                        chances=lotteryNewRules, 
                        first=0, 
                        second=0, 
                        third=0,
                        fourth=0,
                        fifth=0,
                        sixth=0,
                        seventh=0,
                        eighth=0,
                        ninth=0,
                        tenth=0,
                        eleventh=0,
                        twelfth=0,
                        thirteenth=0,
                        fourteenth=0,
                        fifteenth=0,
                        sixteenth=0)

for(i in 1:numTeamsNew){
  x <- allpermNew %>% group_by_at(i) %>% summarise(prob=sum(prob))
  resultNew[unlist(x[1]),i+2] <- x[2]
}

write.csv(resultNew, "~/Documents/Code/Personal/BLA/NewLotteryResults.csv",row.names = FALSE)