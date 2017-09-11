# If the Cleveland Indians win 15 in a row, somewhere in their last 100 games, local window company rebates all July customers (about $2M)
# They want to buy insurance to protect against the payout - what should the premium be?
# Actual contract was for $75k

stretch=100
numTrials = 100000
winProb = 0.61 #the team's winning percentage at the time

flips = matrix(nrow=numTrials,ncol=stretch)
  
for(i in 1:numTrials)  flips[i,] = rbinom(stretch,1,winProb)

maxInARow = rep(0,numTrials)
for (i in 1:numTrials) {
  raw=rle(flips[i,])
  maxInARow[i]=max(raw$lengths[which(raw$values==1)])
}

fairPrice = 2e6 * sum(maxInARow > 14)/numTrials  # about $40k 
riskLoad = 2e6 * sd(maxInARow > 14)/numTrials 
