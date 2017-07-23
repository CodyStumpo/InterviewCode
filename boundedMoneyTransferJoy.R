#Imagine a room full of 100 people with 100 dollars each. 
#With every tick of the clock, every person with money gives a dollar to one randomly chosen other person. 
#After some time progresses, how will the money be distributed?

starting=rep(100,100)
results = data.frame(sim100=starting, 
                     sim1000=starting,
                     sim10000=starting,
                     sim100000=starting)

dosim=function(n, starting){
sim=0
while(sim<=n){
moneyholders=which(starting>0)
target=sample(1:100, length(moneyholders), replace=T)
starting[moneyholders]=starting[moneyholders]-1
starting=starting+tabulate(target, nbins=100)
sim=sim+1
}
return(starting)
}


results$sim100=dosim(100,starting)
results$sim1000=dosim(900,results$sim100)
results$sim10000=dosim(9000,results$sim1000)
results$sim100000=dosim(90000,results$sim10000)


library(ggplot2)
library(ggjoy)
#need to mutate to tall dataframe before using ggjoy
library(tidyr)
resultsT <- gather(results, numsims, money)
ggplot(resultsT, aes(x = money, y = numsims)) + geom_joy()

#observe increasingly exponential distribution over time
