"ant walks randomly on a cube. How many moves does it take on average to get to the opposite corner"

simlength=1000000
results=rep(0,simlength)

for(i in 1:simlength){
position=3
count=0

while (position !=0){
  draw=sample(4:6, 1) #random number 4,5, or 6
  position=switch(position, if(draw==4) 0 else 2, if(draw==4) 3 else 1, 2 )  
  # if you're at 1, go to 0,2,or2.  If you're at 2, go to 3,1,or 1.  If you're at 3, go to 2
  count=count+1  
  }

results[i]=count
}

avg=mean(results)
hist(results, breaks=seq(2, max(results),by=1), freq=FALSE)

theoryx=seq(3, max(results), by=2)
theorycdf=theoryx
theorycdf[1]=2/9
theorypdf=theorycdf
for(i in 2:length(theoryx)) {
  theorypdf[i]=(1-(theorycdf[i-1]))*2/9;
  theorycdf[i]=sum(theorypdf[1:i])}
theoryavg=sum(theoryx*theorypdf)
plot(theoryx, theorypdf)

errorx=seq(1,6); errorx=10^errorx
avgerror=errorx
for(i in 1:length(errorx)) avgerror[i]=(mean(results[1:errorx[i]])-theoryavg)/theoryavg
plot(errorx, avgerror)

matrix(c(errorx, avgerror), ncol=2)

error9=avgerror
for(i in 1:length(errorx)) error9[i]=((length(which(results[1:errorx[i]]==19))/errorx[i])-theorypdf[9])/theorypdf[9]

error18=avgerror
for(i in 1:length(errorx)) error18[i]=((length(which(results[1:errorx[i]]==37))/errorx[i])-theorypdf[18])/theorypdf[18]

matrix(c(errorx, avgerror, error9, error18), ncol=4)

theorypdf2=dgeom(seq(0,length(theoryx)-1), 2/9)
#theorypdf2[1]=2/9; for(i in 2:length(theorypdf2)) theorypdf2[i]=theorypdf2[i-1]*7/9
#for(i in 1:length(theoryx)) theorypdf2[i]=(2*7^((theoryx[i]-3)/2))/(3^(theoryx[i]-1))



"
Just for the sake of closure, it is the Geometric Distribution. 
After 3 moves, there’s a 2/9 chance of being at the goal.
After 4 moves, the ant has to be either 3 or 1 away from the goal (true for every even #)
After 5 moves, the ant has to be either 2 or 0 away from the goal (true for every odd #)
And if you are 2 away on a certain odd #, there is a 2/9 chance the ant will move directly to the goal on the next two moves, and a 7/9 chance it does not (but instead returns to being 2 away).
So the probability of getting to the goal in exactly a particular odd number of moves is 7/9th the probability of the preceding odd number.
"