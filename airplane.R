#There are n seats in an airplane, and n passengers. 
#They have all lost their tickets. 
#So they go ahead and randomly sit in a seat. 
#What is the probability of "at least" one person sitting in their correct seat?

library(combinat)

eval = function(n) {
x=permn(seq(n))
test = seq(n)
for (i in 1:length(x)) {test[i] = any(x[[i]]==seq(n))}
return(sum(test))
}

ex=8
r=sapply(seq(ex), eval)
n=sapply(seq(ex),factorial)
results=data.frame(r, n, r/n)

#f(n) = n * f(n-1) - 1^n
#f(n) / n! is the probability
t=1
i=2
while(i<20){
t=c(t, (i * tail(t,1)) - (-1)^i)
i=i+1
}
prob=t/sapply(seq(19),factorial) #converges to .6321206 (1-1/exp(1))