primeCounter <- function(input) {
maxTest=floor(sqrt(input)) #nothing bigger needs to be checked
test=seq(from = 2,to = input)
for (i in 2:maxTest) #going to do sieve. Remove all multiples of 2, then all multiples of 3,...
  {if (i %in% test) #by the time we get to testing 4, all multiples will already be removed (mult of 2) so don't test
    {
     test = test[test%%i != 0]; #keep only anything that's not a multiple of i
     test=append(test,i,1) # put i itself back in
    }
  }
return(length(test))
}
