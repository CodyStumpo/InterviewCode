# Define fibonacci(N) as the sequence using sum of last N numbers.  
# Fibonacci(2) is the classic 1, 1, 2, 3, 5, 8...
# Write function to compute Nth fibonacci(3) number
# 1, 1, 1, 3, 5, 9, 17,...

fib3 <- function(n) {
  vec=rep(1, n)
  if(n > 3) {
    for(i in 4:n) vec[i] = vec[i-1] + vec[i-2] + vec[i-3]
  }
  return(vec[n])
}
