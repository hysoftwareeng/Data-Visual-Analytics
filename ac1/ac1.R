log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

library(ggplot2)
library(microbenchmark)

small_ite <- 25
large_ite <- 150

fibonacci_seq <- vector(mode="numeric", length=small_ite)
log_factorial_seq <- vector(mode="numeric", length=large_ite)
sum_log_factorial_seq <- vector(mode="numeric", length=large_ite)

for (i in 1:large_ite) {
  log_factorial_seq[i] <- mean(microbenchmark::microbenchmark(log_factorial(i))$time)
  sum_log_factorial_seq[i] <- mean(microbenchmark::microbenchmark(sum_log_factorial(i))$time)
}

for (i in 1:small_ite) {
  fibonacci_seq[i] <- mean(microbenchmark::microbenchmark(fibonacci(i))$time)
}

ggplot2::qplot(x=seq(large_ite), y=log_factorial_seq, xlab="n", ylab="runtime(ns)") + ggtitle("log_factorial Runtime")

ggplot2::qplot(x=seq(large_ite), y=sum_log_factorial_seq, xlab="n", ylab="runtime(ns)") + ggtitle("sum_log_factorial Runtime")


ggplot2::qplot(x=seq(small_ite), y=fibonacci_seq, xlab="n", ylab="runtime(ns)") + ggtitle("fibonacci Runtime")


