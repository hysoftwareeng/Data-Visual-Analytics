#hyang390
#903320189

########### Q2. Log Gamma (Loop) ###############
log_gamma_loop = function(n) {
  result = 0
  while (n > 1) {
    result = result + log(n-1)
    n = n - 1
  }
  return(result)
}

########### Q3. Log Gamma (Recursive) ###############
log_gamma_recursive = function(n) {
  if (n <= 2) {
    return(log(1))
  }
  return(log(n-1) + log_gamma_recursive(n-1))
}

########### Q4. Sum of Log Gamma ###############
sum_log_gamma_loop = function(n) {
  return (sum(sapply(seq(n), log_gamma_loop)))
}

sum_log_gamma_recursive = function(n) {
  return (sum(sapply(seq(n), log_gamma_recursive)))
}

########### Q5. Compare Results to Built-In R Function ###############
sum_lgamma = function(n) {
  return (sum(sapply(seq(n), lgamma)))
}


options(expressions=500000)
#install.packages("ggplot2")
#install.packages("reshape")
library(ggplot2)
library(reshape)
n_vals = seq(100, 4500, by=100)
n_vals_len = length(n_vals)

times_df = data.frame(sum_log_gamma_loop = numeric(n_vals_len),
                      sum_log_gamma_recursive = vector(mode="numeric", length=n_vals_len),
                      sum_lgamma = vector(mode="numeric", length=n_vals_len),
                      n_val = seq(n_vals_len))

for (i in 1:n_vals_len) {
  times_df$sum_log_gamma_loop[i] = system.time(sum_log_gamma_loop(n_vals[i]))[1]
  times_df$sum_log_gamma_recursive[i] = system.time(sum_log_gamma_recursive(n_vals[i]))[1]
  times_df$sum_lgamma[i] = system.time(sum_lgamma(n_vals[i]))[1]
}

times_results = melt(times_df, id.vars="n_val")

plot = ggplot(times_results, aes(x=n_val,y=value,color=variable)) + geom_line()
plot + ggtitle("Runtime Comparisons for Log Gamma Functions") + xlab("n_values (x100)") + ylab("runtime(s)")



