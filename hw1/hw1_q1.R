a = 1:10000000
custom_add = function(n) {
  result = 0
  for (i in seq_along(n)) {
    result = result + (n[i]^3 + log(n[i]))
  }
  return(result)
}

custom_add(a)
sum(a^3 + log(a))

a = 1:1000000;
system.time(custom_add(a))
system.time(sum(a^3 + log(a)))
