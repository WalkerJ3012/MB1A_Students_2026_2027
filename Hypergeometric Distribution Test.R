hyp <- function(n, k, N, K){
  prob = (choose(K,k) * choose(N-K, n-k))/(choose(N,n))
  return(prob)
}

hypd <- function(n, N, K){
  k_values <- 0:n
  p_values <- c()
  for (i in k_values){
    p_values <- append(p_values, c(hyp(n, i, N, K)))
  }
  barplot(p_values, names.arg = k_values)
}

hypd(17, 318, 97)