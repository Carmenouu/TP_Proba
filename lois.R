LoiBinomiale <- function(n, p)
{
  u <- runif(1)
  somme <- 0
  k <- -1
  
  while (u > somme)
  {
    k <- k + 1
    p_k <- choose(n, k) * p^k * (1-p)^(n-k)
    somme <- somme + p_k
  }
  
  return (k)
}