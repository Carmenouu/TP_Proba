Frequency <- function(x, nb) 
{
  S <- 0
  
  for (i in 1:length(x))
  {
    bin <- binary(x[i])[(32-nb+1):32]
    bin <- replace(bin, bin == 0, -1)
    S <- S + sum(bin)
  }
  
  Sobs <- abs(S) / sqrt(nb*length(x))
  return(2 * (1 - pnorm(Sobs)))
}

Runs <- function(x, nb)
{
  S <- 0
  n <- (nb * length(x))
  for (i in 1:length(x))
  {
    bin <- binary(x[i])[(32-nb+1):32]
    S <- S + sum(bin)
  }
  
  pi <- S / n
  
  if (abs(pi - 0.5) >= 2 / sqrt(n)) {
    return(0.0)
  }
  
  S <- 1
  seq_giga <- c()
  for (i in 1:length(x))
  {
    bin <- binary(x[i])[(32-nb+1):32]
    seq_giga <- c(seq_giga, bin)
  }
  
  for (i in 2:n)
  {
    if (seq_giga[i] != seq_giga[i-1]) 
    {
      S <- S + 1
    }
  }
  
  return(2 * (1 - pnorm( abs(S-2*n*pi*(1-pi)) / (2*sqrt(n)*pi*(1-pi) ))))
}

PercentageTest <- function(frequence, min) {
  return(sum(sapply(frequence, function(x) {
    if (x > min) {
      return(1)
    } else {
      return(0)
    }
  })))
}