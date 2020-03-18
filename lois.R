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

FonctionRepartitionBonus <- function(x)
{
  if (x < 0 || x > 1)
  {
    return (0)
  }
  else
  {
    return ( (2*log(1 + x)) / ((log(2)^2) * (1+x)) )
  }
}

LoiBonusInversion <- function()
{
  u <- runif(1)
  return (exp( sqrt(u)*log(2) ) - 1)
}

LoiBonusRejet <- function()
{
  c <- 2 / (log(2)^2)
  while (TRUE)
  {
    y <- runif(1)
    u <- runif(1)
    
    if (u <= FonctionRepartitionBonus(y) / c)
      return (y)
  }
  
}