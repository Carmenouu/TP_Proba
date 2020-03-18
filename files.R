FileMM1 <- function(lambda, mu, D)
{
  arrivees <- c()
  departs <- c()
  
  date_arrivee_prec <- 0
  date_depart_prec <- 0
  
  temps_attente_total <- 0
  
  while (TRUE)
  {
    attente_arrivee <- rexp(1, lambda)
    
    date_arrivee <- date_arrivee_prec + attente_arrivee
    
    if (date_arrivee > D)
    {
      break  
    }
    
    arrivees <- c(arrivees, date_arrivee)
    date_arrivee_prec <- date_arrivee
  }
  
  i <- 1
  
  while (i <= length(arrivees))
  {
    attente_depart <- rexp(1, mu)
    
    
    if (date_depart_prec < arrivees[i])
    {
      date_depart <- arrivees[i] + attente_depart
    }
    else
    {
      date_depart <- date_depart_prec + attente_depart
    }
    
    if (date_depart > D)
    {
      break  
    }
    
    departs <- c(departs, date_depart)
    
    date_depart_prec <- date_depart
    temps_attente_total <- temps_attente_total + (departs[i] - arrivees[i])
    
    i <- i + 1
  }
  
  while (i <= length(arrivees))
  {
    temps_attente_total <- temps_attente_total + (D - arrivees[i])
    i <- i + 1
  }
  
  return(list(arrivees=arrivees, departs=departs, temps_attente_moyen=temps_attente_total / length(arrivees)))
}

EvolutionFileMM1 <- function(arrivees, departs, duree_totale)
{
  dates <- c(0)
  durees <- c()
  nombres <- c(0)
  i_arrivees <- 1
  i_departs <- 1
  len_arrivees <- length(arrivees)
  len_departs <- length(departs)
  
  while (i_arrivees <= len_arrivees || i_departs <= len_departs)
  {
    if (i_departs > len_departs || (i_arrivees <= len_arrivees && arrivees[i_arrivees] <= departs[i_departs]))
    {
      duree <- arrivees[i_arrivees] - dates[length(dates)]
      durees <- c(durees, duree)
      dates <- c(dates, arrivees[i_arrivees])
      nombres <- c(nombres, nombres[length(nombres)] + 1)
      i_arrivees <- i_arrivees + 1
    }
    else if (i_arrivees > len_arrivees || (i_departs <= len_departs && departs[i_departs] <= arrivees[i_arrivees]))
    {
      duree <- departs[i_departs] - dates[length(dates)]
      durees <- c(durees, duree)
      dates <- c(dates, departs[i_departs])
      nombres <- c(nombres, nombres[length(nombres)] - 1)
      i_departs <- i_departs + 1
    }
  }
  
  durees <- c(durees, duree_totale - dates[length(dates)])
  return(list(dates=dates, nombres=nombres, durees=durees))
}

MoyenneTheoriqueN <- function(taux_arrivees, taux_departs)
{
  alpha <- taux_arrivees / taux_departs
  return (alpha / (1 - alpha))
}

MoyenneTheoriqueW <- function(taux_arrivees, taux_departs, lambda)
{
  # Application de la formule de Little : E(N) = E(W)*lambda
  return (MoyenneTheoriqueN(taux_arrivees, taux_departs) / lambda)
}


