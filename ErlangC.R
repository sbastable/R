#' ErlangC Library
#'
#' @param m - the number of agents (integer greater than 0)
#' @param u - the traffic intensity (unitless, given as average handling time (AHT) in time units multiplied by number of calls per same time unit, typically both in seconds)
#'
#' @return returns the probability as a decimal between 0 and 1 of an individual call waiting
#' @export
#'
#' @examples ErlangC_Base(20,(500*(600/3600))) where 0 is the number of agents, 500 is the AHT in seconds, 600 is the number of calls forecast in one hour and 3600 is the number of seconds in one hour.
ErlangC_Base <- function(m,u)
{
  # Calculates base Erlang C probability of waiting
  p <- u/m # set var p to be u/m, which is known as occupancy or utilisation in common call centre terminology
  # sum of u^k / fact(k) over set 0 < k < m - iterative over sum
  xtoy_overyfact <- function(x,y) {return((x ** y)/factorial(y))}
  ssum <- sum(xtoy_overyfact(u,0:(m-1)))
  numerator <- xtoy_overyfact(u,m)
  denominator <- (numerator) + ((1-p)*ssum)
  return (numerator/denominator)
}


print(ErlangC_Base(55,48))

ErlangC_ASA <- function(m,u,t) # m=number of agents, u=traffic intensity, t=AHT time in seconds
{
  p <- u/m # set var p to be u/m, which is known as occupancy or utilisation in common call centre terminology
  numerator <- ErlangC_Base(m,u) * t
  denominator <- m * (1-p)
  return(numerator/denominator)
}

print(ErlangC_ASA(55,48,240))


ErlangC_ServiceLevel <- function(m,u,t,st) # m=number of agents, u=traffic intensity, t=AHT time in seconds, st=desired answer within threshold (secs)
{
 exponent <- -(m-u) * (st/t)
 invsl <- (ErlangC_Base(55,48) * exp(exponent)) # exp() = e^()
 return(1-invsl)
}
print(ErlangC_ServiceLevel(55,48,240,15))



ErlangC_GetRequiredAgents <- function(u,st,t,sl) # u is traffic intensity, st is desired service time in seconds, t=AHT time in seconds, sl is desired service level as decimal
{
  continue <- TRUE
  m <- floor(u) + 1 #initialise m (number of agents) set at integer intensity+1
  
  while(continue)
  {
    
    
    expectedsl <- ErlangC_ServiceLevel(m,u,t,st)
    
  
    if(expectedsl >= sl)
    {
      continue <- FALSE
    }
    
    m <- m+1
  }
  return(m)
}
print(ErlangC_GetRequiredAgents(48,15,240,.845))



