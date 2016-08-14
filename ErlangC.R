#' ErlangC Library
#'
#' @param m - the number of agents (integer greater than 0)
#' @param u - the traffic intensity (unitless, given as average handling time (AHT) in time units multiplied by number of calls per same time unit, typically both in seconds)
#'
#' @return returns the probability as a decimal between 0 and 1 of an individual call waiting
#' @export
#'
#' @examples ErlangC_Base(20,(500*(600/3600)))
ErlangC_Base <- function(m,u)
{
  # Calculates base Erlang C probability of waiting
}
  
ErlangC_GetN <- function(x)
{
  # Steve Bastable 2016-08-13
  # Solves ErlangC service level equation for N (number of agents)
  # http://www.mitan.co.uk/erlang/elgcmath.htm gives an excellent overview of this
  b = x^2
  return(b)
}

print(Square(4))
print(Square(x=4)) # same thing

