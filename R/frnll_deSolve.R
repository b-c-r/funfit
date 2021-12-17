#' Calculation of negative log-likelihoods using lsoda() as method
#'
#' @description
#'
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param Nstart The initial prey/resource density.
#' @param parms The model parameters as needed by e.g. \link[deSolve]{lsoda}.
#' @param tmax The time over which should be simulated.
#' @param tsteps The number of times steps that \link[deSolve]{lsoda} should
#'     return.
#'
#' @details The model parameters included in `parms` are (1) the maximum feeding
#'     rate `Fmax`, the half saturation density, `N0`, the shape parameter `q`,
#'     and the number or density of the predators/consumers, `P`.
#'
#' @return Returns a vector of negative log-likelihoods in the order of
#'     Neaten/Nstart.
#'
#' @export
#'
#' @examples
#'
#' Nstart <- rep(1:100,10)
#' Neaten <- rbinom(length(Nstart),
#'                  size = Nstart,
#'                  prob = .5
#'                  )
#'
#' frnll_deSolve(
#'   Neaten = Neaten,
#'   Nstart = Nstart,
#'   Fmax = 10,
#'   N0 = 5,
#'   q = 1,
#'   P = 1,
#'   tmax = 24
#' )
#'
#' frnll_deSolve(
#'   Neaten = Neaten,
#'   Nstart = rep(1:100,10),
#'   Fmax = 10,
#'   N0 = 5,
#'   q = 1,
#'   P = 1,
#'   tmax = rnorm(length(Nstart), 24, sd = 2)
#' )
#'

frnll_deSolve <- function(Neaten, Nstart, Fmax, N0, q, P, tmax, tsteps = 100){

  if(Fmax <= 0) return(Inf)
  if(N0 <= 0) return(Inf)
  if(q <= -1) return(Inf)
  if(P <= 0) return(Inf)

  parms <- list(Fmax = Fmax,
                N0 = N0,
                q = q,
                P = P)

  y <- frsim_deSolve(Nstart = Nstart,
                     parms = parms,
                     tmax = tmax,
                     tsteps = tsteps)
  lls <- dbinom(x = Neaten,
                size = Nstart,
                prob = y/Nstart,
                log = T)
  nll <- -1*sum(lls)
  return(nll)
}
