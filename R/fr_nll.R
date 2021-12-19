#' Calculation of negative log-likelihoods using lsoda() as method
#'
#' @description
#'
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param Nstart The initial prey/resource density.
#' @param parms The model parameters as needed by e.g. \link[deSolve]{lsoda}.
#' @param tend The time over which should be simulated.
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
#' fr_nll(
#'   Neaten = Neaten,
#'   Nstart = Nstart,
#'   P = rep(1, length(Nstart)),
#'   tend = rep(24, length(Nstart)),
#'   Fmax = 2,
#'   N0 = 5,
#'   q = 1
#' )
#'
#'

fr_nll <- function(Neaten, Nstart, P, tend, Fmax, N0, q, tsteps = 100){

  if(Fmax <= 0) return(Inf)
  if(N0 <= 0) return(Inf)
  if(q <= -1) return(Inf)

  y <- fr_sim(Nstart = Nstart,
              P = P,
              tend = tend,
              Fmax = Fmax,
              N0 = N0,
              q = q,
              tsteps = tsteps)

  lls <- dbinom(x = Neaten,
                size = Nstart,
                prob = y/Nstart,
                log = T)

  nll <- -1*sum(lls)

  return(nll)
}
