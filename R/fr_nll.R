#' Calculation of negative log-likelihoods using lsoda() as method
#'
#' @description
#'
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param Nstart The initial prey/resource density.
#' @param P The consumer/predator abundance/counts/density.
#'     Must be a vector of the length of Nstart.
#' @param tend The end time over up to which should be simulated (e.g. 24h).
#'     Must be a vector of the length of Nstart.
#' @param l10_Fmax The log10 of the maximum feeding rate.
#' @param l10_N0 The log10 of the half saturation density.
#' @param h The Hill exponent. It is functional response shape parameter.
#' @param tsteps The number of steps that should be returned by the solver.
#'
#' @return Returns a single negative log-likelihood value.
#'
#' @export
#'
#' @examples
#'
#' TBA
#'
#'

fr_nll <- function(Neaten,
                   Nstart,
                   P,
                   tend,
                   l10_Fmax,
                   l10_N0,
                   h,
                   penalty = 10000,
                   h_low = 1,
                   tsteps = 50){



  y <- fr_sim(Nstart = Nstart,
              P = P,
              tend = tend,
              Fmax = 10^l10_Fmax,
              N0 = 10^l10_N0,
              h = h,
              tsteps = tsteps)

  lls <- dbinom(x = Neaten,
                size = Nstart,
                prob = y/Nstart,
                log = T)

  nll <- -1*sum(lls)

  if(h < 1) nll <- nll + penalty*(h-h_low)^2

  return(nll)
}

