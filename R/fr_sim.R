#' Calculation of the Functional Response Model using odeintr
#'
#' @description TBA
#'
#' @param Nstart The initial prey/resource density.
#' @param P The consumer/predator abundance/counts/density. Must be a vector of
#'     the length of Nstart.
#' @param tend The end time over up to which should be simulated (e.g. 24h).
#'     Must be a vector of the length of Nstart.
#' @param Fmax The maximum feeding rate.
#' @param N0 The half saturation density.
#' @param q The functional response shape parameter.
#' @param tsteps The number of steps that should be returned by the solver.
#'
#' @return Returns a vector with the number of prey/resource eaten in the order
#'     of Nstart.
#'
#' @import foreach
#'
#' @export
#'
#' @examples
#'
#' Nstart <- 1:100
#'
#' y1 <- fr_sim(
#'   Nstart = Nstart,
#'   P = rep(1, length(Nstart)),
#'   tend = rep(24, length(Nstart)),
#'   Fmax = 2,
#'   N0 = 25,
#'   q = .5
#' )
#'
#' plot(Nstart, y1, type="l")
#'

fr_sim <- function(Nstart, P, tend, Fmax, N0, q, tsteps = 100){
  reps <- 1:length(Nstart)
  Neaten <- foreach::foreach(i = reps, .combine = "c") %do% {
    Nstart[i] - fr_model(
      Nstart = Nstart[i],
      P = P[i],
      tend = tend[i],
      Fmax = Fmax,
      N0 = N0,
      q = q,
      tsteps = tsteps
    )
  }
  return(Neaten)
}
