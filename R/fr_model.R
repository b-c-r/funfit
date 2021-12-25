#' The Functional Response Model
#'
#' @description TBA
#'
#' @param Nstart The initial prey/resource density.
#' @param Fmax The maximum feeding rate.
#' @param N0 The half saturation density.
#' @param h The Hill exponent. It is functional response shape parameter.
#' @param P The consumer/predator abundance/counts/density.
#' @param tend The end time over up to which should be simulated (e.g. 24h).
#'     Must be a vector of the length of Nstart.
#' @param tsteps The number of steps that should be returned by the solver.
#'
#' @return Returns the prey/resource density at Tend.
#'
#' @export
#'
#' @examples
#'
#' fr_model(
#'   Nstart = 25,
#'   Fmax = 2,
#'   N0 = 5,
#'   h = 1,
#'   P = 1,
#'   tend = 24,
#'   tsteps = 100
#'  )
#'

fr_model <- function(Nstart, P, tend, Fmax, N0, h, tsteps){
  FR_set_params(Fmax = Fmax,
                N0 = N0,
                h = h,
                P = P)
  FR(init = Nstart,
     duration = tend,
     step_size = tend/(tsteps-1),
     start = 0.0)[[2]][tsteps]
}
