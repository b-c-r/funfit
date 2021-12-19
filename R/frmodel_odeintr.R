#' The Functional Response Model
#'
#' @description TBA
#'
#' @param Nstart The initial prey/resource density.
#' @param Fmax The maximum feeding rate.
#' @param N0 The half saturation density.
#' @param q The functional response shape parameter.
#' @param P The consumer/predator abundance/counts/density.
#' @param tend The time of the end of the time series (e.g. 24h)
#' @param steps The number of steps at time > 0 at which \link[odeintr] should
#'     compute the result.
#'
#' @return Returns the prey/resource density at Tend.
#'
#' @export
#'
#' @examples
#'
#' frmodel_odeintr(
#'   Nstart = 10,
#'   Fmax = 10,
#'   N0 = 5,
#'   q = 1,
#'   P = 1,
#'   tend = 24,
#'   steps = 100
#'  )
#'

frmodel_odeintr <- function(Nstart, Fmax, N0, q, P, tend, steps){
  FR_set_params(Fmax = Fmax,
                N0 = N0,
                q = q,
                P = P)
  FR(init = Nstart,
     duration = tend,
     step_size = tend/(steps),
     start = 0.0)[[2]][steps+1]
}
