#' Fitting of the functional response model with simulation by lsoda()
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
#' @import bbmle
#'
#' @export
#'
#' @examples
#'
#' data(vucic_pestic_fr)
#'
#' frfit_deSolve(
#'   Neaten = frdata_vp$Neaten,
#'   Nstart = frdata_vp$N0,
#'   P = 1,
#'   tmax = 24
#' )
#'

frfit_deSolve <- function(Nstart, Neaten, P, tmax, tsteps = 100){
  bbmle::mle2(
    minuslogl = frnll_deSolve,
    start = list(
      Fmax = max(Neaten),
      N0 = max(Nstart)/2,
      q = 1
    ),
    data = list(
      Nstart = Nstart,
      Neaten = Neaten,
      tmax = tmax,
      P = P
    )
  )
}
