#' Fitting of the functional response model with simulation by lsoda()
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
#' @import bbmle
#'
#' @export
#'
#' @examples
#'
#' data(dfr_vp)
#'
#' fr_fit(
#'   Neaten = dfr_vp$Neaten,
#'   Nstart = dfr_vp$Nstart,
#'   P = rep(1, length(dfr_vp$Neaten)),
#'   tend = rep(1, length(dfr_vp$Neaten))
#' )
#'

fr_fit <- function(Nstart, Neaten, P, tend, tsteps = 100){

  bbmle::mle2(
    minuslogl = fr_nll,
    start = list(
      Fmax = max(Neaten)*0.8,
      N0 = mean(Nstart),
      q = 1
    ),
    data = list(
      Nstart = Nstart,
      Neaten = Neaten,
      tend = tend,
      P = P
    )
  )
}
