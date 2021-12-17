#' The Functional Response Model for `deSolve`
#'
#' @description
#'
#' @param t The time variable as required by e.g. \link[deSolve]{lsoda}.
#' @param x The density from which x at t+1 should be computed, see e.g.
#'     \link[deSolve]{lsoda}
#' @param parms The model parameters as needed by e.g. \link[deSolve]{lsoda}.
#'
#' @details The model parameters included in `parms` are (1) the maximum feeding
#'     rate `Fmax`, the half saturation density, `N0`, the shape parameter `q`,
#'     and the number or density of the predators/consumers, `P`.
#'
#' @return Returns a list of x-values and dependent y-values.
#'
#' @export
#'
#' @examples
#'
#' frmodel_deSolve(
#'   t = 0, # only required for simulations using e.g. lsoda()
#'   x = 10, # the density at wich x at t+1 should be computed
#'   parms = list(
#'     Fmax = 10,
#'     N0 = 5,
#'     q = 1,
#'     P = 1
#'     )
#'  )
#'

frmodel_deSolve = function(t, x, parms){
  with(as.list(parms),{
    dx = -Fmax*x[1]^(1+q) / (N0^(1+q) + x[1]^(1+q)) * P
    return(list(c(dx)))
  })
}

