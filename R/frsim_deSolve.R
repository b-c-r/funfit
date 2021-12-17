#' The Functional Response Model for `deSolve`
#'
#' @description
#'
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
#' @return Returns a vector with the number of prey/resource eaten in the order
#'     of Nstart.
#'
#' @import
#'     deSolve
#'     foreach
#'
#' @export
#'
#' @examples
#'
#' Nstart <- rep(1:100,10)
#'
#' frsim_deSolve(
#'   Nstart = Nstart,
#'   parms = list(
#'     Fmax = 10,
#'     N0 = 5,
#'     q = 1,
#'     P = 1
#'   ),
#'   tmax = 24
#' )
#'
#' frsim_deSolve(
#'   Nstart = Nstart,
#'   parms = list(
#'     Fmax = 10,
#'     N0 = 5,
#'     q = 1,
#'     P = 1
#'   ),
#'   tmax = rnorm(length(Nstart), 24, sd = 2)
#' )
#'

frsim_deSolve = function(Nstart, parms, tmax, tsteps = 100){
  if(length(tmax) == 1){
    Nstart2 <- unique(Nstart)
    reps <- 1:length(Nstart)
    reps2 <- 1:length(Nstart2)
    times <- seq(0, tmax, length=tsteps)

    Nend2 <- foreach::foreach(i = reps2,
                              .combine = "c") %do% {
                                deSolve::lsoda(y = Nstart2[i],
                                               times = times,
                                               func = frmodel_deSolve,
                                               parms = parms
                                )[[tsteps,2]]
                              }

    Neaten <- foreach::foreach(i = reps,
                               .combine = "c") %do% {
                                 Nstart[i]-Nend2[Nstart2 == Nstart[i]]
                               }
    return(Neaten)
  } else {
    if(length(tmax) != length(Nstart)){
      stop("Error! The length of tmax does not match the length of Nstart.
            Use either a single value or match the length of both variables.")
    }
    reps <- 1:length(Nstart)
    Nend <- foreach::foreach(i = reps,
                             .combine = "c") %do% {
                               times <- seq(0, tmax[i], length=tsteps)
                               deSolve::lsoda(y = Nstart[i],
                                              times = times,
                                              func = frmodel_deSolve,
                                              parms = parms
                               )[[tsteps,2]]
                             }

    Neaten <- Nstart-Nend
    return(Neaten)
  }
}
