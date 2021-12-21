#' Random sampling of Parameters and Selection based on lhs
#'
#' @description TBA
#'
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param Nstart The initial prey/resource density.
#' @param P TBA
#' @param tend The time over which should be simulated.
#' @param Fmax TBA
#' @param N0 TBA
#' @param q TBA
#' @param tsteps The number of times steps that \link[deSolve]{lsoda} should
#'     return.
#' @param lhs_iter Number of random latin hypercube samplings, default is 1000.
#' @param MC Should the code run in parallel? Default is TRUE.
#' @param noC The number of cores that should be used, default is 50% of all cores.
#'
#' @return Returns a data frame with parameter values.
#'
#' @import foreach
#' @import parallel
#' @import doParallel
#'
#' @export
#'
#'

lhs_parms <- function(
  Neaten,
  Nstart,
  P,
  tend,
  Fmax_range,
  N0_range,
  q_range,
  tsteps = 100,
  lhs_iter = 1000,
  MC = T,
  noC = ceiling(parallel::detectCores())
){
  ## guessing parameter ranges:
  lhsvals <- lhs::randomLHS(lhs_iter, 3)

  Fmax_range <- (lhsvals[,1] * (Fmax_range[2]-Fmax_range[1])) + Fmax_range[1]
  N0_range <- (lhsvals[,2] * (N0_range[2]-N0_range[1])) + N0_range[1]
  q_range <- (lhsvals[,3] * (q_range[2]-q_range[1])) + q_range[1]

  ## calculate nlls
  if(MC == T){
    cl <- parallel::makeCluster(noC)
    doParallel::registerDoParallel(cl)

    nlls <- foreach::foreach(
      i = 1:lhs_iter,
      .combine = "c",
      .packages = "funfit") %dopar% {
      fr_nll(
        Neaten = Neaten,
        Nstart = Nstart,
        P = P,
        tend = tend,
        Fmax = Fmax_range[i],
        N0 = N0_range[i],
        q = q_range[i]
      )
    }
    parallel::stopCluster(cl)
  } else {
    nlls <- foreach::foreach(i = 1:lhs_iter,
      .combine = "c",
      .packages = "funfit") %do% {
      fr_nll(
        Neaten = Neaten,
        Nstart = Nstart,
        P = P,
        tend = tend,
        Fmax = Fmax_range[i],
        N0 = N0_range[i],
        q = q_range[i]
      )
    }
  }

  sel_parms <- data.frame(
    Fmax = Fmax_range[nlls == min(nlls)],
    N0 = N0_range[nlls == min(nlls)],
    q = q_range[nlls == min(nlls)],
    nll = nlls[nlls == min(nlls)]
  )

  return(sel_parms)
}
