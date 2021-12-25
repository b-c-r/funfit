#' Random Sampling of Parameters and Selection based on lhs
#'
#' @description TBA
#'
#' @param Nstart The initial prey/resource density.
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param P The consumer/predator abundance/counts/density.
#'     Must be a vector of the length of Nstart.
#' @param tend The end time over up to which should be simulated (e.g. 24h).
#'     Must be a vector of the length of Nstart.
#' @param tsteps The number of steps that should be returned by the solver.
#' @param l10_Fmax The log10 of the maximum feeding rate.
#' @param l10_N0 The log10 of the half saturation density.
#' @param h The Hill exponent. It is functional response shape parameter.
#' @param tsteps The number of steps that should be returned by the solver.
#' @param lhs_iter Number of random latin hypercube samplings.
#' @param MC Use parallel computing? (F/T)
#' @param noC If MC = T, how many cores to use?

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
  l10_Fmax_range,
  l10_N0_range,
  h_range,
  tsteps = 100,
  lhs_iter = 1000,
  MC = T,
  noC = ceiling(parallel::detectCores()/2)
){
  ## guessing parameter ranges:
  lhsvals <- lhs::randomLHS(lhs_iter, 3)

  l10_Fmax_range <- (lhsvals[,1] * (l10_Fmax_range[2]-l10_Fmax_range[1])) + l10_Fmax_range[1]
  l10_N0_range <- (lhsvals[,2] * (l10_N0_range[2]-l10_N0_range[1])) + l10_N0_range[1]
  h_range <- (lhsvals[,3] * (h_range[2]-h_range[1])) + h_range[1]

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
        l10_Fmax = l10_Fmax_range[i],
        l10_N0 = l10_N0_range[i],
        h = h_range[i]
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
        l10_Fmax = l10_Fmax_range[i],
        l10_N0 = l10_N0_range[i],
        h = h_range[i]
      )
    }
  }

  sel_parms <- data.frame(
    l10_Fmax = l10_Fmax_range[nlls == min(nlls)],
    l10_N0 = l10_N0_range[nlls == min(nlls)],
    h = h_range[nlls == min(nlls)],
    nll = nlls[nlls == min(nlls)]
  )

  return(sel_parms)
}
