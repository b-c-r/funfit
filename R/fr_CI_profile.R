#' Profiling the confidence intervalls in parallel
#'
#' @description
#'
#' @param fit The prey/resource items eaten throughout the experimental
#'     trial.
#' @param MC Use parallel computing? (F/T)
#' @param noC If MC = T, how many cores to use?
#'
#' @return Returns the profiled confidence intervals.
#'
#' @export
#'
#' @examples
#'
#' data(dfr_vp)
#'
#' fit <- fr_fit(
#'   Neaten = dfr_vp$Neaten,
#'   Nstart = dfr_vp$Nstart,
#'   P = rep(1, length(dfr_vp$Neaten)),
#'   tend = rep(1, length(dfr_vp$Neaten))
#' )
#'
#' CI <- fr_CI_profile(fit = fit)
#' CI
#'

fr_CI_profile <- function(fit,
                          CI_level = .95,
                          MC = T,
                          noC = 3
){

  parms <- c('l10_Fmax','l10_N0','h')
  cl <- parallel::makeCluster(noC)
  doParallel::registerDoParallel(cl)
  ci <- foreach::foreach(i = 1:3,
                         .combine = 'rbind',
                         .errorhandling = "pass",
                         .packages = c("bbmle","funfit")
  ) %dopar% {
    bbmle::confint(object = fit,
                   parm = parms[i],
                   level = CI_level,
                   method = 'uniroot')
  }
  parallel::stopCluster(cl)

  if(is.na(ci[3])) ci[3] <- 1

  rownames(ci) <- c('l10_Fmax','l10_N0','h')

  return(ci)
}



