#' Bootstrapping the fit to get confidence intervals
#'
#' @description TBA
#'
#' @param fit A mle2 object.
#' @param bt_iter The number of bootstrap iterations.
#' @param MC Use parallel computing? Actually, this is not a question!! (F/T)
#' @param noC If MC = T, how many cores to use? Use all ist the default.
#' @param mle2_tol The tolerance of a single mle2 fit.
#'
#' @details TBA
#'
#' @return Returns a list with (1) the confidence limits, and (2) the bootstrapped parameter samples.
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
#' # only run if you want to take a break...
#' system.time({
#'  frbt <- fr_bootstrap(fit)
#' })
#' CIs:
#' frbt[1]
#'

fr_bootstrap <- function(
  fit,
  bt_iter = 1000,
  MC = T,
  noC = ceiling(parallel::detectCores()),
  mle2_tol = 1e-6
){
  cl <- parallel::makeCluster(noC)
  doParallel::registerDoParallel(cl)

  bt_fit <- foreach::foreach(i = 1:bt_iter,
                          .combine = "rbind",
                          .errorhandling = "remove",
                          .packages = c("bbmle", "funfit")) %dopar% {

                            j <- sample(1:length(fit@data[[1]]), replace = T)

                            bbmle::mle2(
                              minuslogl = fr_nll,
                              start = list(
                                l10_Fmax = fit@coef[[1]],
                                l10_N0 = fit@coef[[2]],
                                h = fit@coef[[3]]
                              ),
                              fixed = list(
                                tsteps = fit@fullcoef[[4]]
                              ),
                              data = list(
                                Nstart = fit@data[[1]][j],
                                Neaten = fit@data[[2]][j],
                                tend = fit@data[[3]][j],
                                P = fit@data[[4]][j]
                              ),
                              control = list(reltol = mle2_tol)
                            )@coef
  }
  parallel::stopCluster(cl)

  CIparms <- data.frame(
    Fmax = 10^quantile(bt_fit[,1], c(0.025,0.975)),
    N0 = 10^quantile(bt_fit[,2], c(0.025,0.975)),
    q = quantile(bt_fit[,3], c(0.025,0.975))
  )
  return(list(CIparms = CIparms, CIsamples = bt_fit) )
}


