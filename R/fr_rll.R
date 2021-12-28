#' Calculation of a relative log-likelihood for CI estimation
#'
#' @description
#'
#' @param x TBA
#' @param which_parm TBA
#' @param min_nll TBA
#' @param l10_Fmax_in The log10 of the maximum feeding rate.
#' @param l10_N0_in The log10 of the half saturation density.
#' @param h_in The Hill exponent. It is functional response shape parameter.
#' @param fit TBA
#' @param CI_level TBA
#' @param df TBA
#' @param mle2_tol TBA
#' @param for_uniroot TBA
#'
#' @return TBA
#'
#' @export
#'
#' @examples
#'
#' TBA
#'
#'

fr_rll <- function(x,
                   which_parm,
                   min_nll,
                   l10_Fmax_in,
                   l10_N0_in,
                   h_in,
                   fit,
                   CI_level,
                   df,
                   mle2_tol = 1e-8,
                   for_uniroot = F){

  if(which_parm == 'l10_Fmax'){
    sl <- list(
      l10_N0 = l10_N0_in,
      h = h_in
    )
    fl <- list(
      l10_Fmax = x,
      tsteps = fit@fullcoef[[4]]
    )
  }

  if(which_parm == 'l10_N0'){
    sl <- list(
      l10_Fmax = l10_Fmax_in,
      h = h_in
    )
    fl <- list(
      l10_N0 = x,
      tsteps = fit@fullcoef[[4]]
    )
  }

  if(which_parm == 'h'){
    sl <- list(
      l10_Fmax = l10_Fmax_in,
      l10_N0 = l10_N0_in
    )
    fl <- list(
      h = x,
      tsteps = fit@fullcoef[[4]]
    )
  }

  fit_rll <- bbmle::mle2(
    minuslogl = fr_nll,
    start = sl,
    fixed = fl,
    data = list(
      Nstart = fit@data$Nstart,
      Neaten = fit@data$Neaten,
      tend = fit@data$tend,
      P = fit@data$P
    ),
    skip.hessian=T,
    control = list(reltol = mle2_tol)
  )

  rll <-  (min_nll-bbmle::logLik(fit_rll)[][1])*2
  rll <- rll-qchisq(CI_level,df)

  if(for_uniroot == T) return(rll)
  return(data.frame(rll = rll,
                    nll = -bbmle::logLik(fit_rll)[][1],
                    l10_Fmax = fit_rll@fullcoef[[1]],
                    l10_N0 = fit_rll@fullcoef[[2]],
                    h = fit_rll@fullcoef[[3]]))
}



