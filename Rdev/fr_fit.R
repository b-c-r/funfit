#' Fitting of the functional response model with simulation by lsoda()
#'
#' @description
#'
#'
#' @param Nstart The initial prey/resource density.
#' @param Neaten The prey/resource items eaten throughout the experimental
#'     trial.
#' @param P The consumer/predator abundance/counts/density.
#'     Must be a vector of the length of Nstart.
#' @param tend The end time over up to which should be simulated (e.g. 24h).
#'     Must be a vector of the length of Nstart.
#' @param tsteps The number of steps that should be returned by the solver.
#' @param lhs_iter_init Number of initial random latin hypercube samplings.
#' @param lhs_iter_val Number of validation random latin hypercube samplings.
#' @param MC Use parallel computing? (F/T)
#' @param noC If MC = T, how many cores to use?
#' @param range_mult The multipliers with which the current best parameters
#'     should be multiplied for the validation random latin hypercube sampling.
#' @param witer_max How many fits should be performed without convergence?
#' @param mle2_tol The tolerance of a single mle2 fit.
#' @param val_tol The tolerance of the validation.
#'
#' @details TBA
#'
#' @return TBA
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

fr_fit <- function(
  Nstart,
  Neaten,
  P,
  tend,
  tsteps = 50,
  lhs_iter_init = 1280,
  lhs_iter_val = 320,
  MC = T,
  noC = ceiling(parallel::detectCores()/2),
  range_mult = c(1.001, 1.1, 2, 5),
  witer_max = 25,
  mle2_tol = 1e-12,
  val_tol = 6
){

  Fmax_range <- c(min(Neaten[Nstart>(max(Nstart)/2)]), max(Neaten)*2) / mean(tend)
  N0_range <- quantile(Nstart,c(.1,.9))
  h_range <- c(1, 4)

  start_parms <- lhs_parms(
    Neaten = Neaten,
    Nstart = Nstart,
    P = P,
    tend = tend,
    l10_Fmax_range = log10(Fmax_range),
    l10_N0_range = log10(N0_range),
    h_range = h_range,
    tsteps = tsteps,
    lhs_iter = lhs_iter_init,
    MC = T,
    noC = noC
  )
  message("parameter values after lhs-sampling:")
  message(paste("Fmax: ", round(10^start_parms$l10_Fmax,3)))
  message(paste("N0: ", round(10^start_parms$l10_N0,3)))
  message(paste("h: ", round(start_parms$h,3)))
  message("#########################################")

  fit <- list()
  nll_fits <- c()

  fit[[1]] <- bbmle::mle2(
    minuslogl = fr_nll,
    start = list(
      l10_Fmax = start_parms$l10_Fmax,
      l10_N0 = start_parms$l10_N0,
      h = start_parms$h
    ),
    fixed = list(
      tsteps = tsteps
    ),
    data = list(
      Nstart = Nstart,
      Neaten = Neaten,
      tend = tend,
      P = P
    ),
    method = "L-BFGS-B",
    lower = list(
      l10_Fmax = -Inf,
      l10_N0 = -Inf,
      h = 1
    ),
    upper = list(
      l10_Fmax = Inf,
      l10_N0 = Inf,
      h = Inf
    ),
    control = list(reltol = mle2_tol)
  )
  nll_fits[1] <- round(bbmle::logLik(fit[[1]])[][1], val_tol)
  message(paste("parameter values after fit 1:", sep = ""))
  message(paste("Fmax: ", round(10^bbmle::coef(fit[[1]])[1],3)))
  message(paste("N0: ", round(10^bbmle::coef(fit[[1]])[2],3)))
  message(paste("q: ", round(bbmle::coef(fit[[1]])[3],3)))
  message(paste("nll: ", nll_fits[1]))
  message("#########################################")
  message("")
  message("start validating the result")
  message("")

  witer <- 2
  while(witer <= witer_max){

    start_parms <- foreach::foreach(i = 1:length(range_mult),
                                    .combine = "rbind") %do% {

      Fmax_range <- c(10^bbmle::coef(fit[[1]])[1]/range_mult[i],
                      bbmle::coef(fit[[1]])[1]*range_mult[i])
      N0_range <- c(10^bbmle::coef(fit[[1]])[2]/range_mult[i],
                      bbmle::coef(fit[[1]])[2]*range_mult[i])
      h_range <- c(bbmle::coef(fit[[1]])[3]/range_mult[i],
                      bbmle::coef(fit[[1]])[3]*range_mult[i])

      h_range[h_range<1] <- 1

      lhs_parms(
        Neaten = Neaten,
        Nstart = Nstart,
        P = P,
        tend = tend,
        l10_Fmax_range = log10(Fmax_range),
        l10_N0_range = log10(N0_range),
        h_range = h_range,
        tsteps = tsteps,
        lhs_iter = lhs_iter_val,
        MC = T,
        noC = noC
      )
    }

    fit[[witer]] <- bbmle::mle2(
      minuslogl = fr_nll,
      start = list(
        l10_Fmax = start_parms$l10_Fmax[start_parms$nll == min(start_parms$nll)],
        l10_N0 = start_parms$l10_N0[start_parms$nll == min(start_parms$nll)],
        h = start_parms$h[start_parms$nll == min(start_parms$nll)]
      ),
      fixed = list(
        tsteps = tsteps
      ),
      data = list(
        Nstart = Nstart,
        Neaten = Neaten,
        tend = tend,
        P = P
      ),
      method = "L-BFGS-B",
      lower = list(
        l10_Fmax = -Inf,
        l10_N0 = -Inf,
        h = 1
      ),
      upper = list(
        l10_Fmax = Inf,
        l10_N0 = Inf,
        h = Inf
      ),
      control = list(reltol = mle2_tol)
    )
    nll_fits[witer] <- round(bbmle::logLik(fit[[witer]])[][1], val_tol)
    message(paste("parameter values after fit ", witer, ":", sep = ""))
    message(paste("Fmax: ", round(10^bbmle::coef(fit[[witer]])[1],3)))
    message(paste("N0: ", round(10^bbmle::coef(fit[[witer]])[2],3)))
    message(paste("q: ", round(bbmle::coef(fit[[witer]])[3],3)))
    message(paste("nll: ", nll_fits[witer]))
    message("#########################################")
    message("")

    if(witer >= 3){
      if(length(unique(nll_fits[(witer-2):witer])) == 1) break
    }
    witer <- witer+1

  }

  if(witer < witer_max){
    return(fit[[witer]])
  } else{
    ret_i <- witer[nll_fits == min(nll_fits)]
    message("no stable fit found, returning best fit")
    return(fit[[ret_i]])
  }
}
