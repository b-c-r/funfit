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
#' @param lhs_iter Number of random latin hypercube samplings, default is 1000.
#' @param MC Should the code run in parallel? Default is TRUE.
#' @param noC The number of cores that should be used, default is 50% of all cores.
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
  tsteps = 100,
  lhs_iter_init = 1000,
  lhs_iter_val = 50,
  MC = T,
  noC = ceiling(parallel::detectCores()/2),
  range_mult = c(1.01, 1.1, 1.5, 2, 5),
  witer_max = 25,
  val_tol = 4
){

  Fmax_range <- c(min(Neaten[Nstart>(max(Nstart)/2)]), max(Neaten)*2) / mean(tend)
  N0_range <- quantile(Nstart,c(.1,.9))
  q_range <- c(0, 2)

  start_parms <- lhs_parms(
    Neaten = Neaten,
    Nstart = Nstart,
    P = P,
    tend = tend,
    Fmax_range = Fmax_range,
    N0_range = N0_range,
    q_range = q_range,
    tsteps = tsteps,
    lhs_iter = lhs_iter_init,
    MC = T,
    noC = noC
  )
  message("parameter values after lhs-sampling:")
  message(paste("Fmax: ", round(start_parms$Fmax,3)))
  message(paste("N0: ", round(start_parms$N0,3)))
  message(paste("q: ", round(start_parms$q,3)))
  message("#########################################")

  fit <- list()
  nll_fits <- c()

  fit[[1]] <- bbmle::mle2(
    minuslogl = fr_nll,
    start = list(
      Fmax = start_parms$Fmax,
      N0 = start_parms$N0,
      q = start_parms$q
    ),
    data = list(
      Nstart = Nstart,
      Neaten = Neaten,
      tend = tend,
      P = P
    )
  )
  nll_fits[1] <- round(bbmle::logLik(fit[[1]])[][1], val_tol)
  message("parameter values after 1st fit:")
  message(paste("Fmax: ", round(bbmle::coef(fit[[1]])[1],3)))
  message(paste("N0: ", round(bbmle::coef(fit[[1]])[2],3)))
  message(paste("q: ", round(bbmle::coef(fit[[1]])[3],3)))
  message(paste("nll: ", nll_fits[1]))
  message("#########################################")
  message("start validating the result")

  witer <- 2
  suc <- F
  while(suc == T | witer <= witer_max){

    start_parms <- foreach::foreach(i = 1:length(range_mult),
                                    .combine = "rbind") %do% {

      Fmax_range <- c(bbmle::coef(fit[[1]])[1]/range_mult[i],
                      bbmle::coef(fit[[1]])[1]*range_mult[i])
      N0_range <- c(bbmle::coef(fit[[1]])[2]/range_mult[i],
                      bbmle::coef(fit[[1]])[2]*range_mult[i])
      q_range <- c(bbmle::coef(fit[[1]])[3]/range_mult[i],
                      bbmle::coef(fit[[1]])[3]*range_mult[i])

      lhs_parms(
        Neaten = Neaten,
        Nstart = Nstart,
        P = P,
        tend = tend,
        Fmax_range = Fmax_range,
        N0_range = N0_range,
        q_range = q_range,
        tsteps = tsteps,
        lhs_iter = lhs_iter_val,
        MC = T,
        noC = noC
      )
    }

    fit[[witer]] <- bbmle::mle2(
      minuslogl = fr_nll,
      start = list(
        Fmax = start_parms$Fmax[start_parms$nll == min(start_parms$nll)],
        N0 = start_parms$N0[start_parms$nll == min(start_parms$nll)],
        q = start_parms$q[start_parms$nll == min(start_parms$nll)]
      ),
      data = list(
        Nstart = Nstart,
        Neaten = Neaten,
        tend = tend,
        P = P
      )
    )
    nll_fits[witer] <- round(bbmle::logLik(fit[[witer]])[][1], val_tol)
    message(paste("parameter values after ", witer," fit:", sep = ""))
    message(paste("Fmax: ", round(bbmle::coef(fit[[witer]])[1],3)))
    message(paste("N0: ", round(bbmle::coef(fit[[witer]])[2],3)))
    message(paste("q: ", round(bbmle::coef(fit[[witer]])[3],3)))
    message(paste("nll: ", nll_fits[witer]))
    message("#########################################")

    if(witer >=5){
      if(length(unique(nll_fits[(witer-4):witer])) == 1) suc <- T
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
