#' Arthropod Feeding Functional Response Data
#'
#' The functional response data is from feeding experiments using arthropods
#' performed by Vucic-Pestic et al. (2010). The data presented here is only
#' the subset that was published by Rosenbaum and Rall (2018, 2019).
#'
#' @docType data
#'
#' @usage data(dfr_vp)
#'
#' @format An object of the class \code{"data.frame"} with two columns:
#' \describe{
#'   \item{Nstart}{Integer. Prey count at the start of the experiment.}
#'   \item{Neaten}{Integer. Eaten prey count at the end of the experiment.}
#' }
#'
#' @keywords datasets
#'
#' @references Vucic-Pestic et al. (2010), J Anim Ecol 79:249-256; doi: \href{https://doi.org/10.1111/j.1365-2656.2009.01622.x}{10.1111/j.1365-2656.2009.01622.x}
#' @references Rosenbaum & Rall (2018), M Ecol Evol 9:2076-2090; doi: \href{https://doi.org/10.1111/j.1365-2656.2009.01622.x}{10.1111/j.1365-2656.2009.01622.x}
#' @references Rosenbaum & Rall (2019), DRYAD; doi: \href{https://doi.org/10.5061/dryad.kb76qj8}{10.5061/dryad.kb76qj8}
#'
#' @source \href{https://datadryad.org/stash/downloads/file_stream/54277}{csv-file on DRYAD}
#'
#' @examples
#'
#' data(dfr_vp)
#'
#' plot(dfr_vp$Nstart, dfr_vp$Neaten)
#'
"dfr_vp"
