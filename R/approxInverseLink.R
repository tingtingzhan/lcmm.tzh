
#' @title Numeric Inverse Link Function of \link[lcmm]{lcmm} Object
#' 
#' @description
#' To numerically retrieve the inverse link *function* from a latent class mixed-effect model.
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @param ... other parameters of \link[stats]{approxfun}
#' 
#' @note
#' See section **Value** of \link[lcmm]{lcmm}.
#' 
#' See `lcmm:::summary.lcmm` very carefully!
#' 
#' Functions, see Page 4 of \url{https://www.jstatsoft.org/article/view/v078i02}
#' `lcmm:::.Contlcmm`, then Fortran code `C_hetmixcont`, look for output `transfY`; 
#' actually in subroutine `transfo_estimee`.
#' 
#' I am totally confused, and cannot reproduce the results. So I finally resort to brutal force!!
#' 
#' `lcmm:::plot.lcmm` with option 'linkfunction' -> `lcmm:::.plotlinkfunction`,
#' see `plot(object, which = 'link')`.
#' 
#' @examples 
#' # to validate
#' validate_inverseLink_lcmm = function(object) {
#'   marker <- object$estimlink[,1L]
#'   transfY <- object$estimlink[,2L]
#'   my_marker <- approxInverseLink_lcmm(object)(transfY)
#'   range(marker - my_marker)
#' }
#' validate_inverseLink_lcmm(m20)
#' validate_inverseLink_lcmm(m21)
#' 
#' @importFrom stats approxfun
#' @export
approxInverseLink_lcmm <- function(object, ...) {
  lk <- object$estimlink
  approxfun(
    x = lk[,2L], # transformed value
    y = lk[,1L], # original value
    ...
  )
}

