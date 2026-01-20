
#' @title Inverse Link Function of \link[lcmm]{lcmm} Object, via Linear Interpolation
#' 
#' @description
#' Inverse link *function* from a latent class mixed-effect model, via linear interpolation.
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @param ... other parameters of function \link[stats]{approxfun}
#' 
#' @note
#' See section **Value** of \link[lcmm]{lcmm}.
#' 
#' See function `lcmm:::summary.lcmm()` very carefully!
#' 
#' Functions, see Page 4 of \url{https://www.jstatsoft.org/article/view/v078i02}
#' `lcmm:::.Contlcmm`, then Fortran code `C_hetmixcont`, look for output `transfY`; 
#' actually in subroutine `transfo_estimee`.
#' 
#' I am totally confused, and cannot reproduce the results. So I finally resort to brutal force!!
#' 
#' `lcmm:::plot.lcmm()` with option 'linkfunction' -> `lcmm:::.plotlinkfunction`,
#' see `plot(object, which = 'link')`.
#' 
#' @keywords internal
#' @importFrom stats approxfun
#' @export
approxlinkinv_lcmm <- function(object, ...) {
  lk <- object$estimlink # 'matrix'
  approxfun(
    x = lk[,2L], # transformed value
    y = lk[,1L], # original value
    ...
  )
}

