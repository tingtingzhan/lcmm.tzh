
#' @title Model Formula of \link[lcmm]{lcmm} Object
#' 
#' @param x \link[lcmm]{lcmm} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' m20 |> formula()
#' @importFrom stats formula
#' @export formula.lcmm
#' @export
formula.lcmm <- function(x, ...) {
  fom <- x$call$fixed
  if (inherits(fom, what = 'formula')) return(fom)
  if (is.call(fom) && fom[[1L]] == '~') return(eval(fom))
  stop('x$call$fixed must be formula')
}


#' @title Number of Subjects in \link[lcmm]{lcmm} Object
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @param ... additional parameters
#' 
#' @details
#' This is clearly documented in \link[lcmm]{lcmm}.
#' 
#' Method dispatch to S3 generic \link[stats]{nobs} is required in S3 generic \link[stats]{BIC}.
#' The end user should use function [nobsText.lcmm()].
#' 
#' @returns 
#' Function [nobs.lcmm()] returns an \link[base]{integer} scalar.
#' 
#' @examples
#' m20 |> nobs()
#' @importFrom stats nobs
#' @export nobs.lcmm
#' @export
nobs.lcmm <- function(object, ...) object[['ns']]



#' @title Log-Likelihood of \link[lcmm]{lcmm} Object
#' 
#' @description ..
#' 
#' @param object \link[lcmm]{lcmm} object
#' 
#' @param ... additional parameters
#' 
#' @note
#' \link[lcmm]{lcmm} object contains elements `$loglik`, `$AIC` and `$BIC`.
#' 
#' @returns
#' Function [logLik.lcmm()] returns a \link[stats]{logLik} object.
#' 
#' @examples
#' m20 |> logLik()
#' m20 |> AIC() |> all.equal.numeric(current = m20$AIC) |> stopifnot()
#' m20 |> BIC() |> all.equal.numeric(current = m20$BIC) |> stopifnot()
#' @keywords internal
#' @importFrom stats logLik
#' @export logLik.lcmm
#' @export
logLik.lcmm <- function(object, ...) {
  # `posfix` is not returned from ?lcmm:::.Contlcmm nor ?lcmm:::.Ordlcmm
  posfix <- eval(object$call$posfix) # may run into error
  ret <- object$loglik
  attr(ret, which = 'nobs') <- nobs.lcmm(object)
  attr(ret, which = 'df') <- length(object$best) - length(posfix)
  class(ret) <- 'logLik'
  return(ret)
}




#' @title Additional S3 Method Dispatches for \link[lcmm]{lcmm} Object
#' 
#' @param x \link[lcmm]{lcmm} object
#' 
#' @note
#' See `lcmm:::summary.lcmm` for details.
#' 
#' @returns 
#' Function [nobsText.lcmm()] returns a \link[base]{character} scalar.
#' 
#' @examples
#' nobsText.lcmm(m20)
#' @keywords internal
#' @name S3_lcmm
#' @importFrom ecip nobsText
#' @export nobsText.lcmm
#' @export
nobsText.lcmm <- function(x) {
  sprintf(fmt = '%d observations from %d `%s`', 
          x$N[5], 
          x[['ns']], 
          x$call$subject)
}

#' @rdname S3_lcmm
#' @importFrom methods new
#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @export desc_.lcmm
#' @export
desc_.lcmm <- function(x) {
  
  'latent class mixed-effect [@Proust06]' |>
    new(Class = 'md_lines', bibentry = .proust06())
  
}



# I am not presenting [ecip()] for \link[lcmm]{lcmm} object
# functions below are correct, but not used right now

# lcmm:::coef.lcmm -> lcmm:::estimates.lcmm
# ?lcmm:::summary.lcmm returns 'matrix' (eh..)
#' @rdname S3_lcmm
#' @importFrom utils capture.output
#' @importFrom ecip coef_
#' @export coef_.lcmm
#' @export
coef_.lcmm <- function(x) {
  capture.output(
    xsum <- x |>
      summary() 
  )
  # xsum is 'matrix'
  ret <- xsum[, 'coef']
  names(ret) <- rownames(xsum)
  return(ret)
}


#' @rdname S3_lcmm
#' @importFrom utils capture.output
#' @importFrom ecip .pval
#' @export .pval.lcmm
#' @export
.pval.lcmm <- function(x) {
  capture.output(
    xsum <- x |>
      summary()
  )
  # xsum is 'matrix'
  ret <- xsum[, 'p-value']
  names(ret) <- rownames(xsum)
  return(ret)
}



#' @title R Markdown Lines for \link[lcmm]{lcmm} Object  
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' list(
#'  'model 1' = m20,
#'  'model 2' = m21
#' ) |> fastmd::render2html()
#' @keywords internal
#' @importFrom fastmd md_ md_autoplot_
#' @importFrom ecip .md_reg
#' @export md_.lcmm
#' @export
md_.lcmm <- function(x, xnm, ...) {
  
  z1 <- .md_reg(x)
  
  z2 <- md_autoplot_(x = x, xnm = xnm, ...)
  
  c(z1, z2) # ?fastmd::c.md_lines
  
}




