
#' @title Get Data from \link[lcmm]{lcmm} Object
#' 
#' @description
#' To obtain the input data for latent class mixed-effect model analysis.
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @details
#' The input \link[lcmm]{lcmm} `object` must have `returndata = TRUE` in its call.
#' Otherwise, an error is returned to tell the end user to 
#' re-run \link[lcmm]{lcmm} call with `returndata = TRUE`.
#' 
#' `$na.action` blah blah.  
#' See argument `na.action` of \link[lcmm]{lcmm}, 
#' only \link[stats]{na.omit} and \link[stats]{na.fail} allowed, for now.
#' 
#' @note
#' Be aware of the name clash with `?lcmm::data_lcmm`, which is a \link[base]{data.frame}.
#' 
#' @returns 
#' Function [getData.lcmm()] returns a \link[base]{data.frame}.
#' 
#' @examples
#' head(getData.lcmm(m20))
#' 
#' @importFrom nlme getData
#' @export getData.lcmm
#' @export
getData.lcmm <- function(object) {
  cl <- object$call
  if (!isTRUE(cl$returndata)) stop('rerun lcmm call with `returndata = TRUE`')
  data <- object[['data']]
  if (!length(object$na.action)) return(data)
  return(data[-object$na.action, , drop = FALSE])
}
