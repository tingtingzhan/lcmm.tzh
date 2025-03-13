

#' @title \link[lcmm]{lcmm} using Initial Value based on `ng = 1L`
#' 
#' @description ..
#' 
#' @param fixed see \link[lcmm]{lcmm}
#' 
#' @param ... ..
#' 
#' @examples
#' data(data_lcmm, package = 'lcmm')
#' m = lcmm_B1(Ydep2~Time, subject='ID',
#'  idiag=TRUE, data=data_lcmm, link='beta', ng = 2L,
#'  verbose=FALSE, returndata=TRUE)
#' 
#' @export
lcmm_B1 <- function(fixed, ...) {
  
  # see ?lcmm::lcmm, on parameter `B`
  
  cl <- match.call()
  if (length(cl$B)) stop('Use lcmm::lcmm instead')
  
  cl[[1L]] <- quote(lcmm)
  if (!length(cl$random)) cl$random <- eval(call('~', fixed[[3L]]))
  
  cl1 <- cl
  cl1$mixture <- NULL
  cl1$ng <- 1L
  lc1 <- eval(cl1)
  
  if (!length(cl$mixture)) cl$mixture <- eval(call('~', fixed[[3L]]))
  cl$B <- quote(lc1)
  
  eval(cl)
  
}


