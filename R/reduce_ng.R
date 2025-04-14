

#' @title Reduce Number of Latent Classes from \link[lcmm]{lcmm} Object
#' 
#' @description 
#' To reduce number of latent classes from \link[lcmm]{lcmm} object.
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @param criterion \link[base]{function}, criterion of elimination. 
#' Either \link[stats]{AIC} or \link[stats]{BIC}.
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @details 
#' 
#' For all three methods (\link[stats]{logLik}, \link[stats]{AIC} and \link[stats]{BIC}), 
#' the comparison is always with the original model.  
#' 
#' This is different from \link[MASS]{stepAIC}, in which the comparison is with the previous round.
#' 
#' @examples
#' reduce_ng_lcmm(m20)
#' 
#' @importFrom lcmm lcmm 
#' @importFrom stats AIC BIC
#' @export
reduce_ng_lcmm <- function(object, criterion = BIC, ...) {
  if (object$ng == 1L) return(object)
  
  data_orig <- getData.lcmm(object)
  
  cl <- object$call
  cl$data <- quote(data_orig)
  cl$verbose <- FALSE
  
  cl1 <- cl
  cl1$ng <- 1L
  cl1$mixture <- NULL
  cl1$B <- NULL
  m1 <- eval(cl1)
  
  cl$B <- quote(m1) # see ?lcmm::lcmm, on parameter `B`
  
  models <- if (object$ng == 2L) {
    list(m1, object)
  } else c(
    list(m1), 
    lapply(2:(object$ng - 1L), FUN = \(g) {
      cl$ng <- g
      cat(c('lcmm with', g, 'latent class(es) running .. '))
      on.exit(return(eval(cl)))
      cat('done!\n')
    }), 
    list(object)
  )
  
  objF <- vapply(models, FUN = criterion, FUN.VALUE = 0)
  #print(objF) # to remove later
  
  id <- which.min(objF) # AIC, BIC
  # which.max(objF) # logLik !!!
  if (id == object$ng) message('Try ng = ', object$ng + 1L) 
  
  ret <- models[[id]]
  if (id < object$ng) attr(ret, which = 'ng_orig') <- object$ng
  return(ret)
  
}



