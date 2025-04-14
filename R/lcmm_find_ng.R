

#' @title \link[lcmm]{lcmm} with Optimal Number of Latent Classes
#' 
#' @description
#' ..
#' 
#' @param fixed see \link[lcmm]{lcmm}
#' 
#' @param criterion ..
#' 
#' @param ... ..
#' 
#' @examples
#' data(data_lcmm, package = 'lcmm')
#' m = lcmm_find_ng(Ydep2~Time, subject='ID',
#'  idiag=TRUE, data=data_lcmm, link='beta',
#'  verbose=FALSE, returndata=TRUE)
#' @keywords internal
#' @importFrom stats AIC BIC
#' @export
lcmm_find_ng <- function(fixed, criterion = BIC, ...) {

  # see ?lcmm::lcmm, on parameter `B`
  
  cl <- match.call()
  if (length(cl$ng)) stop('Use lcmm_B1() instead')
  cl[[1L]] <- quote(lcmm)
  if (!length(cl$random)) cl$random <- eval(call('~', fixed[[3L]]))

  cl1 <- cl
  cl1$mixture <- NULL
  cl1$ng <- ng <- 1L
  cat('Running ng = 1 ... ')
  lc <- lc1 <- eval(cl1)
  objF <- criterion(lc1)
  sprintf('done! %s = %.2f\n', as.character(substitute(criterion)), objF) |> cat()
  
  if (!length(cl$mixture)) cl$mixture <- eval(call('~', fixed[[3L]]))
  cl$B <- quote(lc1)
  
  repeat {
    ng <- ng + 1L
    cl$ng <- ng
    sprintf('Running ng = %d ... ', ng) |> cat()
    lc_run <- eval(cl)
    objF_run <- criterion(lc_run)
    sprintf('done! %s = %.2f\n', as.character(substitute(criterion)), objF_run) |> cat()
    if (objF_run > objF) break
    lc <- lc_run
    objF <- objF_run
  } 
  
  return(lc)
  
}


