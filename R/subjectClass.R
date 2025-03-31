
if (FALSE) {
  debug(subjectClass); subjectClass(status_12, status_23, status_34)
}


subjectClass <- function(...) {
  # see ?lcmm::xclass
  
  dots <- list(...)
  cl <- match.call()
  nm <- as.list.default(cl)[-1L]
  if (all(vapply(nm, FUN = is.symbol, FUN.VALUE = NA))) {
    nm. <- vapply(nm, FUN = as.character, FUN.VALUE = '')
    if (!anyDuplicated(nm.)) names(dots) <- nm.
  } else names(dots) <- seq_along(dots)
  
  if (!all(vapply(dots, FUN = inherits, what = c(
    'hlme', 'lcmm', 'multlcmm', 'Jointlcmm', 'mpjlcmm', 'externX', 'externSurv'
  ), FUN.VALUE = NA))) stop('input must be one of supported models')
  
  xs <- lapply(dots, FUN = \(i) i$pprob[, 1:2, drop = FALSE])
  if (!all(duplicated.default(lapply(xs, FUN = names))[-1L])) stop('`subject` not the same')
  
  subject <- names(xs[[1L]])[1L]
  
  xs1 <- mapply(FUN = \(x, nm) {
    names(x)[2L] <- paste(names(x)[2L], nm, sep = '.') 
    return(x)
  }, x = xs, nm = names(dots), SIMPLIFY = FALSE)
  
  ret <- Reduce(f = function(x, y) {
    merge.data.frame(x, y, by = subject, all = TRUE)
  }, x = xs1)
  
  return(sort_by.data.frame(ret, y = eval(call('~', as.symbol(subject)))))

}
