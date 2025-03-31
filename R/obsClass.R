


#' @title Identifier of Latent Classes from \link[lcmm]{lcmm} Object
#' 
#' @description
#' To obtain the identifier of latent classes from an latent class mixed-effect model.
#' 
#' @param object an \link[lcmm]{lcmm} object
#' 
#' @note
#' See **Value** section of function \link[lcmm]{lcmm}.
#' 
#' @returns 
#' Function [obsClass.lcmm()] returns a \link[base]{factor}.
#' 
#' @note
#' Name clash `methods::getClass`.
#' 
#' Function \link[lcmm]{xclass} requires two models fitted to a same `data`.
#' 
#' @examples
#' data(data_lcmm, package = 'lcmm')
#' dim(data_lcmm)
#' table(m20$pprob$class) # per subject
#' table(obsClass(m20)) # per observation
#' @name obsClass
#' @export
obsClass <- function(object) UseMethod(generic = 'obsClass')


#' @rdname obsClass
#' @export obsClass.lcmm
#' @export
# old name `getclass_lcmm` 
obsClass.lcmm <- function(object) {
  
  subj <- object$call$subject
  if (length(subj) != 1L || !is.character(subj)) stop('lcmm package updated?') # 'character'
  dat <- getData.lcmm(object)
  
  if (!is.data.frame(pred <- object$pred)) stop('lcmm package updated?')
  if (.row_names_info(pred, type = 2L) != .row_names_info(dat, type = 2L)) stop('should not happen')
  if (names(pred)[1L] != subj) stop('lcmm package updated?')
  subj. <- pred[[subj]] # numeric
  if (is.unsorted(subj., strictly = FALSE)) stop('should not happen')
  
  if (!is.data.frame(pprob <- object$pprob)) stop('lcmm package updated?')
  subj_ <- unique.default(subj.)
  if (!identical(pprob[[subj]], subj_)) stop('should not happen')
  
  if (!setequal(pprob$class, seq_len(object$ng))) stop('should not happen')
  #gseq <- seq_len(object$ng)
  cls <- pprob$class[match(x = dat[[subj]], table = subj_)]
  return(as.factor(cls))
  #attr(cls, which = 'levels') <- as.character.default(seq_len(object$ng))
  #class(cls) <- 'factor'
  #return(cls)
}

