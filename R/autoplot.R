


#' @title Plot \link[lcmm]{lcmm} Object
#' 
#' @description ..
#' 
#' @param object \link[lcmm]{lcmm} object
#' 
#' @param type \link[base]{character} scalar, 
#' `'original'` (default) or `'transformed'`
#' 
#' @param ... ..
#' 
#' @examples 
#' autoplot(m20)
#' autoplot(m20, type = 'trans')
#' autoplot(m21)
#' autoplot(m21, type = 'trans')
#' 
#' @importFrom ggplot2 autoplot ggplot 
#' @name autoplot_lcmm
#' @export autoplot.lcmm
#' @export
autoplot.lcmm <- function(object, ...) {
  ggplot() + autolayer.lcmm(object, ...)
  
  # 'facet' will need following code?
  #data <- getData.lcmm(object)
  #ggplot(data = data) + # `data` argument needed, in case we have 'facet'
  #  autolayer.lcmm(object, ...)
}




#' @importFrom ggplot2 autolayer aes geom_line labs facet_grid label_both
#' @rdname autoplot_lcmm
#' @export autolayer.lcmm
#' @export
autolayer.lcmm <- function(object, type = c('original', 'transformed'), ...) {
  
  xs <- all.vars(object$call$fixed[[3L]])
  
  if (length(xs) != 1L) {
    return(invisible()) # more than 1 fixed predictors
    # not working with facet yet
  }
  
  subj <- object$call$subject
  
  dat <- getData.lcmm(object)
  xval1 <- unlist(lapply(split.default(dat[[xs]], f = dat[[subj]]), FUN = sort), use.names = FALSE)
  
  # subject id in `object$pred` is sorted!!!
  pred_prob <- merge.data.frame(x = object$pred, y = object$pprob, by = subj)
  
  predm <- sprintf(fmt = 'pred_m%d', seq_len(object$ng))
  
  # ?lcmm::lcmm, 'marginal predictions'
  mpred <- pred_prob[predm][cbind(seq_len(nrow(pred_prob)), pred_prob$class)] 
  # !!! no way to get 'class-specific marginal *residuals*'
  obs <- pred_prob[['obs']] # 'obs': transformed observations in the latent process scale
  
  type <- match.arg(type)
  
  switch(type, transformed = {
    # do nothing!
  }, original = {
    approx_inv <- approxInverseLink_lcmm(object)
    obs <- approx_inv(obs)
    mpred <- approx_inv(mpred)
  })
  
  cls <- factor(pred_prob$class)
  
  list(
    
    geom_line(mapping = aes(x = xval1, y = obs, group = pred_prob[[subj]], colour = cls), size = .1, alpha = .5),
    
    geom_line(mapping = aes(x = xval1, y = mpred, group = cls, colour = cls)),
    
    #(if (length(facet_nm <- xs[-1L])) {
    #  facet_grid(rows = call('~', str2lang(paste(facet_nm, collapse = '+')), quote(.)), labeller = label_both)
    #}), # currently disabled
    
    labs(x = xs, 
         y = paste0(object$call$fixed[[2L]], switch(type, transformed = ' (Transformed)')), 
         color = 'Latent Class')
    
  )
  
}
