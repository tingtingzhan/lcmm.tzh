


#' @title \link[lcmm]{lcmm} Object  
#' 
#' @examples
#' list(
#'  'model 1' = m20,
#'  'model 2' = m21
#' ) |> fastmd::render2html()
#' 
#' @name lcmm
NULL





#' @export
formula.lcmm <- function(x, ...) {
  fom <- x$call$fixed
  if (inherits(fom, what = 'formula')) return(fom)
  if (is.call(fom) && fom[[1L]] == '~') return(eval(fom))
  stop('x$call$fixed must be formula')
}


# this is clearly documented in \link[lcmm]{lcmm}.
# 
# method dispatch to `S3` generic \link[stats]{nobs} is required in S3 generic \link[stats]{BIC}.
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




# see `lcmm:::summary.lcmm` for details.
#' @importFrom ecip nobsText
#' @export
nobsText.lcmm <- function(x) {
  sprintf(fmt = '%d observations from %d `%s`', 
          x$N[5], 
          x[['ns']], 
          x$call$subject)
}

#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @export
desc_.lcmm <- function(x) {
  'latent class mixed-effect [@Proust06]' |>
    new(Class = 'md_lines', bibentry = .proust06())
}



# I am not presenting [ecip()] for \link[lcmm]{lcmm} object
# functions below are correct, but not used right now
# lcmm:::coef.lcmm -> lcmm:::estimates.lcmm
# ?lcmm:::summary.lcmm returns 'matrix' (eh..)
#' @importFrom utils capture.output
#' @importFrom ecip coef_
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


#' @importFrom utils capture.output
#' @importFrom ecip .pval
#' @export
.pval.lcmm <- function(x) {
  capture.output(
    xsum <- x |>
      summary()
  ) # xsum is 'matrix' !!! packageDate('lcmm') 2025-11-20
  ret <- xsum[, 'p-value']
  names(ret) <- rownames(xsum)
  return(ret)
}







#' @importFrom ggplot2 autoplot ggplot 
#' @export
autoplot.lcmm <- function(object, ...) {
  ggplot() + 
    autolayer.lcmm(object, ...)
  
  # 'facet' will need following code?
  #data <- getData.lcmm(object)
  #ggplot(data = data) + # `data` argument needed, in case we have 'facet'
  #  autolayer.lcmm(object, ...)
}




#' @importFrom geomtextpath geom_textline
#' @importFrom ggplot2 autolayer aes geom_line labs facet_grid label_both
#' @importFrom ggrepel geom_label_repel 
#' @importFrom rlang .data
#' @export
autolayer.lcmm <- function(
    object, 
    type = c('original', 'transformed'), 
    ...
) {
  
  xs <- all.vars(object$call$fixed[[3L]])
  
  if (length(xs) != 1L) {
    return(invisible()) # more than 1 fixed predictors
    # not working with facet yet
  }
  
  subj <- object$call$subject
  
  dat <- getData.lcmm(object)
  xval1 <- split.default(dat[[xs]], f = dat[[subj]]) |> 
    lapply(FUN = sort) |>
    unlist(use.names = FALSE)
  
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
    approx_inv <- approxlinkinv_lcmm(object)
    obs <- approx_inv(obs)
    mpred <- approx_inv(mpred)
  })
  
  cls <- factor(pred_prob$class)
  
  pred_ <- data.frame(
    x = xval1, 
    y = mpred, 
    id = cls,
    label = cls |> as.character() |> sprintf(fmt = 'Latent Class: %s')
  ) |>
    unique.data.frame()
  
  list(
    
    geom_line(
      mapping = aes(x = xval1, y = obs, group = pred_prob[[subj]], colour = cls), 
      linewidth = .1, alpha = .5, show.legend = FALSE
    ),
    
    geom_textline(
      data = pred_, 
      mapping = aes(x = .data$x, y = .data$y, label = .data$label, group = .data$id, colour = .data$id),
      show.legend = FALSE
    ),
    
    (if (is.factor(xval1)) {
      geom_label_repel(data = pred_, mapping = aes(
        x = .data$x, y = .data$y, group = .data$id, colour = .data$id, 
        label = sprintf(fmt = '%.3f', .data$y)
      ), size = 2.5)
    }),
    
    #(if (length(facet_nm <- xs[-1L])) {
    #  facet_grid(rows = call('~', str2lang(paste(facet_nm, collapse = '+')), quote(.)), labeller = label_both)
    #}), # currently disabled
    
    labs(x = xs, 
         y = paste0(object$call$fixed[[2L]], switch(type, transformed = ' (Transformed)')), 
         color = 'Latent Class')
    
  )
  
}






#' @importFrom fastmd md_ md_autoplot_
#' @importFrom ecip md_regression_
#' @export
md_.lcmm <- function(x, xnm, ...) {
  
  z1 <- md_regression_(x)
  
  z2 <- md_autoplot_(x = x, xnm = xnm, ...)
  
  c(z1, z2) # ?fastmd::c.md_lines
  
}




