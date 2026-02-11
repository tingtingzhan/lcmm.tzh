
#' @title bibs of lcmm package
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name lcmm_bib
#' @export
.proust06 <- \(key = 'Proust06', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = c(
      person(family = 'Proust', given = 'C\u00e9cile'), 
      person(family = 'Jacqmin-Gadda', given = 'H\u00e9l\u00e8ne'), 
      person(family = 'Taylor', given = c('Jeremy', 'M.', 'G.')), 
      person(family = 'Ganiayre', given = 'Julien'), 
      person(family = 'Commenges', given = 'Daniel')
    ),
    title = 'A Nonlinear Model with Latent Process for Cognitive Evolution Using Multivariate Longitudinal Data',
    journal = 'Biometrics',
    volume = '62',
    number = '4',
    pages = '1014-1024',
    year = '2006',
    month = '04',
    doi = '10.1111/j.1541-0420.2006.00573.x'
  )
}