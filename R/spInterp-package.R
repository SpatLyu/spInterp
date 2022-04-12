#' @keywords internal
#' @import magrittr
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib spInterp, .registration = TRUE
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  library(magrittr)
}
