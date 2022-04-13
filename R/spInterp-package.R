#' @name spInterp-package
#' @keywords internal
#' @import magrittr
#' @importFrom stats approx weighted.mean
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        ".", "lon", "lat", "w", "x"
      )
    )
  }
}
