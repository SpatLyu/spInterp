# array_2dTo3d <- function(array, I_grid = NULL, dim) {
#   ntime <- dim(array) %>% last()
#   dim <- c(dim, ntime)
#   temp <- array(NA * array[1], dim = dim) %>% array_3dTo2d()
#   if (is.null(I_grid)) {
#     temp <- array
#   } else {
#     temp[I_grid, ] <- array
#   }
#   ans <- set_dim(temp, dim)
#   names_last <- dimnames(array) %>% last()
#   if (!is.null(names_last)) {
#     ans %<>% set_dimnames(list(NULL, NULL, names_last))
#   }
#   ans
# }

rowvec <- function(x, ...) matrix(x, nrow = 1, ...)

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

set_dim <- function(x, dim) {
  dim(x) <- dim
  x
}

set_dimnames <- function(x, value) {
  dimnames(x) <- value
  x
}

listk <- function(...) {
  cols <- as.list(substitute(list(...)))[-1]
  vars <- names(cols)
  Id_noname <- if (is.null(vars)) {
    seq_along(cols)
  } else {
    which(vars == "")
  }
  if (length(Id_noname) > 0) {
    vars[Id_noname] <- sapply(cols[Id_noname], deparse)
  }
  x <- set_names(list(...), vars)
  return(x)
}

last <- function(x) {
  if (is.null(x)) NULL else x[length(x)]
}

findn_small <- function(x, n = 10, decreasing = FALSE, fill.na = TRUE) {
  N <- length(x)
  n2 <- pmin(N, n)

  o <- order(x, decreasing = decreasing)
  if (!fill.na) {
    o[1:n2]
  } else {
    c(o[1:n2], rep(NA, n - n2))
  }
}

#' @importFrom matrixStats rowMeans2
#' @export
matrixStats::rowMeans2

apply_3d <- function(array, dim = 3, FUN = rowMeans2, by = NULL, scale = 1, na.rm = TRUE, ...) {
  dims <- dim(array)
  ndim <- length(dims)
  I_dims <- setdiff(1:ndim, dim)
  dims_head <- dims[I_dims]
  if (dim != ndim) {
    array %<>% aperm(c(I_dims, dim))
  }
  mat <- array_3dTo2d(array)
  if (is.null(by)) {
    ans <- FUN(mat, ..., na.rm = na.rm)
    dim_new <- dims_head
  } else {
    dim_new <- c(dims_head, length(unique(by)))
    ans <- apply_row(mat, by, FUN, scale = scale)
  }
  dim(ans) <- dim_new
  ans
}

apply_row <- function(mat, by, FUN = rowMeans2, scale = 1, ...) {
  if (length(by) != ncol(mat)) {
    stop("Length of by is not equal to ncol of mat")
  }
  if (length(scale) == 1) {
    scale <- rep(scale, length(by))
  }
  grps <- unique(by) %>% sort()
  ans <- lapply(grps, function(grp) {
    I <- which(by == grp)
    factor <- scale[I][1]
    FUN(mat[, I, drop = FALSE] * factor, na.rm = TRUE, ...)
  }) %>% do.call(cbind, .)
  if (!is.matrix(ans)) {
    ans <- as.matrix(ans)
  }
  ans %>%
    set_colnames(grps) %>%
    set_rownames(rownames(mat))
}

array_3dTo2d <- function(array, I_grid = NULL) {
  dim <- dim(array)
  names_last <- dimnames(array) %>% dplyr::last()
  if (length(dim) >= 3) {
    dim(array) <- c(prod(dim[1:2]), dim[3])
  }
  if (!is.null(I_grid)) {
    array <- array[I_grid, ]
  }
  if (!is.null(names_last)) {
    array %<>% set_dimnames(list(NULL, names_last))
  }
  return(array)
}
