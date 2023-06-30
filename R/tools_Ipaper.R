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

# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr
# round_any <- function(x, accuracy, f = round) {
#   f(x / accuracy) * accuracy
# }

writeLines_list <- function(l, f) {
  con <- file(f, "w")
  on.exit(close(con))
  
  for (x in unlist(l)) {
    writeLines(x, con)
    # readr::write_lines(l, f, append = TRUE, num_threads = 1)
  }
  invisible()
}

#' @importFrom dplyr mutate across
dt_round <- function (d, digits = 2) {
  mutate(d, across(where(is.double), ~round(.x, digits)))
}

is_empty <- function (x) {
  is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0
}

mkdir <- function(path) {
  for (path_i in unique(path)) {
    if (!dir.exists(path_i)) {
      dir.create(path_i, recursive = TRUE)
    }
  }
  path
}

# combine two range
c_range <- function(r1, r2) {
  r1 = range(r1, na.rm = TRUE)
  r2 = range(r2, na.rm = TRUE)
  c(min(r1[1], r2[1]), max(r1[2], r2[2]))
}

#' @importFrom dplyr select
#' @export
select.matrix <- function(.data, ...) {
  .data %>% as.data.frame() %>%
    dplyr::select(...) %>%
    as.matrix()
}


# copied from caret
createFolds <- function(y, k = 10, list = TRUE, returnTrain = FALSE) {
  if (class(y)[1] == "Surv") y <- y[, "time"]
  if (is.numeric(y)) {
    cuts <- floor(length(y) / k)
    if (cuts < 2) cuts <- 2
    if (cuts > 5) cuts <- 5
    breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  if (k < length(y)) {
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    for (i in 1:length(numInClass)) {
      min_reps <- numInClass[i] %/% k
      if (min_reps > 0) {
        spares <- numInClass[i] %% k
        seqVector <- rep(1:k, min_reps)
        if (spares > 0) {
          seqVector <- c(seqVector, sample(1:k, spares))
        }
        foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
      } else {
        foldVector[which(y == names(numInClass)[i])] <-
          sample(1:k, size = numInClass[i])
      }
    }
  } else {
    foldVector <- seq(along = y)
  }
  if (list) {
    out <- split(seq(along = y), foldVector)
    names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
    if (returnTrain) {
      out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    }
  } else {
    out <- foldVector
  }
  out
}


GOF <- function(yobs, ysim, w, include.cv = FALSE, include.r = TRUE) UseMethod("GOF", yobs)

GOF.matrix <- function(yobs, ysim, w, include.cv = FALSE, include.r = TRUE) {
  obs = c(yobs)
  sim = c(as.matrix(ysim))
  # obs = rowMeans(yobs, na.rm = TRUE)
  # sim = rowMeans(as.matrix(ysim), na.rm = TRUE)
  GOF(obs, sim, w, include.cv, include.r)
}

#' @importFrom hydroGOF KGE
#' @importFrom dplyr tibble
GOF.default <- function(yobs, ysim, w, include.cv = FALSE, include.r = TRUE) {
  if (missing(w)) {
    w <- rep(1, length(yobs))
  }
  valid <- function(x) !is.na(x) & is.finite(x)
  I <- which(valid(ysim) & valid(yobs) & valid(w))
  n_sim <- length(I)
  ysim <- ysim[I]
  yobs <- yobs[I]
  w <- w[I]
  if (include.cv) {
    CV_obs <- cv_coef(yobs, w)
    CV_sim <- cv_coef(ysim, w)
  }
  if (is_empty(yobs)) {
    out <- c(
      RMSE = NA_real_, KGE = NA_real_, NSE = NA_real_,
      MAE = NA_real_, AI = NA_real_, Bias = NA_real_, Bias_perc = NA_real_,
      n_sim = NA_real_
    )
    if (include.r) {
      out <- c(out, R2 = NA_real_, R = NA_real_, pvalue = NA_real_)
    }
    if (include.cv) {
      out <- c(out, obs = CV_obs, sim = CV_sim)
    }
    return(out)
  }
  KGE <- KGE(ysim, yobs)
  y_mean <- sum(yobs * w) / sum(w)
  SSR <- sum((ysim - y_mean)^2 * w)
  SST <- sum((yobs - y_mean)^2 * w)
  RE <- ysim - yobs
  Bias <- sum(w * RE) / sum(w)
  Bias_perc <- Bias / y_mean
  MAE <- sum(w * abs(RE)) / sum(w)
  RMSE <- sqrt(sum(w * (RE)^2) / sum(w))
  NSE <- 1 - sum((RE)^2 * w) / SST
  if (include.r) {
    R <- NA_real_
    pvalue <- NA_real_
    tryCatch(
      {
        cor.obj <- cor.test(yobs, ysim, use = "complete.obs")
        R <- cor.obj$estimate[[1]]
        pvalue <- cor.obj$p.value
      },
      error = function(e) {
        message(e$message)
      }
    )
    R2 <- R^2
  }
  AI <- NA_real_
  I2 <- which(w == 1)
  if (length(I2) >= 2) {
    yobs <- yobs[I2]
    ysim <- ysim[I2]
    y_mean <- mean(yobs)
    AI <- 1 - sum((ysim - yobs)^2) / sum((abs(ysim - y_mean) +
      abs(yobs - y_mean))^2)
  }
  out <- tibble(R, pvalue, R2, NSE, KGE, RMSE, MAE, Bias, Bias_perc,
    AI = AI, n_sim = n_sim
  )
  if (include.cv) {
    out <- cbind(out, CV_obs, CV_sim)
  }
  return(out)
}

shell <- function(..., ignore.stderr = FALSE, wait = TRUE) {
  FUN <- switch(.Platform$OS.type,
    "windows" = base::shell,
    "unix" = base::system
  )
  suppressWarnings(FUN(..., ignore.stderr = ignore.stderr, wait = wait))
}

find_exe <- function(exe) {
  path <- shell(glue("where.exe {exe}"), intern = TRUE)
  # if (!is.character(path)) path <- NULL
  path
}

prepend_path <- function(NAME, VALUE, head = FALSE) {
  sep <- ifelse(.Platform$OS.type == "windows", ";", ":")
  vals <- Sys.getenv(NAME)
  vals <- if (vals == "") {
    VALUE
  } else {
    if (head) {
      paste0(vals, sep, VALUE)
    } else {
      paste0(VALUE, sep, vals)
    }
  }
  cmd <- sprintf("Sys.setenv(%s=vals)", NAME)
  eval(parse(text = cmd))
}

add_path_anusplin <- function(path = NULL) {
  anusplin_path = options("anusplin_path")[[1]]

  if (is.null(anusplin_path)) {
    anusplin_path = system.file("exec", package = "spInterp")
    options(anusplin_path = anusplin_path)
    
    cat(sprintf("[info] setting anusplin path.\n"))
    prepend_path("PATH", anusplin_path, head = TRUE)
  } else {
    cat(sprintf("[ok] anusplin_path: %s\n", anusplin_path))
  }
}
