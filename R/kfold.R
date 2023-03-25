#' kfold machine learning
#' @name kford_ml
#' 
#' @importFrom plyr llply
#' @export
kford_ml <- function(X, Y, kfold = 5, FUN, ..., seed = 1) { 
  set.seed(seed)
  # 自动把全部为NA的行去除
  X <- as.matrix(X)
  Y <- as.matrix(Y)

  # major improvement, v20220605
  # 任何一个变量为NA都不行
  ind_bad <- apply(X, 1, function(x) any(is.na(x))) | is.na(Y)
  ind_good <- which(!ind_bad)
  # X = X[ind_good, , drop = FALSE]
  # Y = Y[ind_good, , drop = FALSE]

  ind_lst <- createFolds(ind_good, k = kfold, list = TRUE)
  # ind_lst <- Ipaper::chunk(ind_good, kfold) %>% set_names(paste0("fold", 1:kfold))

  res <- llply(ind_lst, kford_calib,
    X = X, Y = Y,
    # FUN = randomForest, ntree = ntree, ...,
    FUN = FUN, ...,
    .progress = "text"
  )
  kford_tidy(res, ind_lst, Y)
}

#' @export
kford_calib <- function(index, X, Y, FUN = xgboost, ...) {
  x_train <- X[-index, , drop = F]
  y_train <- Y[-index, , drop = F]

  x_test <- X[index, , drop = F]
  y_test <- Y[index, , drop = F]

  m <- FUN(x_train, y_train, ...)

  ypred <- predict(m, x_test)
  list(gof = GOF(y_test, ypred), ypred = ypred, model = m)
}


#' @export
kford_tidy <- function(res, ind_lst, Y) {
  kfold_names <- names(ind_lst)

  ## 3. GOF information get
  val <- map(res, ~ .x$ypred) %>% unlist() # pred value
  ypred <- Y * NA
  ypred[unlist(ind_lst)] <- val
  info_all <- GOF(Y, ypred)

  model <- map(res, "model")
  gof <- map(res, "gof") %>%
    c(., all = list(info_all)) %>%
    do.call(rbind, .) %>%
    as.data.table()
  gof %<>% cbind(kfold = c(kfold_names, "all"), .)

  listk(gof, ypred, index = ind_lst, model) %>% set_class("kfold")
  # how to return back to original value?
}

#' @export
predict.kfold <- function(object, newdata, ...) {
  lapply(object$model, function(m) predict(m, newdata, ...))
}

#' @export
print.kfold <- function(x, ...) {
  print(x$gof %>% dplyr::tibble() %>% dt_round(3)) #
  cat("\nFold index:\n")
  print(str(x$index))
}
