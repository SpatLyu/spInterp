#' kfold machine learning
#' @name kfold_ml
#' 
#' @importFrom plyr llply
#' @export
kfold_ml <- function(X, Y, kfold = 5, FUN, ..., weight = NULL, Z = NULL, seed = 1, na.rm = FALSE) { 
  set.seed(seed)
  # 自动把全部为NA的行去除
  X <- as.matrix(X)
  Y <- as.matrix(Y)

  # major improvement, v20220605
  # 任何一个变量为NA都不行
  
  if (na.rm) {
    ind_bad <- apply(X, 1, function(x) any(is.na(x))) | is.na(Y)
    ind_good <- which(!ind_bad)

    X = X[ind_good, , drop = FALSE]
    Y = Y[ind_good, , drop = FALSE]
  } else {
    ind_good <- 1:nrow(X)
  }  
  ind_lst <- createFolds(ind_good, k = kfold, list = TRUE)
  # ind_lst <- Ipaper::chunk(ind_good, kfold) %>% set_names(paste0("fold", 1:kfold))

  result <- llply(ind_lst, kfold_calib,
    X = X, Y = Y,
    # FUN = randomForest, ntree = ntree, ...,
    FUN = FUN, ...,
    Z = Z, 
    weight = weight,
    .progress = "text"
  )
  kfold_tidy(result, ind_lst, Y)
}

#' @export
kfold_calib <- function(index, X, Y, FUN = xgboost, ..., weight = NULL, Z = NULL) {
  x_train <- X[-index, , drop = F]
  y_train <- Y[-index, , drop = F]
  z_train <- if (is.null(Z)) NULL else Z[-index, , drop = F]
  w_train <- if (is.null(weight)) NULL else weight[-index, , drop = F]
  
  x_test <- X[index, , drop = F]
  y_test <- Y[index, , drop = F]

  m <- FUN(x_train, y_train, ..., weight = w_train, Z = z_train)

  ypred <- predict(m, x_test)
  list(gof = GOF(y_test, ypred), ypred = ypred, model = m)
}


#' @importFrom purrr map
#' @export
kfold_tidy <- function(res, ind_lst, Y) {
  kfold_names <- names(ind_lst)

  ## 3. GOF information get
  .val <- map(res, ~ .x$ypred) #%>% unlist() # pred value
  val = do.call(rbind, .val)
  .inds <- unlist(ind_lst) |> set_names(NULL)  

  ypred <- val * NA
  ypred[.inds, ] <- val

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
