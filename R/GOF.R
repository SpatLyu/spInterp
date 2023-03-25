#' @importFrom hydroGOF KGE
#' @importFrom dplyr tibble
GOF <- function (yobs, ysim, w, include.cv = FALSE, include.r = TRUE)
{
    if (missing(w))
        w <- rep(1, length(yobs))
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
        out <- c(RMSE = NA_real_, KGE = NA_real_, NSE = NA_real_,
            MAE = NA_real_, AI = NA_real_, Bias = NA_real_, Bias_perc = NA_real_, 
            n_sim = NA_real_)
        if (include.r)
            out <- c(out, R2 = NA_real_, R = NA_real_, pvalue = NA_real_)
        if (include.cv)
            out <- c(out, obs = CV_obs, sim = CV_sim)
        return(out)
    }
    KGE = KGE(ysim, yobs)
    y_mean <- sum(yobs * w)/sum(w)
    SSR <- sum((ysim - y_mean)^2 * w)
    SST <- sum((yobs - y_mean)^2 * w)
    RE <- ysim - yobs
    Bias <- sum(w * RE)/sum(w)
    Bias_perc <- Bias/y_mean
    MAE <- sum(w * abs(RE))/sum(w)
    RMSE <- sqrt(sum(w * (RE)^2)/sum(w))
    NSE <- 1 - sum((RE)^2 * w)/SST
    if (include.r) {
        R <- NA_real_
        pvalue <- NA_real_
        tryCatch({
            cor.obj <- cor.test(yobs, ysim, use = "complete.obs")
            R <- cor.obj$estimate[[1]]
            pvalue <- cor.obj$p.value
        }, error = function(e) {
            message(e$message)
        })
        R2 = R^2
    }
    AI <- NA_real_
    I2 <- which(w == 1)
    if (length(I2) >= 2) {
        yobs = yobs[I2]
        ysim = ysim[I2]
        y_mean = mean(yobs)
        AI = 1 - sum((ysim - yobs)^2)/sum((abs(ysim - y_mean) +
            abs(yobs - y_mean))^2)
    }
    out <- tibble(R, pvalue, R2, NSE, KGE, RMSE, MAE, Bias, Bias_perc,
        AI = AI, n_sim = n_sim)
    if (include.cv)
        out <- cbind(out, CV_obs, CV_sim)
    return(out)
}
