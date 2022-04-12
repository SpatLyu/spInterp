check_matrix <- function(x) {
  if (is.null(x)) return(x)

  if (is.vector(x)) {
    t(x)
  } else if (!is.matrix(x)) {
    as.matrix(x)
  } else {
    x
  }
}

#' cal_azimuth
#' @export
cal_azimuth <- function(x) {
  ab_angle(c(0, 1), x)
}

#' @export
#' @rdname cal_azimuth
ab_angle <- function(a, b) {
  a %<>% check_matrix()
  b %<>% check_matrix()

  dot = a[, 1] * b[, 1] + a[, 2] * b[, 2]
  la = sqrt(a[, 1]^2 + a[, 2]^2)
  lb = sqrt(b[, 1]^2 + b[, 2]^2)
  ans = sign(b[, 1] - a[, 1])*acos(dot / (la * lb))
  # 右侧为正
  rad2deg(ans) 
}

angle = ab_angle

# #' @export 
# angle <- function(from, to){
#   from %<>% check_matrix()
#   to %<>% check_matrix()
  
#   dot.prods <- from[, 1] * to[, 1] + from[, 2]*to[, 2]
#   # the relative angle of two points
#   central = matrix(0, ncol = 2)
#   norms.x <- distance(from = central, to = from)
#   norms.y <- distance(from = central, to = to)
#   thetas <- acos((dot.prods / (norms.x * norms.y)))
  
#   as.numeric(thetas)
# }

#' @export 
deg2rad <- function(x) x * pi / 180

#' @export 
rad2deg <- function(x) x * 180 /pi 

#' @export 
distance <- function(from, to) {
  sqrt((abs(from[, 1] - to[, 1])^2) + (abs(from[, 2] - to[, 2])^2))
}

#' @export
rdist.earth <- function (x1, x2 = NULL, miles = FALSE, R = NULL) {
  if (is.null(R)) R  = ifelse(miles, 3963.34, 6378.388)

  x1 %<>% check_matrix()
  x2 %<>% check_matrix()
  
  x1 %<>% deg2rad()
  coslat1 <- cos(x1[, 2])
  sinlat1 <- sin(x1[, 2])
  coslon1 <- cos(x1[, 1])
  sinlon1 <- sin(x1[, 1])

  if (is.null(x2)) {
    pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% 
        t(cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1))
    return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
  } else {
    x2 %<>% deg2rad()
    coslat2 <- cos(x2[, 2])
    sinlat2 <- sin(x2[, 2])
    coslon2 <- cos(x2[, 1])
    sinlon2 <- sin(x2[, 1])

    pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% 
        t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
    return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
  }
}

cal_hw <- function(point, dist = 450) {
  funx = function(dx) {
    p2 = point + c(dx, 0)
    # p2[, 1] %<>% pmin(180)
    dist2 = rdist.earth(point, p2)
    abs(dist2 - dist)
  }

  funy = function(dy) {
    p2 = point + c(0, dy)
    # p2[, 2] %<>% pmin(90)
    dist2 = rdist.earth(point, p2)
    abs(dist2 - dist)
  }
  dx = optimize(funx, c(0.001, 90))$minimum
  dy = optimize(funy, c(0.001, 90))$minimum  
  c(dx = dx, dy = dy)
}
