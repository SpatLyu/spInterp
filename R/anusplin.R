#' Create configures for ANUSPLIN
#'
#' Format the input data and generate the configuration file required for ANUSPLIN interpolation.
#'
#' @param dat A data.frame or data.table to interpolate, for the colnames, lon,
#' lat must be included, site and alt are optional, the others are variable names.
#' @param basename Basename for all output files, extensions should not be included.
#' @param range Range of interpolation grid (xmin, xmax, ymin, ymax).
#' @param res The grid resolution (degree).
#' 
#' @param file.alt When `type.alt` is not 0, this is the input grid file name; otherwise
#' a manual constant.
#' 
#' @param unit The unit of `dat`, a non-negative integer, possible values are:
#' - `0`: undefined (**default**)
#' - `1`: meteres
#' - `2`: feet
#' - `3`: kilometers
#' - `4`: miles
#' - `5`: degrees
#' - `6`: radians
#' - `7`: millimetres
#' - `8`: megajoules
#' 
#' @param width The fixed width of numbers in formatted data.
#' 
#' @param alt Type of elevation was treated, possible values are:
#' - `NULL` : no use of elevation
#' - `cov`  : considered as independent covariates (**default**)
#' - `spl`  : considered as independent spline variables
#' 
#' @param lim.lon A vector containing lower and upper limits, `auto` (default)
#' meant to use the minimum and maximum values in the data, or set manually.
#' Data points outside these limits, augmented by margins, are ignored.
#' @param lim.lat Same as `lim.lon`, but for longitude.
#' @param lim.alt Same as `lim.lon`, but for altitude.
#' 
#' @param cvt.lon Transformation and scale factor (**default** is `1`) of longitude.\cr
#' Real Value = (Table Value) * (Scale Factor)\cr
#' 
#' The possible transformations are:
#' - `0`: no transformation (**default**)
#' - `1`: x/a
#' - `2`: ax
#' - `3`: a·log(x+b)
#' - `4`: (x/b)^a
#' - `5`: a·exp(x/b)
#' - `6`: a·tanh(x/b)
#' - `7`: anisotropy angle in degrees
#' - `8`: anisotropy factor - in the direction specified by the anisotropy angle
#' 
#' @param cvt.lat Same as `cvt.lon`, but for latitude.
#' @param cvt.alt Same as `cvt.lon`, but for altitude.
#' @param cvt.coef Parameters used for transformation, one or two real numbers.
#' 
#' @param trans.dep Dependent variable transformation, possible values are:
#' - `0`: no transformation (**default**)
#' - `1`: fit surface to natural logarithm of the data values
#' - `2`: fit surface to the square root of the data values
#' - `5`: occurrence – transform data values by setting all positive value to 1.0 and ignoring all negative values
#' 
#' @param order (default 3L), order of spline, a positive integer.
#' 
#' @param err.wgt Number of relative error variances, a non-negative integer,
#' possible values are:
#' - `0`: data points uniformly weighted for each surface (**default**)
#' - `1`: the same weighting is applied to each surface
#' - `Number of surfaces`: a different weighting is applied to each surface
#' 
#' @param optimize Optimization directive, a non-negative integer, possible values are:
#' 
#' - `0`: common smoothing parameter for all surfaces
#' - `1`: common smoothing directive for all surfaces (**default**)
#' - `2`: different smoothing directive for each surface
#' 
#' @param smooth Smoothing directive for each surface, a non-negative integer,
#' possible values are:
#' - `0`: fixed smoothing parameter - supply value
#' - `1`:  minimise GCV (**default**)
#' - `2`: minimise true mean square error using supplied error standard deviation estimate
#' - `3`: fixed signal - supply value
#' - `4`: minimise GML
#' 
#' @param type.mask Mode of mask grid, a non-negative integer, possible values are:
#' - `0`: mask grid not supplied (**default**)
#' - `1`: generic mask grid
#' - `2`: Arc/Info mask grid
#' - `3`: Idrisi mask grid
#' 
#' @param file.mask Filename of mask grid, only valid if `type.mask` set to positive
#' integer.
#' 
#' @param type.alt Mode of the independent variable, possible values are:
#' - `0`: user supplied constant
#' - `1`: user supplied grid in generic row format with the same size as the grid being calculated
#' - `2`: user supplied Arc/Info grid with same size as the grid being calculated (**default**)
#' - `3`: user supplied Idrisi image with the same size as the grid being calculated
#' 
#' @param type.grd Same as `type.mask`, but for interpolated grid.
#' @param missing Filling of missing values.
#' @param err.cov test
#' 
#' @param grid.pos Grid position option, a non-negative integer, possible values are:
#' - `0`: grid points at cell corners (**default**)
#' - `1`: grid points at cell centres
#' 
#' @param essential If `True`, only export essential process files, large residual
#' file, optimisation parameters file, data list file and validation data file are
#' ignored.
#' 
#' @return a list with three components:
#' - `data`   : formatted data.table of `dat`
#' - `splina` : a vector containing splina parameters
#' - `lapgrd` : a vector containing lapgrd parameters
#' 
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom magrittr %>% %<>%
#' @importFrom data.table as.data.table
#' @importFrom plyr round_any
#' 
#' @example R/examples/ex-anusplin.R
#' @export 
anusplin_params <- function(
    dat, basename, file.alt = NULL,
    range, res = 0.25,
    unit = 0,
    width = 7, missing = -9999,
    alt = "cov",
    lim.lon = "auto", lim.lat = "auto", lim.alt = "auto",
    cvt.lon = c(0, 1), cvt.lat = c(0, 1), cvt.alt = c(1, 1),
    cvt.coef = 1000,
    trans.dep = 0,
    order = 3,
    err.wgt = 0,
    optimize = 1,
    smooth = 1,
    type.mask = 0, file.mask = NULL,
    type.alt = 2,
    type.grd = 2,
    err.cov = 2,
    grid.pos = 1,
    essential = TRUE) {
  # check arguments validation
  if (nchar(missing) > width) {
    stop("Missing values should not be more than the width")
  }
  
  if (type.alt != 0) {
    if (!is.character(file.alt)) stop("1")
  } else {
    if (!is.numeric(file.alt)) stop("2")
  }

  dat %<>% as.data.table()
  names <- colnames(dat)

  if (is.null(alt)) {
    if ("alt" %in% names) dat %<>% dplyr::select(-alt)
    cvt.alt <- NULL
  } else {
    if (lim.alt == "auto") lim.alt = range(dat$alt)
    cvt.alt <- paste(lim.alt, cvt.alt, collapse = " ")
  }

  # n.sur <- length(names) - 2
  ind.spl <- 2
  ind.cov <- sur.spl <- sur.cov <- 0

  if (lim.lon == "auto") lim.lon = range(dat$lon)
  if (lim.lat == "auto") lim.lon = range(dat$lat)
  
  cvt.lon <- paste(lim.lon, cvt.lon, collapse = " ")
  cvt.lat <- paste(lim.lat, cvt.lat, collapse = " ")

  if ("site" %in% names) {
    nchr.site <- nchar(dat$site) %>% max()
    pos.site <- which("site" == names)
    fmt.site <- glue("%{nchr.site}i")
    dat$site %<>% sprintf(fmt = fmt.site)
    # n.sur <- n.sur - 1
  } else {
    nchr.site <- 0
    pos.site <- -1
    fmt.site <- NULL
  }

  pos.coord <- which(names %in% c("lon", "lat"))
  if (length(pos.coord) != 2) {
    stop("Check whether lon lat in colnames of dat")
  }

  dat[, pos.coord] <-
    dat[, lapply(.SD, sprintf, fmt = glue("%{width}.2f")), .SDcols = pos.coord]

  if (!is.null(alt) & "alt" %in% names) {
    dat$alt %<>% sprintf(fmt = glue("%{width}.1f"))
    # n.sur <- n.sur - 1
    if (alt == "cov") {
      ind.cov %<>% add(1)
    } else if (alt == "spl") {
      ind.spl %<>% add(1)
    }
  }

  pos.var <- which(!names %in% c("site", "lon", "lat", "alt"))
  n.sur <- length(pos.var)
  dat[, pos.var] <-
    dat[, lapply(.SD, sprintf, fmt = glue("%{width}.2f")), .SDcols = pos.var]

  file.sur <- glue("{basename}.sur")
  file.cov <- glue("{basename}.cov")

  ## outfile lists:
  # - large residual
  # - optimisation parameters
  # - surface coefficients
  # - data list
  # - error covariance
  if (essential) {
    output <- c("", "", file.sur, "", file.cov)
  } else {
    output <- c(
      glue("{basename}.res"),
      glue("{basename}.opt"),
      file.sur,
      glue("{basename}.lis"),
      file.cov
    )
  }

  rle <- str_extract(dat[1], "(?<=\\.)[0-9]+") %>%
    nchar() %>% rle()
  rle$lengths[which(rle$lengths == 1)] <- ""
  rle$values[which(!is.na(rle$value))] %<>% paste0("f", width, ".", .)
  rle$values[which(is.na(rle$values))] <- glue("a{nchr.site}")

  fmt <- paste0(rle[["lengths"]], rle[["values"]]) %>%
    paste(collapse = ",") %>%
    paste0("(", ., ")")

  splina <- listk(
    basename,
    unit,
    ind.spl, # Number of independent spline variables
    ind.cov, # Number of independent covariates
    sur.spl, # Number of surface independent spline variables
    sur.cov, # Number of surface independent covariates
    cvt.lon, cvt.lat, cvt.alt, cvt.coef,
    trans.dep = paste(trans.dep, collapse = " "), # Dependent variable transformation
    order,   # spline order
    n.sur,   # Number of surfaces
    err.wgt, # Number of relative error variances
    optimize,
    smooth,
    infile = glue("{basename}.dat"),
    nmax = round_any(nrow(dat), 5, f = ceiling),
    nchr.site,
    fmt,
    output, 
    "", # validation data fname
    ""  # nmax_valid
  )
  
  lapgrd <- listk(
    file.sur,
    n.surf = 0,
    type.surfCal = 1,
    file.cov,
    err.cov, 
    "", # Maximum standard errors
    grid.pos,
    pos.coord[1], # Index of first grid variable
    glue("{range[1]} {range[2]} {res}"),
    pos.coord[2], # Index of second grid variable
    glue("{range[3]} {range[4]} {res}"),

    # Mode of mask grid
    ifelse(type.mask, c(type.mask, file.mask), type.mask),

    type.alt, # Mode of the independent variable
    file.alt, # const, Independent variable grid is set to this constant or alt fname

    type.grd, 
    missing, # Special value of output grid
    glue("{names[pos.var]}.grd"),    # oupput grid file name 
    glue("({n.sur + 2}f{width}.2)"), # Output grid format

    # output error
    type.grd, # Mode of output error grids
    missing,
    glue("cov_{names[pos.var]}.grd"),
    glue("({n.sur + 2}f{width}.2)"),
    "",
    ""
  )
  list(data = dat, splina = splina, lapgrd = lapgrd)
}

#' ANUSPLIN writer
#'
#' Write the formatted data, splina and lapgrd configuration files to the same
#' directory.
#'
#' @param dat A formatted data.frame (data.table).
#' @param opt_splina A vector of splina parameters.
#' @param opt_lapgrd A vector of lapgrd parameters.
#' @param outdir The output directory.
#' @param na.width The length of the whitespace to fill in the missing value,
#' automatic calculation (**default**) or manual setting an integer.
#' @param exe Path to splina and lapgrd exe.
#' @param names Filenames of splina and lapgrd paramters, no path required.
#' @param cmd The filename of batch execution script, or no export (**default**).
#' 
#' @importFrom glue glue
#' @importFrom stringr str_match_all
#' @importFrom stats na.omit
#' 
#' @export
anusplin_write <- function(dat,
                           opt_splina,
                           opt_lapgrd,
                           outdir,
                           na.width = "auto",
                           names = c("splina.txt", "lapgrd.txt"), 
                           is.run = FALSE, ...) {
  width <- dat[, lapply(.SD, nchar)] %>%
    na.omit() %>% unique() %>% nrow()
  if (width != 1) stop("Data column width is not fixed.")

  na.width = guess_na_width(dat, opt_splina, na.width)  

  file <- opt_splina$infile #[[length(opt_splina) - 10]]
  write.table(dat, glue("{outdir}/{file}"),
    sep = "", col.names = F, row.names = F, quote = F,
    na = strrep(" ", na.width))

  writeLines_list(opt_splina, glue("{outdir}/{names[1]}"))
  writeLines_list(opt_lapgrd, glue("{outdir}/{names[2]}"))
  
  app_splina = system.file("exec/splina.exe", package = "spInterp")
  app_lapgrd = system.file("exec/lapgrd.exe", package = "spInterp")
  cmd <- c(
    glue("{app_splina} < {names[1]} > splina.log"),
    glue("{app_lapgrd} < {names[2]} > lapgrd.log")
  )
  
  f_cmd = glue("{outdir}/run.cmd") # cd
  writeLines(cmd, f_cmd)
  if (is.run) shell(f_cmd)
  # TODO: return fitted values
}

guess_na_width <- function(dat, opt_splina, na.width = "auto") {
  if (na.width == "auto") {
    na.col <- which(sapply(dat, anyNA)) %>% unname()
    if (length(na.col) > 0) {
      revid <- opt_splina$fmt %>% {
          str_match_all(., "([0-9]+)?[a-z]([0-9]+)")[[1]] # (3f7.2) --> 3f7
        } %>%
        apply(1, \(x) rep(as.numeric(x[3]), ifelse(is.na(x[2]), 1, as.numeric(x[2])))) %>%
        unlist()
      na.width <- revid[na.col] %>% unique()

      if (length(na.width) != 1) {
        stop(glue("The widths of columns containing NA must be unique, but widths are {na.width}"))
      }
    } else {
      na.width <- NULL
    }
  }
  na.width
}
