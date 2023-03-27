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
#' @param type.mask type of `file.mask`, one of c("none", "generic_grid", 
#' "arcinfo_grid", "idrisi_img")`, default `arcinfo_grid`.
#' @param file.mask Filename of mask grid, only valid if `type.mask` set to positive
#' integer.
#' 
#' @param type.alt type of grid, one of `c("const", "generic_grid", "arcinfo_grid", "idrisi_img")`, 
#' default `arcinfo_grid`
#' 
#' @param file.alt file path of alt
#' - `type.alt != "none"`: file.alt is file path
#' - `type.alt == "none"`: file.alt is a const real number
#' 
#' @param alt Type of elevation was treated, possible values are:
#' - `cov`  : considered as independent covariates (**default**)
#' - `spl`  : considered as independent spline variables
#' 
#' @param type.grd type of grid, one of `c("xyz", "generic_grid", "arcinfo_grid", "idrisi_img")`, 
#' default `arcinfo_grid`
#' 
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
#' @inheritSection query_grid_type grid_type
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
#' @importFrom terra rast values ext res
#'
#' @example R/examples/ex-anusplin.R
#' @export
anusplin_make_param <- function(
  dat, basename, 
  range, res = 0.25,

  order = 3L,
  unit = 0L,
  width = 9, missing = -9999L,
  file.alt = NULL, 

  alt = c("cov", "spl"),
  type.alt = "arcinfo_grid",
  type.grd = "arcinfo_grid",
  type.mask = "none", file.mask = NULL,
  
  # lim.lon = "auto", lim.lat = "auto", 
  # lim.alt = "auto",
  cvt.lon = c(0, 1), cvt.lat = c(0, 1), cvt.alt = c(1, 1),
  cvt.coef = 1000,
  
  trans.dep = 0,
  err.wgt = 0,
  optimize = 1,
  smooth = 1,
  
  err.cov = 2,
  grid.pos = 1,
  essential = TRUE) 
{
  type.alt  %<>% query_grid_type()
  type.grd  %<>% query_grid_type()
  type.mask %<>% query_grid_type()
  alt = match.arg(alt)

  # check arguments validation
  if (nchar(missing) > width) 
    stop("Missing values length should not be more than the width")
  
  if (is.null(file.alt)) {
    type.alt = NULL # const
  } else {
    if (!("alt" %in% names(dat))) stop("Please provide `alt` in dat!")
    if (is.character(file.alt)) file.alt %<>% normalizePath("/")
  }
  
  if (is.null(file.alt)) {
    if ("alt" %in% names(dat)) dat$alt <- NULL
    cvt.alt <- NULL
    cvt.coef <- NULL
  } else {
    r.dem <- rast(file.alt) %>% check_rast_grid(res, range)
    lim.alt <- c_range(dat$alt, values(r.dem)) %>% {c(floor(.[1]), ceiling(.[2]))}
    cvt.alt <- paste(c(lim.alt, cvt.alt), collapse = " ")
  }
  
  lim.lon = range[1:2]
  lim.lat = range[3:4]
  cvt.lon <- paste(c(lim.lon, cvt.lon), collapse = " ")
  cvt.lat <- paste(c(lim.lat, cvt.lat), collapse = " ")
  
  dat %<>% as.data.table()
  names <- colnames(dat)

  # n.sur <- length(names) - 2
  ind.spl <- 2
  ind.cov <- sur.spl <- sur.cov <- 0
  
  if ("site" %in% names) {
    # 请不要输入站点编号
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
  
  pos.coord <- match(c('lon', 'lat'), names)
  if (length(pos.coord) != 2) 
    stop("(lon, lat) should be in the colnames of dat")
  
  dat %<>% mutate(across(all_of(c("lon", "lat")), ~sprintf(glue("%{width}.2f"), .x)))
  
  if (!is.null(file.alt)) {
    # n.sur <- n.sur - 1
    dat$alt %<>% sprintf(fmt = glue("%{width}.1f"))
    if (alt == "cov") {
      ind.cov %<>% add(1L)
    } else if (alt == "spl") {
      ind.spl %<>% add(1L)
    }
  }
  
  pos.var <- which(!names %in% c("site", "lon", "lat", "alt"))
  n.sur <- length(pos.var)
  # dat[[pos.var]] %<>% map(~sprintf(glue("%{width}.2f"), .x))
  dat[, pos.var] <-
    dat[, lapply(.SD, sprintf, fmt = glue("%{width}.2f")), .SDcols = pos.var]

  file.sur <- glue("{basename}.sur")
  file.cov <- glue("{basename}.cov")
  
  if (essential) {
    output <- c("", "", file.sur, "", file.cov)
  } else {
    output <- c(
      glue("{basename}.res"), # large residual
      glue("{basename}.opt"), # optimisation parameters
      file.sur,               # surface coefficients
      glue("{basename}.lis"), # data list
      file.cov                # error covariance
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
    cvt.lon,
    cvt.lat,
    cvt.alt,
    cvt.coef,
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
  ) %>% set_class("param")
  
  lapgrd <- listk(
    file.sur,
    n.surf       = 0,
    type.surfCal = 1,
    file.cov,
    err.cov,
    "",           # Maximum standard errors
    grid.pos,
    # Index of first grid variable
    pos_lon      = order(pos.coord)[1],
    range_lon    = glue("{range[1]} {range[2]} {res}"),
    # Index of second grid variable
    pos_lat      = order(pos.coord)[2],
    range_lat    = glue("{range[3]} {range[4]} {res}"),
    
    # Mode of mask grid
    file.mask    = ifelse(type.mask, c(type.mask, file.mask), type.mask),
    
    type.alt, # Mode of the independent variable
    file.alt = file.alt, # const, Independent variable grid is set to this constant or alt fname
    
    type.grd     = type.grd,
    na           = if(is.null(file.alt)) NULL else missing,                     # Special value of output grid
    fout         = glue("{names[pos.var]}.grd"),    # oupput grid file name
    fmt          = glue("({n.sur + 2}f{width}.3)"), # Output grid format
    
    # output error
    type.err     = type.grd, # Mode of output error grids
    na_err       = if(is.null(file.alt)) NULL else missing,

    f_err        = glue("cov_{names[pos.var]}.grd"),
    fmt_err      = glue("({n.sur + 2}f{width}.3)"),
    "",
    ""
  ) %>% set_class("param")
  listk(data = dat, splina, lapgrd) %>% set_class(c("anusplin_param"))
}

#' ANUSPLIN write setting
#' 
#' Write the formatted data, splina and lapgrd configuration files to the same
#' directory.
#'
#' @param outdir The output directory.
#' @param dat A formatted data.frame (data.table), or a list contains `dat`, 
#' `opt_splina` and `opt_lapgrd`
#' @param opt_splina A list of splina parameters.
#' @param opt_lapgrd A list of lapgrd parameters.
#' @param na.width The length of the whitespace to fill in the missing value,
#' automatic calculation (**default**) or manual setting an integer.
#' @param names Filenames of splina and lapgrd paramters, no path required.
#' @param is.run If `TRUE`, run the command.
#'
#' @importFrom glue glue
#' @importFrom stringr str_match_all
#' @importFrom stats na.omit
#' @importFrom data.table fread as.data.table
#' @importFrom terra rast
#' @importFrom purrr imap reduce
#' @importFrom tools file_path_sans_ext
#' @export
anusplin_write_setting <- function(
  param, outdir, ..., na.width = "auto", 
  is.run = FALSE, overwrite = FALSE) 
{
  mkdir(outdir)
  opt_splina <- param$splina
  opt_lapgrd <- param$lapgrd
  dat <- param$data
  
  width <- dat[, lapply(.SD, nchar)] %>%
    na.omit() %>% unique() %>% nrow()
  if (width != 1) stop("Data column width is not fixed.")
  
  na.width = guess_na_width(dat, opt_splina, na.width)
  
  file <- opt_splina$infile #[[length(opt_splina) - 10]]
  write.table(data.frame(dat), glue("{outdir}/{file}"),
              sep = "", col.names = F, row.names = F, quote = F,
              na = strrep(" ", na.width))

  f_pars = c("param_splina.txt", "param_lapgrd.txt")
  writeLines_list(opt_splina, glue("{outdir}/{f_pars[1]}"))
  writeLines_list(opt_lapgrd, glue("{outdir}/{f_pars[2]}"))
  
  app_splina = system.file("exec/splina.exe", package = "spInterp")
  app_lapgrd = system.file("exec/lapgrd.exe", package = "spInterp")
  cmd <- c(
    # "@echo off",
    glue("splina.exe < {f_pars[1]} > splina.log"),
    glue("lapgrd.exe < {f_pars[2]} > lapgrd.log")
    # glue("{app_splina} < {f_pars[1]} > splina.log"),
    # glue("{app_lapgrd} < {f_pars[2]} > lapgrd.log")
  )
  f_cmd = glue("{outdir}/run.cmd")
  writeLines(cmd, f_cmd)
  
  if (is.run) {
    add_path_anusplin()
    if (overwrite) anusplin_rm_cache(outdir)
    glue("pushd {outdir} && run.cmd") %>% shell(translate = T)
  }
}

#' @export
anusplin_rm_cache <- function(outdir = "output") {
  # pattern = param$splina$basename %>% basename() %>% paste0("^", .)
  pattern = c("cov", "grd", "sur", "log") %>% # , "dat"
    paste(".", ., "$", sep = "", collapse = "|")
  fs = dir(outdir, pattern, full.names = TRUE)
  file.remove(fs)
}

#' @export
anusplin_read_output <- function(opt_lapgrd, outdir) {
  # return fitted values
  f_out <- opt_lapgrd$fout
  f_out_full <- paste(outdir, f_out, sep = "/")

  if (!all(file.exists(f_out_full))) {
    stop("The fitted result is not as expected, check whether the parameters are wrong.")
  }

  ## TODO: can be improved later
  rast(f_out_full) %>% as.data.frame(xy = T) %>% data.table()
}
