#' query_grid_type
#' 
#' @section grid_type:
#' - `none`         : not provided
#' - `xyz`          : grid written in X,Y,Z format
#' - `generic_grid` : generic grid written by rows
#' - `arcinfo_grid` : Arc/Info grid
#' - `idrisi_img`   : Idrisi image
#' 
#' @importFrom dplyr tribble
#' @importFrom data.table data.table
query_grid_type <- function(name = "arcinfo_grid") {
  db <- tribble(
    ~code, ~type,
    0, "none",
    0, "xyz",
    0, "const",
    1, "generic_grid",
    2, "arcinfo_grid",
    3, "idrisi_img"
  ) %>% data.table()
  db[type == name, code]
}

#' @importFrom terra res ext
check_rast_grid <- function(r, res, range) {
  if (!all(res(r) == res, as.vector(ext(r)) == range)) {
    stop("Dimensions of rast and `range`, `res` are mismatch!")
  }
  r
}

#' @export
print.param <- function(x, ...) {
  str(x)
  invisible()
}

guess_na_width <- function(dat, opt_splina, na.width = "auto") {
  if (na.width == "auto") {
    na.col <- which(sapply(dat, anyNA)) %>% unname()
    if (length(na.col) > 0) {
      revid <- opt_splina$fmt %>% {
        stringr::str_match_all(., "([0-9]+)?[a-z]([0-9]+)")[[1]] # (3f7.2) --> 3f7
      } %>%
        apply(1, function(x) rep(as.numeric(x[3]), ifelse(is.na(x[2]), 1, as.numeric(x[2])))) %>%
        unlist()
      na.width <- revid[na.col] %>% unique()
      
      if (length(na.width) != 1) {
        stop(glue("The widths of columns containing NA must be unique, but widths are {na.width}"))
      }
    } else {
      na.width <- 0
    }
  }
  na.width
}

read_xyz_multi <- function(fs) {
  map2(
    fs, file_path_sans_ext(basename(fs)),
    ~ fread(.x, col.names = c("x", "y", .y))
  ) %>%
    reduce(merge)
}

# - `Iw`   : 输出整数，w表示输出的字段宽度；
# - `Fw.d` : 输出实数，w表示输出的字段宽度，d表示小数点后的位数；
# - `Ew.d` : 按科学计数法输出实数，w表示输出的字段宽度，d表示指数部分的位数；
# - `A`    : 输出字符型变量；
# - `Lw`   : 输出逻辑型变量，w表示输出的字段宽度；
# - `Gw.d` : 输出实数，根据大小选择Fw.d或Ew.d。

#' @export 
write_dem <- function(r, outfile = "output/dem.asc", digits = 1) {
  raster::writeRaster(raster::raster(r) %>% round(digits),
  # terra::writeRaster(r %>% round(1),
    outfile,
    # datatype = "INT4S",
    NAflag = -9999L,
    overwrite = TRUE
  )
}
