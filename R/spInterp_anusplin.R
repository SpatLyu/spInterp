#' @rdname spInterp
#' @export 
spInterp_anusplin <- function(X, Y, range, outdir = "output", 
  prefix = "RH", 
  file.alt = NULL, 
  overwrite = TRUE, ...)
{
  if (overwrite) anusplin_rm_cache(outdir)
  dat = cbind(as.data.table(X), Y)

  anusplin_make_param(dat, prefix, range, file.alt = file.alt) -> param
  anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
  anusplin_read_output(param$lapgrd, outdir) -> res
  
  list(coord = res[, 1:2], predicted = res[, -(1:2)]) %>% set_class("spInterp")
}
