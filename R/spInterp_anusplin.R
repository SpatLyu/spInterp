#' spInterp_anusplin
#' 
#' @param ... others to [anusplin_make_param()]
#' @export 
spInterp_anusplin <- function(X, Y, range, res = 1, outdir = "output", 
  prefix = "RH", 
  file.alt = NULL, 
  overwrite = TRUE, ..., 
  Z = NULL)
{
  if (overwrite) anusplin_rm_cache(outdir)
  dat = cbind(as.data.table(X), Y)
  
  anusplin_make_param(dat, prefix, range, res, file.alt = file.alt, ...) -> param
  anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
  anusplin_read_output(param$lapgrd, outdir) -> ans
  
  list(coord = ans[, 1:2], predicted = ans[, -(1:2)]) %>% set_class("spInterp")
}
