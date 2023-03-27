
max_nchar <- function(x) {
  UseMethod("max_nchar")
}
#' @export
max_nchar.integer <- function(x) {
  max(nchar(c(min(x), max(x))))
}
#' @export
max_nchar.default <- function(x) {
  indices <- which(!duplicated(x))
  out <- 0L
  for (i in indices) {
    nc_i <- nchar(x[i])
    if (nc_i > out) {
      out <- nc_i
    }
  }
  return(out)
}