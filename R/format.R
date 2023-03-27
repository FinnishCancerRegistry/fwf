fwf_format <- function(x, width) {
  stopifnot(
    length(width) == 1L,
    width >= 0L,
    width %% 1L == 0L,
    width >= max(nchar(x))
  )
  UseMethod("fwf_format")
}
fwf_format.integer <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
fwf_format.numeric <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
fwf_format.character <- function(x, width) {
  formatC(x = x, width = width, flag = " ")
}
fwf_format.default <- function(x, width) {
  fwf_format(as.character(x), width)
}
