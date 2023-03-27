fwf_format <- function(x, width) {
  stopifnot(
    length(width) == 1L,
    width >= 0L,
    is.integer(width),
    !is.na(width)
  )
  UseMethod("fwf_format")
}
#' @export
fwf_format.integer <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
#' @export
fwf_format.numeric <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
#' @export
fwf_format.character <- function(x, width) {
  formatC(x = x, width = width, flag = " ")
}
#' @export
fwf_format.default <- function(x, width) {
  fwf_format(as.character(x), width)
}
