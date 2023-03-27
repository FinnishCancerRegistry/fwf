#' @title Fixed Width Format
#' @description
#' Read and write fixed-width format files.
#' @name fwf
NULL

#' @rdname fwf
#' @param x `[data.frame]` (no default)
#' 
#' `data.frame` to write to disk.
#' 
#' @param path `[character]` (no default)
#' 
#' - `fwf_write`: Path to write to.
#' - `fwf_read`: Path to read from.
#' 
#' @param widths `[NULL, integer]` (default `NULL`)
#' 
#' - `NULL`: the width of each field will be `max(nchar(x[[i]]))` for all
#'   `i in 1:ncol(x)`
#' - `integer`: use these widths; must be of length `ncol(x)`
#' 
#' @param fwrite_arg_list `[NULL]` (default `NULL`)
#' 
#' - `NULL`: No additional arguments passed to [data.table::fwrite].
#' - `list`: Pass these additional arguments passed to [data.table::fwrite].
#' 
#' [data.table::fwrite] args `x` and `file` always set to args `x` and `path`
#' of this function.
#' @export
fwf_write <- function(x, path, widths = NULL, fwrite_arg_list = NULL) {
  # @codedoc_comment_block news("fwf::fwf_write", "2023-03-27", "0.1.0")
  # Fun `fwf_write` included in first version of this package.
  # @codedoc_comment_block news("fwf::fwf_write", "2023-03-27", "0.1.0")
  stopifnot(
    is.data.frame(x)
  )
  stopifnot(
    is.character(path),
    length(path) == 1,
    !is.na(path)
  )
  stopifnot(
    is.null(widths) || (is.integer(widths) && length(widths) == ncol(x))
  )
  stopifnot(
    is.null(fwrite_arg_list) || inherits(fwrite_arg_list, "list")
  )

  if (is.null(widths)) {
    widths <- vapply(x, function(col) max(nchar(col)), integer(1L))
  }
  
  fwf_dt <- data.table::setDT(lapply(seq_along(widths), function(j) {
    fwf_format(x[[j]], width = widths[j])
  }))
  data.table::setnames(fwf_dt, names(fwf_dt), names(x))
  fwrite_arg_list <- as.list(fwrite_arg_list)
  fwrite_arg_list[c("x", "file")] <- list(fwf_dt, path)
  do.call(data.table::fwrite, fwrite_arg_list, quote = TRUE)
}

#' @rdname fwf
#' @param fread_arg_list `[NULL, list]` (default `NULL`)
#' 
#' - `NULL`: No additional arguments passed to [data.table::fread].
#' - `list`: Pass these additional arguments passed to [data.table::fread].
#' 
#' [data.table::fread] arg `file` is always set to arg `path` of this function.
#' @export
fwf_read <- function(path, fread_arg_list = NULL) {
  # @codedoc_comment_block news("fwf::fwf_read", "2023-03-27", "0.1.0")
  # Fun `fwf_read` included in first version of this package.
  # @codedoc_comment_block news("fwf::fwf_read", "2023-03-27", "0.1.0")
  stopifnot(
    is.character(path),
    !is.na(path),
    length(path) == 1L,
    file.exists(path),
    !dir.exists(path)
  )
  stopifnot(
    is.null(fread_arg_list) || inherits(fread_arg_list, "list")
  )
  fread_arg_list <- as.list(fread_arg_list)
  fread_arg_list[["file"]] <- path
  do.call(data.table::fread, fread_arg_list, quote = TRUE)
}
