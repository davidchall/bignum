#' Formatting
#'
#' @param x A [`biginteger`] or [`bigfloat`] vector.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @name bignum-format
NULL

#' @rdname bignum-format
#' @export
format.bignum_biginteger <- function(x, ...) {
  c_biginteger_format(x)
}

#' @rdname bignum-format
#' @export
format.bignum_bigfloat <- function(x, ...) {
  c_bigfloat_format(x)
}