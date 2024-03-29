#' Arbitrary-Precision Integer Vectors
#'
#' @description
#' `biginteger()` and `as_biginteger()` construct a vector designed to store
#' _any_ integer.
#'
#' `is_biginteger()` checks if an object is of class `bignum_biginteger`.
#'
#' @param x Object to be coerced or tested.
#' @return An S3 vector of class `bignum_biginteger`.
#'
#' @examples
#' # default options limit displayed precision
#' biginteger(2)^50L
#'
#' # display full precision
#' format(biginteger(2)^50L, notation = "dec")
#'
#' # lossy casts raise a warning
#' biginteger(c(2, 2.5, 3))
#'
#' # suppress warnings if they are expected
#' suppressWarnings(biginteger(c(2, 2.5, 3)))
#'
#' # unsigned integers can be specified as hexadecimal
#' biginteger("0xffffffff")
#' @seealso
#' [`NA_biginteger_`] to represent missing values.
#'
#' [`format()`][format.bignum_biginteger()] for pretty printing.
#'
#' `vignette("operations")` for supported operations.
#' @name biginteger
NULL

#' Internal constructor for biginteger
#'
#' @param x Character vector for conversion
#' @param cxx Boolean specifying whether to pass data through C++ functions.
#'   Set to `FALSE` for namespace export of constants.
#' @noRd
new_biginteger <- function(x = character(), cxx = TRUE) {
  vec_assert(x, character())

  if (cxx) {
    c_biginteger(x)
  } else {
    new_vctr(x, class = c("bignum_biginteger", "bignum_vctr")) # nocov
  }
}

#' @rdname biginteger
#' @export
biginteger <- function(x = character()) {
  as_biginteger(x)
}

#' @rdname biginteger
#' @export
as_biginteger <- function(x) {
  UseMethod("as_biginteger")
}

#' @rdname biginteger
#' @export
is_biginteger <- function(x) {
  inherits(x, "bignum_biginteger")
}

#' @export
vec_ptype_full.bignum_biginteger <- function(x, ...) {
  "biginteger"
}

#' @export
vec_ptype_abbr.bignum_biginteger <- function(x, ...) {
  "bigint"
}


# Coerce -----------------------------------------------------------------------

#' @export
vec_ptype2.bignum_biginteger.bignum_biginteger <- function(x, y, ...) x

#' @export
vec_ptype2.bignum_biginteger.logical <- function(x, y, ...) x
#' @export
vec_ptype2.logical.bignum_biginteger <- function(x, y, ...) y

#' @export
vec_ptype2.bignum_biginteger.integer <- function(x, y, ...) x
#' @export
vec_ptype2.integer.bignum_biginteger <- function(x, y, ...) y

#' @export
vec_ptype2.bignum_biginteger.double <- function(x, y, ...) x
#' @export
vec_ptype2.double.bignum_biginteger <- function(x, y, ...) y


# Cast -------------------------------------------------------------------------

#' @export
vec_cast.bignum_biginteger.bignum_biginteger <- function(x, to, ...) {
  x
}

#' @export
vec_cast.bignum_biginteger.logical <- function(x, to, ...) {
  new_biginteger(as.character(as.integer(x)))
}

#' @export
vec_cast.logical.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_biginteger_to_logical(x)
  lossy <- !vec_in(x, vec_c(0, 1, NA_biginteger_))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.integer <- function(x, to, ...) {
  new_biginteger(as.character(x))
}

#' @export
vec_cast.integer.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_biginteger_to_integer(x)
  lossy <- abs(x) >= biginteger(2^31) & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  x_big <- vec_cast(x, new_bigfloat())
  rounded <- trunc(x_big)
  out <- new_biginteger(vec_data(rounded))
  lossy <- (rounded != x_big & !is.na(x)) | is.infinite(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.double.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_biginteger_to_double(x)
  lossy <- abs(x) >= biginteger(2)^53L & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  rounded <- trunc(x)
  out <- new_biginteger(vec_data(rounded))
  lossy <- (rounded != x & !is.na(x)) | is.infinite(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.character <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.character.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
as.logical.bignum_biginteger <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, logical()))
}

#' @export
as.integer.bignum_biginteger <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, integer()))
}

#' @export
as.double.bignum_biginteger <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, double()))
}

#' @export
as.character.bignum_biginteger <- function(x, ...) {
  format(x)
}

#' @export
as_biginteger.default <- function(x) {
  warn_on_lossy_cast(vec_cast(x, new_biginteger()))
}

#' @export
as_biginteger.character <- function(x) {
  new_biginteger(x)
}
