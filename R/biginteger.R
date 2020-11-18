#' Arbitrary-Precision Integer Vectors
#'
#' Creates or tests for arbitrary-precision integer vectors.
#'
#' @param ... Numeric or character vectors.
#' @param x Object to be coerced or tested.
#'
#' @seealso
#' [`NA_biginteger_`] to represent missing values.
#'
#' [`format()`][format.bignum_biginteger()] for pretty printing.
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
biginteger <- function(...) {
  ellipsis::check_dots_unnamed()
  as_biginteger(as.character(c(...)))
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
  lossy <- abs(x) >= biginteger(2)^31L & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.double <- function(x, to, ...,  x_arg = "", to_arg = "") {
  out <- new_biginteger(as.character(x))
  lossy <- floor(x) != x & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.double.bignum_biginteger <- function(x, to, ...,  x_arg = "", to_arg = "") {
  out <- c_biginteger_to_double(x)
  lossy <- abs(x) >= biginteger(2)^53L & !is.na(x)
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
