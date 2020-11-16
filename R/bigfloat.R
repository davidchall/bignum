#' High-Precision Numeric Vectors
#'
#' Creates or tests for high-precision floating-point numeric vectors.
#'
#' @inheritParams biginteger
#'
#' @examples
#' bigfloat(1 / 3)
#' bigfloat(1) / 3
#' @seealso
#' [`NA_bigfloat_`] to represent missing values.
#'
#' [`format()`][format.bignum_bigfloat()] for pretty printing.
#' @name bigfloat
NULL

#' Internal constructor for bigfloat
#'
#' @param x Character vector for conversion
#' @param cxx Boolean specifying whether to pass data through C++ functions.
#'   Set to `FALSE` for namespace export of constants.
#' @noRd
new_bigfloat <- function(x = character(), cxx = TRUE) {
  vec_assert(x, character())

  if (cxx) {
    c_bigfloat(x)
  } else {
    new_vctr(x, class = c("bignum_bigfloat", "bignum_vctr")) # nocov
  }
}

#' @rdname bigfloat
#' @export
bigfloat <- function(...) {
  ellipsis::check_dots_unnamed()
  as_bigfloat(as.character(c(...)))
}

#' @rdname bigfloat
#' @export
as_bigfloat <- function(x) {
  UseMethod("as_bigfloat")
}

#' @rdname bigfloat
#' @export
is_bigfloat <- function(x) {
  inherits(x, "bignum_bigfloat")
}

#' @export
vec_ptype_full.bignum_bigfloat <- function(x, ...) {
  "bigfloat"
}

#' @export
vec_ptype_abbr.bignum_bigfloat <- function(x, ...) {
  "bigflt"
}


# Coerce -----------------------------------------------------------------------

#' @export
vec_ptype2.bignum_bigfloat.bignum_bigfloat <- function(x, y, ...) x

#' @export
vec_ptype2.bignum_bigfloat.logical <- function(x, y, ...) x
#' @export
vec_ptype2.logical.bignum_bigfloat <- function(x, y, ...) y

#' @export
vec_ptype2.bignum_bigfloat.integer <- function(x, y, ...) x
#' @export
vec_ptype2.integer.bignum_bigfloat <- function(x, y, ...) y

#' @export
vec_ptype2.bignum_bigfloat.double <- function(x, y, ...) x
#' @export
vec_ptype2.double.bignum_bigfloat <- function(x, y, ...) y

#' @export
vec_ptype2.bignum_bigfloat.bignum_biginteger <- function(x, y, ...) x
#' @export
vec_ptype2.bignum_biginteger.bignum_bigfloat <- function(x, y, ...) y


# Cast -------------------------------------------------------------------------

#' @export
vec_cast.bignum_bigfloat.bignum_bigfloat <- function(x, to, ...) {
  x
}

#' @export
vec_cast.bignum_bigfloat.logical <- function(x, to, ...) {
  new_bigfloat(as.character(as.integer(x)))
}

#' @export
vec_cast.logical.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_bigfloat_to_logical(x)
  lossy <- !vec_in(x, vec_c(0, 1, NA_bigfloat_, NaN))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.integer <- function(x, to, ...) {
  new_bigfloat(as.character(x))
}

#' @export
vec_cast.integer.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_bigfloat_to_integer(x)
  lossy <- xor(is.na(x), is.na(out))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.double <- function(x, to, ...) {
  new_bigfloat(as.character(x))
}

#' @export
vec_cast.double.bignum_bigfloat <- function(x, to, ...,  x_arg = "", to_arg = "") {
  out <- c_bigfloat_to_double(x)
  out_compare <- vec_cast(out, bigfloat())
  x_na <- is.na(x)
  lossy <- (out_compare != x & !x_na) | xor(x_na, is.na(out))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.bignum_biginteger <- function(x, to, ...) {
  new_bigfloat(vec_data(x))
}

#' @export
vec_cast.bignum_biginteger.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  rounded <- c_bigfloat_floor(x)
  out <- new_biginteger(vec_data(rounded))
  lossy <- rounded != x & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.character <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.character.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
as.logical.bignum_bigfloat <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, logical()))
}

#' @export
as.integer.bignum_bigfloat <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, integer()))
}

#' @export
as.double.bignum_bigfloat <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, double()))
}

#' @export
as.character.bignum_bigfloat <- function(x, ...) {
  format(x)
}

#' @export
as_bigfloat.default <- function(x) {
  vec_cast(x, new_bigfloat())
}

#' @export
as_bigfloat.character <- function(x) {
  new_bigfloat(x)
}


# Comparison operations --------------------------------------------------------

#' @export
vec_proxy_compare.bignum_bigfloat <- function(x, ...) {
  stop_unsupported(x, "vec_proxy_compare") # nocov
}

vec_compare_impl.bignum_bigfloat <- function(x, y, na_equal = FALSE) {
  c_bigfloat_compare(x, y, na_equal)
}

#' @export
vec_proxy_order.bignum_bigfloat <- function(x, ...) {
  c_bigfloat_rank(x)
}

#' @export
is.na.bignum_bigfloat <- function(x) {
  is.na(c_bigfloat_to_double(x))
}
