#' Comparison Operations
#'
#' @description
#' [`biginteger`] and [`bigfloat`] vectors support the standard
#' [comparison operations][Comparison].
#'
#' ```
#' x < y
#' x > y
#' x <= y
#' x >= y
#' x == y
#' x != y
#' ```
#'
#' @inheritSection bignum-arith Recycling rules
#' @return A logical vector.
#' @name bignum-compare
NULL

# comparison operations --------------------------------------------------------

#' @export
`<=.bignum_vctr` <- function(e1, e2) {
  vec_compare_bignum(e1, e2) <= 0
}

#' @export
`<.bignum_vctr` <- function(e1, e2) {
  vec_compare_bignum(e1, e2) < 0
}

#' @export
`>=.bignum_vctr` <- function(e1, e2) {
  vec_compare_bignum(e1, e2) >= 0
}

#' @export
`>.bignum_vctr` <- function(e1, e2) {
  vec_compare_bignum(e1, e2) > 0
}


# bignum implementations -------------------------------------------------------

vec_compare_bignum <- function(x, y, na_equal = FALSE) {
  vec_assert(x)
  vec_assert(y)
  vec_assert(na_equal, ptype = logical(), size = 1L)

  args <- vec_recycle_common(x, y)
  args <- vec_cast_common(!!!args)

  vec_compare_impl(args[[1]], args[[2]], na_equal)
}

vec_compare_impl <- function(x, y, na_equal = FALSE) {
  UseMethod("vec_compare_impl")
}

vec_compare_impl.default <- function(x, y, na_equal = FALSE) {
  type <- vec_ptype_full(x)
  abort(paste0("vec_compare_impl not implemented for <", type, ">"))
}


# biginteger -------------------------------------------------------------------

#' @export
vec_proxy_compare.bignum_biginteger <- function(x, ...) {
  stop_unsupported(x, "vec_proxy_compare") # nocov
}

vec_compare_impl.bignum_biginteger <- function(x, y, na_equal = FALSE) {
  c_biginteger_compare(x, y, na_equal)
}

#' @export
vec_proxy_order.bignum_biginteger <- function(x, ...) {
  c_biginteger_rank(x)
}


# bigfloat ---------------------------------------------------------------------

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
