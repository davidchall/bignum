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
