#' Arbitrary-Precision Integer Vectors
#'
#' Creates or tests for arbitrary-precision integer vectors.
#'
#' @param x Object to be coerced or tested
#' @param ... Further arguments passed to or from other methods
#'
#' @seealso [bigfloat()], [integer()]
#' @name biginteger
NULL

new_biginteger <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("bignum_biginteger", "bignum_vctr"))
}

#' @rdname biginteger
#' @export
biginteger <- function(x = character()) {
  as_biginteger(x)
}

#' @rdname biginteger
#' @export
as_biginteger <- function(x, ...) {
  UseMethod("as_biginteger")
}

#' @rdname biginteger
#' @export
is_biginteger <- function(x) {
  inherits(x, "bignum_biginteger")
}

#' @rdname biginteger
#' @export
format.bignum_biginteger <- function(x, ...) {
  c_biginteger_format(x)
}

#' @export
vec_ptype_full.bignum_biginteger <- function(x, ...) {
  "biginteger"
}

#' @export
vec_ptype_abbr.bignum_biginteger <- function(x, ...) {
  "bigint"
}

#' @description The `NA_biginteger_` constant supports missing values.
#' @format NULL
#' @rdname biginteger
#' @export
NA_biginteger_ <- new_biginteger(NA_character_)


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
  c_integer_to_biginteger(as.integer(x))
}

#' @export
vec_cast.logical.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_biginteger_to_logical(x)
  lossy <- !vec_in(x, vec_c(0, 1, NA_biginteger_))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.integer <- function(x, to, ...) {
  c_integer_to_biginteger(x)
}

#' @export
vec_cast.integer.bignum_biginteger <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_biginteger_to_integer(x)
  lossy <- abs(x) >= biginteger(2)^31L & !is.na(x)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_biginteger.double <- function(x, to, ...,  x_arg = "", to_arg = "") {
  out <- c_double_to_biginteger(x)
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
as_biginteger.default <- function(x, ...) {
  warn_on_lossy_cast(vec_cast(x, biginteger()))
}

#' @export
as_biginteger.character <- function(x, ...) {
  c_character_to_biginteger(x)
}


# Comparison operations --------------------------------------------------------

#' @export
vec_proxy_compare.bignum_biginteger <- function(x, ...) {
  abort("vec_proxy_compare is not implemented for vectors of type <biginteger>.")
}

vec_compare_impl.bignum_biginteger <- function(x, y, na_equal = FALSE) {
  c_biginteger_compare(x, y, na_equal)
}

#' @export
vec_proxy_order.bignum_biginteger <- function(x, ...) {
  c_biginteger_rank(x)
}


# Arithmetic operations --------------------------------------------------------

#' @method vec_arith bignum_biginteger
#' @export
#' @export vec_arith.bignum_biginteger
#' @rdname bignum-vctrs
vec_arith.bignum_biginteger <- function(op, x, y, ...) {
  UseMethod("vec_arith.bignum_biginteger", y)
}

#' @method vec_arith.bignum_biginteger default
#' @export
vec_arith.bignum_biginteger.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.bignum_biginteger bignum_biginteger
#' @export
vec_arith.bignum_biginteger.bignum_biginteger <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_biginteger_add(x, y),
    "-" = c_biginteger_subtract(x, y),
    "*" = c_biginteger_multiply(x, y),
    "/" = c_bigfloat_divide(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "^" = c_biginteger_pow(x, vec_cast(y, integer())),
    "%%" = c_biginteger_remainder(x, y),
    "%/%" = c_biginteger_quotient(x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_biginteger integer
#' @export
vec_arith.bignum_biginteger.integer <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_biginteger_add(x, vec_cast(y, biginteger())),
    "-" = c_biginteger_subtract(x, vec_cast(y, biginteger())),
    "*" = c_biginteger_multiply(x, vec_cast(y, biginteger())),
    "/" = c_bigfloat_divide(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "^" = c_biginteger_pow(x, y),
    "%%" = c_biginteger_remainder(x, vec_cast(y, biginteger())),
    "%/%" = c_biginteger_quotient(x, vec_cast(y, biginteger())),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_biginteger double
#' @export
vec_arith.bignum_biginteger.double <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "-" = c_bigfloat_subtract(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "*" = c_bigfloat_multiply(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "/" = c_bigfloat_divide(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "^" = c_bigfloat_pow(vec_cast(x, bigfloat()), vec_cast(y, bigfloat())),
    "%%" = c_biginteger_remainder(x, vec_cast(y, biginteger())),
    "%/%" = c_biginteger_quotient(x, vec_cast(y, biginteger())),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_biginteger MISSING
#' @export
vec_arith.bignum_biginteger.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = x * -1L,
    "+" = x,
    stop_incompatible_op(op, x, y)
  )
}


# Mathematical operations ------------------------------------------------------

#' @export
vec_math.bignum_biginteger <- function(.fn, .x, ..., na.rm = FALSE) {
  switch(
    .fn,

    # Summary group
    prod = c_biginteger_prod(.x, na.rm),
    sum = c_biginteger_sum(.x, na.rm),

    # Math group
    abs = c_biginteger_abs(.x),
    sign = c_biginteger_sign(.x),
    sqrt = sqrt(vec_cast(.x, bigfloat())),
    ceiling = .x,
    floor = .x,
    trunc = .x,
    cumsum = c_biginteger_cumsum(.x),
    cumprod = c_biginteger_cumprod(.x),
    cummax = c_biginteger_cummax(.x),
    cummin = c_biginteger_cummin(.x),
    exp = exp(vec_cast(.x, bigfloat())),
    expm1 = expm1(vec_cast(.x, bigfloat())),
    log = log(vec_cast(.x, bigfloat())),
    log10 = log10(vec_cast(.x, bigfloat())),
    log2 = log2(vec_cast(.x, bigfloat())),
    log1p = log1p(vec_cast(.x, bigfloat())),
    cos = cos(vec_cast(.x, bigfloat())),
    cosh = cosh(vec_cast(.x, bigfloat())),
    cospi = cospi(vec_cast(.x, bigfloat())),
    sin = sin(vec_cast(.x, bigfloat())),
    sinh = sinh(vec_cast(.x, bigfloat())),
    sinpi = sinpi(vec_cast(.x, bigfloat())),
    tan = tan(vec_cast(.x, bigfloat())),
    tanh = tanh(vec_cast(.x, bigfloat())),
    tanpi = tanpi(vec_cast(.x, bigfloat())),
    acos = acos(vec_cast(.x, bigfloat())),
    acosh = acosh(vec_cast(.x, bigfloat())),
    asin = asin(vec_cast(.x, bigfloat())),
    asinh = asinh(vec_cast(.x, bigfloat())),
    atan = atan(vec_cast(.x, bigfloat())),
    atanh = atanh(vec_cast(.x, bigfloat())),
    gamma = gamma(vec_cast(.x, bigfloat())),
    lgamma = lgamma(vec_cast(.x, bigfloat())),

    # Other
    mean = c_biginteger_sum(.x, na.rm) / sum(!is.na(.x)),
    is.nan = FALSE,
    is.finite = !is.na(.x),
    is.infinite = FALSE,

    stop_unsupported(.x, .fn)
  )
}
