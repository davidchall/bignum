#' High-Precision Numeric Vectors
#'
#' Creates or tests for high-precision floating-point numeric vectors.
#'
#' @inheritParams biginteger
#'
#' @examples
#' bigfloat(1 / 3)
#' bigfloat(1) / 3
#' @seealso [biginteger()], [double()]
#' @name bigfloat
NULL

new_bigfloat <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("bignum_bigfloat", "bignum_vctr"))
}

#' @rdname bigfloat
#' @export
bigfloat <- function(x = character()) {
  as_bigfloat(x)
}

#' @rdname bigfloat
#' @export
as_bigfloat <- function(x, ...) {
  UseMethod("as_bigfloat")
}

#' @rdname bigfloat
#' @export
is_bigfloat <- function(x) {
  inherits(x, "bignum_bigfloat")
}

#' @rdname bigfloat
#' @export
format.bignum_bigfloat <- function(x, scientific = FALSE, ...) {
  if (!is_bool(scientific)) {
    abort("`scientific` must be TRUE or FALSE.")
  }

  c_bigfloat_format(x, scientific)
}

#' @export
vec_ptype_full.bignum_bigfloat <- function(x, ...) {
  "bigfloat"
}

#' @export
vec_ptype_abbr.bignum_bigfloat <- function(x, ...) {
  "bigflt"
}

#' @description The `NA_bigfloat_` constant supports missing values.
#' @format NULL
#' @rdname bigfloat
#' @export
NA_bigfloat_ <- new_bigfloat(NA_character_)

#' @description The `bigpi` constant is a higher precision version of [`pi`].
#' @format NULL
#' @rdname bigfloat
#' @export
bigpi <- new_bigfloat("3.14159265358979323846264338327950288419716939937510")


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
  c_integer_to_bigfloat(as.integer(x))
}

#' @export
vec_cast.logical.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_bigfloat_to_logical(x)
  lossy <- !vec_in(x, vec_c(0, 1, NA_bigfloat_, NaN))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.integer <- function(x, to, ...) {
  c_integer_to_bigfloat(x)
}

#' @export
vec_cast.integer.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- c_bigfloat_to_integer(x)
  lossy <- xor(is.na(x), is.na(out))
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_cast.bignum_bigfloat.double <- function(x, to, ...) {
  c_double_to_bigfloat(x)
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
  c_character_to_bigfloat(c_biginteger_format(x))
}

#' @export
vec_cast.bignum_biginteger.bignum_bigfloat <- function(x, to, ..., x_arg = "", to_arg = "") {
  floored <- c_bigfloat_floor(x)
  out <- as_biginteger(unclass(floored))
  lossy <- floored != x & !is.na(x)
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
  vec_cast(x, logical())
}

#' @export
as.integer.bignum_bigfloat <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
as.double.bignum_bigfloat <- function(x, ...) {
  vec_cast(x, double())
}

#' @export
as.character.bignum_bigfloat <- function(x, ...) {
  format(x)
}

#' @export
as_bigfloat.default <- function(x, ...) {
  vec_cast(x, bigfloat())
}

#' @export
as_bigfloat.character <- function(x, ...) {
  c_character_to_bigfloat(x)
}


# Comparison operations --------------------------------------------------------

#' @export
vec_proxy_compare.bignum_bigfloat <- function(x, ...) {
  abort("vec_proxy_compare is not implemented for vectors of type <bigfloat>.")
}

vec_compare_impl.bignum_bigfloat <- function(x, y, na_equal = FALSE) {
  c_bigfloat_compare(x, y, na_equal)
}

#' @export
vec_proxy_order.bignum_bigfloat <- function(x, ...) {
  c_bigfloat_rank(x)
}


# Arithmetic operations --------------------------------------------------------

#' @method vec_arith bignum_bigfloat
#' @export
#' @export vec_arith.bignum_bigfloat
#' @rdname bignum-vctrs
vec_arith.bignum_bigfloat <- function(op, x, y, ...) {
  UseMethod("vec_arith.bignum_bigfloat", y)
}

#' @method vec_arith.bignum_bigfloat default
#' @export
vec_arith.bignum_bigfloat.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.bignum_bigfloat bignum_bigfloat
#' @export
vec_arith.bignum_bigfloat.bignum_bigfloat <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(x, y),
    "-" = c_bigfloat_subtract(x, y),
    "*" = c_bigfloat_multiply(x, y),
    "/" = c_bigfloat_divide(x, y),
    "^" = c_bigfloat_pow(x, y),
    "%%" = c_bigfloat_remainder(x, y),
    "%/%" = c_bigfloat_quotient(x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_bigfloat numeric
#' @export
vec_arith.bignum_bigfloat.numeric <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(x, vec_cast(y, bigfloat())),
    "-" = c_bigfloat_subtract(x, vec_cast(y, bigfloat())),
    "*" = c_bigfloat_multiply(x, vec_cast(y, bigfloat())),
    "/" = c_bigfloat_divide(x, vec_cast(y, bigfloat())),
    "^" = c_bigfloat_pow(x, vec_cast(y, bigfloat())),
    "%%" = c_bigfloat_remainder(x, vec_cast(y, bigfloat())),
    "%/%" = c_bigfloat_quotient(x, vec_cast(y, bigfloat())),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.numeric bignum_bigfloat
#' @export
vec_arith.numeric.bignum_bigfloat <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(vec_cast(x, bigfloat()), y),
    "-" = c_bigfloat_subtract(vec_cast(x, bigfloat()), y),
    "*" = c_bigfloat_multiply(vec_cast(x, bigfloat()), y),
    "/" = c_bigfloat_divide(vec_cast(x, bigfloat()), y),
    "^" = c_bigfloat_pow(vec_cast(x, bigfloat()), y),
    "%%" = c_bigfloat_remainder(vec_cast(x, bigfloat()), y),
    "%/%" = c_bigfloat_quotient(vec_cast(x, bigfloat()), y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_bigfloat bignum_biginteger
#' @export
vec_arith.bignum_bigfloat.bignum_biginteger <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(x, vec_cast(y, bigfloat())),
    "-" = c_bigfloat_subtract(x, vec_cast(y, bigfloat())),
    "*" = c_bigfloat_multiply(x, vec_cast(y, bigfloat())),
    "/" = c_bigfloat_divide(x, vec_cast(y, bigfloat())),
    "^" = c_bigfloat_pow(x, vec_cast(y, bigfloat())),
    "%%" = c_bigfloat_remainder(x, vec_cast(y, bigfloat())),
    "%/%" = c_bigfloat_quotient(x, vec_cast(y, bigfloat())),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_biginteger bignum_bigfloat
#' @export
vec_arith.bignum_biginteger.bignum_bigfloat <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(vec_cast(x, bigfloat()), y),
    "-" = c_bigfloat_subtract(vec_cast(x, bigfloat()), y),
    "*" = c_bigfloat_multiply(vec_cast(x, bigfloat()), y),
    "/" = c_bigfloat_divide(vec_cast(x, bigfloat()), y),
    "^" = c_bigfloat_pow(vec_cast(x, bigfloat()), y),
    "%%" = c_bigfloat_remainder(vec_cast(x, bigfloat()), y),
    "%/%" = c_bigfloat_quotient(vec_cast(x, bigfloat()), y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.bignum_bigfloat MISSING
#' @export
vec_arith.bignum_bigfloat.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = x * -1L,
    "+" = x,
    stop_incompatible_op(op, x, y)
  )
}


# Mathematical operations ------------------------------------------------------

#' @export
vec_math.bignum_bigfloat <- function(.fn, .x, ..., na.rm = FALSE) {
  switch(
    .fn,

    # Summary group
    sum = c_bigfloat_sum(.x, na.rm),
    prod = c_bigfloat_prod(.x, na.rm),

    # Math group
    abs = c_bigfloat_abs(.x),
    sign = c_bigfloat_sign(.x),
    sqrt = c_bigfloat_sqrt(.x),
    ceiling = c_bigfloat_ceiling(.x),
    floor = c_bigfloat_floor(.x),
    trunc = c_bigfloat_trunc(.x),
    cumsum = c_bigfloat_cumsum(.x),
    cumprod = c_bigfloat_cumprod(.x),
    cummax = c_bigfloat_cummax(.x),
    cummin = c_bigfloat_cummin(.x),
    exp = c_bigfloat_exp(.x),
    expm1 = c_bigfloat_expm1(.x),
    log = c_bigfloat_log(.x),
    log10 = c_bigfloat_log10(.x),
    log2 = c_bigfloat_log2(.x),
    log1p = c_bigfloat_log1p(.x),
    cos = c_bigfloat_cos(.x),
    cosh = c_bigfloat_cosh(.x),
    cospi = c_bigfloat_cos(bigpi * (.x %% 2)),
    sin = c_bigfloat_sin(.x),
    sinh = c_bigfloat_sinh(.x),
    sinpi = c_bigfloat_sin(bigpi * (.x %% 2)),
    tan = c_bigfloat_tan(.x),
    tanh = c_bigfloat_tanh(.x),
    tanpi = c_bigfloat_tan(bigpi * (.x %% 1)),
    acos = c_bigfloat_acos(.x),
    acosh = c_bigfloat_acosh(.x),
    asin = c_bigfloat_asin(.x),
    asinh = c_bigfloat_asinh(.x),
    atan = c_bigfloat_atan(.x),
    atanh = c_bigfloat_atanh(.x),
    gamma = c_bigfloat_gamma(.x),
    lgamma = c_bigfloat_lgamma(.x),

    # Other
    mean = c_bigfloat_sum(.x, na.rm) / sum(!is.na(.x)),
    is.nan = is.nan(allow_lossy_cast(vec_cast(.x, double()))),
    is.infinite = is.infinite(allow_lossy_cast(vec_cast(.x, double()))),
    is.finite = is.finite(allow_lossy_cast(vec_cast(.x, double()))),

    stop_unsupported(.x, .fn)
  )
}

#' @export
is.na.bignum_bigfloat <- function(x) {
  is.na(c_bigfloat_to_double(x))
}
