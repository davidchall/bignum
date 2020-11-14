# base -------------------------------------------------------------------------

#' @method vec_arith integer
#' @export
#' @export vec_arith.integer
#' @rdname bignum-vctrs
vec_arith.integer <- function(op, x, y, ...) {
  UseMethod("vec_arith.integer", y)
}

#' @method vec_arith.integer default
#' @export
vec_arith.integer.default <- function(op, x, y, ...) {
  vec_arith.numeric(op, x, y)
}


# common -----------------------------------------------------------------------

#' @method vec_arith bignum_vctr
#' @export
#' @export vec_arith.bignum_vctr
#' @rdname bignum-vctrs
vec_arith.bignum_vctr <- function(op, x, y, ...) {
  UseMethod("vec_arith.bignum_vctr", y)
}

#' @method vec_arith.bignum_vctr default
#' @export
vec_arith.bignum_vctr.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.bignum_vctr bignum_vctr
#' @export
vec_arith.bignum_vctr.bignum_vctr <- function(op, x, y, ...) {
  vec_arith_bignum(op, x, y)
}

#' @method vec_arith.bignum_vctr numeric
#' @export
vec_arith.bignum_vctr.numeric <- function(op, x, y, ...) {
  vec_arith_bignum(op, x, y)
}

#' @method vec_arith.numeric bignum_vctr
#' @export
vec_arith.numeric.bignum_vctr <- function(op, x, y, ...) {
  vec_arith_bignum(op, x, y)
}

#' @method vec_arith.bignum_vctr MISSING
#' @export
vec_arith.bignum_vctr.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = x * -1L,
    "+" = x,
    stop_incompatible_op(op, x, y)
  )
}

vec_arith_bignum <- function(op, x, y) {
  args <- vec_recycle_common(
    vec_cast(x, bigfloat()),
    vec_cast(y, bigfloat())
  )
  x2 <- args[[1L]]
  y2 <- args[[2L]]

  switch(
    op,
    "+" = c_bigfloat_add(x2, y2),
    "-" = c_bigfloat_subtract(x2, y2),
    "*" = c_bigfloat_multiply(x2, y2),
    "/" = c_bigfloat_divide(x2, y2),
    "^" = c_bigfloat_pow(x2, y2),
    "%%" = c_bigfloat_modulo(x2, y2),
    "%/%" = vec_cast(trunc(x2 / y2), biginteger()),
    stop_incompatible_op(op, x, y)
  )
}


# biginteger -------------------------------------------------------------------

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
  vec_arith.bignum_vctr(op, x, y)
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
    "^" = c_biginteger_pow(x, vec_cast(y, integer())),
    "%%" = c_biginteger_modulo(x, y),
    "%/%" = c_biginteger_quotient(x, y),
    vec_arith_bignum(op, x, y)
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
    "^" = c_biginteger_pow(x, y),
    "%%" = c_biginteger_modulo(x, vec_cast(y, biginteger())),
    "%/%" = c_biginteger_quotient(x, vec_cast(y, biginteger())),
    vec_arith_bignum(op, x, y)
  )
}

#' @method vec_arith.integer bignum_biginteger
#' @export
vec_arith.integer.bignum_biginteger <- function(op, x, y, ...) {
  args <- vec_recycle_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(
    op,
    "+" = c_biginteger_add(vec_cast(x, biginteger()), y),
    "-" = c_biginteger_subtract(vec_cast(x, biginteger()), y),
    "*" = c_biginteger_multiply(vec_cast(x, biginteger()), y),
    "^" = c_biginteger_pow(vec_cast(x, biginteger()), vec_cast(y, integer())),
    "%%" = c_biginteger_modulo(vec_cast(x, biginteger()), y),
    "%/%" = c_biginteger_quotient(vec_cast(x, biginteger()), y),
    vec_arith_bignum(op, x, y)
  )
}
