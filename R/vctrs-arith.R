#' Arithmetic Operations
#'
#' @description
#' [`biginteger`] and [`bigfloat`] vectors support the standard
#' [arithmetic operations][Arithmetic].
#'
#' ```
#' + x
#' - x
#' x + y
#' x - y
#' x * y
#' x / y
#' x ^ y
#' x %% y
#' x %/% y
#' ```
#'
#' @section Recycling rules:
#' The two inputs have **compatible sizes** if:
#'
#' * Both input vectors have the same length, or
#' * One input is a scalar (length 1) which is recycled.
#'
#' @return
#' These arithmetic operations are **type-stable**, which means the output type
#' depends only on the input types (not the input values). A biginteger vector
#' is returned when the result must be an integer (e.g., addition of two integers).
#' Otherwise a bigfloat vector is returned.
#'
#' The following table summarizes the return type for each combination, where
#' "integer-like" refers to integer and biginteger vectors and "float-like"
#' refers to double and bigfloat vectors.
#'
#' | **Input 1**  | **Operator**        | **Input 2**  |     | **Result** |
#' | :----------- | :-----------------: | :----------- | :-: | :--------- |
#' | Integer-like | +, -, *, ^, %%, %/% | Integer-like | ->  | biginteger |
#' | Integer-like | /                   | Integer-like | ->  | bigfloat   |
#' | Integer-like | +, -, *, /, ^, %%   | Float-like   | ->  | bigfloat   |
#' | Integer-like | %/%                 | Float-like   | ->  | biginteger |
#' | Float-like   | +, -, *, /, ^, %%   | Integer-like | ->  | bigfloat   |
#' | Float-like   | %/%                 | Integer-like | ->  | biginteger |
#' | Float-like   | +, -, *, /, ^, %%   | Float-like   | ->  | bigfloat   |
#' | Float-like   | %/%                 | Float-like   | ->  | biginteger |
#'
#' @name bignum-arith
NULL

vec_arith_bigfloat <- function(op, x, y) {
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

vec_arith_biginteger <- function(op, x, y) {
  args <- vec_recycle_common(
    vec_cast(x, biginteger()),
    vec_cast(y, biginteger())
  )
  x2 <- args[[1L]]
  y2 <- args[[2L]]

  switch(
    op,
    "+" = c_biginteger_add(x2, y2),
    "-" = c_biginteger_subtract(x2, y2),
    "*" = c_biginteger_multiply(x2, y2),
    "^" = c_biginteger_pow(x2, vec_cast(y, integer())),
    "%%" = c_biginteger_modulo(x2, y2),
    "%/%" = c_biginteger_quotient(x2, y2),
    vec_arith_bigfloat(op, x, y)
  )
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
  vec_arith_bigfloat(op, x, y)
}

#' @method vec_arith.bignum_vctr numeric
#' @export
vec_arith.bignum_vctr.numeric <- function(op, x, y, ...) {
  vec_arith_bigfloat(op, x, y)
}

#' @method vec_arith.numeric bignum_vctr
#' @export
vec_arith.numeric.bignum_vctr <- function(op, x, y, ...) {
  vec_arith_bigfloat(op, x, y)
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
  vec_arith_biginteger(op, x, y)
}

#' @method vec_arith.bignum_biginteger numeric
#' @export
vec_arith.bignum_biginteger.numeric <- function(op, x, y, ...) {
  if (is.integer(y)) {
    vec_arith_biginteger(op, x, y)
  } else {
    vec_arith_bigfloat(op, x, y)
  }
}

#' @method vec_arith.numeric bignum_biginteger
#' @export
vec_arith.numeric.bignum_biginteger <- function(op, x, y, ...) {
  if (is.integer(x)) {
    vec_arith_biginteger(op, x, y)
  } else {
    vec_arith_bigfloat(op, x, y)
  }
}
