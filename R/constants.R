#' Constants
#'
#' @description
#' `NA_biginteger_` and `NA_bigfloat_` support missing values.
#'
#' `bigpi` is a higher precision version of [`pi`].
#'
#' @examples
#' NA_biginteger_
#'
#' NA_bigfloat_
#'
#' bigpi
#' @seealso [`NA`], [`pi`]
#' @name bignum-constants
NULL

#' @format NULL
#' @rdname bignum-constants
#' @export
NA_biginteger_ <- new_biginteger(NA_character_)

#' @format NULL
#' @rdname bignum-constants
#' @export
NA_bigfloat_ <- new_bigfloat(NA_character_)

#' @format NULL
#' @rdname bignum-constants
#' @export
bigpi <- new_bigfloat("3.14159265358979323846264338327950288419716939937510")
