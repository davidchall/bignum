#' Constants
#'
#' @description
#' `NA_biginteger_` and `NA_bigfloat_` support missing values.
#'
#' `bigpi` is a higher precision version of [`pi`].
#'
#' @return A [`biginteger`] or [`bigfloat`] vector of length 1.
#' @seealso [`NA`] and [`pi`] are the base constants.
#' @examples
#' NA_biginteger_
#'
#' NA_bigfloat_
#'
#' bigpi
#' @name bignum-constants
NULL

#' @format NULL
#' @rdname bignum-constants
#' @export
NA_biginteger_ <- new_biginteger(NA_character_, cxx = FALSE)

#' @format NULL
#' @rdname bignum-constants
#' @export
NA_bigfloat_ <- new_bigfloat(NA_character_, cxx = FALSE)

#' @format NULL
#' @rdname bignum-constants
#' @export
bigpi <- new_bigfloat("3.14159265358979323846264338327950288419716939937510", cxx = FALSE)
