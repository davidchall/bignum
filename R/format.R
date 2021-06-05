#' Formatting
#'
#' Format a bignum vector for pretty printing.
#'
#' @param x A [`biginteger`] or [`bigfloat`] vector.
#' @param ... These dots are for future extensions and must be empty.
#' @param sigfig Number of significant figures to show. Must be positive.
#'   Cannot be combined with `digits`.
#'
#'   If both `sigfig` and `digits` are unspecified, then the `"bignum.sigfig"`
#'   option is consulted (which has a default value of 7).
#' @param digits Number of digits to show after the decimal point.
#'   Positive values indicate the exact number of digits to show.
#'   Negative values indicate the maximum number of digits to show (terminal
#'   zeros are hidden if there are no subsequent non-zero digits).
#'   Cannot be combined with `sigfig`.
#' @param notation How should the vector be displayed? Choices:
#'
#'   * `"dec"`: Use decimal notation, regardless of width.
#'   * `"sci"`: Use scientific notation.
#'   * `"hex"`: Use hexadecimal notation ([`biginteger`] only).
#' @return Character vector
#'
#' @examples
#' x <- biginteger(2)^100L
#'
#' # default shows maximum precision
#' format(x)
#' @name bignum-format
NULL

#' @rdname bignum-format
#' @export
format.bignum_biginteger <- function(x, ..., sigfig = NULL, digits = NULL,
                                     notation = c("dec", "sci", "hex")) {
  notation <- arg_match(notation)

  if (notation == "sci") {
    format(
      vec_cast(x, new_bigfloat()),
      ...,
      sigfig = sigfig,
      digits = digits,
      notation = notation
    )
  } else {
    c_biginteger_format(x, notation = notation)
  }
}

#' @rdname bignum-format
#' @export
format.bignum_bigfloat <- function(x, ..., sigfig = NULL, digits = NULL,
                                   notation = c("dec", "sci")) {
  notation <- arg_match(notation)

  digits_args <- parse_digits(sigfig, digits)

  c_bigfloat_format(
    x,
    notation = notation,
    digits = digits_args$digits,
    is_sigfig = digits_args$is_sigfig
  )
}

parse_digits <- function(sigfig, digits) {
  if (!is.null(sigfig) && !is.null(digits)) {
    abort("The `sigfig` or `digits` arguments are mutually exclusive.")
  } else if (!is.null(sigfig)) {
    sigfig <- vec_cast(sigfig, integer(), x_arg = "sigfig")
    vec_assert(sigfig, ptype = integer(), size = 1)
    if (sigfig < 1) {
      abort("Must show at least one significant figure.")
    }

    display_digits <- sigfig
    display_sigfig <- TRUE
  } else if (!is.null(digits)) {
    digits <- vec_cast(digits, integer(), x_arg = "digits")
    vec_assert(digits, ptype = integer(), size = 1)

    display_digits <- digits
    display_sigfig <- FALSE
  } else {
    display_digits <- getOption("bignum.sigfig", 7L)
    display_sigfig <- TRUE
  }

  list(
    digits = display_digits,
    is_sigfig = display_sigfig
  )
}
