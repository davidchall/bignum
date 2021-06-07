#' Format a bignum vector
#'
#' @description
#' Customize how a [`biginteger`] or [`bigfloat`] vector is displayed.
#' The precision can be controlled with a number of significant figures, or with
#' a maximum or fixed number of digits after the decimal point. You can also
#' choose between decimal, scientific and hexadecimal notations.
#'
#' The default formatting applied when printing depends on the type of object:
#'
#' * **standalone vector:** consults `"bignum.sigfig"` and `"bignum.max_dec_width"`
#' * **tibble column:** consults `"pillar.sigfig"` and `"pillar.max_dec_width"`
#'
#' @param x A [`biginteger`] or [`bigfloat`] vector.
#' @param ... These dots are for future extensions and must be empty.
#' @param sigfig Number of significant figures to show. Must be positive.
#'   Cannot be combined with `digits`.
#'
#'   If both `sigfig` and `digits` are unspecified, then consults the
#'   `"bignum.sigfig"` option (default: 7).
#' @param digits Number of digits to show after the decimal point.
#'   Positive values indicate the exact number of digits to show.
#'   Negative values indicate the maximum number of digits to show (terminal
#'   zeros are hidden if there are no subsequent non-zero digits).
#'   Cannot be combined with `sigfig`.
#' @param notation How should the vector be displayed? Choices:
#'
#'   * `"fit"`: Use decimal notation if it fits, otherwise use scientific
#'     notation. Consults the `"bignum.max_dec_width"` option (default: 13).
#'   * `"dec"`: Use decimal notation, regardless of width.
#'   * `"sci"`: Use scientific notation.
#'   * `"hex"`: Use hexadecimal notation (positive [`biginteger`] only).
#' @return Character vector
#'
#' @examples
#' # default uses decimal notation
#' format(bigfloat(1e12))
#'
#' # until it becomes too wide, then it uses scientific notation
#' format(bigfloat(1e13))
#'
#' # hexadecimal notation is supported for positive integers
#' format(biginteger(255), notation = "hex")
#'
#' # significant figures
#' format(bigfloat(12.5), sigfig = 2)
#'
#' # fixed digits after decimal point
#' format(bigfloat(12.5), digits = 2)
#'
#' # maximum digits after decimal point
#' format(bigfloat(12.5), digits = -2)
#' @name bignum-format
NULL

#' @rdname bignum-format
#' @export
format.bignum_biginteger <- function(x, ..., sigfig = NULL, digits = NULL,
                                     notation = c("fit", "dec", "sci", "hex")) {
  notation <- arg_match(notation)

  switch(notation,
    fit = format_fit(x, sigfig = sigfig, digits = digits),
    sci = format(
      vec_cast(x, new_bigfloat()),
      ...,
      sigfig = sigfig,
      digits = digits,
      notation = notation
    ),
    c_biginteger_format(x, notation = notation)
  )
}

#' @rdname bignum-format
#' @export
format.bignum_bigfloat <- function(x, ..., sigfig = NULL, digits = NULL,
                                   notation = c("fit", "dec", "sci")) {
  notation <- arg_match(notation)

  if (notation == "fit") {
    format_fit(x, sigfig = sigfig, digits = digits)
  } else {
    digits_args <- parse_digits_args(sigfig, digits)

    c_bigfloat_format(
      x,
      notation = notation,
      digits = digits_args$digits,
      is_sigfig = digits_args$is_sigfig
    )
  }
}

format_fit <- function(x, ...) {
  max_dec_width <- getOption("bignum.max_dec_width", 13L)
  if (!is_scalar_integerish(max_dec_width)) {
    abort("Option bignum.max_dec_width must be an integer.")
  }

  out <- format(x, ..., notation = "dec")
  if (any(nchar(out) > max_dec_width, na.rm = TRUE)) {
    out <- format(x, ..., notation = "sci")
  }
  out
}

parse_digits_args <- function(sigfig, digits) {
  if (!is.null(sigfig) && !is.null(digits)) {
    abort("The `sigfig` or `digits` arguments are mutually exclusive.")
  } else if (!is.null(sigfig)) {
    if (!is_scalar_integerish(sigfig) || sigfig < 1) {
      abort("`sigfig` must be a non-zero positive integer.")
    }
    display_digits <- as.integer(sigfig)
    display_sigfig <- TRUE
  } else if (!is.null(digits)) {
    if (!is_scalar_integerish(digits)) {
      abort("`digits` must be an integer.")
    }
    display_digits <- as.integer(digits)
    display_sigfig <- FALSE
  } else {
    sigfig <- getOption("bignum.sigfig", 7L)
    if (!is_scalar_integerish(sigfig) || sigfig < 1) {
      abort("Option bignum.sigfig must be a non-zero positive integer.")
    }
    display_digits <- as.integer(sigfig)
    display_sigfig <- TRUE
  }

  list(
    digits = display_digits,
    is_sigfig = display_sigfig
  )
}
