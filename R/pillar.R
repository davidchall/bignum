split_decimal <- function(x, sigfig, notation) {
  fmt <- format(x, sigfig = sigfig, notation = notation)

  num <- is.finite(x)
  neg <- !is.na(x) & x < 0
  other <- ifelse(!num, fmt, rep_along(x, NA_character_))

  # split number into mantissa + exponent
  mnt <- rep_along(x, NA_character_)
  exp <- rep_along(x, NA_integer_)
  if (notation == "sci") {
    mnt_exp <- strsplit(fmt, "e", fixed = TRUE)
    mnt[num] <- vapply(mnt_exp[num], getElement, "", 1)
    exp[num] <- as.integer(vapply(mnt_exp[num], getElement, "", 2))

    # don't show exponent for exact zero
    exp[x == 0] <- NA_integer_
  } else {
    mnt[num] <- fmt[num]
  }

  # split mantissa into lhs + dec + rhs
  dec <- grepl(".", mnt, fixed = TRUE)
  lhs <- rep_along(x, "")
  rhs <- rep_along(x, "")
  lhs_rhs <- strsplit(mnt, ".", fixed = TRUE)
  lhs[num] <- vapply(lhs_rhs[num], getElement, "", 1)
  has_rhs <- vapply(lhs_rhs, length, 1L) == 2
  rhs[has_rhs] <- vapply(lhs_rhs[has_rhs], getElement, "", 2)

  out <- list(
    num = num, # lgl
    neg = neg, # lgl
    lhs = lhs, # chr
    dec = dec, # lgl
    rhs = rhs, # chr
    exp = exp, # int
    other = other # chr
  )

  widths <- get_decimal_widths(out)
  attr(out, "widths") <- widths
  attr(out, "width") <- widths$total

  out
}

get_decimal_widths <- function(x) {
  lhs_width <- max(nchar(x$lhs))
  dec_width <- as.integer(any(x$dec))
  rhs_width <- max(nchar(x$rhs))
  mnt_width <- lhs_width + dec_width + rhs_width

  exp <- x$exp[!is.na(x$exp)]
  exp_width <- any(exp < 0) + max(2 + trunc(log10(abs(exp) + 0.5)), 0)
  other_width <- max(nchar(x$other[!x$num], type = "width"), 0)

  total_width <- max(mnt_width + exp_width, other_width)

  list(
    lhs = lhs_width,
    dec = dec_width,
    rhs = rhs_width,
    exp = exp_width,
    other = other_width,
    total = total_width
  )
}

# Dynamically exported, see zzz.R
pillar_shaft.bignum_vctr <- function(x, ...) {
  sigfig <- getOption("pillar.sigfig", 3L)
  if (!is_scalar_integerish(sigfig) || sigfig < 1) {
    abort("Option pillar.sigfig must be a non-zero positive integer.")
  }

  max_dec_width <- getOption("pillar.max_dec_width", 13L)
  if (!is_scalar_integerish(max_dec_width)) {
    abort("Option pillar.max_dec_width must be an integer.")
  }

  dec <- split_decimal(x, sigfig = sigfig, notation = "dec")
  sci <- split_decimal(x, sigfig = sigfig, notation = "sci")

  dec_width <- attr(dec, "width")
  sci_width <- attr(sci, "width")

  if (dec_width > max_dec_width) {
    dec <- NULL
    dec_width <- NULL
  }

  pillar::new_pillar_shaft(
    list(dec = dec, sci = sci),
    width = dec_width %||% sci_width,
    min_width = min(dec_width, sci_width),
    class = "pillar_shaft_bignum"
  )
}

style_mantissa <- function(x) {
  out <- paste0(
    pillar::align(x$lhs, align = "right"),
    ifelse(x$dec, ".", ifelse(any(x$dec), " ", "")),
    pillar::align(x$rhs, align = "left")
  )
  pillar::style_num(out, x$neg)
}

# copied from pillar:::supernum()
style_exponent <- function(x) {
  num <- !is.na(x)
  if (!any(num)) {
    return(rep_along(x, ""))
  }

  neg <- num & x < 0
  if (any(neg)) {
    sign_chr <- ifelse(neg, "-", "+")
    sign_chr[!num] <- " "
  } else {
    sign_chr <- rep_along(x, "")
  }

  digits <- as.character(abs(x))
  digits[!num] <- ""

  exp <- paste0(sign_chr, format(digits, justify = "right"))

  paste0(
    pillar::style_subtle(ifelse(num, "e", " ")),
    pillar::style_num(exp, neg)
  )
}

style_other <- function(x, extra_width) {
  widths <- attr(x, "widths")
  mnt_width <- widths$lhs + widths$dec + widths$rhs

  other <- if (widths$other <= widths$lhs + extra_width) {
    # right align with decimal point
    paste0(x$other, strrep(" ", widths$total - widths$lhs))
  } else if (widths$other <= mnt_width + extra_width) {
    # right align with mantissa
    paste0(x$other, strrep(" ", widths$total - mnt_width))
  } else {
    x$other
  }

  pillar::style_na(other)
}

style_bignum <- function(x, extra_width) {
  ifelse(
    x$num,
    paste0(style_mantissa(x), style_exponent(x$exp)),
    style_other(x, extra_width)
  )
}

#' @export
format.pillar_shaft_bignum <- function(x, width, ...) {
  min_width <- attr(x, "min_width")
  if (width < min_width) {
    abort(paste0("Need at least width ", min_width, ", requested ", width, "."))
  }

  fmt <- if (is.null(x$dec) || width < attr(x$dec, "width")) {
    x$sci
  } else {
    x$dec
  }

  row <- style_bignum(fmt, width - attr(fmt, "width"))

  # pad because pillar expects 'width'
  used_width <- pillar::get_extent(row)
  row <- paste0(strrep(" ", pmax(width - used_width, 0)), row)

  pillar::new_ornament(row, align = "right")
}
