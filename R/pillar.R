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

  dec <- format(x, notation = "dec", sigfig = sigfig)
  sci <- format(x, notation = "sci", sigfig = sigfig)

  dec_width <- pillar::get_max_extent(dec)
  sci_width <- pillar::get_max_extent(sci)

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

style_bignum <- function(x) {
  x[is.na(x)] <- pillar::style_na("NA")

  x
}

#' @export
format.pillar_shaft_bignum <- function(x, width, ...) {
  min_width <- attr(x, "min_width")
  if (width < min_width) {
    abort(paste0("Need at least width ", min_width, ", requested ", width, "."))
  }

  fmt <- if (is.null(x$dec) || width < attr(x, "width")) {
    x$sci
  } else {
    x$dec
  }

  row <- style_bignum(fmt)

  # pad because pillar expects 'width'
  used_width <- pillar::get_extent(row)
  row <- paste0(strrep(" ", width - used_width), row)

  pillar::new_ornament(row, align = "right")
}
