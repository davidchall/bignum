# Dynamically exported, see zzz.R
pillar_shaft.bignum_vctr <- function(x, ...) {
  fit <- format(x, notation = "fit")
  sci <- format(x, notation = "sci")

  pillar::new_pillar_shaft(
    list(fit = fit, sci = sci),
    width = pillar::get_max_extent(fit),
    min_width = pillar::get_max_extent(sci),
    class = "pillar_shaft_bignum"
  )
}

#' @export
format.pillar_shaft_bignum <- function(x, width, ...) {
  ornament <- if (attr(x, "width") <= width) {
    x$fit
  } else {
    x$sci
  }

  ornament[is.na(ornament)] <- pillar::style_na("NA")

  pillar::new_ornament(ornament, align = "right")
}
