# nocov start
.onLoad <- function(...) {
  s3_register("pillar::pillar_shaft", "bignum_vctr")

  invisible()
}
# nocov end
