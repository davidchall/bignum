vec_math_bigfloat <- function(.fn, .x, ..., na.rm = FALSE) {
  switch(.fn,

    # Summary group
    sum = c_bigfloat_sum(.x, na.rm),
    prod = c_bigfloat_prod(.x, na.rm),

    # Math group
    abs = c_bigfloat_abs(.x),
    sign = c_bigfloat_sign(.x),
    sqrt = c_bigfloat_sqrt(.x),
    ceiling = c_bigfloat_ceiling(.x),
    floor = c_bigfloat_floor(.x),
    trunc = c_bigfloat_trunc(.x),
    cumsum = c_bigfloat_cumsum(.x),
    cumprod = c_bigfloat_cumprod(.x),
    cummax = c_bigfloat_cummax(.x),
    cummin = c_bigfloat_cummin(.x),
    exp = c_bigfloat_exp(.x),
    expm1 = c_bigfloat_expm1(.x),
    log = bigfloat_log(.x, ...),
    log10 = c_bigfloat_log10(.x),
    log2 = c_bigfloat_log2(.x),
    log1p = c_bigfloat_log1p(.x),
    cos = c_bigfloat_cos(.x),
    cosh = c_bigfloat_cosh(.x),
    cospi = c_bigfloat_cos(bigpi * (.x %% 2)),
    sin = c_bigfloat_sin(.x),
    sinh = c_bigfloat_sinh(.x),
    sinpi = c_bigfloat_sin(bigpi * (.x %% 2)),
    tan = c_bigfloat_tan(.x),
    tanh = c_bigfloat_tanh(.x),
    tanpi = c_bigfloat_tan(bigpi * (.x %% 1)),
    acos = c_bigfloat_acos(.x),
    acosh = c_bigfloat_acosh(.x),
    asin = c_bigfloat_asin(.x),
    asinh = c_bigfloat_asinh(.x),
    atan = c_bigfloat_atan(.x),
    atanh = c_bigfloat_atanh(.x),
    gamma = c_bigfloat_gamma(.x),
    lgamma = c_bigfloat_lgamma(.x),
    digamma = c_bigfloat_digamma(.x),
    trigamma = c_bigfloat_trigamma(.x),

    # Other
    mean = c_bigfloat_sum(.x, na.rm) / sum(!is.na(.x)),
    is.nan = vec_data(.x) %|% "NA" == "NaN",
    is.infinite = vec_data(.x) %in% c("Inf", "-Inf"),
    is.finite = !(vec_data(.x) %in% c(NA, "NaN", "Inf", "-Inf")),

    # else
    stop_unsupported(.x, .fn)
  )
}

#' @export
vec_math.bignum_bigfloat <- function(.fn, .x, ..., na.rm = FALSE) {
  vec_math_bigfloat(.fn, .x, ..., na.rm = na.rm)
}

#' @export
vec_math.bignum_biginteger <- function(.fn, .x, ..., na.rm = FALSE) {
  switch(.fn,

    # Summary group
    sum = c_biginteger_sum(.x, na.rm),
    prod = c_biginteger_prod(.x, na.rm),

    # Math group
    abs = c_biginteger_abs(.x),
    sign = c_biginteger_sign(.x),
    ceiling = .x,
    floor = .x,
    trunc = .x,
    cumsum = c_biginteger_cumsum(.x),
    cumprod = c_biginteger_cumprod(.x),
    cummax = c_biginteger_cummax(.x),
    cummin = c_biginteger_cummin(.x),

    # Other
    mean = c_biginteger_sum(.x, na.rm) / sum(!is.na(.x)),
    is.nan = rep_along(.x, FALSE),
    is.finite = !is.na(.x),
    is.infinite = rep_along(.x, FALSE),

    # else
    vec_math_bigfloat(.fn, .x, ..., na.rm = na.rm)
  )
}

# functions with unnamed arguments require special handling --------------------

bigfloat_log <- function(x, base) {
  if (is_missing(base)) {
    c_bigfloat_log(x)
  } else {
    c_bigfloat_log(x) / c_bigfloat_log(vec_cast(base, bigfloat()))
  }
}
