# bignum (development version)

## New features

* `format()` functions now support customized output.
    * New `sigfig` and `digits` arguments control the displayed precision.
    * New `notation` argument chooses decimal, scientific or hexadecimal output.
    * New options `"bignum.sigfig"` and `"bignum.max_dec_width"` determine the default formatting.
* When a bignum vector is stored in a [tibble](https://tibble.tidyverse.org) column, the formatting is adjusted to aid reading data vertically.
    * The decimal point and exponent are aligned across rows and negative numbers are colored red.
    * The options `"pillar.sigfig"` and `"pillar.max_dec_width"` determine tibble formatting.
    * See `vignette("digits", package = "pillar")` for details.
    
## Bug fixes

* Casting a non-integer `double()` to `biginteger()` now returns the truncated integer, consistent with base vectors. Previously it would return `NA`. A lossy cast warning is still raised.
* Casting a large `double()` to `biginteger()` now works correctly. Previously it might return `NA`, depending on the value of `options("scipen")`.
* Casting `Inf` to `biginteger()` now raises a lossy cast warning.
* Casting a large `biginteger()` to `bigfloat()` now raises a lossy cast warning when the `bigfloat()` precision is exceeded.
* `is.finite()` and `is.infinite()` now correctly handle large `bigfloat()` values. Previously, such large values were considered to be infinite.


# bignum 0.1.0

First CRAN release.

* Numeric vector classes:
    * `biginteger()` stores any integer (i.e. arbitrary precision).
    * `bigfloat()` stores 50 decimal digits of precision.

* Constants for common situations: `NA_biginteger_`, `NA_bigfloat_`, `bigpi`.

* Support for many basic operations (see `vignette("operations")`):
    * Check for special values: `is.na()`, `is.finite()`, `is.infinite()`,
      `is.nan()`
    * Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
    * Arithmetic: `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`
    * Mathematical:
        * `sum()`, `prod()`, `max()`, `min()`, `range()`, `mean()`
        * `cumsum()`, `cumprod()`, `cummax()`, `cummin()`
        * `floor()`, `ceiling()`, `trunc()`
        * `abs()`, `sign()`, `sqrt()`
        * `log()`, `log10()`, `log2()`, `log1p()`, `exp()`, `expm1()`
        * `cos()`, `sin()`, `tan()`, `acos()`, `asin()`, `atan()`, `cospi()`,
          `sinpi()`, `tanpi()`
        * `cosh()`, `sinh()`, `tanh()`, `acosh()`, `asinh()`, `atanh()`
        * `gamma()`, `lgamma()`, `factorial()`, `lfactorial()`
