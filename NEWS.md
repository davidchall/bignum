# bignum (development version)

* `format()` functions now support customized output (#15).
    * New `sigfig` and `digits` arguments control the displayed precision.
    * New `notation` argument chooses decimal, scientific or hexadecimal output.
    * New options `"bignum.sigfig"` and `"bignum.max_dec_width"` determine the default formatting.
* Fixed how `biginteger()` vectors are created from large `double()` vectors (e.g. `biginteger(1e10)`). This would previously return `NA` (#16).
* Fixed identification of lossy casts when converting between `biginteger()` and `bigfloat()` vectors. This would previously return `NA` silently, but now it raises a warning.


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
