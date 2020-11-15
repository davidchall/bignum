test_that("sum() works", {
  x <- c(2, 3, NA)
  ans <- sum(x, na.rm = TRUE)

  expect_equal(sum(biginteger(x), na.rm = TRUE), biginteger(ans))
  expect_equal(sum(biginteger(x), na.rm = FALSE), NA_biginteger_)

  expect_equal(sum(bigfloat(x), na.rm = TRUE), bigfloat(ans))
  expect_equal(sum(bigfloat(x), na.rm = FALSE), NA_bigfloat_)
})

test_that("prod() works", {
  x <- c(2, 3, NA)
  ans <- prod(x, na.rm = TRUE)

  expect_equal(prod(biginteger(x), na.rm = TRUE), biginteger(ans))
  expect_equal(prod(biginteger(x), na.rm = FALSE), NA_biginteger_)

  expect_equal(prod(bigfloat(x), na.rm = TRUE), bigfloat(ans))
  expect_equal(prod(bigfloat(x), na.rm = FALSE), NA_bigfloat_)
})

test_that("mean() works", {
  x <- c(2, 3, NA)
  ans <- mean(x, na.rm = TRUE)

  expect_equal(mean(biginteger(x), na.rm = TRUE), bigfloat(ans))
  expect_equal(mean(biginteger(x), na.rm = FALSE), NA_bigfloat_)
  expect_equal(mean(biginteger(x[!is.na(x)]), na.rm = FALSE), bigfloat(ans))

  expect_equal(mean(bigfloat(x), na.rm = TRUE), bigfloat(ans))
  expect_equal(mean(bigfloat(x), na.rm = FALSE), NA_bigfloat_)
  expect_equal(mean(bigfloat(x[!is.na(x)]), na.rm = FALSE), bigfloat(ans))
})

test_that("ceiling() works", {
  x <- c(2.5, -2.5)
  ans <- ceiling(x)

  expect_equal(ceiling(biginteger(ceiling(x))), biginteger(ans))
  expect_equal(ceiling(bigfloat(x)), bigfloat(ans))
})

test_that("floor() works", {
  x <- c(2.5, -2.5)
  ans <- floor(x)

  expect_equal(floor(biginteger(floor(x))), biginteger(ans))
  expect_equal(floor(bigfloat(x)), bigfloat(ans))
})

test_that("trunc() works", {
  x <- c(2.5, -2.5)
  ans <- trunc(x)

  expect_equal(trunc(biginteger(trunc(x))), biginteger(ans))
  expect_equal(trunc(bigfloat(x)), bigfloat(ans))
})

test_that("special value math works", {
  x <- c(1, NA, NaN, Inf, -Inf)
  expect_equal(is.nan(biginteger(x)), is.nan(suppressWarnings(as.integer(x))))
  expect_equal(is.nan(bigfloat(x)), is.nan(x))
  expect_equal(is.finite(biginteger(x)), is.finite(suppressWarnings(as.integer(x))))
  expect_equal(is.finite(bigfloat(x)), is.finite(x))
  expect_equal(is.infinite(biginteger(x)), is.infinite(suppressWarnings(as.integer(x))))
  expect_equal(is.infinite(bigfloat(x)), is.infinite(x))
})

test_that("math returning same type works", {
  check_math <- function(fun, x) {
    expect_equal(
      as.integer(fun(biginteger(x))),
      suppressWarnings(fun(x))
    )
    expect_equal(
      as.double(fun(bigfloat(x))),
      suppressWarnings(fun(x))
    )
  }

  x <- c(-2, 2)
  check_math(abs, x)
  check_math(sign, x)

  x <- c(2, 3, NA, 1)
  check_math(cumsum, x)
  check_math(cumprod, x)
  check_math(cummax, x)
  check_math(cummin, x)
})

test_that("math returning float works", {
  check_math <- function(fun, x) {
    expect_equal(
      allow_lossy_cast(as.double(fun(biginteger(x)))),
      suppressWarnings(fun(x))
    )
    expect_equal(
      allow_lossy_cast(as.double(fun(bigfloat(x)))),
      suppressWarnings(fun(x))
    )
  }

  x <- c(2, 3, NA, -1)
  check_math(sqrt, x)
  check_math(exp, x)
  check_math(expm1, x)
  check_math(log, x)
  check_math(log10, x)
  check_math(log2, x)
  check_math(log1p, x)
  check_math(cos, x)
  check_math(sin, x)
  check_math(tan, x)
  check_math(cosh, x)
  check_math(sinh, x)
  check_math(tanh, x)
  check_math(cospi, x)
  check_math(sinpi, x)
  check_math(tanpi, x)
  check_math(acos, x)
  check_math(asin, x)
  check_math(atan, x)
  check_math(acosh, x)
  check_math(asinh, x)
  check_math(atanh, x)
  check_math(gamma, x)
  check_math(lgamma, x)
})

test_that("other operations fail", {
  expect_error(trigamma(biginteger(2)), class = "bignum_error_unsupported")
  expect_error(trigamma(bigfloat(2)), class = "bignum_error_unsupported")
})
