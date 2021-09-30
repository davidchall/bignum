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
  expect_equal(is.nan(suppressWarnings(biginteger(x))), is.nan(suppressWarnings(as.integer(x))))
  expect_equal(is.nan(bigfloat(x)), is.nan(x))
  expect_equal(is.finite(suppressWarnings(biginteger(x))), is.finite(suppressWarnings(as.integer(x))))
  expect_equal(is.finite(bigfloat(x)), is.finite(x))
  expect_equal(is.infinite(suppressWarnings(biginteger(x))), is.infinite(suppressWarnings(as.integer(x))))
  expect_equal(is.infinite(bigfloat(x)), is.infinite(x))

  x <- bigfloat("1e1000")
  expect_true(is.finite(x))
  expect_false(is.infinite(x))
})

test_that("math returning same type works", {
  check_math <- function(x, fun, ...) {
    expect_equal(
      as.integer(fun(biginteger(x), ...)),
      suppressWarnings(fun(x, ...))
    )
    expect_equal(
      as.double(fun(bigfloat(x), ...)),
      suppressWarnings(fun(x, ...))
    )
  }

  x <- c(-2, 2)
  check_math(x, abs)
  check_math(x, sign)

  x <- c(2, 3, NA, 1)
  check_math(x, cumsum)
  check_math(x, cumprod)
  check_math(x, cummax)
  check_math(x, cummin)
})

test_that("math returning float works", {
  check_math <- function(x, fun, ...) {
    expect_equal(
      suppressWarnings(as.double(fun(biginteger(x), ...))),
      suppressWarnings(fun(x, ...))
    )
    expect_equal(
      suppressWarnings(as.double(fun(bigfloat(x), ...))),
      suppressWarnings(fun(x, ...))
    )
  }

  x <- c(2, 3, NA, -1)
  check_math(x, sqrt)
  check_math(x, exp)
  check_math(x, expm1)
  check_math(x, log)
  check_math(x, log, 2)
  check_math(x, log, base = 2)
  check_math(x, log10)
  check_math(x, log2)
  check_math(x, log1p)
  check_math(x, cos)
  check_math(x, sin)
  check_math(x, tan)
  check_math(x, cosh)
  check_math(x, sinh)
  check_math(x, tanh)
  check_math(x, cospi)
  check_math(x, sinpi)
  check_math(x, tanpi)
  check_math(c(-1, 0, 1, NA), acos)
  check_math(c(-1, 0, 1, NA), asin)
  check_math(x, atan)
  check_math(x, acosh)
  check_math(x, asinh)
  check_math(x, atanh)
  check_math(x, gamma)
  check_math(x, lgamma)
  check_math(x, digamma)
  check_math(c(1, NA), trigamma)
})
