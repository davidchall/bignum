test_that("sum() works", {
  expect_equal(sum(biginteger(c(2, 3, NA)), na.rm = TRUE), biginteger(5))
  expect_equal(sum(biginteger(c(2, 3, NA)), na.rm = FALSE), NA_biginteger_)

  expect_equal(sum(bigfloat(c(2, 3, NA)), na.rm = TRUE), bigfloat(5))
  expect_equal(sum(bigfloat(c(2, 3, NA)), na.rm = FALSE), NA_bigfloat_)
})

test_that("prod() works", {
  expect_equal(prod(biginteger(c(2, 3, NA)), na.rm = TRUE), biginteger(6))
  expect_equal(prod(biginteger(c(2, 3, NA)), na.rm = FALSE), NA_biginteger_)

  expect_equal(prod(bigfloat(c(2, 3, NA)), na.rm = TRUE), bigfloat(6))
  expect_equal(prod(bigfloat(c(2, 3, NA)), na.rm = FALSE), NA_bigfloat_)
})

test_that("mean() works", {
  expect_equal(mean(biginteger(c(2, 3, NA)), na.rm = TRUE), bigfloat(2.5))
  expect_equal(mean(biginteger(c(2, 3, NA)), na.rm = FALSE), NA_bigfloat_)
  expect_equal(mean(biginteger(c(2, 3)), na.rm = FALSE), bigfloat(2.5))

  expect_equal(mean(bigfloat(c(2, 3, NA)), na.rm = TRUE), bigfloat(2.5))
  expect_equal(mean(bigfloat(c(2, 3, NA)), na.rm = FALSE), NA_bigfloat_)
  expect_equal(mean(bigfloat(c(2, 3)), na.rm = FALSE), bigfloat(2.5))
})

test_that("abs() works", {
  x <- c(2, -2)
  ans <- c(2, 2)

  expect_equal(abs(biginteger(x)), biginteger(ans))
  expect_equal(abs(bigfloat(x)), bigfloat(ans))
})

test_that("sign() works", {
  x <- c(2, -2)
  ans <- c(1, -1)

  expect_equal(sign(biginteger(x)), biginteger(ans))
  expect_equal(sign(bigfloat(x)), bigfloat(ans))
})

test_that("sqrt() works", {
  x <- 4
  ans <- 2

  expect_equal(sqrt(biginteger(x)), bigfloat(ans))
  expect_equal(sqrt(bigfloat(x)), bigfloat(ans))
})

test_that("ceiling() works", {
  x <- c(2.5, -2.5)
  ans <- c(3, -2)

  expect_equal(ceiling(biginteger(ceiling(x))), biginteger(ans))
  expect_equal(ceiling(bigfloat(x)), bigfloat(ans))
})

test_that("floor() works", {
  x <- c(2.5, -2.5)
  ans <- c(2, -3)

  expect_equal(floor(biginteger(floor(x))), biginteger(ans))
  expect_equal(floor(bigfloat(x)), bigfloat(ans))
})

test_that("trunc() works", {
  x <- c(2.5, -2.5)
  ans <- c(2, -2)

  expect_equal(trunc(biginteger(trunc(x))), biginteger(ans))
  expect_equal(trunc(bigfloat(x)), bigfloat(ans))
})

test_that("other operations fail", {
  expect_error(trigamma(biginteger(2)), class = "bignum_error_unsupported")
  expect_error(trigamma(bigfloat(2)), class = "bignum_error_unsupported")
})
