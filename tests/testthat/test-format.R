test_that("bigfloat: input validation", {
  mwe <- function(...) format(bigfloat(1), ...)

  expect_error(mwe(sigfig = 1, digits = 1))

  expect_error(mwe(sigfig = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(mwe(sigfig = "1"), class = "vctrs_error_incompatible_type")
  expect_error(mwe(sigfig = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(mwe(sigfig = 0), regexp = "must be 1 or greater")

  expect_error(mwe(digits = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(mwe(digits = "1"), class = "vctrs_error_incompatible_type")
  expect_error(mwe(digits = c(1, 2)), class = "vctrs_error_assert_size")

  withr::with_options(list(bignum.sigfig = 1.5), {
    expect_error(mwe(), class = "vctrs_error_cast_lossy")
  })
  withr::with_options(list(bignum.sigfig = "1"), {
    expect_error(mwe(), class = "vctrs_error_incompatible_type")
  })
  withr::with_options(list(bignum.sigfig = c(1, 2)), {
    expect_error(mwe(), class = "vctrs_error_assert_size")
  })
  withr::with_options(list(bignum.sigfig = 0), {
    expect_error(mwe(), regexp = "must be 1 or greater")
  })

  withr::with_options(list(bignum.max_dec_width = 1.5), {
    expect_error(mwe(), class = "vctrs_error_cast_lossy")
  })
  withr::with_options(list(bignum.max_dec_width = "1"), {
    expect_error(mwe(), class = "vctrs_error_incompatible_type")
  })
  withr::with_options(list(bignum.max_dec_width = c(1, 2)), {
    expect_error(mwe(), class = "vctrs_error_assert_size")
  })
  withr::with_options(list(bignum.max_dec_width = 0), {
    expect_error(mwe(), regexp = "must be 1 or greater")
  })

  expect_error(mwe(notation = "hex"))
})

test_that("biginteger: input validation", {
  mwe <- function(..., notation = "sci") format(biginteger(1), ..., notation = notation)

  expect_error(mwe(sigfig = 1, digits = 1))

  expect_error(mwe(sigfig = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(mwe(sigfig = "1"), class = "vctrs_error_incompatible_type")
  expect_error(mwe(sigfig = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(mwe(sigfig = 0), regexp = "must be 1 or greater")

  expect_error(mwe(digits = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(mwe(digits = "1"), class = "vctrs_error_incompatible_type")
  expect_error(mwe(digits = c(1, 2)), class = "vctrs_error_assert_size")

  expect_error(mwe(notation = "unknown"))
})

test_that("bigfloat: dec notation works", {
  expect_equal(
    format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), sigfig = 3, notation = "dec"),
    c("1.23", "12.3", "123.", "1235.", "12345")
  )
  withr::with_options(list(bignum.sigfig = 4), {
    expect_equal(
      format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), notation = "dec"),
      c("1.235", "12.35", "123.4", "1235.", "12345")
    )
  })

  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), sigfig = 3, notation = "dec"),
    c("1", "1.00", "123", "123.", "568.")
  )
  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), digits = 2, notation = "dec"),
    c("1.00", "1.00", "123.00", "123.45", "567.89")
  )
  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), digits = -2, notation = "dec"),
    c("1", "1.00", "123", "123.45", "567.89")
  )

  expect_equal(
    format(bigfloat(c(NA, Inf, -Inf, NaN)), notation = "dec"),
    c(NA_character_, "Inf", "-Inf", "NaN")
  )
})

test_that("biginteger: dec notation works", {
  expect_equal(format(biginteger(123456789), notation = "dec"), "123456789")

  expect_equal(format(NA_biginteger_, notation = "dec"), NA_character_)
})

test_that("bigfloat: sci notation works", {
  expect_equal(
    format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), sigfig = 3, notation = "sci"),
    c("1.23e+00", "1.23e+01", "1.23e+02", "1.23e+03", "1.23e+04")
  )
  withr::with_options(list(bignum.sigfig = 4), {
    expect_equal(
      format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), notation = "sci"),
      c("1.235e+00", "1.235e+01", "1.234e+02", "1.234e+03", "1.234e+04")
    )
  })

  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), sigfig = 3, notation = "sci"),
    c("1e+00", "1.00e+00", "1.23e+02", "1.23e+02", "5.68e+02")
  )
  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), digits = 2, notation = "sci"),
    c("1.00e+00", "1.00e+00", "1.23e+02", "1.23e+02", "5.68e+02")
  )
  expect_equal(
    format(bigfloat(c(1, 1.00001, 123, 123.45, 567.89)), digits = -2, notation = "sci"),
    c("1e+00", "1.00e+00", "1.23e+02", "1.23e+02", "5.68e+02")
  )

  expect_equal(
    format(bigfloat(c(NA, Inf, -Inf, NaN)), notation = "sci"),
    c(NA_character_, "Inf", "-Inf", "NaN")
  )
})

test_that("biginteger: sci notation works", {
  expect_equal(
    format(biginteger(c(10000, 10001, 12345, 56789)), sigfig = 3, notation = "sci"),
    c("1e+04", "1.00e+04", "1.23e+04", "5.68e+04")
  )
  withr::with_options(list(bignum.sigfig = 4), {
    expect_equal(
      format(biginteger(c(10000, 10001, 12345, 56789)), notation = "sci"),
      c("1e+04", "1.000e+04", "1.234e+04", "5.679e+04")
    )
  })
  expect_equal(
    format(biginteger(c(10000, 10001, 12345, 56789)), digits = 2, notation = "sci"),
    c("1.00e+04", "1.00e+04", "1.23e+04", "5.68e+04")
  )
  expect_equal(
    format(biginteger(c(10000, 10001, 12345, 56789)), digits = -2, notation = "sci"),
    c("1e+04", "1.00e+04", "1.23e+04", "5.68e+04")
  )

  expect_equal(format(NA_biginteger_, notation = "sci"), NA_character_)
})

test_that("bigfloat: fit notation works", {
  expect_equal(format(bigfloat(1234567890123), notation = "fit"), "1234567890123")
  expect_equal(format(bigfloat(123456789012.3), notation = "fit"), "123456789012.")
  expect_equal(format(bigfloat(1234567890123.4), notation = "fit"), "1.234568e+12")

  withr::with_options(list(bignum.max_dec_width = 9L), {
    expect_equal(format(bigfloat(123456789), notation = "fit"), "123456789")
    expect_equal(format(bigfloat(12345678.9), notation = "fit"), "12345679.")
    expect_equal(format(bigfloat(123456789.1), notation = "fit"), "1.234568e+08")
  })
})

test_that("biginteger: fit notation works", {
  expect_equal(format(biginteger(10)^12L, notation = "fit"), "1000000000000")
  expect_equal(format(biginteger(10)^13L, notation = "fit"), "1e+13")
  expect_equal(format(-biginteger(10)^11L, notation = "fit"), "-100000000000")
  expect_equal(format(-biginteger(10)^12L, notation = "fit"), "-1e+12")

  withr::with_options(list(bignum.max_dec_width = 5L), {
    expect_equal(format(biginteger(10)^4L, notation = "fit"), "10000")
    expect_equal(format(biginteger(10)^5L, notation = "fit"), "1e+05")
    expect_equal(format(-biginteger(10)^3L, notation = "fit"), "-1000")
    expect_equal(format(-biginteger(10)^4L, notation = "fit"), "-1e+04")
  })
})

test_that("biginteger: hex notation works", {
  expect_equal(format(biginteger(255), notation = "hex"), "0xff")
  expect_equal(format(biginteger(-255), notation = "hex"), NA_character_)
  expect_equal(format(NA_biginteger_, notation = "hex"), NA_character_)
})
