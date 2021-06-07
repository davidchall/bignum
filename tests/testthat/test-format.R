test_that("bigfloat: input validation", {
  expect_snapshot(
    {
      format(bigfloat(1), sigfig = 1, digits = 1)

      format(bigfloat(1), sigfig = 1.5)
      format(bigfloat(1), sigfig = "1")
      format(bigfloat(1), sigfig = c(1, 2))
      format(bigfloat(1), sigfig = 0)

      format(bigfloat(1), digits = 1.5)
      format(bigfloat(1), digits = "1")
      format(bigfloat(1), digits = c(1, 2))

      format(bigfloat(1), notation = "hex")
    },
    error = TRUE
  )
})

test_that("biginteger: input validation", {
  expect_snapshot(
    {
      format(biginteger(1), notation = "sci", sigfig = 1, digits = 1)

      format(biginteger(1), notation = "sci", sigfig = 1.5)
      format(biginteger(1), notation = "sci", sigfig = "1")
      format(biginteger(1), notation = "sci", sigfig = c(1, 2))
      format(biginteger(1), notation = "sci", sigfig = 0)

      format(biginteger(1), notation = "sci", digits = 1.5)
      format(biginteger(1), notation = "sci", digits = "1")
      format(biginteger(1), notation = "sci", digits = c(1, 2))

      format(biginteger(1), notation = "unknown")
    },
    error = TRUE
  )
})

test_that("options: input validation", {
  expect_snapshot(
    {
      with_options(bignum.sigfig = 1.5, format(bigfloat(1)))
      with_options(bignum.sigfig = "1", format(bigfloat(1)))
      with_options(bignum.sigfig = c(1, 2), format(bigfloat(1)))
      with_options(bignum.sigfig = 0, format(bigfloat(1)))

      with_options(bignum.max_dec_width = 1.5, format(bigfloat(1)))
      with_options(bignum.max_dec_width = "1", format(bigfloat(1)))
      with_options(bignum.max_dec_width = c(1, 2), format(bigfloat(1)))
    },
    error = TRUE
  )
})

test_that("bigfloat: dec notation works", {
  expect_snapshot({
    x <- bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345))
    format(x, sigfig = 3, notation = "dec")
    with_options(bignum.sigfig = 4, format(x, notation = "dec"))
  })

  expect_snapshot({
    x <- bigfloat(c(1, 1.00001, 123, 123.45, 567.89))
    format(x, sigfig = 3, notation = "dec")
    format(x, digits = 2, notation = "dec")
    format(x, digits = -2, notation = "dec")
  })

  expect_equal(
    format(bigfloat(c(NA, Inf, -Inf, NaN)), notation = "dec"),
    c(NA_character_, "Inf", "-Inf", "NaN")
  )
})

test_that("biginteger: dec notation works", {
  expect_snapshot({
    format(biginteger(123456789), notation = "dec")
  })

  expect_equal(format(NA_biginteger_, notation = "dec"), NA_character_)
})

test_that("bigfloat: sci notation works", {
  expect_snapshot({
    x <- bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345))
    format(x, sigfig = 3, notation = "sci")
    with_options(bignum.sigfig = 4, format(x, notation = "sci"))
  })

  expect_snapshot({
    x <- bigfloat(c(1, 1.00001, 123, 123.45, 567.89))
    format(x, sigfig = 3, notation = "sci")
    format(x, digits = 2, notation = "sci")
    format(x, digits = -2, notation = "sci")
  })

  expect_equal(
    format(bigfloat(c(NA, Inf, -Inf, NaN)), notation = "sci"),
    c(NA_character_, "Inf", "-Inf", "NaN")
  )
})

test_that("biginteger: sci notation works", {
  expect_snapshot({
    x <- biginteger(c(10000, 10001, 12345, 56789))
    format(x, sigfig = 3, notation = "sci")
    format(x, digits = 2, notation = "sci")
    format(x, digits = -2, notation = "sci")
    with_options(bignum.sigfig = 4, format(x, notation = "sci"))
  })

  expect_equal(format(NA_biginteger_, notation = "sci"), NA_character_)
})

test_that("bigfloat: fit notation works", {
  expect_snapshot({
    format(bigfloat(1234567890123), notation = "fit")
    format(bigfloat(123456789012.3), notation = "fit")
    format(bigfloat(1234567890123.4), notation = "fit")
  })

  expect_snapshot({
    with_options(bignum.max_dec_width = 9L, format(bigfloat(123456789), notation = "fit"))
    with_options(bignum.max_dec_width = 9L, format(bigfloat(12345678.9), notation = "fit"))
    with_options(bignum.max_dec_width = 9L, format(bigfloat(123456789.1), notation = "fit"))
  })
})

test_that("biginteger: fit notation works", {
  expect_snapshot({
    format(biginteger(10)^12L, notation = "fit")
    format(biginteger(10)^13L, notation = "fit")
    format(-biginteger(10)^11L, notation = "fit")
    format(-biginteger(10)^12L, notation = "fit")
  })

  expect_snapshot({
    with_options(bignum.max_dec_width = 5L, format(biginteger(10)^4L, notation = "fit"))
    with_options(bignum.max_dec_width = 5L, format(biginteger(10)^5L, notation = "fit"))
    with_options(bignum.max_dec_width = 5L, format(-biginteger(10)^3L, notation = "fit"))
    with_options(bignum.max_dec_width = 5L, format(-biginteger(10)^4L, notation = "fit"))
  })
})

test_that("biginteger: hex notation works", {
  expect_snapshot({
    format(biginteger(255), notation = "hex")
    format(biginteger(-255), notation = "hex")
  })

  expect_equal(format(NA_biginteger_, notation = "hex"), NA_character_)
})
