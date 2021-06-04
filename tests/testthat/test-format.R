test_that("trailing zeros dropped", {
  expect_equal(format(bigfloat(0.2)), "0.2")
})

test_that("input validation", {
  expect_error(format(bigfloat(1), sigfig = 1, digits = 1))

  expect_error(format(bigfloat(1), sigfig = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(format(bigfloat(1), sigfig = "1"), class = "vctrs_error_incompatible_type")
  expect_error(format(bigfloat(1), sigfig = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(format(bigfloat(1), sigfig = 0))

  expect_error(format(bigfloat(1), digits = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(format(bigfloat(1), digits = "1"), class = "vctrs_error_incompatible_type")
  expect_error(format(bigfloat(1), digits = c(1, 2)), class = "vctrs_error_assert_size")
})

test_that("sigfig works", {
  expect_equal(
    format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), sigfig = 3, notation = "dec"),
    c("1.23", "12.3", "123.", "1235.", "12345")
  )
  expect_equal(
    format(bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345)), sigfig = 3, notation = "sci"),
    c("1.23e+00", "1.23e+01", "1.23e+02", "1.23e+03", "1.23e+04")
  )
})
