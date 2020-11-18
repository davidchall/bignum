test_that("inequality comparisons work", {
  x <- c(-1, 0, 1, NA)

  expect_equal(biginteger(x) < 0, x < 0)
  expect_equal(biginteger(x) > 0, x > 0)
  expect_equal(biginteger(x) >= 0, x >= 0)
  expect_equal(biginteger(x) <= 0, x <= 0)

  expect_equal(bigfloat(x) < 0, x < 0)
  expect_equal(bigfloat(x) > 0, x > 0)
  expect_equal(bigfloat(x) >= 0, x >= 0)
  expect_equal(bigfloat(x) <= 0, x <= 0)

  expect_equal(
    vec_compare_bignum(biginteger(x), 0, na_equal = FALSE),
    vec_compare(x, 0, na_equal = FALSE)
  )
  expect_equal(
    vec_compare_bignum(biginteger(x), 0, na_equal = TRUE),
    vec_compare(x, 0, na_equal = TRUE)
  )
  expect_equal(
    vec_compare_bignum(bigfloat(x), 0, na_equal = FALSE),
    vec_compare(x, 0, na_equal = FALSE)
  )
  expect_equal(
    vec_compare_bignum(bigfloat(x), 0, na_equal = TRUE),
    vec_compare(x, 0, na_equal = TRUE)
  )
})
