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

  # difficult cases
  expect_equal(biginteger(x) < 0.5, x < 0.5)
  expect_equal(biginteger(10)^100L < 0.5, FALSE)

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
  expect_equal(
    vec_compare_bignum(0, bigfloat(x), na_equal = FALSE),
    vec_compare(0, x, na_equal = FALSE)
  )
  expect_equal(
    vec_compare_bignum(0, bigfloat(x), na_equal = TRUE),
    vec_compare(0, x, na_equal = TRUE)
  )
  expect_equal(
    vec_compare_bignum(bigfloat(x), NA, na_equal = FALSE),
    vec_compare(x, NA, na_equal = FALSE)
  )
  expect_equal(
    vec_compare_bignum(bigfloat(x), NA, na_equal = TRUE),
    vec_compare(x, NA, na_equal = TRUE)
  )
})

test_that("sort works", {
  x <- c(0, -1, 1, NA)

  expect_equal(
    sort(biginteger(x), decreasing = FALSE, na.last = NA),
    biginteger(c(-1, 0, 1))
  )
  expect_equal(
    sort(biginteger(x), decreasing = TRUE, na.last = NA),
    biginteger(c(1, 0, -1))
  )
  expect_equal(
    sort(biginteger(x), decreasing = FALSE, na.last = TRUE),
    biginteger(c(-1, 0, 1, NA))
  )
  expect_equal(
    sort(biginteger(x), decreasing = FALSE, na.last = FALSE),
    biginteger(c(NA, -1, 0, 1))
  )

  expect_equal(
    sort(bigfloat(x), decreasing = FALSE, na.last = NA),
    bigfloat(c(-1, 0, 1))
  )
  expect_equal(
    sort(bigfloat(x), decreasing = TRUE, na.last = NA),
    bigfloat(c(1, 0, -1))
  )
  expect_equal(
    sort(bigfloat(x), decreasing = FALSE, na.last = TRUE),
    bigfloat(c(-1, 0, 1, NA))
  )
  expect_equal(
    sort(bigfloat(x), decreasing = FALSE, na.last = FALSE),
    bigfloat(c(NA, -1, 0, 1))
  )
})

test_that("min/max works", {
  x <- c(0, -1, 1, NA)

  expect_equal(min(biginteger(x), na.rm = FALSE), NA_biginteger_)
  expect_equal(min(biginteger(x), na.rm = TRUE), biginteger(-1))
  expect_equal(max(biginteger(x), na.rm = FALSE), NA_biginteger_)
  expect_equal(max(biginteger(x), na.rm = TRUE), biginteger(1))

  expect_equal(min(bigfloat(x), na.rm = FALSE), NA_bigfloat_)
  expect_equal(min(bigfloat(x), na.rm = TRUE), bigfloat(-1))
  expect_equal(max(bigfloat(x), na.rm = FALSE), NA_bigfloat_)
  expect_equal(max(bigfloat(x), na.rm = TRUE), bigfloat(1))
})
