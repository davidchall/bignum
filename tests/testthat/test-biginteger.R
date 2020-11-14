test_that("zero-length input works", {
  expect_s3_class(biginteger(), "bignum_biginteger")
  expect_length(biginteger(), 0)
})

test_that("coercion works", {
  x <- biginteger(1:10)

  expect_equal(vec_ptype2(x, x), biginteger())

  expect_equal(vec_ptype2(x, TRUE), biginteger())
  expect_equal(vec_ptype2(TRUE, x), biginteger())

  expect_equal(vec_ptype2(x, 1L), biginteger())
  expect_equal(vec_ptype2(1L, x), biginteger())

  expect_equal(vec_ptype2(x, 1), biginteger())
  expect_equal(vec_ptype2(1, x), biginteger())

  expect_equal(vec_ptype2(x, NA), biginteger())
  expect_equal(vec_ptype2(NA, x), biginteger())

  expect_equal(vec_ptype2(x, unspecified()), biginteger())
  expect_equal(vec_ptype2(unspecified(), x), biginteger())

  expect_error(vec_ptype2(x, ""), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2("", x), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(data.frame(), x), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(x, data.frame()), class = "vctrs_error_incompatible_type")
})

test_that("casting works", {
  x <- biginteger(1:10)

  expect_equal(vec_cast(x, biginteger()), x)
  expect_equal(as_biginteger(x), x)

  expect_equal(vec_cast(biginteger(c(1, 0)), logical()), c(TRUE, FALSE))
  expect_equal(vec_cast(c(TRUE, FALSE), biginteger()), biginteger(c(1, 0)))
  expect_equal(as.logical(biginteger(c(1, 0))), c(TRUE, FALSE))
  expect_equal(as_biginteger(c(TRUE, FALSE)), biginteger(c(1, 0)))

  expect_equal(vec_cast(x, integer()), 1:10)
  expect_equal(vec_cast(1:10, biginteger()), x)
  expect_equal(as.integer(x), 1:10)
  expect_equal(as_biginteger(1:10), x)

  expect_equal(vec_cast(x, double()), as.double(1:10))
  expect_equal(vec_cast(as.double(1:10), biginteger()), x)
  expect_equal(as.double(x), as.double(1:10))
  expect_equal(as_biginteger(as.double(1:10)), x)

  expect_error(vec_cast(x, character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(as.character(1:10), biginteger()), class = "vctrs_error_incompatible_type")
  expect_equal(as.character(x), as.character(1:10))
  expect_equal(as_biginteger(as.character(1:10)), x)
})

test_that("lossy casts are caught", {
  # biginteger -> logical
  lossy_val <- as_biginteger(2)
  expect_error(vec_cast(lossy_val, logical()), class = "vctrs_error_cast_lossy")
  expect_warning(as.logical(lossy_val), class = "bignum_warning_cast_lossy")

  # biginteger -> integer
  lossy_val <- as_biginteger(2)^31L
  expect_equal(as.integer(lossy_val - 1L), 2147483647L)
  expect_error(vec_cast(lossy_val, integer()), class = "vctrs_error_cast_lossy")
  expect_warning(as.integer(lossy_val), class = "bignum_warning_cast_lossy")

  lossy_val <- -lossy_val
  expect_equal(as.integer(lossy_val + 1L), -2147483647L)
  expect_error(vec_cast(lossy_val, integer()), class = "vctrs_error_cast_lossy")
  expect_warning(as.integer(lossy_val), class = "bignum_warning_cast_lossy")

  # biginteger -> double
  lossy_val <- as_biginteger(2)^53L
  expect_equal(as.double(lossy_val - 1L), 9007199254740991)
  expect_error(vec_cast(lossy_val, double()), class = "vctrs_error_cast_lossy")
  expect_warning(as.double(lossy_val), class = "bignum_warning_cast_lossy")

  lossy_val <- -lossy_val
  expect_equal(as.double(lossy_val + 1L), -9007199254740991)
  expect_error(vec_cast(lossy_val, double()), class = "vctrs_error_cast_lossy")
  expect_warning(as.double(lossy_val), class = "bignum_warning_cast_lossy")

  # double -> biginteger
  lossy_val <- 1.5
  expect_error(vec_cast(lossy_val, biginteger()), class = "vctrs_error_cast_lossy")
  expect_warning(as_biginteger(lossy_val), class = "bignum_warning_cast_lossy")
})

test_that("combination works", {
  expect_s3_class(vec_c(biginteger(), biginteger()), "bignum_biginteger")

  expect_s3_class(vec_c(biginteger(), logical()), "bignum_biginteger")
  expect_s3_class(vec_c(logical(), biginteger()), "bignum_biginteger")

  expect_s3_class(vec_c(biginteger(), integer()), "bignum_biginteger")
  expect_s3_class(vec_c(integer(), biginteger()), "bignum_biginteger")

  expect_s3_class(vec_c(biginteger(), double()), "bignum_biginteger")
  expect_s3_class(vec_c(double(), biginteger()), "bignum_biginteger")

  expect_error(vec_c(biginteger(), character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_c(character(), biginteger()), class = "vctrs_error_incompatible_type")
})

test_that("missing value works", {
  expect_true(is.na(NA_biginteger_))

  expect_equal(as_biginteger(NA), NA_biginteger_)
  expect_equal(as.logical(NA_biginteger_), NA)

  expect_equal(as_biginteger(NA_integer_), NA_biginteger_)
  expect_equal(as.integer(NA_biginteger_), NA_integer_)

  expect_equal(as_biginteger(NA_real_), NA_biginteger_)
  expect_equal(as.double(NA_biginteger_), NA_real_)

  expect_equal(as_biginteger(NA_character_), NA_biginteger_)
  expect_equal(as.character(NA_biginteger_), NA_character_)
})
