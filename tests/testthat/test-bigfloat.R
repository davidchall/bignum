test_that("zero-length input works", {
  expect_s3_class(bigfloat(), "bignum_bigfloat")
  expect_length(bigfloat(), 0)
})

test_that("coercion works", {
  x <- bigfloat(1:10)

  expect_equal(vec_ptype2(x, x), bigfloat())

  expect_equal(vec_ptype2(x, TRUE), bigfloat())
  expect_equal(vec_ptype2(TRUE, x), bigfloat())

  expect_equal(vec_ptype2(x, 1L), bigfloat())
  expect_equal(vec_ptype2(1L, x), bigfloat())

  expect_equal(vec_ptype2(x, 1), bigfloat())
  expect_equal(vec_ptype2(1, x), bigfloat())

  expect_equal(vec_ptype2(x, biginteger(1)), bigfloat())
  expect_equal(vec_ptype2(biginteger(1), x), bigfloat())

  expect_equal(vec_ptype2(x, NA), bigfloat())
  expect_equal(vec_ptype2(NA, x), bigfloat())

  expect_equal(vec_ptype2(x, unspecified()), bigfloat())
  expect_equal(vec_ptype2(unspecified(), x), bigfloat())

  expect_error(vec_ptype2(x, ""), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2("", x), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(data.frame(), x), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(x, data.frame()), class = "vctrs_error_incompatible_type")
})

test_that("casting works", {
  x <- bigfloat(1:10)

  expect_equal(vec_cast(x, bigfloat()), x)
  expect_equal(as_bigfloat(x), x)

  expect_equal(vec_cast(bigfloat(c(1, 0)), logical()), c(TRUE, FALSE))
  expect_equal(vec_cast(c(TRUE, FALSE), bigfloat()), bigfloat(c(1, 0)))
  expect_equal(as.logical(bigfloat(c(1, 0))), c(TRUE, FALSE))
  expect_equal(as_bigfloat(c(TRUE, FALSE)), bigfloat(c(1, 0)))

  expect_equal(vec_cast(x, integer()), 1:10)
  expect_equal(vec_cast(1:10, bigfloat()), x)
  expect_equal(as.integer(x), 1:10)
  expect_equal(as_bigfloat(1:10), x)

  expect_equal(vec_cast(x, double()), as.double(1:10))
  expect_equal(vec_cast(as.double(1:10), bigfloat()), x)
  expect_equal(as.double(x), as.double(1:10))
  expect_equal(as_bigfloat(as.double(1:10)), x)

  expect_equal(vec_cast(x, biginteger()), as_biginteger(1:10))
  expect_equal(vec_cast(as_biginteger(1:10), bigfloat()), x)
  expect_equal(as_biginteger(x), as_biginteger(1:10))
  expect_equal(as_bigfloat(as_biginteger(1:10)), x)

  expect_error(vec_cast(x, character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(as.character(1:10), bigfloat()), class = "vctrs_error_incompatible_type")
  expect_equal(as.character(x), as.character(1:10))
  expect_equal(as_bigfloat(as.character(1:10)), x)
})

test_that("lossy casts are caught", {
  expect_error(as.logical(as_bigfloat(2)), class = "vctrs_error_cast_lossy")

  # R integer type is a 32-bit signed integer
  max_integer <- as_bigfloat(2)^31 - 1
  expect_equal(as.integer(max_integer), 2147483647L)
  expect_error(as.integer(max_integer + 1L), class = "vctrs_error_cast_lossy")

  min_integer <- -max_integer
  expect_equal(as.integer(min_integer), -2147483647L)
  expect_error(as.integer(min_integer - 1L), class = "vctrs_error_cast_lossy")
})

test_that("combination works", {
  expect_s3_class(vec_c(bigfloat(), bigfloat()), "bignum_bigfloat")

  expect_s3_class(vec_c(bigfloat(), logical()), "bignum_bigfloat")
  expect_s3_class(vec_c(logical(), bigfloat()), "bignum_bigfloat")

  expect_s3_class(vec_c(bigfloat(), integer()), "bignum_bigfloat")
  expect_s3_class(vec_c(integer(), bigfloat()), "bignum_bigfloat")

  expect_s3_class(vec_c(bigfloat(), double()), "bignum_bigfloat")
  expect_s3_class(vec_c(double(), bigfloat()), "bignum_bigfloat")

  expect_s3_class(vec_c(bigfloat(), biginteger()), "bignum_bigfloat")
  expect_s3_class(vec_c(biginteger(), bigfloat()), "bignum_bigfloat")

  expect_error(vec_c(bigfloat(), character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_c(character(), bigfloat()), class = "vctrs_error_incompatible_type")
})

test_that("missing value works", {
  expect_true(is.na(NA_bigfloat_))
  expect_false(is.nan(NA_bigfloat_))
  expect_false(is.infinite(NA_bigfloat_))
  expect_false(is.finite(NA_bigfloat_))

  expect_equal(as_bigfloat(NA), NA_bigfloat_)
  expect_equal(as.logical(NA_bigfloat_), NA)

  expect_equal(as_bigfloat(NA_integer_), NA_bigfloat_)
  expect_equal(as.integer(NA_bigfloat_), NA_integer_)

  expect_equal(as_bigfloat(NA_real_), NA_bigfloat_)
  expect_equal(as.double(NA_bigfloat_), NA_real_)

  expect_equal(as_bigfloat(NA_character_), NA_bigfloat_)
  expect_equal(as.character(NA_bigfloat_), NA_character_)
})

test_that("NaN works", {
  expect_true(is.na(bigfloat(NaN)))
  expect_true(is.nan(bigfloat(NaN)))
  expect_false(is.infinite(bigfloat(NaN)))
  expect_false(is.finite(bigfloat(NaN)))

  expect_equal(as.logical(bigfloat(NaN)), NA)
  expect_equal(as.integer(bigfloat(NaN)), NA_integer_)
  expect_equal(as.double(bigfloat(NaN)), NaN)
  expect_equal(as.character(bigfloat(NaN)), "NaN")
})

test_that("infinity works", {
  expect_false(is.na(Inf))
  expect_false(is.na(-Inf))
  expect_false(is.nan(Inf))
  expect_false(is.nan(-Inf))
  expect_true(is.infinite(bigfloat(Inf)))
  expect_true(is.infinite(bigfloat(-Inf)))
  expect_false(is.finite(bigfloat(Inf)))
  expect_false(is.finite(bigfloat(-Inf)))

  expect_false(is.infinite(bigfloat(0)))
  expect_true(is.finite(bigfloat(0)))

  expect_error(as.logical(bigfloat(Inf)), class = "vctrs_error_cast_lossy")
  expect_error(as.logical(bigfloat(-Inf)), class = "vctrs_error_cast_lossy")
  expect_error(as.integer(bigfloat(Inf)), class = "vctrs_error_cast_lossy")
  expect_error(as.integer(bigfloat(-Inf)), class = "vctrs_error_cast_lossy")
  expect_equal(as.double(bigfloat(Inf)), Inf)
  expect_equal(as.double(bigfloat(-Inf)), -Inf)
  expect_equal(as.character(bigfloat(Inf)), "Inf")
  expect_equal(as.character(bigfloat(-Inf)), "-Inf")

  expect_equal(bigfloat(1) / 0, bigfloat(Inf))
  expect_equal(bigfloat(-1) / 0, bigfloat(-Inf))
})
