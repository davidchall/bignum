test_that("validates from", {
  expect_snapshot_error(seq(biginteger(1:2)), class = "vctrs_error_assert_size")
  expect_snapshot_error(seq(NA_biginteger_))
})

test_that("validates length.out / along.with exclusiveness", {
  expect_snapshot_error(seq(biginteger(1), length.out = 1, along.with = 2))
})

test_that("only takes two optional args", {
  x <- biginteger(1)
  expect_snapshot_error(seq(x, to = 1, by = 1, length.out = 1))
  expect_snapshot_error(seq(x, to = 1, by = 1, along.with = 1))
})

test_that("requires two optional args", {
  x <- biginteger(1)
  expect_snapshot_error(seq(x, to = 1))
  expect_snapshot_error(seq(x, by = 1))
  expect_snapshot_error(seq(x, length.out = 1))
  expect_snapshot_error(seq(x, along.with = 1))
})

test_that("validates to", {
  x <- biginteger(1)
  expect_snapshot_error(seq(x, to = 1:2, by = 1), class = "vctrs_error_assert_size")
  expect_snapshot_error(seq(x, to = "1", by = 1), class = "vctrs_error_incompatible_type")
  expect_snapshot_error(seq(x, to = NA_integer_, by = 1))
})

test_that("validates by", {
  x <- biginteger(1)
  expect_snapshot_error(seq(x, to = x, by = 1:2), class = "vctrs_error_assert_size")
  expect_snapshot_error(seq(x, to = x, by = "x"), class = "vctrs_error_incompatible_type")
  expect_snapshot_error(seq(x, to = x, by = NA_integer_))
  expect_snapshot_error(seq(x, to = x, by = 0))
  expect_snapshot_error(seq(x, to = x, by = biginteger(0)))
})

test_that("validates length.out", {
  x <- biginteger(1)
  expect_snapshot_error(seq(x, to = x, length.out = 1:2), class = "vctrs_error_assert_size")
  expect_snapshot_error(seq(x, to = x, length.out = "x"), class = "vctrs_error_incompatible_type")
  expect_snapshot_error(seq(x, to = x, length.out = NA_integer_))
  expect_snapshot_error(seq(x, to = x, length.out = -1))
})

# TODO: wait for rlang 1.0
# test_that("validates dots", {
#   expect_snapshot_error(seq(biginteger(1), 1, 1, 1, 1, 1))
# })

test_that("validates from/to/by signs", {
  expect_snapshot_error(seq(biginteger(1), to = 2, by = -1))
  expect_snapshot_error(seq(biginteger(2), to = 1, by = 1))

  expect_snapshot_error(seq(bigfloat(1), to = 2, by = -1))
  expect_snapshot_error(seq(bigfloat(2), to = 1, by = 1))
})

test_that("seq(to, by) works", {
  expect_equal(seq(biginteger(0), to = 4, by = 2), biginteger(c(0, 2, 4)))
  expect_equal(seq(biginteger(0), to = 5, by = 2), biginteger(c(0, 2, 4)))

  expect_equal(seq(biginteger(0), to = -4, by = -2), biginteger(c(0, -2, -4)))
  expect_equal(seq(biginteger(0), to = -5, by = -2), biginteger(c(0, -2, -4)))

  expect_equal(seq(biginteger(4), to = 0, by = -2), biginteger(c(4, 2, 0)))
  expect_equal(seq(biginteger(4), to = -1, by = -2), biginteger(c(4, 2, 0)))

  expect_equal(seq(bigfloat(0), to = 4, by = 2), bigfloat(c(0, 2, 4)))
  expect_equal(seq(bigfloat(0), to = 5, by = 2), bigfloat(c(0, 2, 4)))

  expect_equal(seq(bigfloat(0), to = -4, by = -2), bigfloat(c(0, -2, -4)))
  expect_equal(seq(bigfloat(0), to = -5, by = -2), bigfloat(c(0, -2, -4)))

  expect_equal(seq(bigfloat(4), to = 0, by = -2), bigfloat(c(4, 2, 0)))
  expect_equal(seq(bigfloat(4), to = -1, by = -2), bigfloat(c(4, 2, 0)))
})

test_that("seq(to, length.out) works", {
  expect_equal(seq(biginteger(0), to = 4, length.out = 2), biginteger(c(0, 4)))
  expect_equal(seq(biginteger(0), to = 4, length.out = 1), biginteger(c(0)))
  expect_equal(seq(biginteger(0), to = 4, length.out = 5), biginteger(c(0:4)))

  expect_equal(seq(biginteger(0), to = -4, length.out = 2), biginteger(c(0, -4)))
  expect_equal(seq(biginteger(0), to = -6, length.out = 3), biginteger(c(0, -3, -6)))

  expect_equal(seq(biginteger(0), to = 4, along.with = 1:2), biginteger(c(0, 4)))

  expect_equal(seq(bigfloat(0), to = 4, length.out = 2), bigfloat(c(0, 4)))
  expect_equal(seq(bigfloat(0), to = 4, length.out = 1), bigfloat(c(0)))
  expect_equal(seq(bigfloat(0), to = 4, length.out = 5), bigfloat(c(0:4)))

  expect_equal(seq(bigfloat(0), to = -4, length.out = 2), bigfloat(c(0, -4)))
  expect_equal(seq(bigfloat(0), to = -6, length.out = 3), bigfloat(c(0, -3, -6)))

  expect_equal(seq(bigfloat(0), to = 4, along.with = 1:2), bigfloat(c(0, 4)))
})

test_that("enforces non-fractional results for biginteger", {
  expect_snapshot_error(seq(biginteger(1), to = 2, length.out = 3))
  expect_snapshot_error(seq(biginteger(1), to = 2, along.with = 1:3))

  expect_equal(seq(bigfloat(1), to = 2, length.out = 3), bigfloat(c(1, 1.5, 2)))
  expect_equal(seq(bigfloat(1), to = 2, along.with = 1:3), bigfloat(c(1, 1.5, 2)))
})

test_that("seq(by, length.out) works", {
  expect_equal(seq(biginteger(0), by = 2, length.out = 3), biginteger(c(0, 2, 4)))
  expect_equal(seq(biginteger(0), by = -2, length.out = 3), biginteger(c(0, -2, -4)))

  expect_equal(seq(biginteger(0), by = 2, along.with = 1:3), biginteger(c(0, 2, 4)))

  expect_equal(seq(bigfloat(0), by = 2.5, length.out = 3), bigfloat(c(0, 2.5, 5)))
  expect_equal(seq(bigfloat(0), by = -2.5, length.out = 3), bigfloat(c(0, -2.5, -5)))

  expect_equal(seq(bigfloat(0), by = 2.5, along.with = 1:3), bigfloat(c(0, 2.5, 5)))
})

test_that("`to` is always cast to `from`", {
  expect_equal(
    seq(biginteger(0), to = bigfloat(1), by = 2),
    seq(biginteger(0), to = 1, by = 2)
  )

  expect_snapshot_error(seq(biginteger(0), to = bigfloat(2.5), by = 2))
})
