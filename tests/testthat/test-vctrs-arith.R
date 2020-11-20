test_that("addition works", {
  x <- c(2, NA)
  y <- 3
  ans <- x + y

  expect_equal(biginteger(x) + biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) + bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) + bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) + biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) + as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) + biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) + as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) + bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) + as.double(y), bigfloat(ans))
  expect_equal(as.double(x) + biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) + as.double(y), bigfloat(ans))
  expect_equal(as.double(x) + bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) + as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) + biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) + as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) + bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("subtraction works", {
  x <- c(2, NA)
  y <- 3
  ans <- x - y

  expect_equal(biginteger(x) - biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) - bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) - bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) - biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) - as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) - biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) - as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) - bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) - as.double(y), bigfloat(ans))
  expect_equal(as.double(x) - biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) - as.double(y), bigfloat(ans))
  expect_equal(as.double(x) - bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) - as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) - biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) - as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) - bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("multiplication works", {
  x <- c(2, NA)
  y <- 3
  ans <- x * y

  expect_equal(biginteger(x) * biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) * bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) * bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) * biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) * as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) * biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) * as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) * bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) * as.double(y), bigfloat(ans))
  expect_equal(as.double(x) * biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) * as.double(y), bigfloat(ans))
  expect_equal(as.double(x) * bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) * as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) * biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) * as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) * bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("division works", {
  x <- c(6, NA)
  y <- 3
  ans <- x / y

  expect_equal(biginteger(x) / biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) / bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) / bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) / biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) / as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) / biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) / as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) / bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) / as.double(y), bigfloat(ans))
  expect_equal(as.double(x) / biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) / as.double(y), bigfloat(ans))
  expect_equal(as.double(x) / bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) / as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) / biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) / as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) / bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("exponentiation works", {
  x <- c(2, NA)
  y <- 3
  ans <- x^y

  expect_equal(biginteger(x) ^ biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) ^ bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) ^ bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) ^ biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) ^ as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) ^ biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) ^ as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) ^ bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) ^ as.double(y), bigfloat(ans))
  expect_equal(as.double(x) ^ biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) ^ as.double(y), bigfloat(ans))
  expect_equal(as.double(x) ^ bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) ^ as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) ^ biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) ^ as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) ^ bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("modulo works", {
  x <- c(5, NA)
  y <- 2
  ans <- x %% y

  expect_equal(biginteger(x) %% biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) %% bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) %% bigfloat(y), bigfloat(ans))
  expect_equal(bigfloat(x) %% biginteger(y), bigfloat(ans))

  expect_equal(biginteger(x) %% as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) %% biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) %% as.integer(y), bigfloat(ans))
  expect_equal(as.integer(x) %% bigfloat(y), bigfloat(ans))

  expect_equal(biginteger(x) %% as.double(y), bigfloat(ans))
  expect_equal(as.double(x) %% biginteger(y), bigfloat(ans))
  expect_equal(bigfloat(x) %% as.double(y), bigfloat(ans))
  expect_equal(as.double(x) %% bigfloat(y), bigfloat(ans))

  expect_error(biginteger(x) %% as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) %% biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) %% as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) %% bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("quotient works", {
  x <- c(5, NA)
  y <- 2
  ans <- x %/% y

  expect_equal(biginteger(x) %/% biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) %/% bigfloat(y), biginteger(ans))

  expect_equal(biginteger(x) %/% bigfloat(y), biginteger(ans))
  expect_equal(bigfloat(x) %/% biginteger(y), biginteger(ans))

  expect_equal(biginteger(x) %/% as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) %/% biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) %/% as.integer(y), biginteger(ans))
  expect_equal(as.integer(x) %/% bigfloat(y), biginteger(ans))

  expect_equal(biginteger(x) %/% as.double(y), biginteger(ans))
  expect_equal(as.double(x) %/% biginteger(y), biginteger(ans))
  expect_equal(bigfloat(x) %/% as.double(y), biginteger(ans))
  expect_equal(as.double(x) %/% bigfloat(y), biginteger(ans))

  expect_error(biginteger(x) %/% as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) %/% biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) %/% as.character(y), class = "vctrs_error_incompatible_op")
  expect_error(as.character(x) %/% bigfloat(y), class = "vctrs_error_incompatible_op")
})

test_that("unary operations work", {
  x <- c(2, NA)

  expect_equal(+biginteger(x), biginteger(x))
  expect_equal(+bigfloat(x), bigfloat(x))

  expect_equal(-biginteger(x), biginteger(-x))
  expect_equal(-bigfloat(x), bigfloat(-x))
})

test_that("other operations fail", {
  x <- c(2, NA)
  y <- 3

  expect_error(biginteger(x) & biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) & bigfloat(y), class = "vctrs_error_incompatible_op")

  expect_error(biginteger(x) & bigfloat(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) & biginteger(y), class = "vctrs_error_incompatible_op")

  expect_error(biginteger(x) & as.integer(y), class = "vctrs_error_incompatible_op")
  expect_error(as.integer(x) & biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) & as.integer(y), class = "vctrs_error_incompatible_op")
  expect_error(as.integer(x) & bigfloat(y), class = "vctrs_error_incompatible_op")

  expect_error(biginteger(x) & as.double(y), class = "vctrs_error_incompatible_op")
  expect_error(as.double(x) & biginteger(y), class = "vctrs_error_incompatible_op")
  expect_error(bigfloat(x) & as.double(y), class = "vctrs_error_incompatible_op")
  expect_error(as.double(x) & bigfloat(y), class = "vctrs_error_incompatible_op")

  expect_error(!biginteger(x), class = "vctrs_error_incompatible_op")
  expect_error(!bigfloat(x), class = "vctrs_error_incompatible_op")
})
