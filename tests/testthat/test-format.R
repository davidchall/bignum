test_that("full precision works", {
  one_third <- bigfloat(1) / 3
  expect_equal(nchar(format(one_third)), 52)
  expect_equal(nchar(format(one_third + 1)), 51)
})

test_that("trailing zeros dropped", {
  expect_equal(format(bigfloat(0.2)), "0.2")
})
