test_that("biginteger formats correctly", {
  x <- c(biginteger(2^seq(1, 40, 10)), NA)

  expect_snapshot(pillar::pillar_shaft(x))
  expect_snapshot(print(pillar::pillar_shaft(x), width = 8))
  expect_snapshot(print(pillar::pillar_shaft(x), width = 9))
  expect_error(print(pillar::pillar_shaft(x), width = 5))
})
