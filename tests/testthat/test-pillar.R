test_that("options validation", {
  expect_snapshot({
    with_options(pillar.sigfig = 1.5, pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = "1", pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = 0, pillar::pillar_shaft(bigfloat(1)))
  }, error = TRUE)

  expect_snapshot({
    with_options(pillar.max_dec_width = 1.5, pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.max_dec_width = "1", pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.max_dec_width = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
  }, error = TRUE)
})

test_that("biginteger formats correctly", {
  expect_error(print(pillar::pillar_shaft(biginteger(2)^40L), width = 5))

  expect_snapshot({
    x <- c(biginteger(2^seq(1, 40, 10)), NA)
    pillar::pillar_shaft(x)
    print(pillar::pillar_shaft(x), width = 8)
    print(pillar::pillar_shaft(x), width = 9)
  })

  expect_snapshot({
    x <- c(biginteger(2^seq(1, 40, 10)), NA)
    with_options(
      pillar.sigfig = 4,
      print(pillar::pillar_shaft(x), width = 9)
    )
    with_options(
      pillar.max_dec_width = 8,
      pillar::pillar_shaft(x)
    )
  })
})
