force_dec <- function(x) {
  out <- with_options(pillar.max_dec_width = Inf, pillar::pillar_shaft(x))
  out$sci <- NULL
  attr(out, "width") <- attr(out$dec, "width")
  attr(out, "min_width") <- attr(out$dec, "width")
  out
}

force_sci <- function(x) {
  out <- pillar::pillar_shaft(x)
  if (!is.null(out$dec)) {
    out$dec <- NULL
    attr(out, "width") <- attr(out$sci, "width")
    attr(out, "min_width") <- attr(out$sci, "width")
  }
  out
}

test_that("options validation", {
  expect_snapshot(error = TRUE, {
    with_options(pillar.sigfig = 1.5, pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = "1", pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.sigfig = 0, pillar::pillar_shaft(bigfloat(1)))
  })

  expect_snapshot(error = TRUE, {
    with_options(pillar.max_dec_width = 1.5, pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.max_dec_width = "1", pillar::pillar_shaft(bigfloat(1)))
    with_options(pillar.max_dec_width = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
  })
})

test_that("width calculations work", {
  expect_error(print(pillar::pillar_shaft(biginteger(2)^40L), width = 5))

  expect_snapshot({
    x <- c(biginteger(2^seq(1, 40, 10)), NA)
    pillar::pillar_shaft(x)
    print(pillar::pillar_shaft(x), width = 8)
    print(pillar::pillar_shaft(x), width = 9)
    with_options(pillar.max_dec_width = 8, pillar::pillar_shaft(x))
  })
})

test_that("decimal: omit decimal point when possible", {
  expect_snapshot(force_dec(bigfloat(10^(0:3))))
})

test_that("decimal: center aligned on decimal point", {
  expect_snapshot(force_dec(bigfloat((10^(-3:4)) * c(-1, 1))))
})

test_that("decimal: special values alignment", {
  expect_snapshot(force_dec(bigfloat(c(987.654, NA, NaN, Inf, 0.00123))))
  expect_snapshot(force_dec(bigfloat(c(987.654, NA, NaN, Inf, -Inf, 0.00123))))
})

test_that("decimal: sigfig adjustment works", {
  expect_snapshot({
    x <- bigfloat(9.87654321) * 10^(3:-3)
    force_dec(x)
    with_options(pillar.sigfig = 5, force_dec(x))
  })
})

test_that("scientific: omit signs when possible", {
  expect_snapshot(force_sci(bigfloat(10^(0:3))))
})

test_that("scientific: include signs when necessary", {
  expect_snapshot(force_sci(bigfloat((10^(-3:4)) * c(-1, 1))))
})

test_that("scientific: omit exponent when possible", {
  expect_snapshot(force_sci(bigfloat(c(0, 1, 10))))
})

test_that("scientific: exponent is right aligned", {
  expect_snapshot(force_sci(bigfloat(c(1e-100, 1e0, 1e10))))
})

test_that("scientific: special values alignment", {
  expect_snapshot(force_sci(bigfloat(c(987.654, NA, NaN, Inf, 0.00123))))
  expect_snapshot(force_sci(bigfloat(c(987.654, NA, NaN, Inf, -Inf, 0.00123))))
})

test_that("scientific: sigfig adjustment works", {
  expect_snapshot({
    x <- bigfloat(9.87654321) * 10^(3:-3)
    force_sci(x)
    with_options(pillar.sigfig = 5, force_sci(x))
  })
})
