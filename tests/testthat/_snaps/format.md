# bigfloat: input validation

    Code
      format(bigfloat(1), sigfig = 1, digits = 1)
    Error <rlang_error>
      The `sigfig` or `digits` arguments are mutually exclusive.
    Code
      format(bigfloat(1), sigfig = 1.5)
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(bigfloat(1), sigfig = "1")
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(bigfloat(1), sigfig = c(1, 2))
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(bigfloat(1), sigfig = 0)
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(bigfloat(1), digits = 1.5)
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(bigfloat(1), digits = "1")
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(bigfloat(1), digits = c(1, 2))
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(bigfloat(1), notation = "hex")
    Error <rlang_error>
      `notation` must be one of "fit", "dec", or "sci".

# biginteger: input validation

    Code
      format(biginteger(1), notation = "sci", sigfig = 1, digits = 1)
    Error <rlang_error>
      The `sigfig` or `digits` arguments are mutually exclusive.
    Code
      format(biginteger(1), notation = "sci", sigfig = 1.5)
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(biginteger(1), notation = "sci", sigfig = "1")
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(biginteger(1), notation = "sci", sigfig = c(1, 2))
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(biginteger(1), notation = "sci", sigfig = 0)
    Error <rlang_error>
      `sigfig` must be a non-zero positive integer.
    Code
      format(biginteger(1), notation = "sci", digits = 1.5)
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(biginteger(1), notation = "sci", digits = "1")
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(biginteger(1), notation = "sci", digits = c(1, 2))
    Error <rlang_error>
      `digits` must be an integer.
    Code
      format(biginteger(1), notation = "unknown")
    Error <rlang_error>
      `notation` must be one of "fit", "dec", "sci", or "hex".

# options: input validation

    Code
      with_options(bignum.sigfig = 1.5, format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.sigfig must be a non-zero positive integer.
    Code
      with_options(bignum.sigfig = "1", format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.sigfig must be a non-zero positive integer.
    Code
      with_options(bignum.sigfig = c(1, 2), format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.sigfig must be a non-zero positive integer.
    Code
      with_options(bignum.sigfig = 0, format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.sigfig must be a non-zero positive integer.
    Code
      with_options(bignum.max_dec_width = 1.5, format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.max_dec_width must be an integer.
    Code
      with_options(bignum.max_dec_width = "1", format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.max_dec_width must be an integer.
    Code
      with_options(bignum.max_dec_width = c(1, 2), format(bigfloat(1)))
    Error <rlang_error>
      Option bignum.max_dec_width must be an integer.

# bigfloat: dec notation works

    Code
      x <- bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345))
      format(x, sigfig = 3, notation = "dec")
    Output
      [1] "1.23"  "12.3"  "123."  "1235." "12345"
    Code
      with_options(bignum.sigfig = 4, format(x, notation = "dec"))
    Output
      [1] "1.235" "12.35" "123.4" "1235." "12345"

---

    Code
      x <- bigfloat(c(1, 1.00001, 123, 123.45, 567.89))
      format(x, sigfig = 3, notation = "dec")
    Output
      [1] "1"    "1.00" "123"  "123." "568."
    Code
      format(x, digits = 2, notation = "dec")
    Output
      [1] "1.00"   "1.00"   "123.00" "123.45" "567.89"
    Code
      format(x, digits = -2, notation = "dec")
    Output
      [1] "1"      "1.00"   "123"    "123.45" "567.89"

# biginteger: dec notation works

    Code
      format(biginteger(123456789), notation = "dec")
    Output
      [1] "123456789"

# bigfloat: sci notation works

    Code
      x <- bigfloat(c(1.2345, 12.345, 123.45, 1234.5, 12345))
      format(x, sigfig = 3, notation = "sci")
    Output
      [1] "1.23e+00" "1.23e+01" "1.23e+02" "1.23e+03" "1.23e+04"
    Code
      with_options(bignum.sigfig = 4, format(x, notation = "sci"))
    Output
      [1] "1.235e+00" "1.235e+01" "1.234e+02" "1.234e+03" "1.234e+04"

---

    Code
      x <- bigfloat(c(1, 1.00001, 123, 123.45, 567.89))
      format(x, sigfig = 3, notation = "sci")
    Output
      [1] "1e+00"    "1.00e+00" "1.23e+02" "1.23e+02" "5.68e+02"
    Code
      format(x, digits = 2, notation = "sci")
    Output
      [1] "1.00e+00" "1.00e+00" "1.23e+02" "1.23e+02" "5.68e+02"
    Code
      format(x, digits = -2, notation = "sci")
    Output
      [1] "1e+00"    "1.00e+00" "1.23e+02" "1.23e+02" "5.68e+02"

# biginteger: sci notation works

    Code
      x <- biginteger(c(10000, 10001, 12345, 56789))
      format(x, sigfig = 3, notation = "sci")
    Output
      [1] "1e+04"    "1.00e+04" "1.23e+04" "5.68e+04"
    Code
      format(x, digits = 2, notation = "sci")
    Output
      [1] "1.00e+04" "1.00e+04" "1.23e+04" "5.68e+04"
    Code
      format(x, digits = -2, notation = "sci")
    Output
      [1] "1e+04"    "1.00e+04" "1.23e+04" "5.68e+04"
    Code
      with_options(bignum.sigfig = 4, format(x, notation = "sci"))
    Output
      [1] "1e+04"     "1.000e+04" "1.234e+04" "5.679e+04"

# bigfloat: fit notation works

    Code
      format(bigfloat(1234567890123), notation = "fit")
    Output
      [1] "1234567890123"
    Code
      format(bigfloat(123456789012.3), notation = "fit")
    Output
      [1] "123456789012."
    Code
      format(bigfloat(1234567890123.4), notation = "fit")
    Output
      [1] "1.234568e+12"

---

    Code
      with_options(bignum.max_dec_width = 9L, format(bigfloat(123456789), notation = "fit"))
    Output
      [1] "123456789"
    Code
      with_options(bignum.max_dec_width = 9L, format(bigfloat(12345678.9), notation = "fit"))
    Output
      [1] "12345679."
    Code
      with_options(bignum.max_dec_width = 9L, format(bigfloat(123456789.1), notation = "fit"))
    Output
      [1] "1.234568e+08"

# biginteger: fit notation works

    Code
      format(biginteger(10)^12L, notation = "fit")
    Output
      [1] "1000000000000"
    Code
      format(biginteger(10)^13L, notation = "fit")
    Output
      [1] "1e+13"
    Code
      format(-biginteger(10)^11L, notation = "fit")
    Output
      [1] "-100000000000"
    Code
      format(-biginteger(10)^12L, notation = "fit")
    Output
      [1] "-1e+12"

---

    Code
      with_options(bignum.max_dec_width = 5L, format(biginteger(10)^4L, notation = "fit"))
    Output
      [1] "10000"
    Code
      with_options(bignum.max_dec_width = 5L, format(biginteger(10)^5L, notation = "fit"))
    Output
      [1] "1e+05"
    Code
      with_options(bignum.max_dec_width = 5L, format(-biginteger(10)^3L, notation = "fit"))
    Output
      [1] "-1000"
    Code
      with_options(bignum.max_dec_width = 5L, format(-biginteger(10)^4L, notation = "fit"))
    Output
      [1] "-1e+04"

# biginteger: hex notation works

    Code
      format(biginteger(255), notation = "hex")
    Output
      [1] "0xff"
    Code
      format(biginteger(-255), notation = "hex")
    Output
      [1] NA

