# options validation

    Code
      with_options(pillar.sigfig = 1.5, pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.sigfig must be a non-zero positive integer.
    Code
      with_options(pillar.sigfig = "1", pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.sigfig must be a non-zero positive integer.
    Code
      with_options(pillar.sigfig = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.sigfig must be a non-zero positive integer.
    Code
      with_options(pillar.sigfig = 0, pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.sigfig must be a non-zero positive integer.

---

    Code
      with_options(pillar.max_dec_width = 1.5, pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.max_dec_width must be an integer.
    Code
      with_options(pillar.max_dec_width = "1", pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.max_dec_width must be an integer.
    Code
      with_options(pillar.max_dec_width = c(1, 2), pillar::pillar_shaft(bigfloat(1)))
    Error <rlang_error>
      Option pillar.max_dec_width must be an integer.

# biginteger formats correctly

    Code
      x <- c(biginteger(2^seq(1, 40, 10)), NA)
      pillar::pillar_shaft(x)
    Output
      <pillar_ornament>
               2
            2048
         2097152
      2147483648
              NA
    Code
      print(pillar::pillar_shaft(x), width = 8)
    Output
      <pillar_ornament>
         2e+00
      2.05e+03
      2.10e+06
      2.15e+09
            NA
    Code
      print(pillar::pillar_shaft(x), width = 9)
    Output
      <pillar_ornament>
          2e+00
       2.05e+03
       2.10e+06
       2.15e+09
             NA

---

    Code
      x <- c(biginteger(2^seq(1, 40, 10)), NA)
      with_options(pillar.sigfig = 4, print(pillar::pillar_shaft(x), width = 9))
    Output
      <pillar_ornament>
          2e+00
      2.048e+03
      2.097e+06
      2.147e+09
             NA
    Code
      with_options(pillar.max_dec_width = 8, pillar::pillar_shaft(x))
    Output
      <pillar_ornament>
         2e+00
      2.05e+03
      2.10e+06
      2.15e+09
            NA
