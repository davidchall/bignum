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

# width calculations work

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
        2   e0
        2.05e3
        2.10e6
        2.15e9
       NA     
    Code
      print(pillar::pillar_shaft(x), width = 9)
    Output
      <pillar_ornament>
         2   e0
         2.05e3
         2.10e6
         2.15e9
        NA     
    Code
      with_options(pillar.max_dec_width = 8, pillar::pillar_shaft(x))
    Output
      <pillar_ornament>
      2   e0
      2.05e3
      2.10e6
      2.15e9
        NA  

# decimal: omit decimal point when possible

    Code
      force_dec(bigfloat(10^(0:3)))
    Output
      <pillar_ornament>
         1
        10
       100
      1000

# decimal: center aligned on decimal point

    Code
      force_dec(bigfloat((10^(-3:4)) * c(-1, 1)))
    Output
      <pillar_ornament>
         -0.001
          0.01 
         -0.1  
          1    
        -10    
        100    
      -1000    
      10000    

# decimal: special values alignment

    Code
      force_dec(bigfloat(c(987.654, NA, NaN, Inf, 0.00123)))
    Output
      <pillar_ornament>
      988.     
       NA      
      NaN      
      Inf      
        0.00123

---

    Code
      force_dec(bigfloat(c(987.654, NA, NaN, Inf, -Inf, 0.00123)))
    Output
      <pillar_ornament>
      988.     
             NA
            NaN
            Inf
           -Inf
        0.00123

# decimal: sigfig adjustment works

    Code
      x <- bigfloat(9.87654321) * 10^(3:-3)
      force_dec(x)
    Output
      <pillar_ornament>
      9877.     
       988.     
        98.8    
         9.88   
         0.988  
         0.0988 
         0.00988
    Code
      with_options(pillar.sigfig = 5, force_dec(x))
    Output
      <pillar_ornament>
      9876.5      
       987.65     
        98.765    
         9.8765   
         0.98765  
         0.098765 
         0.0098765

# scientific: omit signs when possible

    Code
      force_sci(bigfloat(10^(0:3)))
    Output
      <pillar_ornament>
      1e0
      1e1
      1e2
      1e3

# scientific: include signs when necessary

    Code
      force_sci(bigfloat((10^(-3:4)) * c(-1, 1)))
    Output
      <pillar_ornament>
      -1e-3
       1e-2
      -1e-1
       1e+0
      -1e+1
       1e+2
      -1e+3
       1e+4

# scientific: omit exponent when possible

    Code
      force_sci(bigfloat(c(0, 1, 10)))
    Output
      <pillar_ornament>
      0  
      1e0
      1e1

# scientific: exponent is right aligned

    Code
      force_sci(bigfloat(c(1e-100, 1, 1e+10)))
    Output
      <pillar_ornament>
      1e-100
      1e+  0
      1e+ 10

# scientific: special values alignment

    Code
      force_sci(bigfloat(c(987.654, NA, NaN, Inf, 0.00123)))
    Output
      <pillar_ornament>
      9.88e+2
        NA   
       NaN   
       Inf   
      1.23e-3

---

    Code
      force_sci(bigfloat(c(987.654, NA, NaN, Inf, -Inf, 0.00123)))
    Output
      <pillar_ornament>
      9.88e+2
        NA   
       NaN   
       Inf   
      -Inf   
      1.23e-3

# scientific: sigfig adjustment works

    Code
      x <- bigfloat(9.87654321) * 10^(3:-3)
      force_sci(x)
    Output
      <pillar_ornament>
      9.88e+3
      9.88e+2
      9.88e+1
      9.88e+0
      9.88e-1
      9.88e-2
      9.88e-3
    Code
      with_options(pillar.sigfig = 5, force_sci(x))
    Output
      <pillar_ornament>
      9.8765e+3
      9.8765e+2
      9.8765e+1
      9.8765e+0
      9.8765e-1
      9.8765e-2
      9.8765e-3

