# biginteger formats correctly

    Code
      pillar::pillar_shaft(x)
    Output
      <pillar_ornament>
               2
            2048
         2097152
      2147483648
              NA

---

    Code
      print(pillar::pillar_shaft(x), width = 8)
    Output
      <pillar_ornament>
         2e+00
      2.05e+03
      2.10e+06
      2.15e+09
            NA

---

    Code
      print(pillar::pillar_shaft(x), width = 9)
    Output
      <pillar_ornament>
          2e+00
       2.05e+03
       2.10e+06
       2.15e+09
             NA

