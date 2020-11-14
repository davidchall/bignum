
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bignum

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bignum)](https://CRAN.R-project.org/package=bignum)
<!-- badges: end -->

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("davidchall/bignum")
```

## Usage

``` r
library(bignum)

biginteger(2)^100L
#> <biginteger[1]>
#> [1] 1267650600228229401496703205376

bigfloat(2.5)^100
#> <bigfloat[1]>
#> [1] 6223015277861141707144064053780124240590.2521687212

# calculate factorial 50!
prod(biginteger(1:50))
#> <biginteger[1]>
#> [1] 30414093201713378043612608166064768844377641568960512000000000000
```

-----

Please note that the bignum project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
