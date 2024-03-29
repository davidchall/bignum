% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vctrs-arith.R
\name{bignum-arith}
\alias{bignum-arith}
\title{Arithmetic operations}
\value{
These arithmetic operations are \strong{type-stable}, which means the output type
depends only on the input types (not the input values). A biginteger vector
is returned when the result must be an integer (e.g., addition of two integers).
Otherwise a bigfloat vector is returned.

The following table summarizes the return type for each combination, where
"integer-like" refers to integer and biginteger vectors and "float-like"
refers to double and bigfloat vectors.\tabular{lclcl}{
   \strong{Input 1} \tab \strong{Operator} \tab \strong{Input 2} \tab  \tab \strong{Result} \cr
   Integer-like \tab +, -, *, ^, \%\% \tab Integer-like \tab -> \tab biginteger \cr
   Integer-like \tab +, -, *, ^, \%\% \tab Float-like \tab -> \tab bigfloat \cr
   Float-like \tab +, -, *, ^, \%\% \tab Integer-like \tab -> \tab bigfloat \cr
   Float-like \tab +, -, *, ^, \%\% \tab Float-like \tab -> \tab bigfloat \cr
   Any \tab / \tab Any \tab -> \tab bigfloat \cr
   Any \tab \%/\% \tab Any \tab -> \tab biginteger \cr
}
}
\description{
\link{biginteger} and \link{bigfloat} vectors support the standard arithmetic operations.
The base R documentation can be found at \link{Arithmetic}.
}
\examples{
x <- biginteger(5)
y <- bigfloat(2)

+x
-x
x + y
x - y
x * y
x / y
x^y
x \%\% y
x \%/\% y
}
\seealso{
Other bignum operations: 
\code{\link{bignum-compare}},
\code{\link{bignum-math}},
\code{\link{bignum-special}}
}
\concept{bignum operations}
