#include <cpp11.hpp>
#include "operations.h"
#include "compare.h"
#include "format.h"
#include "bigfloat_vector.h"


[[cpp11::register]]
cpp11::strings c_bigfloat(cpp11::strings x) {
  return bigfloat_vector(x).encode();
}

/*-----------*
 *  Casting  *
 *-----------*/
[[cpp11::register]]
cpp11::logicals c_bigfloat_to_logical(cpp11::strings x) {
  bigfloat_vector input(x);
  cpp11::writable::logicals output(input.size());

  for (std::size_t i=0; i<input.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_LOGICAL;
    } else if (boost::multiprecision::isnan(input.data[i])) {
      output[i] = NA_LOGICAL;
    } else {
      output[i] = input.data[i] == 0 ? FALSE : TRUE;
    }
  }

  return output;
}

[[cpp11::register]]
cpp11::integers c_bigfloat_to_integer(cpp11::strings x) {
  bigfloat_vector input(x);
  cpp11::writable::integers output(input.size());

  int vmax = std::numeric_limits<int>::max();
  int vmin = std::numeric_limits<int>::min();

  for (std::size_t i=0; i<input.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_INTEGER;
    } else if (boost::multiprecision::isnan(input.data[i])) {
      output[i] = NA_INTEGER;
    } else if (input.data[i] < vmin || input.data[i] > vmax) {
      output[i] = NA_INTEGER;
    } else {
      output[i] = static_cast<int>(input.data[i]);
    }
  }

  return output;
}

[[cpp11::register]]
cpp11::doubles c_bigfloat_to_double(cpp11::strings x) {
  bigfloat_vector input(x);
  cpp11::writable::doubles output(input.size());

  for (std::size_t i=0; i<input.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_REAL;
    } else {
      output[i] = static_cast<double>(input.data[i]);
    }
  }

  return output;
}


/*---------*
 *  Other  *
 *---------*/
[[cpp11::register]]
cpp11::strings c_bigfloat_format(cpp11::strings x,
                                 cpp11::strings notation,
                                 cpp11::integers digits,
                                 bool is_sigfig) {
  if (digits.size() != 1) {
    cpp11::stop("`digits` must be a scalar.");
  }

  return format_bigfloat_vector(
    bigfloat_vector(x),
    format_notation(notation[0]),
    digits[0],
    is_sigfig
  );
}


/*-------------------------*
 *  Comparison operations  *
 *-------------------------*/
[[cpp11::register]]
cpp11::integers c_bigfloat_compare(cpp11::strings lhs, cpp11::strings rhs, bool na_equal) {
  return bignum_cmp(bigfloat_vector(lhs), bigfloat_vector(rhs), na_equal);
}

[[cpp11::register]]
cpp11::integers c_bigfloat_rank(cpp11::strings x) {
  return dense_rank<bigfloat_type>(bigfloat_vector(x));
}


/*-------------------------*
 *  Arithmetic operations  *
 *-------------------------*/
[[cpp11::register]]
cpp11::strings c_bigfloat_add(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return x + y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_subtract(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return x - y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_multiply(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return x * y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_divide(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return x / y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_pow(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type & y) { return boost::multiprecision::pow(x, y); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_modulo(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return boost::multiprecision::fmod(x, y); }
  ).encode();
}


/*---------------------------*
 *  Mathematical operations  *
 *---------------------------*/
[[cpp11::register]]
cpp11::strings c_bigfloat_sum(cpp11::strings x, bool na_rm) {
  return accumulate_operation(
    bigfloat_vector(x), bigfloat_vector(1, 0), na_rm,
    [](const bigfloat_type &a, const bigfloat_type &b) { return a + b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_prod(cpp11::strings x, bool na_rm) {
  return accumulate_operation(
    bigfloat_vector(x), bigfloat_vector(1, 1), na_rm,
    [](const bigfloat_type &a, const bigfloat_type &b) { return a * b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cumsum(cpp11::strings x) {
  return partial_accumulate_operation(
    bigfloat_vector(x),
    [](const bigfloat_type &a, const bigfloat_type &b) { return a + b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cumprod(cpp11::strings x) {
  return partial_accumulate_operation(
    bigfloat_vector(x),
    [](const bigfloat_type &a, const bigfloat_type &b) { return a * b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cummax(cpp11::strings x) {
  return partial_accumulate_operation(
    bigfloat_vector(x),
    [](const bigfloat_type &a, const bigfloat_type &b) { return std::max(a, b); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cummin(cpp11::strings x) {
  return partial_accumulate_operation(
    bigfloat_vector(x),
    [](const bigfloat_type &a, const bigfloat_type &b) { return std::min(a, b); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_abs(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::abs(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sign(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return x.sign(); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sqrt(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::sqrt(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_ceiling(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::ceil(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_floor(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::floor(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_trunc(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::trunc(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_exp(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::exp(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_expm1(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::expm1(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::log(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log10(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::log10(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log2(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::log2(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log1p(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::log1p(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cos(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::cos(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cosh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::cosh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sin(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::sin(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sinh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::sinh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_tan(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::tan(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_tanh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::tanh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_acos(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::acos(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_acosh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::acosh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_asin(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::asin(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_asinh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::asinh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_atan(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::atan(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_atanh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::atanh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_gamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::tgamma(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_lgamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::multiprecision::lgamma(x); }
  ).encode();
}
