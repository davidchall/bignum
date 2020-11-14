#include <cpp11.hpp>
#include "operations.h"
#include "compare.h"
#include "biginteger_vector.h"


/*-----------*
 *  Casting  *
 *-----------*/
[[cpp11::register]]
cpp11::strings c_integer_to_biginteger(cpp11::integers x) {
  biginteger_vector output(x.size());

  for (std::size_t i=0; i<x.size(); ++i) {
    if (x[i] == NA_INTEGER) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = biginteger_type(x[i]);
      } catch (...) {
        output.is_na[i] = true;
      }
    }
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_double_to_biginteger(cpp11::doubles x) {
  biginteger_vector output(x.size());

  for (std::size_t i=0; i<x.size(); ++i) {
    if (isnan(x[i])) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = biginteger_type(x[i]);
      } catch (...) {
        output.is_na[i] = true;
      }
    }
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_character_to_biginteger(cpp11::strings x) {
  return biginteger_vector(x).encode();
}

[[cpp11::register]]
cpp11::logicals c_biginteger_to_logical(cpp11::strings x) {
  biginteger_vector input(x);
  cpp11::writable::logicals output(input.size());

  for (std::size_t i=0; i<input.size(); ++i) {
    if (input.is_na[i]) {
      output[i] = NA_LOGICAL;
    } else {
      output[i] = input.data[i] == 0 ? FALSE : TRUE;
    }
  }

  return output;
}

[[cpp11::register]]
cpp11::integers c_biginteger_to_integer(cpp11::strings x) {
  biginteger_vector input(x);
  cpp11::writable::integers output(input.size());

  int vmax = std::numeric_limits<int>::max();
  int vmin = std::numeric_limits<int>::min();

  for (std::size_t i=0; i<input.size(); ++i) {
    if (input.is_na[i]) {
      output[i] = NA_INTEGER;
    } else if (input.data[i] < vmin || input.data[i] > vmax) {
      output[i] = NA_INTEGER;
    } else {
      output[i] = input.data[i].convert_to<int>();
    }
  }

  return output;
}

[[cpp11::register]]
cpp11::doubles c_biginteger_to_double(cpp11::strings x) {
  biginteger_vector input(x);
  cpp11::writable::doubles output(input.size());

  for (std::size_t i=0; i<input.size(); ++i) {
    if (input.is_na[i]) {
      output[i] = NA_REAL;
    } else {
      output[i] = input.data[i].convert_to<double>();
    }
  }

  return output;
}


/*---------*
 *  Other  *
 *---------*/
[[cpp11::register]]
cpp11::strings c_biginteger_format(cpp11::strings x) {
  return biginteger_vector(x).format();
}


/*-------------------------*
 *  Comparison operations  *
 *-------------------------*/
[[cpp11::register]]
cpp11::integers c_biginteger_compare(cpp11::strings lhs, cpp11::strings rhs, bool na_equal) {
  return bignum_cmp(biginteger_vector(lhs), biginteger_vector(rhs), na_equal);
}

[[cpp11::register]]
cpp11::integers c_biginteger_rank(cpp11::strings x) {
  return dense_rank<biginteger_type>(biginteger_vector(x));
}


/*-------------------------*
 *  Arithmetic operations  *
 *-------------------------*/
[[cpp11::register]]
cpp11::strings c_biginteger_add(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    biginteger_vector(lhs), biginteger_vector(rhs),
    [](const biginteger_type &x, const biginteger_type &y) { return x + y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_subtract(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    biginteger_vector(lhs), biginteger_vector(rhs),
    [](const biginteger_type &x, const biginteger_type &y) { return x - y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_multiply(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    biginteger_vector(lhs), biginteger_vector(rhs),
    [](const biginteger_type &x, const biginteger_type &y) { return x * y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_pow(cpp11::strings lhs, cpp11::integers rhs) {
  return binary_operation(
    biginteger_vector(lhs), rhs,
    [](const biginteger_type &x, int y) { return boost::multiprecision::pow(x, y); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_remainder(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    biginteger_vector(lhs), biginteger_vector(rhs),
    [](const biginteger_type &x, const biginteger_type &y) { return x % y; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_quotient(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    biginteger_vector(lhs), biginteger_vector(rhs),
    [](const biginteger_type &x, const biginteger_type &y) { return x / y; }
  ).encode();
}


/*---------------------------*
 *  Mathematical operations  *
 *---------------------------*/
[[cpp11::register]]
cpp11::strings c_biginteger_sum(cpp11::strings x, bool na_rm) {
  return accumulate_operation(
    biginteger_vector(x), biginteger_vector(1, 0), na_rm,
    [](const biginteger_type &a, const biginteger_type &b) { return a + b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_prod(cpp11::strings x, bool na_rm) {
  return accumulate_operation(
    biginteger_vector(x), biginteger_vector(1, 1), na_rm,
    [](const biginteger_type &a, const biginteger_type &b) { return a * b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_cumsum(cpp11::strings x) {
  return partial_accumulate_operation(
    biginteger_vector(x),
    [](const biginteger_type &a, const biginteger_type &b) { return a + b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_cumprod(cpp11::strings x) {
  return partial_accumulate_operation(
    biginteger_vector(x),
    [](const biginteger_type &a, const biginteger_type &b) { return a * b; }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_cummax(cpp11::strings x) {
  return partial_accumulate_operation(
    biginteger_vector(x),
    [](const biginteger_type &a, const biginteger_type &b) { return std::max(a, b); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_cummin(cpp11::strings x) {
  return partial_accumulate_operation(
    biginteger_vector(x),
    [](const biginteger_type &a, const biginteger_type &b) { return std::min(a, b); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_abs(cpp11::strings lhs) {
  return unary_operation(
    biginteger_vector(lhs),
    [](const biginteger_type &x) { return boost::multiprecision::abs(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_sign(cpp11::strings lhs) {
  return unary_operation(
    biginteger_vector(lhs),
    [](const biginteger_type &x) { return x.sign(); }
  ).encode();
}
