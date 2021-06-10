#include <cpp11.hpp>
#include "biginteger_vector.h"
#include "operations.h"
#include "compare.h"
#include "format.h"

namespace mp = boost::multiprecision;


[[cpp11::register]]
cpp11::strings c_biginteger(cpp11::strings x) {
  return biginteger_vector(x).encode();
}

/*-----------*
 *  Casting  *
 *-----------*/
[[cpp11::register]]
cpp11::logicals c_biginteger_to_logical(cpp11::strings x) {
  biginteger_vector input(x);
  cpp11::writable::logicals output(input.size());

  for (std::size_t i=0; i<input.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

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
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
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
cpp11::doubles c_biginteger_to_double(cpp11::strings x) {
  biginteger_vector input(x);
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
cpp11::strings c_biginteger_format(cpp11::strings x, cpp11::strings notation) {
  if (notation.size() != 1) {
    cpp11::stop("`notation` must be a scalar."); // # nocov
  }

  return format_biginteger_vector(
    biginteger_vector(x),
    format_notation(notation[0])
  );
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
    [](const biginteger_type &x, int y) { return mp::pow(x, y); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_modulo(cpp11::strings lhs, cpp11::strings rhs) {
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
    [](const biginteger_type &x) { return mp::abs(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_sign(cpp11::strings lhs) {
  return unary_operation(
    biginteger_vector(lhs),
    [](const biginteger_type &x) { return x.sign(); }
  ).encode();
}
