#include <cpp11.hpp>
#include "bigfloat_vector.h"
#include "operations.h"
#include "compare.h"
#include "format.h"

namespace mp = boost::multiprecision;


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
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_LOGICAL;
    } else if (mp::isnan(input.data[i])) {
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
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_INTEGER;
    } else if (mp::isnan(input.data[i])) {
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
    if (i % 8192 == 0) {
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
  if (notation.size() != 1) {
    cpp11::stop("`notation` must be a scalar."); // # nocov
  }
  if (digits.size() != 1) {
    cpp11::stop("`digits` must be a scalar."); // # nocov
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
    [](const bigfloat_type &x, const bigfloat_type & y) { return mp::pow(x, y); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_modulo(cpp11::strings lhs, cpp11::strings rhs) {
  return binary_operation(
    bigfloat_vector(lhs), bigfloat_vector(rhs),
    [](const bigfloat_type &x, const bigfloat_type &y) { return mp::fmod(x, y); }
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
    [](const bigfloat_type &x) { return mp::abs(x); }
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
    [](const bigfloat_type &x) { return mp::sqrt(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_ceiling(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::ceil(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_floor(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::floor(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_trunc(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::trunc(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_exp(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::exp(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_expm1(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::expm1(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::log(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log10(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::log10(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log2(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::log2(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_log1p(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::log1p(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cos(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::cos(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_cosh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::cosh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sin(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::sin(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_sinh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::sinh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_tan(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::tan(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_tanh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::tanh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_acos(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::acos(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_acosh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::acosh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_asin(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::asin(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_asinh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::asinh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_atan(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::atan(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_atanh(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::atanh(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_gamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::tgamma(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_lgamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return mp::lgamma(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_digamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::math::digamma(x); }
  ).encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_trigamma(cpp11::strings lhs) {
  return unary_operation(
    bigfloat_vector(lhs),
    [](const bigfloat_type &x) { return boost::math::trigamma(x); }
  ).encode();
}


/*-----------------------*
 *  Sequence operations  *
 *-----------------------*/
[[cpp11::register]]
cpp11::strings c_bigfloat_seq_to_by(const cpp11::strings& from,
                                    const cpp11::strings& to,
                                    const cpp11::strings& by) {
  const bigfloat_type start = bigfloat_type(std::string(from[0]));
  const bigfloat_type end = bigfloat_type(std::string(to[0]));
  const bigfloat_type step = bigfloat_type(std::string(by[0]));

  // Base seq() requires negative `by` when creating a decreasing seq, so this
  // helps be compatible with that.
  if (start > end && step > 0) {
    cpp11::stop("When `from` is greater than `to`, `by` must be negative.");
  }
  if (start < end && step < 0) {
    cpp11::stop("When `from` is less than `to`, `by` must be positive.");
  }

  const bigfloat_type num = end - start;
  const bigfloat_type den = step;
  const bigfloat_type length_out = trunc(num / den) + 1;

  const std::size_t size = static_cast<std::size_t>(length_out);

  bigfloat_vector output(size);

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_seq_to_lo(const cpp11::strings& from,
                                    const cpp11::strings& to,
                                    const cpp11::integers& length_out) {
  const bigfloat_type start = bigfloat_type(std::string(from[0]));
  const bigfloat_type end = bigfloat_type(std::string(to[0]));
  const std::size_t size = length_out[0];

  bigfloat_vector output(size);

  if (size == 1) {
    // Avoid division by zero
    output.data[0] = start;
    return output.encode();
  }

  const bigfloat_type num = end - start;
  const bigfloat_type den = static_cast<bigfloat_type>(size - 1);

  const bigfloat_type step = num / den;

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_bigfloat_seq_by_lo(const cpp11::strings& from,
                                    const cpp11::strings& by,
                                    const cpp11::integers& length_out) {
  const bigfloat_type start = bigfloat_type(std::string(from[0]));
  const bigfloat_type step = bigfloat_type(std::string(by[0]));
  const std::size_t size = length_out[0];

  bigfloat_vector output(size);

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}
