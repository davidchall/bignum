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
    if (i % 8192 == 0) {
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
    if (i % 8192 == 0) {
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


/*-----------------------*
 *  Sequence operations  *
 *-----------------------*/
[[cpp11::register]]
cpp11::strings c_biginteger_seq_to_by(const cpp11::strings& from,
                                      const cpp11::strings& to,
                                      const cpp11::strings& by) {
  const biginteger_type start = biginteger_type(std::string(from[0]));
  const biginteger_type end = biginteger_type(std::string(to[0]));
  const biginteger_type step = biginteger_type(std::string(by[0]));

  // Base seq() requires negative `by` when creating a decreasing seq, so this
  // helps be compatible with that.
  if (start > end && step > 0) {
    cpp11::stop("When `from` is greater than `to`, `by` must be negative.");
  }
  if (start < end && step < 0) {
    cpp11::stop("When `from` is less than `to`, `by` must be positive.");
  }

  const biginteger_type num = end - start;
  const biginteger_type den = step;
  const biginteger_type length_out = num / den + 1;

  const std::size_t size = static_cast<std::size_t>(length_out);

  biginteger_vector output(size);

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_seq_to_lo(const cpp11::strings& from,
                                      const cpp11::strings& to,
                                      const cpp11::integers& length_out) {
  const biginteger_type start = biginteger_type(std::string(from[0]));
  const biginteger_type end = biginteger_type(std::string(to[0]));
  const std::size_t size = length_out[0];

  biginteger_vector output(size);

  if (size == 1) {
    // Avoid division by zero
    output.data[0] = start;
    return output.encode();
  }

  const biginteger_type num = end - start;
  const biginteger_type den = static_cast<biginteger_type>(size - 1);

  const biginteger_type step = num / den;
  const biginteger_type rem = num % den;

  if (rem != 0) {
    cpp11::stop(
      "The supplied output size does not result in a non-fractional "
      "sequence between `from` and `to`."
    );
  }

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}

[[cpp11::register]]
cpp11::strings c_biginteger_seq_by_lo(const cpp11::strings& from,
                                      const cpp11::strings& by,
                                      const cpp11::integers& length_out) {
  const biginteger_type start = biginteger_type(std::string(from[0]));
  const biginteger_type step = biginteger_type(std::string(by[0]));
  const std::size_t size = length_out[0];

  biginteger_vector output(size);

  for (std::size_t i=0; i<size; ++i) {
    output.data[i] = start + step * i;
  }

  return output.encode();
}
