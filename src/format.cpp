#include "format.h"


enum bignum_format_notation format_notation(const std::string &input) {
  if (input == "dec") return bignum_format_dec;
  if (input == "sci") return bignum_format_sci;
  if (input == "hex") return bignum_format_hex;
  else cpp11::stop("Found unexpected formatting notation."); // # nocov
};

cpp11::strings format_biginteger_vector(const biginteger_vector &x,
                                        enum bignum_format_notation notation) {
  cpp11::writable::strings output(x.size());
  std::stringstream ss;

  switch (notation) {
  case bignum_format_hex:
    ss.setf(std::ios::hex, std::ios::basefield);
    ss.setf(std::ios::showbase);
    break;
  case bignum_format_dec:
    break;
  default:
    cpp11::stop("Found unexpected formatting notation."); // # nocov
  }

  for (std::size_t i=0; i<x.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (x.is_na[i]) {
      output[i] = NA_STRING;
    } else if (notation == bignum_format_hex && x.data[i] < 0) {
      output[i] = NA_STRING;
    } else {
      ss << x.data[i];
      output[i] = ss.str();
      ss.str("");
    }
  }

  return output;
};

cpp11::strings format_bigfloat_vector(const bigfloat_vector &x,
                                      enum bignum_format_notation notation,
                                      int digits, bool is_sigfig) {
  cpp11::writable::strings output(x.size());

  for (std::size_t i=0; i<x.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (x.is_na[i]) {
      output[i] = NA_STRING;
    } else if (boost::multiprecision::isnan(x.data[i])) {
      output[i] = "NaN";
    } else if (isinf(x.data[i])) {
      output[i] = x.data[i] > 0 ? "Inf" : "-Inf";
    } else {
      output[i] = format_bigfloat(x.data[i], notation, digits, is_sigfig);
    }
  }

  return output;
};

std::string format_bigfloat(const bigfloat_type &x,
                            enum bignum_format_notation notation,
                            int digits, bool is_sigfig) {
  std::stringstream ss;

  bool hide_terminal_zeros = is_sigfig || digits < 0;
  digits = abs(digits);

  switch (notation) {
  case bignum_format_sci:
    ss.setf(std::ios::scientific, std::ios::floatfield);
    ss.precision(is_sigfig ? std::max(digits - 1, 0) : digits);
    break;
  case bignum_format_dec:
    ss.setf(std::ios::fixed, std::ios::floatfield);
    ss.precision(is_sigfig ? std::max(digits - predecimal_digits(x), 0) : digits);
    break;
  default:
    cpp11::stop("Found unexpected formatting notation."); // # nocov
  }

  std::string output;
  if (ss.precision() == 0) {
    // multiprecision library uses max_digits10 when precision is zero
    ss << std::setprecision(1) << boost::multiprecision::round(x);
    output = trim_terminal_zeros(ss.str(), false);
    hide_terminal_zeros = true;
  } else {
    ss << x;
    output = ss.str();
  }

  if (hide_terminal_zeros && !has_nonzero_hidden_digits(x, output)) {
    output = trim_terminal_zeros(output, true);
  }

  return output;
}

int predecimal_digits(const bigfloat_type &x) {
  if (x == 0) {
    return 1;
  }

  bigfloat_type pre = 1 + boost::multiprecision::floor(boost::multiprecision::log10(boost::multiprecision::abs(x)));
  return static_cast<int>(pre);
};

bool has_nonzero_hidden_digits(const bigfloat_type &x, const std::string &visible) {
  if (x == 0) {
    return false;
  }

  bigfloat_type y(visible);
  return boost::multiprecision::abs(x - y) >= std::numeric_limits<bigfloat_type>::epsilon();
};

std::string trim_terminal_zeros(const std::string &s, bool trim_point) {
  std::size_t pos_exp = s.find_first_of("e");

  std::string mantissa, exponent;
  if (pos_exp == std::string::npos) {
    mantissa = s;
  } else {
    mantissa = s.substr(0, pos_exp);
    exponent = s.substr(pos_exp, s.size());
  }

  mantissa = mantissa.substr(0, mantissa.find_last_not_of("0") + 1);

  if (trim_point && mantissa[mantissa.size() - 1] == '.') {
    mantissa = mantissa.substr(0, mantissa.size() - 1);
  }

  return mantissa + exponent;
};
