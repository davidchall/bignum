#ifndef __BIGNUM_FORMAT__
#define __BIGNUM_FORMAT__

#include <vector>
#include <cpp11.hpp>
#include "bigfloat_vector.h"
#include "biginteger_vector.h"


enum bignum_format_notation {
  bignum_format_fit,
  bignum_format_dec,
  bignum_format_sci,
  bignum_format_hex,
  bignum_format_encode,
  bignum_format_unknown
};

static enum bignum_format_notation format_notation(const std::string &input) {
  if (input == "fit") return bignum_format_fit;
  if (input == "dec") return bignum_format_dec;
  if (input == "sci") return bignum_format_sci;
  if (input == "hex") return bignum_format_hex;
  else return bignum_format_unknown;
};


cpp11::strings format_biginteger_vector(const biginteger_vector &x,
                                        enum bignum_format_notation notation);

cpp11::strings format_bigfloat_vector(const bigfloat_vector &x,
                                      enum bignum_format_notation notation,
                                      int digits = 0,
                                      bool is_sigfig = true);

std::string format_bigfloat(const bigfloat_type &x,
                            enum bignum_format_notation notation,
                            int digits,
                            bool is_sigfig);

int predecimal_digits(const bigfloat_type &x);
bool has_nonzero_hidden_digits(const bigfloat_type &x, const std::string &visible);
std::string trim_terminal_zeros(const std::string &s, bool trim_point);

#endif
