#ifndef __BIGNUM_FORMAT__
#define __BIGNUM_FORMAT__

#include "bigfloat_vector.h"
#include "biginteger_vector.h"


enum bignum_format_notation {
  bignum_format_dec,
  bignum_format_sci,
  bignum_format_hex
};

enum bignum_format_notation format_notation(const std::string &input);

cpp11::strings format_biginteger_vector(const biginteger_vector &x,
                                        enum bignum_format_notation notation);

cpp11::strings format_bigfloat_vector(const bigfloat_vector &x,
                                      enum bignum_format_notation notation,
                                      int digits, bool is_sigfig);

std::string format_bigfloat(const bigfloat_type &x,
                            enum bignum_format_notation notation,
                            int digits, bool is_sigfig);

int predecimal_digits(const bigfloat_type &x);
bool has_nonzero_hidden_digits(const bigfloat_type &x, const std::string &visible);
std::string trim_terminal_zeros(const std::string &s, bool trim_point);

#endif
