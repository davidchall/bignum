#include "biginteger_vector.h"


biginteger_vector::biginteger_vector(cpp11::strings x) : biginteger_vector(x.size()) {
  for (std::size_t i=0; i<x.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (x[i] == NA_STRING) {
      is_na[i] = true;
    } else {
      try {
        data[i] = biginteger_type(std::string(x[i]));
      } catch (...) {
        is_na[i] = true;
      }
    }
  }
}


cpp11::strings biginteger_vector::encode() const {
  cpp11::writable::strings result = format();
  result.attr("class") = {"bignum_biginteger", "bignum_vctr", "vctrs_vctr"};
  return result;
}


cpp11::strings biginteger_vector::format() const {
  cpp11::writable::strings output(size());

  for (std::size_t i=0; i<size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (is_na[i]) {
      output[i] = NA_STRING;
    } else {
      output[i] = data[i].str();
    }
  }

  return output;
}
