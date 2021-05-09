#include "bigfloat_vector.h"


bigfloat_vector::bigfloat_vector(cpp11::strings x) : bigfloat_vector(x.size()) {
  for (std::size_t i=0; i<x.size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (x[i] == NA_STRING) {
      is_na[i] = true;
    } else {
      try {
        data[i] = bigfloat_type(std::string(x[i]));
      } catch (...) {
        is_na[i] = true;
      }
    }
  }
}


cpp11::strings bigfloat_vector::format(std::stringstream &ss) const {
  cpp11::writable::strings output(size());

  for (std::size_t i=0; i<size(); ++i) {
    if (i % 10000 == 0) {
      cpp11::check_user_interrupt();
    }

    if (is_na[i]) {
      output[i] = NA_STRING;
    } else if (boost::multiprecision::isnan(data[i])) {
      output[i] = "NaN";
    } else if (isinf(data[i])) {
      output[i] = data[i] > 0 ? "Inf" : "-Inf";
    } else {
      ss << data[i];
      output[i] = ss.str();
      ss.str("");
    }
  }

  return output;
}


cpp11::strings bigfloat_vector::encode() const {
  // https://www.boost.org/doc/libs/1_74_0/libs/multiprecision/doc/html/boost_multiprecision/tut/limits/constants.html#boost_multiprecision.tut.limits.constants.max_digits10
  std::stringstream ss;
  ss.precision(std::numeric_limits<bigfloat_type>::max_digits10);

  cpp11::writable::strings output = format(ss);

  output.attr("class") = {"bignum_bigfloat", "bignum_vctr", "vctrs_vctr"};
  return output;
}
