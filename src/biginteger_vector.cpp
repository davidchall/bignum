#include "biginteger_vector.h"
#include "format.h"


biginteger_vector::biginteger_vector(cpp11::strings x) : biginteger_vector(x.size()) {
  std::size_t vsize = x.size();
  for (std::size_t i=0; i<vsize; ++i) {
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
  cpp11::writable::strings output = format_biginteger_vector(*this, bignum_format_encode);

  output.attr("class") = {"bignum_biginteger", "bignum_vctr", "vctrs_vctr"};
  return output;
}
