#include "bigfloat_vector.h"
#include "format.h"


bigfloat_vector::bigfloat_vector(cpp11::strings x) : bigfloat_vector(x.size()) {
  std::size_t vsize = x.size();
  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 8192 == 0) {
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


cpp11::strings bigfloat_vector::encode() const {
  cpp11::writable::strings output = format_bigfloat_vector(
    *this,
    bignum_format_dec,
    std::numeric_limits<bigfloat_type>::max_digits10,
    true
  );

  output.attr("class") = {"bignum_bigfloat", "bignum_vctr", "vctrs_vctr"};
  return output;
}
