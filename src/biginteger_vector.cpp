#include "biginteger_vector.h"
#include "format.h"


biginteger_vector::biginteger_vector(cpp11::strings x) : biginteger_vector(x.size()) {
  std::size_t vsize = x.size();
  for (std::size_t i=0; i<vsize; ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (x[i] == NA_STRING || x[i].size() == 0) {
      is_na[i] = true;
    } else {
      try {
        std::string str(x[i]);

        // remove leading zeros (unless hexadecimal)
        if (str[0] == '0') {
          if (str.size() >= 2 && str.compare(0, 2, "0x") != 0 && str.compare(0, 2, "0X") != 0) {
            str.erase(0, str.find_first_not_of('0'));
          }
        }

        data[i] = biginteger_type(str);
      } catch (...) {
        is_na[i] = true;
      }
    }
  }
}


cpp11::strings biginteger_vector::encode() const {
  cpp11::writable::strings output = format_biginteger_vector(*this, bignum_format_dec);

  output.attr("class") = {"bignum_biginteger", "bignum_vctr", "vctrs_vctr"};
  return output;
}
