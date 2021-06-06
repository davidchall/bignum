#ifndef __BIGFLOAT_VECTOR__
#define __BIGFLOAT_VECTOR__

#include <vector>
#include <cpp11.hpp>
#include <boost/multiprecision/cpp_bin_float.hpp>


typedef boost::multiprecision::cpp_bin_float_50 bigfloat_type;

class bigfloat_vector {
public:
  std::vector<bigfloat_type> data;
  std::vector<bool> is_na;
  std::size_t size() const { return data.size(); }


  bigfloat_vector(std::size_t count = 0, const bigfloat_type &value = 0, bool is_na = false)
    : data(count, value), is_na(count, is_na) {}

  bigfloat_vector(cpp11::strings x);

  cpp11::strings encode() const;
};

#endif
