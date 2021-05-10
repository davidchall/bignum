#ifndef __BIGINTEGER_VECTOR__
#define __BIGINTEGER_VECTOR__

#include <vector>
#include <cpp11.hpp>
#include <boost/multiprecision/cpp_int.hpp>


typedef boost::multiprecision::checked_cpp_int biginteger_type;

class biginteger_vector {
public:
  std::vector<biginteger_type> data;
  std::vector<bool> is_na;
  std::size_t size() const { return data.size(); }


  biginteger_vector(std::size_t count = 0, const biginteger_type &value = 0, bool is_na = false)
    : data(count, value), is_na(count, is_na) {}

  biginteger_vector(cpp11::strings x);

  cpp11::strings format() const;
  cpp11::strings encode() const;
};

#endif
