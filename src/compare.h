#include <vector>
#include <algorithm>
#include <cpp11.hpp>


template<class Vec>
cpp11::integers bignum_cmp(const Vec &lhs, const Vec &rhs, bool na_equal) {
  if (lhs.size() != rhs.size()) {
    cpp11::stop("Incompatible sizes"); // # nocov
  }

  cpp11::writable::integers output(lhs.size());

  for (std::size_t i=0; i<lhs.size(); ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (!na_equal && (lhs.is_na[i] || rhs.is_na[i])) {
      output[i] = NA_INTEGER;
    } else if (lhs.is_na[i] && rhs.is_na[i]) {
      output[i] = 0;
    } else if (lhs.is_na[i]) {
      output[i] = -1;
    } else if (rhs.is_na[i]) {
      output[i] = 1;
    } else if (lhs.data[i] < rhs.data[i]) {
      output[i] = -1;
    } else if (lhs.data[i] > rhs.data[i]) {
      output[i] = 1;
    } else {
      output[i] = 0;
    }
  }

  return output;
}

template<class T>
std::vector<int> std_dense_rank(const std::vector<T> &input) {
  std::vector<int> result(input.size());
  std::vector<std::pair<T, size_t> > sorted(input.size());

  for (size_t i=0; i<input.size(); ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    sorted[i] = std::make_pair(input[i], i);
  }

  std::sort(sorted.begin(), sorted.end());

  std::pair<T, size_t> rank(sorted[0].first, 1);
  for (size_t i=0; i<input.size(); ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (sorted[i].first != rank.first) {
      rank.first = sorted[i].first;
      rank.second++;
    }
    result[sorted[i].second] = rank.second;
  }

  return result;
}

template<class bignum_type, class Vec>
cpp11::integers dense_rank(const Vec &input) {
  cpp11::writable::integers output(input.size());

  std::vector<bignum_type> without_na;
  for (size_t i=0; i<input.size(); ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (!input.is_na[i]) {
      without_na.push_back(input.data[i]);
    }
  }

  std::vector<int> ranks = std_dense_rank(without_na);

  for (size_t i=0, i_rank=0; i<input.size(); ++i) {
    if (i % 8192 == 0) {
      cpp11::check_user_interrupt();
    }

    if (input.is_na[i]) {
      output[i] = NA_INTEGER;
    } else {
      output[i] = ranks[i_rank++];
    }
  }

  return output;
}
