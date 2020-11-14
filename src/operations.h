#include <cpp11.hpp>


template<class Vec, class Func>
Vec unary_operation(const Vec &x, const Func &UnaryOperation) {
  Vec output(x.size());

  for (std::size_t i=0; i<x.size(); ++i) {
    if (x.is_na[i]) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = UnaryOperation(x.data[i]);
      } catch (...) {
        output.is_na[i] = true;
      }
    }
  }

  return output;
}

template<class Vec, class Func>
Vec binary_operation(const Vec &lhs, const Vec &rhs, const Func &BinaryOperation) {
  if (lhs.size() != rhs.size()) {
    cpp11::stop("Incompatible sizes");
  }

  Vec output(lhs.size());

  for (std::size_t i=0; i<lhs.size(); ++i) {
    if (lhs.is_na[i] || rhs.is_na[i]) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = BinaryOperation(lhs.data[i], rhs.data[i]);
      } catch (...) {
        output.is_na[i] = true;
      }
    }
  }

  return output;
}

template<class Vec, class Func>
Vec binary_operation(const Vec &lhs, const cpp11::integers &rhs, const Func &BinaryOperation) {
  if (lhs.size() != rhs.size()) {
    cpp11::stop("Incompatible sizes");
  }

  Vec output(lhs.size());

  for (std::size_t i=0; i<lhs.size(); ++i) {
    if (lhs.is_na[i] || rhs[i] == NA_INTEGER) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = BinaryOperation(lhs.data[i], rhs[i]);
      } catch (...) {
        output.is_na[i] = true;
      }
    }
  }

  return output;
}

template<class Vec, class Func>
Vec accumulate_operation(const Vec &x, const Vec &init, bool na_rm, const Func &BinaryOperation) {
  if (init.size() != 1) {
    cpp11::stop("Initial value of C++ accumulate function must have 1 element");
  }

  Vec output = init;

  for (std::size_t i=0; i<x.size(); ++i) {
    if (x.is_na[i] || std::isnan(static_cast<double>(x.data[i]))) {
      if (na_rm) {
        continue;
      } else {
        output.is_na[0] = true;
        break;
      }
    } else {
      try {
        output.data[0] = BinaryOperation(output.data[0], x.data[i]);
      } catch (...) {
        output.is_na[0] = true;
        break;
      }
    }
  }

  return output;
}

template<class Vec, class Func>
Vec partial_accumulate_operation(const Vec &x, const Func &BinaryOperation) {
  Vec output(x.size());

  // initialize first element
  output.data[0] = x.data[0];
  output.is_na[0] = x.is_na[0];

  for (std::size_t i=1; i<x.size(); ++i) {
    if (x.is_na[i] || std::isnan(static_cast<double>(x.data[i])) || output.is_na[i-1]) {
      output.is_na[i] = true;
    } else {
      try {
        output.data[i] = BinaryOperation(output.data[i-1], x.data[i]);
      } catch (...) {
        output.is_na[i] = true;
        break;
      }
    }
  }

  return output;
}
