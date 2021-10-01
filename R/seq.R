#' Sequences: bignum
#'
#' @description
#' This is a bignum method for the [seq()] generic.
#'
#' Using `seq()` on bignum objects always retains the type of `from`.
#'
#' When calling `seq()`, exactly two of the following must be specified:
#' - `to`
#' - `by`
#' - Either `length.out` or `along.with`
#'
#' @param from Start value of the sequence. A [`biginteger`] or [`bigfloat`] scalar.
#' @param to End value of the sequence. `to` is cast to the type of `from`.
#' @param by Amount to increment the sequence by. `by` is cast to the type of `from`.
#' @param length.out Length of the resulting sequence.
#' @param along.with Vector who's length determines the length of the resulting sequence.
#' @param ... These dots are for future extensions and must be empty.
#' @return A sequence with the type of `from`.
#'
#' @export
seq.bignum_vctr <- function(from,
                            to = NULL,
                            by = NULL,
                            length.out = NULL,
                            along.with = NULL,
                            ...) {
  # TODO: wait for rlang 1.0
  # check_dots_empty()

  vec_assert(from, size = 1L)
  if (is.na(from)) {
    abort("`from` can't be `NA`.")
  }

  has_to <- !is_null(to)
  has_by <- !is_null(by)
  has_lo <- !is_null(length.out)
  has_aw <- !is_null(along.with)

  if (has_aw) {
    if (has_lo) {
      abort("Can only specify one of `length.out` and `along.with`.")
    } else {
      has_lo <- TRUE
      length.out <- vec_size(along.with)
    }
  }

  n_has <- sum(has_to, has_by, has_lo)

  if (n_has != 2L) {
    message <- paste0(
      "Must specify exactly two of:\n",
      "- `to`\n",
      "- `by`\n",
      "- Either `length.out` or `along.with`"
    )
    abort(message)
  }

  if (has_to) {
    to <- vec_cast(to, from, x_arg = "to", to_arg = "from")

    vec_assert(to, size = 1L, arg = "to")
    if (is.na(to)) {
      abort("`to` can't be `NA`.")
    }
  }

  if (has_by) {
    by <- vec_cast(by, from, x_arg = "by", to_arg = "from")

    vec_assert(by, size = 1L, arg = "by")
    if (is.na(by)) {
      abort("`by` can't be `NA`.")
    }
    if (by == 0) {
      abort("`by` can't be `0`.")
    }
  }

  if (has_lo) {
    length.out <- check_length_out(length.out, arg = "length.out")
  }

  if (has_to) {
    if (has_by) {
      if (is_biginteger(from)) {
        c_biginteger_seq_to_by(from, to, by)
      } else {
        c_bigfloat_seq_to_by(from, to, by)
      }
    } else {
      if (is_biginteger(from)) {
        c_biginteger_seq_to_lo(from, to, length.out)
      } else {
        c_bigfloat_seq_to_lo(from, to, length.out)
      }
    }
  } else {
    if (is_biginteger(from)) {
      c_biginteger_seq_by_lo(from, by, length.out)
    } else {
      c_bigfloat_seq_by_lo(from, by, length.out)
    }
  }
}

check_length_out <- function(length.out, arg) {
  length.out <- vec_cast(length.out, integer(), x_arg = arg)

  vec_assert(length.out, size = 1L, arg = arg)

  if (is.na(length.out)) {
    abort(paste0("`", arg, "` can't be `NA`."))
  }

  if (length.out < 0) {
    abort(paste0("`", arg, "` can't be negative."))
  }

  length.out
}
