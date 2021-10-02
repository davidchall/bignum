# validates from

    `from` must have size 1, not size 2.

---

    `from` can't be `NA`.

# validates length.out / along.with exclusiveness

    Can only specify one of `length.out` and `along.with`.

# only takes two optional args

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

---

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

# requires two optional args

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

---

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

---

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

---

    Must specify exactly two of:
    - `to`
    - `by`
    - Either `length.out` or `along.with`

# validates to

    `to` must have size 1, not size 2.

---

    Can't convert `to` <character> to match type of `from` <biginteger>.

---

    `to` can't be `NA`.

# validates by

    `by` must have size 1, not size 2.

---

    Can't convert `by` <character> to match type of `from` <biginteger>.

---

    `by` can't be `NA`.

---

    `by` can't be `0`.

---

    `by` can't be `0`.

# validates length.out

    `length.out` must have size 1, not size 2.

---

    Can't convert `length.out` <character> to <integer>.

---

    `length.out` can't be `NA`.

---

    `length.out` can't be negative.

# validates from/to/by signs

    When `from` is less than `to`, `by` must be positive.

---

    When `from` is greater than `to`, `by` must be negative.

---

    When `from` is less than `to`, `by` must be positive.

---

    When `from` is greater than `to`, `by` must be negative.

# enforces non-fractional results for biginteger

    The supplied output size does not result in a non-fractional sequence between `from` and `to`.

---

    The supplied output size does not result in a non-fractional sequence between `from` and `to`.

# `to` is always cast to `from`

    Can't convert from `to` <bigfloat> to `from` <biginteger> due to loss of precision.
    * Locations: 1

