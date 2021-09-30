stop_unsupported <- function(x, method) {
  msg <- paste0("`", method, ".", class(x)[[1]], "()` not supported.")
  abort(msg, class = "bignum_error_unsupported")
}

warn_on_lossy_cast <- function(expr, x_ptype = NULL, to_ptype = NULL) {
  withCallingHandlers(
    vctrs_error_cast_lossy = function(err) {
      if (!is_null(x_ptype) && !vec_is(err$x, x_ptype)) {
        return()
      }
      if (!is_null(to_ptype) && !vec_is(err$to, to_ptype)) {
        return()
      }

      warn(error = err, class = "bignum_warning_cast_lossy")
      invokeRestart("vctrs_restart_error_cast_lossy")
    },
    expr
  )
}

#' @export
cnd_header.bignum_warning_cast_lossy <- function(cnd, ...) {
  x_label <- format_arg_label(vec_ptype_full(cnd$error$x), cnd$error$x_arg)
  to_label <- format_arg_label(vec_ptype_full(cnd$error$to), cnd$error$to_arg)
  loss_type <- loss_type(cnd$error$loss_type)
  paste0(
    "Loss of ", loss_type, " while converting from ",
    x_label, " to ", to_label, "."
  )
}

#' @export
cnd_body.bignum_warning_cast_lossy <- function(cnd, ...) {
  cnd_body(cnd$error)
}

#' @export
cnd_footer.bignum_warning_cast_lossy <- function(cnd, ...) {
  cnd_footer(cnd$error)
}

#' @export
# TODO: remove when rlang 1.0 is released
conditionMessage.bignum_warning_cast_lossy <- function(c) {
  cnd_message(c)
}


# Helpers -----------------------------------------------------------------

loss_type <- function(x) {
  stopifnot(
    is_character(x),
    all(x %in% c("precision", "generality"))
  )
  x[[1]]
}

format_arg_label <- function(type, arg = "") {
  type <- paste0("<", type, ">")
  if (nzchar(arg)) {
    paste0("`", arg, "` ", type)
  } else {
    type
  }
}
