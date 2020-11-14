stop_unsupported <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not supported.")
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

      condition_data <- unclass(err)
      condition_data$class <- c("bignum_warning_cast_lossy", "vctrs_error_cast_lossy")
      do.call(warn, condition_data)

      invokeRestart("vctrs_restart_error_cast_lossy")
    },
    expr
  )
}

#' @export
cnd_header.bignum_warning_cast_lossy <- function(cnd, ...) {
  x_label <- format_arg_label(vec_ptype_full(cnd$x), cnd$x_arg)
  to_label <- format_arg_label(vec_ptype_full(cnd$to), cnd$to_arg)
  loss_type <- loss_type(cnd$loss_type)
  glue::glue("Loss of {loss_type} while converting from {x_label} to {to_label}.")
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
