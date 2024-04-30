# Standalone file: do not edit by hand
# Source: <https://github.com/r-lib/rlang/blob/main/R/standalone-purrr.R>
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/rlang
# file: standalone-purrr.R
# last-updated: 2023-02-23
# license: https://unlicense.org
# imports: rlang
# ---
#
# This file provides a minimal shim to provide a purrr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# ## Changelog
#
# 2023-02-23:
# * Added `list_c()`
#
# 2022-06-07:
# * `transpose()` is now more consistent with purrr when inner names
#   are not congruent (#1346).
#
# 2021-12-15:
# * `transpose()` now supports empty lists.
#
# 2021-05-21:
# * Fixed "object `x` not found" error in `imap()` (@mgirlich)
#
# 2020-04-14:
# * Removed `pluck*()` functions
# * Removed `*_cpl()` functions
# * Used `as_function()` to allow use of `~`
# * Used `.` prefix for helpers
#
# nocov start

map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}
.rlang_purrr_args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}

keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ...) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }

  inner_names <- names(.l[[1]])

  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      } else {
        x
      }
    })
  }

  # This way missing fields are subsetted as `NULL` instead of causing
  # an error
  .l <- map(.l, as.list)

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
.rlang_purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

list_c <- function(x) {
  inject(c(!!!x))
}

# nocov end

compose <- function(..., .dir = c("backward", "forward")){
  purrr::compose(..., .dir = .dir)
}

compose2 <- function(..., .dir = c("backward", "forward")){
  dots <- rlang::dots_list(..., .named = TRUE)
  fns <- lapply(dots, rlang::as_function)
  purrr::compose(!!!fns)
}

cfn <- function(..., .dir = c("backward", "forward")){
  compose2(..., .dir = .dir)
}

partial <- function(.f, ...){
  purrr::partial(.f = .f, ...)
}

p_fn <- function(.f, ...){
  purrr::partial(.f = .f, ...)
}

# modify_if
modify_if <- function(.x, .p, .f, ..., .else = NULL) {
  purrr:::modify_if(.x = .x, .p = .p, .f = .f, .else = .else)
}
#
# modify_in
modify_in <- function(.x, .where, .f, ...) {
  purrr:::modify_in(.x = .x, .where = .where, .f = .f)
}
#
# modify_tree
modify_tree <- function(
    x, ..., leaf = identity, is_node = NULL,
    pre = identity, post = identity) {
  purrr:::modify_tree(
    x = x, leaf = leaf, is_node = is_node,
    pre = pre, post = post
  )
}
#
# modify_depth
modify_depth <- function(.x, .depth, .f, ..., .ragged = .depth <
                           0, .is_node = NULL) {
  purrr:::modify_depth(
    .x = .x, .depth = .depth, .f = .f, .ragged = .ragged,
    .is_node = .is_node
  )
}
#
# list_modify
list_modify <- function(.x, ..., .is_node = NULL) {
  purrr:::list_modify(.x = .x, .is_node = .is_node)
}
#
# modify
modify <- function(.x, .f, ...) {
  purrr:::modify(.x = .x, .f = .f)
}
#
# modify_at
modify_at <- function(.x, .at, .f, ...) {
  purrr:::modify_at(.x = .x, .at = .at, .f = .f)
}
#
# imodify
imodify <- function(.x, .f, ...) {
  purrr:::imodify(.x = .x, .f = .f)
}
#
# modify2
modify2 <- function(.x, .y, .f, ...) {
  purrr:::modify2(.x = .x, .y = .y, .f = .f)
}
#
# modify_where
modify_where <- function(.x, .where, .f, ..., .purrr_error_call = caller_env()) {
  purrr:::modify_where(.x = .x, .where = .where, .f = .f, .purrr_error_call = .purrr_error_call)
}
#
# quietly
quietly <- function(.f) {
  purrr:::quietly(.f)
}
#
# possibly
possibly <- function(.f, otherwise = NULL, quiet = TRUE) {
  purrr:::possibly(.f = .f, otherwise = otherwise, quiet = quiet)
}
#
# safely
safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  purrr:::safely(.f = .f, otherwise = otherwise, quiet = quiet)
}
#
