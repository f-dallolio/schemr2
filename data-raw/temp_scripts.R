call_trace_context <- function(call, fn) {
  rlang:::call_trace_context(call = call, fn = fn)
}

call_inspect <- function(...) {
  rlang:::call_inspect()
}

call_zap_inline <- function(x) {
  rlang:::call_zap_inline(x = x)
}

call_parse_type <- function(call) {
  rlang:::call_parse_type(call = call)
}

call_deparser <- function(x) {
  rlang:::call_deparser(x = x)
}

call_standardise <- function(call, env = caller_env()) {
  rlang:::call_standardise(call = call, env = env)
}

call_deparse_highlight <- function(call, arg) {
  rlang:::call_deparse_highlight(call = call, arg = arg)
}

call_name <- function(call) {
  rlang:::call_name(call = call)
}

call_unnamespace <- function(x) {
  rlang:::call_unnamespace(x = x)
}

call_deparse <- function(x, lines = new_lines()) {
  rlang:::call_deparse(x = x, lines = lines)
}

call_delimited_type <- function(call) {
  rlang:::call_delimited_type(call = call)
}

call_args_names <- function(call) {
  rlang:::call_args_names(call = call)
}

call_type <- function(x) {
  rlang:::call_type(x = x)
}

call_match <- function(
    call = NULL, fn = NULL, ..., defaults = FALSE,
    dots_env = NULL, dots_expand = TRUE) {
  rlang:::call_match(
    call = call, fn = fn, defaults = defaults,
    dots_env = dots_env, dots_expand = dots_expand
  )
}

call_modify <- function(.call, ..., .homonyms = c(
                          "keep", "first",
                          "last", "error"
                        ), .standardise = NULL, .env = caller_env()) {
  rlang:::call_modify(
    .call = .call, .homonyms = .homonyms,
    .standardise = .standardise, .env = .env
  )
}

call_fix_car <- function(call) {
  rlang:::call_fix_car(call = call)
}

call_has_precedence <- function(call, parent_call, side = NULL) {
  rlang:::call_has_precedence(
    call = call, parent_call = parent_call,
    side = side
  )
}

call_print_type <- function(call) {
  rlang:::call_print_type(call = call)
}

call_fn <- function(call, env = caller_env()) {
  rlang:::call_fn(call = call, env = env)
}

call_args <- function(call) {
  rlang:::call_args(call = call)
}

call_add_namespace <- function(call, fn) {
  rlang:::call_add_namespace(call = call, fn = fn)
}

call_ns <- function(call) {
  rlang:::call_ns(call = call)
}

call_restore <- function(x, to) {
  rlang:::call_restore(x = x, to = to)
}

call_print_fine_type <- function(call) {
  rlang:::call_print_fine_type(call = call)
}

call_type_sum <- function(x) {
  rlang:::call_type_sum(x = x)
}
