check_quosure <- function(x,
                          ...,
                          allow_na = FALSE,
                          allow_null = FALSE,
                          show_value = TRUE,
                          arg = caller_arg(x),
                          call = caller_env()){
  if (!missing(x)) {
    if (rlang::is_quosure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  rlang::stop_input_type(x,
                         "a quosure",
                         ...,
                         allow_na = FALSE,
                         allow_null = allow_null,
                         arg = arg,
                         call = call
  )
}



quos_get_expr <- function(x){
  check_quosures(x)
  lapply(x, quo_get_expr)
}

quos_get_env <- function(x){
  check_quosures(x)
  lapply(x, quo_get_env)
}

quos_eval_tidy <- function(x){
  check_quosures(x)
  lapply(x, eval_tidy)
}


quo_is_string <- function(x){
  check_quosure(x)
  is_string(quo_get_expr(x))
}
quo_is_function <- function(x){
  check_quosure(x)
  is_function(eval_tidy(x))
}
quo_is_formula <- function(x){
  check_quosure(x)
  is_formula(eval_tidy(x))
}


quos_are <- function(x, .p, ..., arg = caller_arg(x), call = caller_env()){
  check_quosures(x)
  exprs <- quos_get_expr(x)
  out <- vapply(exprs, as_function(.p), logical(1))
  if(all(out)){
    TRUE
  } else {
    not_true0 <- paste(which(!out), collapse = ", ")
    not_true <- paste0("c(", not_true0,")")
    cli::cli_warn(
      c("All elements in {.arg {arg}} must return TRUE. Problematic: {arg}[{not_true}]"),
      call = call
    )
    FALSE
  }
}
quos_are_strings <- function(x,
                             .p = rlang::is_string,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()){
  quos_are(x = x, .p = .p, arg = arg, call = call)
}
quos_are_calls <- function(x,
                             .p = rlang::is_call,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()){
  quos_are(x = x, .p = .p, arg = arg, call = call)
}
quos_are_symbols <- function(x,
                             .p = rlang::is_symbol,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()){
  quos_are(x = x, .p = .p, arg = arg, call = call)
}
quos_are_symbolic <- function(x,
                             .p = rlang::is_symbolic,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()){
  quos_are(x = x, .p = .p, arg = arg, call = call)
}
quos_are_fns <- function(x,
                         ...,
                         arg = caller_arg(x),
                         call = caller_env()){
  quos <- quos_eval_tidy(x)
  out <- vapply(quos, \(x) is_function(x) || is_formula(x), logical(1))
  if(all(out)){
    TRUE
  } else {
    not_true0 <- paste(which(!out), collapse = ", ")
    not_true <- paste0("c(", not_true0,")")
    cli::cli_warn(
      c("All elements in {.arg {arg}} must a <function> or a <formula>. Problematic: {arg}[{not_true}]"),
      call = call
    )
    FALSE
  }
}

