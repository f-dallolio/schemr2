#'
#' @export
sym_add_ns <- function(x, ns = NULL){
  x <- obj_as_call(enexpr(x), enexpr_input = FALSE)[[1]]
  ns <- obj_as_call(enexpr(ns), enexpr_input = FALSE)[[1]]
  call_add_ns(call = !!x, ns = !!ns)[[1]]
}
#'
#' @export
call_add_ns <- function (call, ns = NULL, private = FALSE){
  call <- obj_as_call(rlang::enexpr(call), enexpr_input = FALSE)

  if(!is.null(rlang::enexpr(ns))){
    ns <- rlang::ensym(ns)
  }

  if (!is.call(call) || !is.symbol(call[[1]])) {
    return(call)
  }

  sym <- call[[1]]
  nm <- rlang::as_string(sym)
  if (nm %in% c("::", ":::")) {
    return(call)
  }

  if(is.null(ns)){
    fn <- get0(nm, envir = globalenv(), mode = "function")
    stopifnot(is.function(fn))
    ns <- environmentName(environment(fn))
    op <- "::"
  } else {
    ns <- rlang::as_string(rlang::ensym(ns))
    if(rlang:::ns_exports_has(asNamespace(ns), nm)){
      op <- "::"
    } else {
      op <- ":::"
    }
  }
  namespaced_sym <- call(op, rlang::sym(ns), sym)
  call[[1]] <- namespaced_sym
  call
}
#'
#' @export
call_match2 <- function (call = NULL, fn = NULL, ...,
                         add_ns  = FALSE, fmls_syms = FALSE,
                         defaults = TRUE, dots_env = NULL,
                         dots_expand = TRUE) {
  if(add_ns){
    call <- call_add_ns(!!call)
  }

  if(is.null(fn)){
    fn <- lang2fun(call)
  }

  out <- rlang::call_match(call = call, fn = fn,
                           defaults = defaults, dots_env = dots_env,
                           dots_expand = dots_expand)

  if(fmls_syms){
    args <- fn_fmls_syms(fn)
    args_nms <- call_args_names(out)
    call_modify(call, !!!args[args_nms])
  } else {
    out
  }
}
#'
#' @export
call_match_import <- function (call = NULL,
                               ...,
                               dots_env = NULL,
                               dots_expand = TRUE) {
  call_match2(call = call, fn = NULL, add_ns = TRUE, fmls_syms = TRUE,
              defaults = TRUE, dots_env = dots_env, dots_expand = dots_expand)
}
#'
#' @export
new_function2 <- function(args, body, env = globalenv()){
  new_function(args = args, body = call("{", body), env = env)
}
#'
#' @export
new_function_import <- function(fn, ns = NULL, env = globalenv()){
  call <- call_add_ns(!!enexpr(fn), !!enexpr(ns))
  body <- call_match2(call, fn, add_ns = TRUE, fmls_syms = TRUE)
  new_fn <- new_function2(args = formals(fn), body = body, env = env)
  out <- call("<-", sym(call_name(body)), new_fn)
  out2 <- paste_flat(style_expr(out), c = "\n")
  setNames(out2, call_name(body))
}
#'
#' @export
get_fns <- function(..., ns = NULL){
  exprs <- exprs(..., .named = TRUE)
  nms <- modify_if(exprs, Negate(is_string), deparse)
  if(is.null(enexpr(ns))){
    env <- globalenv()
  } else {
    ns0 <- as_string(ensym(ns))
    env <- asNamespace(ns0)
  }
  out <- lapply(nms, get0, env = env)
  nss <- vapply(out, ns_env_name, character(1))
  # out_nms <- mapply(sym_add_ns, nms, nss)
  setNames(out, nms)
}


