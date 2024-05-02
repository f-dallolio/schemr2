ns_env2 <- function(x = NULL..., .env = caller_env()){
  ns0 <- enexpr(x)
  x <- try(eval(ns0, envir = .env), silent = TRUE)
  if(is_string(ns0) || is_function(x) || is_environment(x)){
    ns_env(x)
  } else {
    ns_env(deparse(ns0))
  }
}

caller_arg2 <- function (arg){
  arg <- substitute(arg)
  check_arg(arg)
  expr <- do.call(substitute, list(arg), envir = caller_env())
  if(is.character(expr)){
    expr
  } else {
    deparse(expr)
  }
}
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

  if(typeof(fn) == "builtin"){
    return(call_primitive(fn))
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
new_fn_import <- function(fn, ns = NULL, env = globalenv()){
  ns0 <- enexpr(ns)
  fn0 <- enexpr(fn)

  if(is.null(ns0)){
    if(is_string(fn0)) fn <- get(fn0, mode = "function")
    call <- call_add_ns(!!enexpr(fn0))
  } else {
    if(is_string(ns0)) {
      env_get <- asNamespace(ns0)
    } else {
      env_get <- asNamespace(deparse(ns0))
    }
    if(is_string(fn0)) fn <- get(fn0, envir = env_get, mode = "function")
    call <- call_add_ns(!!enexpr(fn0), !!enexpr(ns0))
  }
  body <- call_match2(call, fn, add_ns = TRUE, fmls_syms = TRUE)
  new_fn <- new_function2(args = formals(fn), body = body, env = env)
  new_fn
}


new_fn_negate <- function(fn, ns = NULL, env = globalenv()){
  args <- call_args(current_call())
  new_fn <- do.call("new_fn_import", args)
  body <- body(new_fn)
  if(deparse(body[[1]]) == "{"){
    body(new_fn) <- call("{", call("!", body[[2]]))
  } else {
    body(new_fn) <- call("!", body[[2]])
  }
  new_fn
}


fn_body_modify <- function(fn, expr){
  stopifnot(is.function(fn))
  body <- body(fn)
  if(deparse(body[[1]]) == "{"){
    body(fn) <- call("{", expr)
  } else {
    body(fn) <- expr
  }
  environment(fn) <- globalenv()
  fn
}


call_primitive <- function(fn, ..., .arg = caller_arg(fn)){
  stopifnot(is.primitive(fn))
  out <- gsub("function", .arg,
              gsub(" ", "",
                   gsub(".Primitive.*", "", capture.output(is.null))))
  str2lang(out)
}

fn_body2 <- function(fn = caller_fn(),..., .arg = caller_arg(fn)){
  stopifnot(is.function(fn))
  body <- try(body(fn))
  if(is_symbolic(body)){
    body
  } else {
    call <- call_primitive(fn = fn)

  }
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


