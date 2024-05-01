#' Convert strings, symbols, and calls into `call_simple`
#'
#' @param ... a string, symbol, or call.
#' @param enexpr_input logical. If `TRUE`, `rlang::enexpr` is applied to input.Defaults to `TRUE`.
#' @param ns_sym logical. If `FALSE`, only symbols are converted into `call_simple`. If `TRUE`, both `symbol` and `namespaced_symbol` (`namespace::function`). Defaults to `TRUE`.
#' @param str_as_expr logical. if `TRUE`, strings are quoted. Not if `FALSE`. Defaults to `TRUE`.
#'
#' @return a `call_simple`.
#' @export
#'
#' @examples
#' objs_as_call(mean)
#' objs_as_call("mean")
#' objs_as_call(mean())
#' objs_as_call("mean()")
#' objs_as_call(base::mean)
#' objs_as_call("base::mean")
objs_as_call <- function(...,
                         enexpr_input = TRUE,
                         ns_sym = TRUE,
                         str_as_expr = TRUE){

  if(enexpr_input){
    x <- rlang::enexprs(...)
  } else {
    x <- rlang::list2(...)
  }

  if(str_as_expr){
    x <- purrr::modify_if(x, rlang::is_string, str2lang)
  }

  if(ns_sym){
    fn <- function(x) rlang::is_call(x, c("::", ":::")) || rlang::is_symbol(x)
  } else {
    fn <- function(x) rlang::is_symbol(x)
  }

  purrr::modify_if(x, fn, new_call)

}
#'
#' @export
obj_as_call <- function(x,
                        enexpr_input = TRUE,
                        ns_sym = TRUE,
                        str_as_expr = TRUE){



  if(enexpr_input){ x <- rlang::enexpr(x) }
  if(str_as_expr && is_string(x)){ x <- str2lang(x) }
  if(ns_sym && is_call(x, name = c("::", ":::"))){
    x <- new_call(x)
  }
  objs_as_call(x, enexpr_input = FALSE)[[1]]
}

