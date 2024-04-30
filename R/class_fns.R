# msg <- function(x, i = NULL, ..., arg = caller_arg(x)){
#   if(!is_null(i)){
#     arg <- sprintf("..%i: %s", i, arg)
#   }
#   type <- rlang:::obj_type_friendly(x)
#   setNames(arg, type)
# }
#
# msgs <- function(...){
#   x <- dots_list(..., .named = TRUE)
#   if(length(x) > 1){
#     names(x) <- sprintf("..%i: %s", seq_along(x), names(x))
#   }
#   types <- map_chr(x, rlang:::obj_type_friendly)
#   out <- map(split(names(x), types), str_oxford_and)
#   nn <- max(nchar(names(out)))
#   nms <- str_pad(names(out), nn, side = "right", pad = " ")
#   sprintf("%s - %s", nms, out)
# }
#' @export
as_lambda <- function(x, env = global_env(), ..., arg = caller_arg(x), call = caller_env()){
  if( is_function(x) ){
    call <- call2(arg, quote(.x))
    x <- new_formula(NULL, call, env = env)
  }
  as_function(x)
}
#' @export
lambda_out <- function(x){
  bdy <- as.list(fn_body(x))
  out <- deparse(bdy[[length(bdy)]])
  gsub("~", "", out)
}
#' @export
fn_list <- function(..., env = global_env()){
  x <- dots_list(..., .named = TRUE)
  out <- imap(x, ~as_lambda(.x, env = env, arg = .y))
  new_fn_list(x = out, env = env)
}
#' @export
new_fn_list <- function(x = list(), ..., env = global_env()){
  stopifnot(every(x, is_lambda))
  nms <- map_chr(x, lambda_out)
  fmls <- map(x, fn_fmls)
  body <- map(x, fn_body)
  fields <- df_list(nms = nms, fmls = fmls, body = body)
  new_rcrd(fields = fields, env = env, class = "fns")
}
#' @export
is_fns <- function(x){
  if(inherits(x, "fns")){
    return(TRUE)
  } else {
    if( is_list(x) ) {
      every(x, is_function)
    } else {
      FALSE
    }
  }
}
#' @export
fns_get_fmls <- function(x){
  stopifnot(is_fns(x))
  vctrs::field(x, "fmls")
}
#' @export
fns_get_body <- function(x){
  stopifnot(is_fns(x))
  vctrs::field(x, "body")
}
#' @export
fns_get_nms <- function(x){
  stopifnot(is_fns(x))
  vctrs::field(x, "nms")
}
#' @export
fns_get_env <- function(x){
  stopifnot(is_fns(x))
  attr(x, "env")
}
#' @export
format.fns <- function(x){
  out <- map2(fns_get_fmls(x), fns_get_body(x),
              new_function,
              env = fns_get_env(x))
  setNames(x, fns_get_nms(x))
}
#' @export
vec_ptype_full.fns <- function(x){
  paste0("list_of<functions>")
}
#' @export
obj_print_data.fns <- function(x){
  cat(sprintf("~%s", fns_get_nms(x)))
}
