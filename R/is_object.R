is_name = function(x, string = NULL) {
  rlang::is_string(x, string)
}
is_string = function(x, string = NULL) {
  rlang::is_string(x, string)
}
is_bool <- function (x){
  is_logical(x, n = 1) && !is.na(x)
}
is_arg <-function(x, name = NULL) {
  rlang::is_symbol(x, name)
}
is_data_frame = function(x) {
  base::is.data.frame(x)
}
is_number_decimal = function(x, n = NULL){
  out <- is.numeric(x)
  if(is.null(n)) out else all(out,length(x) == n)
}
is_number_whole <- function(x, n = NULL) {
  rlang::is_integerish(x, n)
}
#
# ensure_logical <- function(x, .p, ..., arg = caller_arg(), call = caller_env()){
#
# }
# assertthat:::fail_default(is.numeric(letters))
# fail_default
#
# rlang::is_string
#
# vecs <- c(
#   "logical",
#   "bool",
#   "number_decimal"
#   "numeric"
#   "number_whole",
#   "integerish",
#   "integer",
#   "character",
#   "string" = "name"
#
#   "data_frame" = "data frame",
#
# )
#
# exprs(
#   "formula",
#   "symbol" = "arg",
#   "closure" = ,
#   "function",
#   "call",
#   "call_simple" = "simple call ({.fn rlang::call_simple})",
#
#
#
#   ,
#   ,
# )
#
# x <- apropos("^is[._]", where = TRUE, mode = "function")
# x2 <- setNames(names(x),str_remove_all(x, "^is[._]"))
# tbl_is = tibble(fn = str_remove_all(x, "^is[._]"),
#        ns_num = as.integer(names(x)),
#        ns_name = search()[as.integer(names(x))] |>
#          str_split_i("[:]{1,3}", 2)) |>
#   filter(ns_name |> str_detect("schemr", negate = TRUE))
#
# rlang_tbl_is <- tbl_is |>
#   filter(ns_name == "rlang") |>
#   pull(fn)
#
# paste0("is_", rlang_tbl_is) |>
#   map(~ call2(.fn = .x, .ns = "rlang") |> as_label())
#
#
# x <- as_function(~ mean(.x))
#
# get_lambda_call <- function(x, as_string = TRUE){
#   bdy <- as.list(fn_body(x))
#   fn_call <- rev(bdy)[[1]]
#   cll_nm <- call_name(fn_call)
#   ns_nm <- ns_env_name(get0(cll_nm))
#   call <- call2(cll_nm, !!!call_args(fn_call), .ns = ns_nm)
#   if(as_string){
#     as_label(call)
#   } else {
#     call
#   }
# }
# fn <- eval(as_function(~ mean(.x)))
#
# x
# call <- ~qumean(.x)
#
# default_error <- function (call, ...){
#
#   if(is_call_simple(call) && is.null(call_ns(call))){
#     fn <- try(eval(call[[1]]), silent = TRUE)
#     ns <- ns_env_name(fn)
#     call <- call2(call_name(call), !!!call_args(call), .ns = ns)
#   } else if (is_formula(call)){
#     fn <- as_function(call)
#     call <- get_lambda_call(fn, as_string = T)
#   }
#
#   call_string <- deparse(call, width.cutoff = 60L)
#
#   if (length(call_string) > 1L) {
#     call_string <- paste0(call_string[1L], "...")
#   }
#   paste0(call_string, " is not TRUE")
# }
#
# default_error(call)
