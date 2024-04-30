#' #' If ... then ...
#' #'
#' #' @param .x an object.
#' #' @param .f a function.
#' #' @param .then an object.
#' #'
#' #' @export
#' #'
#' if_then <- function(x, ..., .then = x){
#'   expr <- enexpr(x)
#'   if(is_bool(expr)){
#'     check_dots_empty()
#'     test <- expr
#'   } else {
#'     fns_out <- map_lgl(list2(...), ~ as_function(.x)(x))
#'     test <- all(fns_out)
#'   }
#'
#'   if( test ) {
#'     .then
#'   } else {
#'     x
#'   }
#' }
#'
#' if_then(letters, is_character)
#' if_then(is.character(letters))
#'
#'
#' expr_if_not_exists <- function(x){
#'   quo <- enquo(x)
#'   out <- possibly(eval_tidy)(quo)
#'   if(is.null(out)){
#'     out <- quo_get_expr(quo)
#'   }
#'   out
#' }
#'
#' lang0 <- function(x, ..., enexpr_input = TRUE){
#'   if(enexpr_input){
#'     x <- enexpr(x)
#'   }
#'
#'   if(is_string(x)){
#'     x <- str2lang(x)
#'   }
#'   if(is_call(x, c("::", ":::")) || is_symbol(x)){
#'     as.call(list(x))
#'   } else {
#'     x
#'   }
#' }
#' lang <- function(x){
#'
#' }
#'
#'
#'
#'
#' lang("mean")
#'
