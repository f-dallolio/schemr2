#' Is object a symbol?
#'
#' @param x An object to test.
#' @param .ns_call a logical.
#'
#' @export
is_symbol2 <- function(x, .ns_call = TRUE){
  rlang::is_symbol(x) || is_call(x, name = c("::", ":::"))
}
