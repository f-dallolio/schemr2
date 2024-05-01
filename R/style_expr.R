#' Style an expression
#'
#' @param x an expression.
#'
#' @export
style_expr <- function(x){
  if(is.character(x)){
    styler::style_text(x)
  } else {
    styler::style_text(deparse(x))
  }
}
