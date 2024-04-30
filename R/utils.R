#' If ... then ...
#'
#' @param .x an object.
#' @param .f a function.
#' @param .then an object.
#'
#' @export
#'
if_then <- function(.x, .p, .then = .x){
  test <- as_function(.p)(.x)
  if( test ) {
    if(is_function(.then) || is_formula(.then)){
      as_function(.then)(.x)
    } else {
      .then
    }
  } else {
    .x
  }
}
#'
#' @export
ifnot_then <- function(.x, .p, .then = .x){
  test <- as_function(.p)(.x)
  if( test ) {
    if(is_function(.then) || is_formula(.then)){
      .then
    } else {
      as_function(.then)(.x)
    }
  } else {
    .x
  }
}

