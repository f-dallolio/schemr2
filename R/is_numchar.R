#' Is an object a numeric vetor?
#'
#' @param x a character vector.
#'
#' @return a logical.
#' @export
is_numchar <- function(x){
  is.character(x) &&
    !is.null(purrr::possibly(as.numeric)(x))
}
#' @export
is_numeric2 <- function(x){
  is.numeric(x) || is_numchar(x)
}
