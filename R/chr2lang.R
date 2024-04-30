#' Vectorized `str2lang`
#'
#' @param x a character vector.
#' @param .named a logical.
#'
#' @export
chr2lang <- function(x,.named = TRUE){
  check_character(x)
  out <- lapply(x, str2lang)
  if(.named){
    setNames(out, x)
  } else {
    out
  }
}
