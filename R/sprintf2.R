#' Variation of `sprintf`
#'
#' @param ... values to be passed into fmt.
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param .before a character vector.
#'
#' @export
#'
sprintf2 <- function(..., fmt, .before = NULL){
  dots <- c(.before, list2(...))
  inject(sprintf(fmt, !!!(dots)))
}
