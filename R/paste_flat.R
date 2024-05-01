#' Flatten String
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' @param .before a character vector.
#' @param c an optional character string to separate the results. Not NA_character_. When collapse is a string, the result is always a string (character of length 1).
#' @param recycle0 	logical indicating if zero-length character arguments should result in the zero-length chara
#'
#' @return a character vector.
#' @export
#'
paste_flat <- function(..., .before = NULL, c = " ", recycle0 = FALSE){
  paste(c(.before, ...), collapse = c, recycle0 = recycle0)
}
