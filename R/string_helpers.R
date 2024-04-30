#' @export
str_numpad <- function(x,
                       width = Inf,
                       side = "left",
                       pad = "0", use_width = TRUE){

  stopifnot(is_numeric2(x))
  if(is.numeric(x)){ x <- as.character(x) }
  if(is.infinite(width)){ width <- max(nchar(x)) }

  stringr::str_pad(string = x,
                   width = width,
                   side = side,
                   pad = pad,
                   use_width = use_width)
}
#'
#' @export
str_pad2 <- function(string,
                     width = Inf,
                     side = c("left", "right", "both"),
                     pad = " ", use_width = TRUE){

  if(is.infinite(width)){ width <- max(nchar(string)) }

  stringr::str_pad(string = string,
                   width = width,
                   side = side,
                   pad = pad,
                   use_width = use_width)
}
#'
#' @export
str_oxford <- function(chr, sep = ", ", final = "and"){
  rlang:::oxford_comma(chr, sep, final)
}
#'
#' @export
str_oxford_and <- function(chr){
  str_oxford(chr, sep = ", ", final = "and")
}
#'
#' @export
str_oxford_or <- function(chr, sep = ", "){
  str_oxford(chr, sep = ", ", final = "or")
}
