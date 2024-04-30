mod_fn  <- function(.f, ...) {
  purrr::partial(.f = .f, ...)
}
c_fn <- function (..., .dir = c("backward", "forward")){
  purrr::compose(..., dir = dir)
}
str_oxford <- function(chr, sep = ", ", final = "and"){
  rlang:::oxford_comma(chr, sep, final)
}
str_oxford_and <- function(chr){
  str_oxford(chr, sep = ", ", final = "and")
}
str_oxford_or <- function(chr, sep = ", "){
  str_oxford(chr, sep = ", ", final = "or")
}
