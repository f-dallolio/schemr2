#' @export
pkg2name <- function(x){
  deparse(str2lang(x)[[3]])
}
#' @export
pkg2ns <- function(x){
  asNamespace(pkg2name(x))
}
