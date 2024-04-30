#' @export
obj_type_friendly <- function(x, value = TRUE){
  rlang:::obj_type_friendly(x = x, value = value)
}
#' @export
vec_type_friendly <- function(x, length){
  rlang:::vec_type_friendly(x = x, length = length)
}
#' @export
as_friendly_type <- function(type){
  rlang:::.rlang_as_friendly_type(type = type)
}
#' @export
as_predicate_friendly_type_of <- function(x){
  rlang:::as_predicate_friendly_type_of(x = x)
}
#' @export
type_sum <- function(x){
  rlang:::rlang_type_sum(x)
}
#' @export
prefix_name <- function(x, prefix = ".."){
  if(length(x) == 1){
    setnames(x, "")
  } else {
    nms <- sprintf("%s%i: %s", prefix, seq_along(x), names(x))
    set_names(x, nms)
  }
}
