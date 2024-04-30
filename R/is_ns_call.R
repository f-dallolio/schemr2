#' Title
#'
#' @param x a `call_simple`.
#' @param ns_type a string.
#'
#' @return a logical.
#' @examples
#' x0 <- quote(mean())
#' x1 <- quote(base::mean())
#' x2 <- quote(base:::mean())
#'
#' is_ns_call(x0)
#' is_ns_call(x1)
#' is_ns_call(x2)
#'
#' is_ns_call(x1, "export")
#' is_ns_call(x1, "private")
#'
#' is_ns_call(x2, "export")
#' is_ns_call(x2, "private")

#'
#' @name is_ns_call
NULL

#'
#' @export
#' @rdname is_ns_call
is_ns_call <- function(x, ns_type = c("all", "export", "private")){
  check_call_simple(x)
  ns_type_vec <- c(export = "::", private = ":::")
  ns_type <- match.arg(ns_type)
  if(ns_type == "all"){
    ns_type <- unname(ns_type_vec)
  } else {
    ns_type <- ns_type_vec[ns_type]
  }
  is_call(x[[1]], name = ns_type)
}
#'
#' @export
#' @rdname is_ns_call
is_ns_call_export <- function(x){
  is_ns_call(x, ns_type = "export")
}
#'
#' @export
#' @rdname is_ns_call
is_ns_call_private <- function(x){
  is_ns_call(x, ns_type = "private")
}
