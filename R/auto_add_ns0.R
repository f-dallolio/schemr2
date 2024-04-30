#' Automatically add namespace to a call.
#'
#' @param x a `call_simple`.
#' @param ... .
#' @param .match_call a logical. if `TRUE`, `call_match` is also called. Defaults to `TRUE`.
#' @param .match_call_defaults a logical. Defaults to `TRUE`..
#'
#' @return a `call_simple`(namespaced).
#' @export
#'
#' @examples
#' call <- quote(data.frame(x))
#' call
#' # data.frame(x)
#' auto_add_ns0(call)
#' # base::data.frame(x, row.names = NULL, check.rows = FALSE, check.names = TRUE,
#' #     fix.empty.names = TRUE, stringsAsFactors = FALSE)

auto_add_ns0 <- function(x, ..., .match_call = TRUE, .match_call_defaults = TRUE){
  check_call_simple(x)

  nm <- call_name(x)
  fn <- get0(nm, mode = "function")
  fn_ok <- !is.null(fn)
  if(fn_ok){
    ns <- ns_env_name(fn)
  } else {
    ns <- NULL
  }

  out <- call2(.fn = nm, !!!call_args(x), .ns = ns)
  if(.match_call && fn_ok){
    call_match(out, fn, defaults = .match_call_defaults)
  } else {
    out
  }

}
