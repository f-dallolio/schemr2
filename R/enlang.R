#' Convert an object(s) to an empty `call_simple`
#'
#' @param x an R object.
#' @param ... R objects.
#' @param .named If TRUE, unnamed inputs are automatically named with `as_label()`. This is equivalent to applying `exprs_auto_name()` on the result. If FALSE, unnamed elements are left as is and, if fully unnamed, the list is given minimal names (a vector of ""). If NULL, fully unnamed results are left with NULL names.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one of "trailing", "none", "all". If "trailing", only the last argument is ignored if it is empty. Named arguments are not considered empty.
#' @param .unquote_names Whether to treat ⁠:=⁠ as =. Unlike =, the ⁠:=⁠ syntax supports names injection.
#' @param .ignore_null Whether to ignore unnamed null arguments. Can be "none" or "all".
#' @param .homonyms How to treat arguments with the same name. The default, "keep", preserves these arguments. Set .homonyms to "first" to only keep the first occurrences, to "last" to keep the last occurrences, and to "error" to raise an informative error and indicate what arguments have duplicated names.
#' @param .check_assign Whether to check for ⁠<-⁠ calls. When TRUE a warning recommends users to use = if they meant to match a function parameter or wrap the ⁠<-⁠ call in curly braces otherwise. This ensures assignments are explicit.
#'
#' @return a single or a list of `call_simple`.
#' @export
#'
#' @examples
#' enlang(mean)
enlang <- function(x){
  x <- rlang::enexpr(x)
  obj_as_call(x, enexpr_input = FALSE)
}
#'
#' @export
#' @examples
#' enlangs(mean, "mean", "base::eval")
enlangs <- function(..., .named = TRUE,
                    .ignore_empty = c("trailing", "none", "all"),
                    .ignore_null = c("none", "all"),
                    .unquote_names = TRUE,
                    .homonyms = c("keep", "first", "last", "error"),
                    .check_assign = FALSE){

  x <- enexprs(..., .named = .named,
               .ignore_empty = .ignore_empty, .ignore_null = .ignore_null,
               .unquote_names = .unquote_names, .homonyms = .homonyms,
               .check_assign = .check_assign)
  if(.named){
    chr_id <- purrr::map_lgl(x, is.character)
    names(x)[chr_id] <- x[chr_id]
  }

  objs_as_call(!!!x, enexpr_input = FALSE)
}

