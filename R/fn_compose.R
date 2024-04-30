#' Compose and evaluate functions
#'
#' @param .x a vector.
#' @param ... functions or formulas (lambda functions).
#' @param .dir composition direction. Defaults to `forward` (unlike `purrr::compose`). Alternative is `backward`.
#'
#' @return a list.
#' @export
#'
fn_compose <- function (.x = NULL, ...,
                        .dir = c("forward", "backward")){
  x <- rlang::dots_list(..., .named = TRUE)
  fn <- purrr::compose(!!!map(x, as_function), .dir = match.arg(.dir))
  if(is.null(.x)){
    fn
  } else {
    fn(.x)
  }
}
#' @export
fn_c <- function (.x = NULL, ...,
                  .dir = c("forward", "backward")){
  fn_compose(.x = .x, ..., .dir = .dir)
}
