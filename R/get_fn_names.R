#' @export
get_fn_names0 <- function(x, with_ns = FALSE){
  stopifnot(is.list(x) &&
              all(vapply(x, is.function, logical(1))))
  nms <- vapply(x, .get_fn_name, character(1), with_ns)
  nms
  # setNames(x, nms)
}
#'
#' @export
get_fn_names <- function(..., with_ns = FALSE){
  x <- dots_list(..., .named = FALSE)
  stopifnot(is.list(x) &&
              all(vapply(x, is.function, logical(1))))
  nms <- vapply(x, .get_fn_name, character(1), with_ns)
  nms
  # setNames(x, nms)
}

.get_fn_name <- function(fn, with_ns = FALSE){
  envir <- environment(fn)
  if(is.null(envir)){
    envir <- asNamespace("base")
  }
  nms <- getNamespaceExports(envir)
  fns <- lapply(nms, get0, envir = envir)
  ids <- vapply(fns, identical, logical(1), fn)
  nm <- nms[ids]
  if(length(nm)){
    nms <- names(envir)
    fns <- lapply(nms, get, envir = envir)
    ids <- vapply(fns, identical, logical(1), fn)
    nm <- nms[ids]
  }

  if(with_ns && !is.primitive(x)){
    export_flag <- nm %in% getNamespaceExports(envir)
    ns_name <- environmentName(envir)
    if(export_flag){
      sprintf("%s::%s", ns_name, nm)
    } else {
      sprintf("%s:::%s", ns_name, nm)
    }
  } else {
    nm
  }
}
