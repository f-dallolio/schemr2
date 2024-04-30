.import_fn <- function(x, ns) {
  rlang::check_installed(ns)
  env <- as.list(asNamespace(ns))
  fn <- env[x]
  ns_sym <- call(":::", str2lang(ns), str2lang(x))
  fn <- eval(ns_sym)
  fmls_names <- rlang::fn_fmls_names(fn)
  fmls <- rlang::fn_fmls_syms(eval(ns_sym))[fmls_names != "..."]
  call_new <- as.call(append(list(ns_sym), fmls))

  functionBody(fn) <- call("{", call_new)
  out_call <- call("<-", str2lang(x), fn)

  out_call
}

import_fns <- function(ns, ..., .name = ns) {
  dots <- c(...)
  out <- lapply(dots, .import_fn, ns = ns)
  name <- paste0("import_fns__", .name, ".R")
  path <- paste0(here::here(), "/R/", name)
  out <- structure(setNames(out, dots),
    path = path
  )
  tmp_file <- tempfile(pattern = paste0(.name, "_"), fileext = ".R")
  if(file.exists(tmp_file)){
    file.remove(tmp_file)
  }

  out <- lapply(
    out,
    \(x) c(paste(deparse(x), collapse = "\n"), "") |>
      cat(sep = "\n", file = tmp_file, append = TRUE)
  )

  styler:::style_file(path = tmp_file)
  usethis::edit_file(path = tmp_file)
}

