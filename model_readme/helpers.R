try_catch_na <- function(f, ...) {
  tryCatch({
    f(...)
  }, error = function(cond) {
    message(cond)
    NA
  })
}

filter_to_exists <- function(lst) {
  lst[lapply(lst, length) > 0]
}

get_num_hits <- function(string, pattern) {
  gregexpr(pattern, string) |> 
    regmatches(x = string) |> 
    filter_to_exists() |> 
    length()
}

get_num_opening_parens <- function(string) {
  get_num_hits(string, "\\(")
}

get_num_closing_parens <- function(string) {
  get_num_hits(string, "\\)")
}

append_vector <- function(v, obj, idx) {
  if (idx >= length(v)) {
    append(v, obj)
  } else {
    append(v, obj, after = idx)
  }
}

ulapply <- function(l, f, ...) {
  lapply(l, f, ...) |> 
    unlist()
}
