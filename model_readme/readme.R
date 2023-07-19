source("model_readme/helpers.R")
source("model_readme/model_mkdown_outputs.R")

MODEL_HEADER <- "# Models"
README <- "README.md"
README_MARKER <- "#~"

is_readme <- function(lines) {
  ulapply(lines, function(line) startsWith(trimws(line), README_MARKER))
}

conditionally_add_section_header <- function() {
  con <- file(README)
  current_readme <- readLines(con = con)
  model_idx <- which(current_readme == MODEL_HEADER)
  if (!length(model_idx)) {
    write(paste0("\n", MODEL_HEADER), file = README, append = TRUE)
    model_idx <- length(current_readme) + 2
  }
  close(con)
  return(model_idx)
}

delete_current_model_section <- function() {
  con <- file(README)
  current_readme <- readLines(con = con)
  model_idx <- which(current_readme == MODEL_HEADER)
  rm_rows <- NA
  headers_after_models <- intersect(which(startsWith(current_readme, "# ")), 
                                    which(1:length(current_readme) > model_idx))
  closing_idx <- ifelse(length(headers_after_models),
                        headers_after_models[1], 
                        length(current_readme))
  rm_rows <- (model_idx + 1):(closing_idx)
  current_readme <- current_readme[-rm_rows]
  write(current_readme, file = README)
  close(con)
}


add_model_to_readme <- function(current_readme, header_name, models) {
  model_idx <- which(current_readme == MODEL_HEADER)
  header_text <- paste0("## ", header_name)
  # Append to end of Models section
  headers_after_models <- intersect(which(startsWith(current_readme, "# ")), 
                                    which(1:length(current_readme) > model_idx))
  model_idx <- ifelse(length(headers_after_models),
                      headers_after_models[1], 
                      length(current_readme))
  current_readme <- append_vector(current_readme, header_text, model_idx)
  header_idx <- which(current_readme == header_text)
  # Add model outputs to section
  model_outputs <- c(
    try_catch_na(create_model_overview_output, models),
    try_catch_na(create_coef_output, models),
    try_catch_na(create_residuals_plot, models)
  )
  offset <- 0
  for (i in 1:length(model_outputs)) {
    offset <- i
    output <- model_outputs[i]
    if (!is.na(output)) {
      output_location <- header_idx + offset
      current_readme <- append_vector(current_readme, sprintf("%s\n", output), output_location)
    } else {
      offset <- offset - 1
    }
  }
  current_readme
}


extract_fn_call <- function(lines, readme_idx) {
  next_idx <- readme_idx + 1
  next_line <- lines[next_idx]
  fn_call <- next_line
  num_opening_parens <- get_num_opening_parens(next_line)
  num_closing_parens <- get_num_closing_parens(next_line)
  while (next_idx <= length(lines) && num_opening_parens != num_closing_parens) {
    next_idx <- next_idx + 1
    next_line <- lines[next_idx]
    num_opening_parens <- num_opening_parens + get_num_opening_parens(next_line)
    num_closing_parens <- num_closing_parens + get_num_closing_parens(next_line)
    fn_call <- paste0(fn_call, next_line)
  }
  gsub(".+<-", "", fn_call)
}

get_header <- function(line) {
  strsplit(line, README_MARKER)[[1]][2] |> 
    gsub(pattern = "\\{.*\\}", "", replacement = "") |> 
    trimws()
}

get_subheader <- function(line) {
  headers <- strsplit(line, README_MARKER)[[1]][2]
  regmatches(headers, regexpr("\\{.*\\}", headers)) |> 
    gsub(pattern = "\\{|\\}", replacement = "") |>
    trimws()
}

create_model_list <- function(subsection_headers, fn_calls) {
  if (length(subsection_headers) == length(fn_calls)) {
    1:length(subsection_headers) |> 
      ulapply(function(idx) {
        sprintf('`%s` = %s', subsection_headers[idx], fn_calls[idx])
      }) |>
      paste(collapse = ",") |> 
      sprintf(fmt = "list(%s)")
  } else {
    paste(fn_calls, collapse = ",") |> 
      sprintf(fmt = "list(%s)")
  }
}

process_readme <- function(filename) {
  source(filename)
  conditionally_add_section_header()
  delete_current_model_section()
  con <- file(filename)
  lines <- readLines(con = con)
  readme_con <- file(README)
  current_readme <- readLines(con = readme_con)
  processed_headers <- c()
  for (i in 1:length(lines)) {
    line <- lines[i]
    if (i < length(lines) & is_readme(line)) {
      header_name <- get_header(line)
      if (header_name %in% processed_headers) next
      stopifnot(length(header_name) | !is.na(header_name))
      
      all_subsection_idxs <- which(ulapply(lines, function(l) grepl(header_name, l) && is_readme(l)))
      all_subsection_headers <- ulapply(lines[all_subsection_idxs], get_subheader)
      fn_calls <- ulapply(all_subsection_idxs, 
                                function(idx) extract_fn_call(lines, idx))
      model_list <- create_model_list(all_subsection_headers, fn_calls)
      message("ADDING TO README: ", header_name)
      fn_call <- sprintf("add_model_to_readme(current_readme, \"%s\", %s)",
                         header_name, model_list)
      current_readme <- eval(parse(text = fn_call))
      processed_headers <- append(processed_headers, header_name)
    }
  }
  write(paste(current_readme, collapse = "\n"), file = README)
  close(readme_con)
  close(con)
}