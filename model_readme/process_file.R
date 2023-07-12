source("model_readme/readme.R")

Sys.getenv("README_EXECUTABLE") |> 
  process_readme()
