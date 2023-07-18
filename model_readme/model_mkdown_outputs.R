create_coef_plot <- function(tidy_model) {
  img_dir <- file.path("model_readme", "img")
  if (!dir.exists(img_dir)) dir.create(img_dir)
  file_name <- sprintf("readme-coef-plot-%s.png", 
                       digest::digest(tidy_model$estimate, algo = "md5"))
  file_path <- file.path(img_dir, file_name)
  if ("Model Name" %in% colnames(tidy_model)) {
    coef_plot <- ggplot2::ggplot(tidy_model, ggplot2::aes(y = term, color = `Model Name`))
  } else {
    coef_plot <- ggplot2::ggplot(tidy_model, ggplot2::aes(y = term))
  }
  coef_plot <- 
    coef_plot +
    ggplot2::geom_point(ggplot2::aes(x=estimate), shape=15, size=3) +
    ggplot2::geom_linerange(ggplot2::aes(xmin=conf.low, xmax=conf.high)) +
    ggplot2::geom_vline(xintercept = 0, linetype="dashed") + 
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Estimate (95% CI)", y = "Term")
  ggplot2::ggsave(file_path, plot=coef_plot, device="png", dpi = 75)
  sprintf('<p align="center">![](%s)</p>', file_path)
}

create_coef_output <- function(models) {
  model_names <- names(models)
  tbls <- lapply(models, gtsummary::tbl_regression)
  if (length(tbls) == 1) {
    model_table <- 
      tbls[[1]] |> 
      gtsummary::as_gt() |> 
      gt::as_raw_html()
  } else {
    if (length(model_names) == length(tbls)) {
      spanner <- model_names
    } else {
      spanner <- ulapply(1:length(tbls), function(i) sprintf("m%s", i))
    }
    model_table <- 
      gtsummary::tbl_merge(tbls, 
                           tab_spanner = spanner) |> 
      gtsummary::as_gt() |> 
      gt::as_raw_html()
  }
  tidy_models <- lapply(1:length(models), function(i) {
    tidy_m <- broom::tidy(models[[i]], conf.int = TRUE)
    if (length(models) > 1) {
      tidy_m[, "Model Name"] <- model_names[i]
    }
    tidy_m
  })
  
  coef_plot <- create_coef_plot(do.call(rbind, tidy_models))
  if (as.logical(Sys.getenv("COEF_PLOT"))) {
    paste0("### Components\n",
           model_table,
           "\n",
           coef_plot)
  } else {
    paste0("### Components\n", model_table)
  }
}


create_model_overview_output <- function(models) {
  model_names <- names(models)
  glanced_models <- lapply(1:length(models), function(i) {
    m <- 
      models[[i]] |> 
      broom::glance() |> 
      t()
    m[, 1] <- round(m[, 1], 3)
    colnames(m) <- model_names[i]
    m
  })
  model_table <-
    do.call(cbind, glanced_models) |> 
    as.data.frame() |> 
    gt::gt(rownames_to_stub = TRUE)
  if (length(models) == 1) {
    model_table <- model_table |> gt::tab_options(column_labels.hidden = TRUE)
  }
  paste0("### Summary\n", model_table |> gt::as_raw_html())
}

create_residuals_plot <- function(models) {
  model_names <- names(models)
  augmented_models <- lapply(models, broom::augment)
  if (length(models) > 1) {
    augmented_models <- lapply(1:length(augmented_models), function(i) {
      m <- augmented_models[[i]]
      m[, "Model Name"] <- model_names[i]
      subset(m, select = c(.resid, .fitted, .std.resid, `Model Name`))
    })
  }
  augmented_model <- do.call(rbind, augmented_models)
  img_dir <- file.path("model_readme", "img")
  if (!dir.exists(img_dir)) dir.create(img_dir)
  file_name <- sprintf("readme-residuals-plot-%s.png", 
                       digest::digest(augmented_model$`.resid`, algo = "md5"))
  file_path <- file.path(img_dir, file_name)
  res_plot <- ggplot2::ggplot(augmented_model, ggplot2::aes(x=.fitted, y=.resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "blue") +
    ggplot2::labs(x = "Fitted Values",
                  y = "Residuals",
                  title = "Residuals Plot") + 
    ggplot2::theme_bw()
  
  if ("Model Name" %in% colnames(augmented_model)) {
    res_plot <-res_plot + ggplot2::facet_grid(rows = ggplot2::vars(`Model Name`))
  }
  ggplot2::ggsave(file_path, plot=res_plot, device="png", dpi = 100)
  paste0("### Residuals\n", sprintf("![](%s)", file_path))
}
