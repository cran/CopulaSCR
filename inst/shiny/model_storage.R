model_cache_directory <- getOption("CopulaSCR.cache_dir",
  tools::R_user_dir("CopulaSCR", which = "cache"))

cache_directory_control <- function(id, button_id, value) {
  shiny::tagList(
    shiny::div(class = "form-group shiny-input-container", style = "width:100%",
      shiny::tags$label(`for` = id, "Cache directory"),
      shiny::tags$input(id = id, type = "text", class = "form-control",
        value = value, readonly = "readonly", title = value)),
    shiny::actionButton(button_id, "Choose folder...", icon = shiny::icon("folder-open")))
}

cache_folder_start <- function(default) {
  start <- path.expand(default)
  while (!dir.exists(start) && !identical(dirname(start), start)) start <- dirname(start)
  if (!dir.exists(start)) start <- path.expand("~")
  normalizePath(start, winslash = "/", mustWork = TRUE)
}

choose_cache_directory <- function(default) {
  if (!capabilities("tcltk") || !requireNamespace("tcltk", quietly = TRUE))
    stop("The system folder picker requires Tcl/Tk in this local R installation.")
  start <- cache_folder_start(default)
  # Give the native dialog a visible, topmost owner so it cannot open behind
  # the browser or the R console. Remove the tiny owner as soon as it closes.
  owner <- tcltk::tktoplevel()
  on.exit(try(tcltk::tkdestroy(owner), silent = TRUE))
  tcltk::tkwm.title(owner, "CopulaSCR folder selector")
  tcltk::tkwm.overrideredirect(owner, TRUE)
  tcltk::tkwm.geometry(owner, "1x1+0+0")
  tcltk::tcl("wm", "attributes", owner, "-topmost", TRUE)
  tcltk::tcl("update", "idletasks")
  tcltk::tkraise(owner)
  tcltk::tcl("focus", "-force", owner)
  chosen <- tcltk::tclvalue(tcltk::tcl("tk_chooseDirectory",
    parent = owner, initialdir = start, title = "Choose model cache folder", mustexist = TRUE))
  if (!nzchar(chosen)) return(NULL)
  normalizePath(chosen, winslash = "/", mustWork = TRUE)
}

validate_cache_directory <- function(directory) {
  if (length(directory) != 1L || is.na(directory) || !nzchar(trimws(directory)))
    stop("Enter a cache directory, or uncheck Save the fitted model to disk.")
  directory <- path.expand(trimws(directory))
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE))
    stop("Could not create the cache directory. Choose a writable folder.")
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  probe <- tempfile("cache-write-check-", tmpdir = directory)
  on.exit(unlink(probe))
  if (!file.create(probe, showWarnings = FALSE))
    stop("The cache directory is not writable. Choose another folder.")
  directory
}

model_cache_key <- function(mapped, config) digest::digest(list(
  format = 1L, package = as.character(utils::packageVersion("CopulaSCR")),
  mapped = mapped, config = config), algo = "sha256")

make_model_bundle <- function(mapped, association, marginal, config, input_settings = list(),
                              prediction_marginal = NULL, prediction_marginal_settings = NULL) {
  structure(list(format_version = 1L,
    package_version = as.character(utils::packageVersion("CopulaSCR")),
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    mapped = mapped, association = association, marginal = marginal,
    config = config, input_settings = input_settings,
    prediction_marginal = prediction_marginal,
    prediction_marginal_settings = prediction_marginal_settings,
    cache_key = model_cache_key(mapped, config)), class = "CopulaSCR_workspace_model")
}

validate_model_bundle <- function(bundle) {
  if (!inherits(bundle, "CopulaSCR_workspace_model") ||
      !identical(bundle$format_version, 1L))
    stop("Choose an Analysis model RDS exported by this app. Older model-only RDS files lack the dataset and mapping needed to restore an analysis.")
  if (!identical(bundle$package_version, as.character(utils::packageVersion("CopulaSCR"))))
    stop("This model was saved with a different CopulaSCR version. Refit it using the current package before restoring it.")
  x <- bundle$mapped
  if (!is.list(x) || !is.data.frame(x$raw) || !is.data.frame(x$data) ||
      length(x$k) != 1L || !is.finite(x$k) || x$k < 1 || x$k != floor(x$k) ||
      nrow(x$raw) != nrow(x$data) || length(x$names) != x$k ||
      !all(c("D", "dD", paste0("T", seq_len(x$k)), paste0("d", seq_len(x$k))) %in% names(x$data)))
    stop("The saved model has an invalid dataset or variable mapping.")
  validate_analysis_table(x$raw)
  expected <- if (x$k > 1L) "mscr" else "scrassonp"
  if (!inherits(bundle$association, expected) ||
      !is.list(bundle$config) ||
      !all(c("family", "B", "seed", "se_method") %in% names(bundle$config)) ||
      !identical(bundle$cache_key, model_cache_key(x, bundle$config)))
    stop("The saved model is incomplete or does not match its data and settings.")
  if (!is.null(bundle$marginal) &&
      !inherits(bundle$marginal, if (x$k > 1L) "mscr" else "scrsurv"))
    stop("The saved marginal result is not compatible with this analysis.")
  if (!is.null(bundle$prediction_marginal) &&
      (x$k != 1L || !inherits(bundle$prediction_marginal, "scrsurv")))
    stop("The saved prediction marginal is not compatible with this analysis.")
  bundle
}

read_model_bundle <- function(path) validate_model_bundle(readRDS(path))

write_model_cache <- function(bundle, directory = model_cache_directory) {
  validate_model_bundle(bundle)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, paste0(bundle$cache_key, ".rds"))
  temp <- tempfile("model-", tmpdir = directory, fileext = ".rds")
  on.exit(unlink(temp))
  saveRDS(bundle, temp)
  if (!file.copy(temp, path, overwrite = TRUE)) stop("Could not save the model cache.")
  writeLines(bundle$cache_key, file.path(directory, "latest.txt"))
  invisible(path)
}

read_cached_model <- function(key = NULL, directory = model_cache_directory) {
  if (is.null(key)) {
    pointer <- file.path(directory, "latest.txt")
    if (!file.exists(pointer)) return(NULL)
    key <- readLines(pointer, n = 1L, warn = FALSE)
  }
  if (length(key) != 1L || !grepl("^[a-f0-9]{64}$", key))
    stop("The model cache index is invalid.")
  path <- file.path(directory, paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  read_model_bundle(path)
}
