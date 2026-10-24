#' Launch the CopulaSCR Analysis Application
#'
#' Open the Shiny interface for data import, variable mapping, association
#' analysis, marginal analysis, and survival prediction.
#'
#' @param launch.browser Whether to open the application in a browser. Passed
#'   to [shiny::runApp()].
#' @param host Host address to bind to. Defaults to the local computer.
#' @param port Port to listen on, or `NULL` to choose an available port.
#'
#' @details
#' The application requires the optional packages \pkg{shiny}, \pkg{digest},
#' \pkg{scales}, and \pkg{png}. Model fits are cached in
#' `tools::R_user_dir("CopulaSCR", which = "cache")`. Set the
#' `CopulaSCR.cache_dir` option before launching to use a different directory.
#' Exported analysis model RDS files can be imported through the interface.
#'
#' @return The value returned by [shiny::runApp()], invisibly. The function runs
#'   until the application is stopped.
#' @export
#' @examples
#' if (interactive() && all(vapply(c("shiny", "digest", "scales", "png"),
#'     requireNamespace, logical(1), quietly = TRUE))) {
#'   run_app()
#' }
run_app <- function(launch.browser = getOption("shiny.launch.browser", interactive()),
                    host = "127.0.0.1", port = NULL) {
  dependencies <- c("shiny", "digest", "scales", "png")
  missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1),
    quietly = TRUE)]
  if (length(missing)) {
    stop("Install the application dependencies first: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))", call. = FALSE)
  }
  app_dir <- system.file("shiny", package = "CopulaSCR")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("The application files were not found. Reinstall CopulaSCR.", call. = FALSE)
  }
  invisible(shiny::runApp(appDir = app_dir, launch.browser = launch.browser,
    host = host, port = port))
}
