#' Create a D2SB Quarto report from template
#'
#' @param path Path to the report file to create (e.g. "Reports/eda.qmd").
#' @param template Name of the report template (currently unused).
#' @param ... Named parameters to substitute into the template (title, author, etc.).
#' @export
create_report <- function(path = "Reports/new.qmd",
                          template = "default",
                          ...) {

  params <- list(...)

  fs::dir_create(fs::path_dir(path), recurse = TRUE)

  if (fs::file_exists(path)) {
    rlang::abort("Report file already exists at `path`.")
  }

  # Locate template
  template_root <- system.file("templates", "report", "skeleton",
                               package = "D2SBtools")
  if (template_root == "") {
    rlang::abort("Could not find report template skeleton in the package.")
  }

  template_file <- fs::path(template_root, "report.qmd")
  template_txt <- readLines(template_file)

  writeLines(template_txt, path)
  if (rstudioapi::isAvailable()) {
    try(rstudioapi::navigateToFile(path), silent = TRUE)
  }

  message(glue::glue("✅ Created new template-based report at {path}"))
  invisible(path)
}
