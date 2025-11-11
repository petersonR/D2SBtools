#' Create a new D2SB project from the standard template
#'
#' @param project_name Name of the new project (used for folder name, project.yml, and .Rproj file).
#' @param path Directory where the project should be created. If `NULL`,
#'   the function will create a folder named `project_name` in the current working directory.
#' @param open Logical; whether to open the new project in RStudio.
#' @param ... Named parameters to fill into `project.yml` template
#'   (e.g., title, analyst, pi, status, etc.)
#'
#' @export
create_project <- function(project_name,
                           path = NULL,
                           open = interactive(),
                           ...) {

  if (missing(project_name) || !nzchar(project_name)) {
    rlang::abort("Please provide a non-empty `project_name`.")
  }

  # Default to creating a folder under the current working directory
  if (is.null(path)) {
    path <- fs::path_abs(project_name)
  } else {
    path <- fs::path_abs(path, project_name)
  }

  if (fs::dir_exists(path) && length(fs::dir_ls(path)) > 0) {
    rlang::abort(glue::glue("Target directory '{path}' exists and is not empty."))
  }


  # 1. Find template skeleton
  skeleton_root <- system.file("templates", "project", "skeleton",
                               package = "D2SBtools")
  if (skeleton_root == "") {
    rlang::abort("Could not find project template skeleton in the package.")
  }

  # 2. Copy the entire skeleton
  fs::dir_create(path)
  template_contents <- list.files(
    skeleton_root,
    all.files   = TRUE,   # include dotfiles like .Rproj, .gitignore, etc.
    full.names  = TRUE,
    no..        = TRUE    # skip '.' and '..'
  )

  ok <- file.copy(template_contents, path, recursive = TRUE)
  if (!all(ok)) {
    warning("Some template files/directories could not be copied.")
  }

  # 3. Gather parameters
  current_info <- c(
    list(project_name = project_name,
    date_created = as.character(Sys.Date())),
    list(...))

  # Set project parameters one at a time
  template_info <- yaml::read_yaml(paste0(path, "/Admin/project.yml"))

  # Copy over missing info from template
  current_info <- c(current_info, template_info[!(names(template_info) %in% names(current_info))])

  yaml::write_yaml(current_info, file = paste0(path, "/Admin/project.yml"))

  # Rename .Rproj file
  file.rename(paste0(path, "/.Rproj"), paste0(path, "/", project_name, ".Rproj"))

  # Optionally open in RStudio
  if (open && rstudioapi::isAvailable()) {
    rstudioapi::openProject(path, newSession = TRUE)
  }

  message(glue::glue("✅ Created project '{project_name}' at {path}"))
  invisible(path)
}
