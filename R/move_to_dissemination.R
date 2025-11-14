#' Move a rendered D2SB Quarto report into the SharePoint Dissemination folder
#'
#' @param path_from Path to the report file that exists (e.g. "Reports/eda.html").
#' @param subdir_to which subdirectory on sharepoint to copy to
#' @param overwrite whether to overwrite or not
#' @param ... Named parameters to substitute into the template (title, author, etc.).
#' @export
#'

move_to_dissemination <- function(path_from, subdir_to, overwrite = FALSE, ...) {


  if(!file.exists(path_from))
    rlang::abort(paste0("no report found at", path))

  dir_path <- sp_path("Dissemination")

  if(!dir.exists(dir_path)) {
    rlang::warn("Dissemination subdirectory not found; one was created")
    dir.create(dir_path)
  }

  subdir_to <- file.path(dir_path, subdir_to)

  if(!dir.exists(subdir_to)) {
    rlang::warn("Dissemination subdirectory not found; one was created")
    dir.create(subdir_to)
  }

  # Get date from file metadata or from "now"
  info <- file.info(path_from)
  report_time <- info$mtime
  date_str <- format(report_time, "%Y-%m-%d")

  # Build destination filename with date appended before extension
  base_name <- basename(path_from)                # e.g., "my_report.html"
  stem      <- tools::file_path_sans_ext(base_name)  # "my_report"
  ext       <- sub(".*\\.", "", base_name)       # "html"

  new_name  <- paste0(date_str, "-", stem, ".", ext)
  path_to <- file.path(subdir_to, new_name)

  # Copy the file
  ok <- file.copy(path_from, path_to, overwrite = overwrite)
  if (!ok) {
    if(file.exists(path_to))
      stop("File already exists at", path_to, "; set overwrite = TRUE?")
    stop("Failed to copy report to: ", path_to)
  }

  message("Copied report to: ", path_to)
  invisible(path_to)
}
