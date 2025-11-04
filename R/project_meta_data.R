#' Get data for project
#'
#' Allows you to get misc project data parameters
#'
#' @param param Project parameter to be gotten (if missing, retrieves all)
#' @importFrom yaml read_yaml write_yaml
#' @importFrom here here
#'
#' @return a named character vector with the requested param, or all project data
#'
#' @export
#'
get_project_data <- function(param){
  if(!file.exists(here("Admin/project.yml"))) {
    stop("Admin/project.yml not found at", here())
  }

  current_info <- yaml::read_yaml(here("Admin/project.yml"))
  if(missing(param))
    param <- names(current_info)
  unlist(current_info[param])
}

#' get the path to where data live on sharepoint for this project
#'
#' use instead of `here` or `setwd` to get data location from sharepoint
#' for D2SB projects.
#'
#' @param file_name optionally returns full file name for a specific file
#'
#' @return a director or file_name path that will return the proper
#' local location of a file or directory for the project.
#'
#' @export

sp_path <- function(file_name = "") {
  d2sb_sp_path <- Sys.getenv("D2SB_SP_PATH")
  if(d2sb_sp_path == "")
    stop("set D2SB_SP_PATH=[local D2SB SP path] via usethis::edit_r_environ(score = 'user')")
  proj_path <- get_project_data("data_path_from_sharepoint")
  if(!dir.exists(path <- file.path(d2sb_sp_path, proj_path)))
    stop("data path not found; check that project.yml has data_path_from_sharepoint param set")
  file.path(path, file_name)
}
