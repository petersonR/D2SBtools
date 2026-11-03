#' Share a document on the project site and log it as a deliverable
#'
#' Copies a file you are sharing with the PI (a rendered report, an abstract,
#' a manuscript draft, a poster) into the project's `Site/` folder and logs it
#' in `Admin/products.csv` with [add_product()], in one step. The next
#' [build_project_site()] and [publish_site()] put it on the project website.
#'
#' The copy is the exact file as shared, filed by type and dated:
#' `Site/reports/2026-11-02-eda.html`. A file whose name (without a leading
#' date) matches an earlier one in the same section is a new version of it:
#' it gets its own products row (one deliverable is one row), linked to the
#' previous version through `related`, and reuses its title unless you give
#' one. The website shows the latest version with the earlier ones listed
#' under it.
#'
#' Files that people rename between versions (`Manuscript_v3_JD.docx`, then
#' `RSV paper final.docx`) can be kept together as one document with `as`.
#'
#' `path` can also be a web link, for a draft the team edits online (for
#' example a manuscript in SharePoint, started from a version you shared).
#' Nothing is copied: the link is logged as its own products row, related to
#' the document named by `as`, and the document's page gets a button to it.
#'
#' Datasets are never put on the site, because everyone who syncs it gets a
#' copy. Log a dataset hand-off with [add_product()] instead.
#'
#' @param path File to share, or a web link to a draft edited online.
#' @param type Product type; one of the types in [product_vocabulary()] except
#'   `"dataset"`. It decides the site section (`report` goes in `reports/`,
#'   `sap` and `sow` in `plans/`, and so on).
#' @param as Name of the document this is a version of, used in place of the
#'   file's own name (e.g. `"manuscript"`). Required for a web link.
#' @param title Title shown on the site and in the products log. Defaults to
#'   the previous version's title, or else the file name.
#' @param status,notes,related,source_file Passed to [add_product()]. A web
#'   link's `status` defaults to `"in_progress"`.
#' @param date Date shared, used in the file name and as `actual_date`.
#' @param overwrite Replace a file already shared under the same name and
#'   date. Its products row is kept as it is.
#' @param root Project root.
#' @param analyst,pi,project Passed to [add_product()]; default to
#'   `Admin/project.yml` in `root`.
#'
#' @return The product id, invisibly.
#' @examples
#' \dontrun{
#' add_to_site("Reports/eda.html", "report", title = "Exploratory analysis")
#'
#' # A week later, the revised report: linked to the first automatically
#' add_to_site("Reports/eda.html", "report", notes = "Adds sensitivity analyses")
#'
#' # The PI's edits to the abstract, sent back by email
#' add_to_site("~/Downloads/idweek-abstract.docx", "abstract",
#'             status = "submitted", notes = "PI's final edits")
#'
#' # Manuscript versions under whatever names they arrive with
#' add_to_site("~/Downloads/Manuscript_v3_JD.docx", "manuscript", as = "manuscript",
#'             title = "RSV trends manuscript", notes = "PI's draft")
#' add_to_site("Manuscript/manuscript-rp-comments.docx", "manuscript", as = "manuscript",
#'             notes = "Tracked changes on the PI's draft")
#'
#' # The PI wants to co-edit online from here
#' add_to_site("https://uiowa.sharepoint.com/...", "manuscript", as = "manuscript")
#' }
#' @export
add_to_site <- function(path, type, as = NULL, title = NULL, status = "completed",
                        date = Sys.Date(), notes = NA, related = NA,
                        source_file = NA, overwrite = FALSE,
                        root = here::here(),
                        analyst = NULL, pi = NULL, project = NULL) {
  is_link <- length(path) == 1 && grepl("^https?://", path)
  if (is_link && is.null(as))
    rlang::abort("A link needs `as`: the name of the document it is a draft of (e.g. as = \"manuscript\").")
  if (is_link && missing(status)) status <- "in_progress"
  if (!is_link && (!file.exists(path) || dir.exists(path)))
    rlang::abort(glue::glue("No file at '{path}'."))
  if (!is.null(as) && (length(as) != 1 || !nzchar(version_stem(as)) || grepl("[/\\\\]", as)))
    rlang::abort("`as` must be a single document name without folders, e.g. \"manuscript\".")
  if (identical(type, "dataset"))
    rlang::abort(c(
      "Datasets don't go on the project site: everyone who syncs it gets a copy.",
      i = "Log the hand-off with add_product(\"dataset\", ...) and share the data through an approved route."))
  if (!type %in% names(site_sections))
    rlang::abort(glue::glue(
      "type '{type}' has no site section. Use one of: {paste(names(site_sections), collapse = ', ')}."))
  ext <- if (is_link) "" else tolower(tools::file_ext(path))
  if (ext %in% site_data_ext)
    rlang::abort(glue::glue(
      "'{basename(path)}' looks like a data file (.{ext}), which doesn't go on the project site."))

  date <- format(as.Date(date), "%Y-%m-%d")
  stem <- version_stem(if (is.null(as)) basename(path) else as)
  section <- site_sections[[type]]
  rel <- if (is_link) path else
    file.path("Site", section, paste0(date, "-", stem, ".", tools::file_ext(path)))
  to <- file.path(root, rel)

  products_path <- file.path(root, "Admin", "products.csv")
  products <- read_products(products_path)
  existing_row <- products[!is.na(products$artifact_url) & products$artifact_url == rel, ]

  if (!is_link && file.exists(to) && !overwrite)
    rlang::abort(glue::glue(
      "'{rel}' was already shared today. Set `overwrite = TRUE` to replace it, or pass a different `date`."))

  # Earlier versions: same stem in the same section, logged in products.csv.
  earlier <- products[!is.na(products$artifact_url) &
                        products$artifact_url != rel &
                        dirname(products$artifact_url) == file.path("Site", section) &
                        version_stem(products$artifact_url) == stem, ]
  earlier <- earlier[order(earlier$artifact_url), ]
  previous <- if (nrow(earlier) > 0) earlier[nrow(earlier), ] else NULL

  if (is.null(title))
    title <- if (!is.null(previous)) previous$title else stem
  if ((length(related) == 1 && is.na(related)) && !is.null(previous))
    related <- previous$id

  if (!is_link) {
    fs::dir_create(dirname(to))
    if (!file.copy(path, to, overwrite = TRUE))
      rlang::abort(glue::glue("Failed to copy '{path}' to '{to}'."))
  }

  if (nrow(existing_row) > 0) {
    message(glue::glue("Replaced {rel}; its products row {existing_row$id[1]} is unchanged."))
    return(invisible(existing_row$id[1]))
  }

  info <- read_project_yml(root)
  id <- tryCatch(
    add_product(
      type = type, title = title, status = status, actual_date = date,
      related = related, source_file = source_file, notes = notes,
      artifact_url = rel,
      analyst = analyst %||% info$analyst %||% NA,
      pi = pi %||% info$pi %||% NA,
      project = project %||% info$project_name %||%
        rlang::abort("No `project_name` in Admin/project.yml; pass `project`."),
      path = products_path),
    error = function(e) {
      if (!is_link) unlink(to)
      rlang::abort(glue::glue("Couldn't log '{rel}' in products.csv, so it wasn't added to the site."),
                   parent = e)
    })
  message(glue::glue(if (is_link) "Linked the draft of '{stem}' on the site." else "Added {rel} to the site."))
  invisible(id)
}

# Product type -> site folder. The order is the order of the site's menus.
site_sections <- c(
  report = "reports", manuscript = "manuscripts", abstract = "abstracts",
  presentation = "presentations", figures = "figures", grant = "grants",
  sap = "plans", sow = "plans", software = "software"
)

site_section_labels <- c(
  reports = "Reports", manuscripts = "Manuscripts", abstracts = "Abstracts",
  presentations = "Presentations", figures = "Figures", grants = "Grants",
  plans = "Plans", software = "Software"
)

site_data_ext <- c("csv", "tsv", "rds", "rda", "rdata", "parquet", "feather",
                   "arrow", "sas7bdat", "sav", "dta", "xpt", "sqlite", "db",
                   "duckdb", "xlsx", "xls")

# "Site/reports/2026-11-02-eda.html" -> "eda"
version_stem <- function(x) {
  sub("^\\d{4}-\\d{2}-\\d{2}-", "", tools::file_path_sans_ext(basename(x)))
}

read_project_yml <- function(root) {
  path <- file.path(root, "Admin", "project.yml")
  if (!file.exists(path))
    rlang::abort(glue::glue("No Admin/project.yml in '{root}'. Is `root` the project folder?"))
  yaml::read_yaml(path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
