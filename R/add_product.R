#' Allowed values for the products log
#'
#' The suggested vocabulary for the `type` and `status` columns of
#' `Admin/products.csv`, kept deliberately short. [add_product()] warns on
#' anything else, so new categories are possible but never silent. To add a
#' category for the whole group, add it here. Use `notes` for detail a type
#' cannot carry.
#'
#' One deliverable is one row. A `sap` is a standalone analysis plan document;
#' an analysis plan written as a section of a grant application is part of
#' that `grant` row, not a separate `sap`.
#'
#' @return A list with character vectors `type` and `status`.
#' @export
product_vocabulary <- function() {
  list(
    type = c(
      "sow", "sap", "report", "manuscript", "abstract", "presentation",
      "figures", "dataset", "software", "grant"
    ),
    status = c(
      "planned", "in_progress", "completed", "submitted", "accepted",
      "published", "canceled"
    )
  )
}

# Column order of Admin/products.csv.
product_columns <- c(
  "id", "type", "title", "analyst", "pi", "target_date", "actual_date",
  "status", "artifact_url", "related", "source_file", "notes"
)

#' Log a deliverable in the project's products file
#'
#' Appends one row to `Admin/products.csv`, the record of everything a project
#' has delivered or plans to deliver. Analyst and PI default to the values in
#' `Admin/project.yml`, and the id is the project name plus the next number in
#' sequence (`VHF-001`, `VHF-002`, ...). The file is created, with its header,
#' if it does not exist yet.
#'
#' Projects started before the CSV log have `Admin/products_minimal.xlsx`. The
#' first `add_product()` call in such a project converts it to
#' `Admin/products.csv`, keeping every row, and says so; the workbook is left
#' in place to delete once the CSV has been checked.
#'
#' Columns:
#' * `id`: unique handle, `<project>-<NNN>`.
#' * `type`, `status`: see [product_vocabulary()].
#' * `title`: human-readable name of the deliverable.
#' * `analyst`, `pi`: people primarily responsible.
#' * `target_date`, `actual_date`: planned and actual delivery (YYYY-MM-DD).
#' * `artifact_url`: link or path to the artifact (SharePoint, RDSS, DOI).
#' * `related`: ids of related products, or tags such as `grant=ID`,
#'   separated by `; `.
#' * `source_file`: notebook or script that generated the artifact.
#' * `notes`: optional context.
#'
#' @param type,title,status,target_date,actual_date,artifact_url,related,source_file,notes
#'   Column values; see Details. Dates may be `Date` objects or
#'   `"YYYY-MM-DD"` strings. Leave a column `NA` to keep it blank.
#' @param analyst,pi People responsible; default to `Admin/project.yml`.
#' @param id Product id; by default the next one in sequence.
#' @param project Project name used to build the id; defaults to
#'   `project_name` in `Admin/project.yml`.
#' @param path Products file to append to.
#'
#' @return The new product's id, invisibly.
#'
#' @examples
#' \dontrun{
#' add_product(
#'   type = "figures",
#'   title = "Manuscript figures 1-3 for journal submission",
#'   source_file = "Code/make_figures.R",
#'   related = "VHF-003"
#' )
#'
#' # A planned deliverable
#' add_product("abstract", "IDWeek abstract", status = "planned",
#'             target_date = "2027-04-01", actual_date = NA)
#' }
#'
#' @export
add_product <- function(type, title, status = "completed",
                        actual_date = Sys.Date(), target_date = NA,
                        related = NA, source_file = NA, notes = NA,
                        artifact_url = NA,
                        analyst = get_project_data("analyst"),
                        pi = get_project_data("pi"),
                        id = NULL,
                        project = get_project_data("project_name"),
                        path = here::here("Admin", "products.csv")) {
  vocab <- product_vocabulary()
  if (!type %in% vocab$type) {
    rlang::warn(glue::glue(
      "type '{type}' is not in product_vocabulary(); logged anyway."
    ))
  }
  if (!status %in% vocab$status) {
    rlang::warn(glue::glue(
      "status '{status}' is not in product_vocabulary(); logged anyway."
    ))
  }
  if (is.na(actual_date) && status == "completed") {
    rlang::warn("A completed product has no `actual_date`.")
  }

  products <- read_products(path)

  if (is.null(id)) {
    # Everything after the last "-" is the sequence number; ids that do not
    # follow the scheme are ignored rather than breaking the count.
    taken <- suppressWarnings(as.integer(sub(".*-", "", products$id)))
    id <- sprintf("%s-%03d", project, max(c(0L, taken), na.rm = TRUE) + 1L)
  }
  if (id %in% products$id) {
    rlang::abort(glue::glue("Product id '{id}' is already in {path}."))
  }

  as_date_string <- function(x) {
    if (length(x) != 1 || is.na(x)) return(NA_character_)
    d <- as.Date(x, optional = TRUE)
    if (is.na(d)) rlang::abort(glue::glue("Not a date: {x}"))
    format(d, "%Y-%m-%d")
  }
  one <- function(x) {
    x <- unlist(x)
    if (length(x) != 1) {
      rlang::abort("Each column value must be a single value.")
    }
    as.character(x)
  }

  row <- data.frame(
    id = one(id), type = one(type), title = one(title),
    analyst = one(analyst), pi = one(pi),
    target_date = as_date_string(target_date),
    actual_date = as_date_string(actual_date),
    status = one(status), artifact_url = one(artifact_url),
    related = one(related), source_file = one(source_file),
    notes = one(notes)
  )

  # A file last saved by Excel may lack a final newline, which would glue this
  # row onto the previous one.
  size <- file.size(path)
  if (size > 0 && readBin(path, "raw", n = size)[size] != as.raw(0x0a)) {
    cat("\n", file = path, append = TRUE)
  }

  utils::write.table(
    row, path, append = TRUE, sep = ",", na = "", row.names = FALSE,
    col.names = FALSE, qmethod = "double", fileEncoding = "UTF-8"
  )
  message("Logged ", id, " in ", path)
  invisible(id)
}

# Read the products file, creating it with a header if it is missing. Every
# column is read as character so ids and dates round-trip unchanged. A header
# that does not match product_columns stops here: appending to a file with
# different columns would silently put values under the wrong headings.
read_products <- function(path) {
  if (!file.exists(path)) {
    xlsx <- file.path(dirname(path), "products_minimal.xlsx")
    if (file.exists(xlsx)) {
      return(migrate_products_xlsx(xlsx, path))
    }
    fs::dir_create(dirname(path))
    utils::write.csv(
      stats::setNames(
        as.data.frame(matrix(character(), 0, length(product_columns))),
        product_columns
      ),
      path, row.names = FALSE
    )
    message("Created ", path)
  }

  # Excel adds a byte-order mark when saving CSV, and often omits the final
  # newline; add_product() repairs the latter, so its warning is noise here.
  products <- withCallingHandlers(
    utils::read.csv(
      path, colClasses = "character", na.strings = "", check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    ),
    warning = function(w) {
      if (grepl("incomplete final line", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  if (!identical(names(products), product_columns)) {
    rlang::abort(c(
      glue::glue("{path} does not have the expected columns."),
      i = paste("Expected:", paste(product_columns, collapse = ", ")),
      x = paste("Found:", paste(names(products), collapse = ", "))
    ))
  }
  products
}

# Convert a pre-CSV products workbook to products.csv. Rows are copied as they
# are, with one exception: Excel stores dates as day counts, which become
# YYYY-MM-DD. Returns the rows, as read_products() does.
migrate_products_xlsx <- function(xlsx, path) {
  rlang::check_installed("readxl", "to convert products_minimal.xlsx")
  message(
    "Updating ", basename(xlsx), " to the new products.csv template. ",
    "Check ", path, ", then delete the workbook."
  )

  # The template named its sheet "products"; some projects have "Sheet1".
  sheets <- readxl::excel_sheets(xlsx)
  sheet <- if ("products" %in% sheets) "products" else sheets[1]
  products <- as.data.frame(
    readxl::read_xlsx(xlsx, sheet = sheet, col_types = "text")
  )
  if (!identical(names(products), product_columns)) {
    rlang::abort(c(
      glue::glue("{xlsx} does not have the expected columns; convert it by hand."),
      i = paste("Expected:", paste(product_columns, collapse = ", ")),
      x = paste("Found:", paste(names(products), collapse = ", "))
    ))
  }

  for (col in c("target_date", "actual_date")) {
    x <- products[[col]]
    serial <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
    x[serial] <- format(
      as.Date(floor(as.numeric(x[serial])), origin = "1899-12-30"),
      "%Y-%m-%d"
    )
    products[[col]] <- x
  }

  utils::write.csv(products, path, row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  products
}
