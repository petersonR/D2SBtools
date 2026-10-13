# Every call passes analyst, pi, and project, so the tests never read
# Admin/project.yml from wherever here() happens to point.
log_product <- function(path, ...) {
  suppressMessages(add_product(
    ..., analyst = "Analyst", pi = "PI", project = "DEMO", path = path
  ))
}

read_back <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = "")
}

test_that("creates the file and numbers products in sequence", {
  path <- file.path(withr::local_tempdir(), "Admin", "products.csv")

  id1 <- log_product(path, type = "report", title = "First")
  id2 <- log_product(path, type = "abstract", title = "Second",
                     status = "planned", actual_date = NA,
                     target_date = "2027-04-01")

  expect_equal(c(id1, id2), c("DEMO-001", "DEMO-002"))
  p <- read_back(path)
  expect_named(p, c("id", "type", "title", "analyst", "pi", "target_date",
                    "actual_date", "status", "artifact_url", "related",
                    "source_file", "notes"))
  expect_equal(p$actual_date, c(format(Sys.Date()), NA))
  expect_equal(p$target_date, c(NA, "2027-04-01"))
  expect_equal(p$status, c("completed", "planned"))
  expect_equal(p$analyst, c("Analyst", "Analyst"))
})

test_that("the next id follows the highest existing number", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  log_product(path, type = "sow", title = "a", id = "DEMO-007")
  log_product(path, type = "sow", title = "b", id = "legacy-handle")
  expect_equal(log_product(path, type = "sow", title = "c"), "DEMO-008")
})

test_that("commas, quotes, and non-ASCII text round-trip", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  notes <- 'fig1–fig3, 1,200 dpi; "final" version'
  log_product(path, type = "figures", title = "Figures",
              notes = notes, related = "DEMO-001; grant=R01")
  p <- read_back(path)
  expect_equal(p$notes, notes)
  expect_equal(p$related, "DEMO-001; grant=R01")
})

test_that("off-vocabulary values warn but are logged", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  expect_warning(log_product(path, type = "graphs", title = "x"), "type")
  expect_warning(
    log_product(path, type = "sow", title = "y", status = "done"),
    "status"
  )
  expect_equal(nrow(read_back(path)), 2)
})

test_that("duplicate ids and bad dates are refused", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  log_product(path, type = "sow", title = "a")
  expect_error(log_product(path, type = "sow", title = "b", id = "DEMO-001"),
               "already")
  expect_error(log_product(path, type = "sow", title = "c",
                           actual_date = "last tuesday"), "Not a date")
  expect_equal(nrow(read_back(path)), 1)
})

test_that("a file with different columns is refused, not appended to", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  writeLines(c("id,title", "X-1,Old"), path)
  expect_error(log_product(path, type = "sow", title = "a"),
               "expected columns")
  expect_equal(readLines(path), c("id,title", "X-1,Old"))
})

test_that("a file saved by Excel (BOM, no final newline) is appended cleanly", {
  path <- file.path(withr::local_tempdir(), "products.csv")
  header <- paste(D2SBtools:::product_columns, collapse = ",")
  bom <- rawToChar(as.raw(c(0xef, 0xbb, 0xbf)))
  cat(bom, header, "\n", "DEMO-001,sow,Old,,,,,completed,,,,", sep = "",
      file = path)

  expect_equal(log_product(path, type = "sow", title = "New"), "DEMO-002")
  p <- utils::read.csv(path, colClasses = "character",
                       fileEncoding = "UTF-8-BOM")
  expect_equal(p$title, c("Old", "New"))
})

test_that("the vocabulary has no duplicates", {
  v <- product_vocabulary()
  expect_false(anyDuplicated(v$type) > 0)
  expect_false(anyDuplicated(v$status) > 0)
})

test_that("an old products_minimal.xlsx is converted on first use", {
  skip_if_not_installed("readxl")
  for (fixture in c("products_minimal.xlsx", "products_minimal_sheet1.xlsx")) {
    dir <- withr::local_tempdir()
    file.copy(test_path("fixtures", fixture),
              file.path(dir, "products_minimal.xlsx"))
    path <- file.path(dir, "products.csv")

    expect_message(
      id <- add_product(type = "figures", title = "New", analyst = "Analyst",
                        pi = "PI", project = "DEMO", path = path),
      "Updating products_minimal.xlsx to the new products.csv template"
    )

    p <- read_back(path)
    expect_equal(id, "DEMO-003", info = fixture)
    expect_equal(p$id, c("DEMO-001", "DEMO-002", "DEMO-003"), info = fixture)
    # The Excel date serial becomes an ISO date; text and blank dates survive.
    expect_equal(p$actual_date, c("2026-08-07", NA, format(Sys.Date())),
                 info = fixture)
    expect_equal(p$target_date, c(NA, "2027-04-01", NA), info = fixture)
    expect_equal(p$notes[1], 'Has a comma, and "quotes"', info = fixture)
    # The workbook is left for the user to delete.
    expect_true(file.exists(file.path(dir, "products_minimal.xlsx")))

    # Once the CSV exists, the workbook is ignored.
    expect_no_message(
      suppressWarnings(read_products(path)),
      message = "Updating"
    )
  }
})
