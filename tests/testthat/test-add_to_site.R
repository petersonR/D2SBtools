# A made-up project with an empty products log.
make_project <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  fs::dir_create(file.path(root, "Admin"))
  yaml::write_yaml(list(project_name = "DEMO", title = "Demo study",
                        pi = "Dr. Example", analyst = "A. Analyst"),
                   file.path(root, "Admin", "project.yml"))
  writeLines(paste(D2SBtools:::product_columns, collapse = ","),
             file.path(root, "Admin", "products.csv"))
  root
}

share <- function(root, name, text = name) {
  f <- file.path(withr::local_tempdir(.local_envir = parent.frame()), name)
  writeLines(text, f)
  f
}

test_that("add_to_site files a dated copy and logs it", {
  root <- make_project()
  f <- share(root, "eda.html", "first")
  id <- suppressMessages(add_to_site(f, "report", title = "Exploratory analysis",
                                     date = "2026-10-14", root = root))
  expect_equal(id, "DEMO-001")
  rel <- "Site/reports/2026-10-14-eda.html"
  expect_equal(readLines(file.path(root, rel)), "first")

  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  expect_equal(p$artifact_url, rel)
  expect_equal(p$title, "Exploratory analysis")
  expect_equal(p$actual_date, "2026-10-14")
  expect_equal(p$pi, "Dr. Example")
})

test_that("a later version gets its own row, linked to the previous one", {
  root <- make_project()
  suppressMessages(add_to_site(share(root, "eda.html"), "report", title = "EDA",
                               date = "2026-10-14", root = root))
  # Already-dated names are not dated twice.
  id <- suppressMessages(add_to_site(share(root, "2026-11-02-eda.html"), "report",
                                     date = "2026-11-02", notes = "Revised", root = root))
  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  expect_equal(nrow(p), 2)
  expect_equal(p$title[2], "EDA")
  expect_equal(p$related[2], "DEMO-001")
  expect_true(file.exists(file.path(root, "Site/reports/2026-11-02-eda.html")))
})

test_that("add_to_site won't silently replace a file or duplicate its row", {
  root <- make_project()
  suppressMessages(add_to_site(share(root, "eda.html", "v1"), "report", date = "2026-10-14", root = root))
  expect_error(add_to_site(share(root, "eda.html", "v2"), "report", date = "2026-10-14", root = root),
               "already shared")
  expect_message(add_to_site(share(root, "eda.html", "v2"), "report", date = "2026-10-14",
                             overwrite = TRUE, root = root), "unchanged")
  expect_equal(readLines(file.path(root, "Site/reports/2026-10-14-eda.html")), "v2")
  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  expect_equal(nrow(p), 1)
})

test_that("datasets and data files never go on the site", {
  root <- make_project()
  expect_error(add_to_site(share(root, "cohort.html"), "dataset", root = root), "Datasets")
  expect_error(add_to_site(share(root, "cohort.csv"), "report", root = root), "data file")
  expect_error(add_to_site(share(root, "x.html"), "nonsense", root = root), "no site section")
  expect_false(dir.exists(file.path(root, "Site")))
})

test_that("a failed log leaves no file on the site", {
  root <- make_project()
  yaml::write_yaml(list(title = "No project name"), file.path(root, "Admin", "project.yml"))
  expect_error(add_to_site(share(root, "eda.html"), "report", date = "2026-10-14", root = root),
               "Couldn't log")
  expect_false(file.exists(file.path(root, "Site/reports/2026-10-14-eda.html")))
})

test_that("build_project_site writes pages from Site/, products and meeting notes", {
  root <- make_project()
  suppressMessages({
    add_to_site(share(root, "eda.html"), "report", title = "EDA", date = "2026-10-14", root = root)
    add_to_site(share(root, "eda.html"), "report", date = "2026-11-02", notes = "Revised", root = root)
    add_to_site(share(root, "abstract.docx"), "abstract", title = "IDWeek abstract",
                status = "submitted", date = "2026-10-20", root = root)
  })
  writeLines(c("# Meeting notes", "", "## 2026-11-01", "", "- Talked", "", "## 2026-10-01", "", "- Met"),
             file.path(root, "meeting_notes.md"))

  out <- file.path(withr::local_tempdir(), "build")
  expect_message(build_project_site(root, out, render = FALSE), NA)
  expect_true(file.exists(file.path(out, "reports", "eda.qmd")))
  expect_true(file.exists(file.path(out, "abstracts", "index.qmd")))
  expect_true(file.exists(file.path(out, "meeting-notes.qmd")))
  expect_setequal(list.files(file.path(out, "assets")),
                  c("d2sb-site.scss", "d2sb-logo-reversed.svg", "favicon.svg"))

  index <- paste(readLines(file.path(out, "index.qmd")), collapse = "\n")
  expect_match(index, "Demo study")
  expect_match(index, "### 2026-11-01")
  expect_match(index, "IDWeek abstract")
  expect_match(index, "2 versions")

  eda <- paste(readLines(file.path(out, "reports", "eda.qmd")), collapse = "\n")
  expect_match(eda, "2026-11-02 \\(latest\\)")
  expect_match(eda, "2026-10-14-eda.html")
  # The latest version is shown inside its page; a .docx can't be, so it's a button.
  expect_match(eda, '<iframe class="doc-frame" src="2026-11-02-eda.html"', fixed = TRUE)
  abstract <- paste(readLines(file.path(out, "abstracts", "abstract.qmd")), collapse = "\n")
  expect_no_match(abstract, "iframe")
  expect_match(abstract, "2026-10-20-abstract.docx", fixed = TRUE)
})

test_that("build_project_site won't replace a folder it didn't build", {
  root <- make_project()
  out <- withr::local_tempdir()
  writeLines("mine", file.path(out, "notes.txt"))
  dir.create(file.path(root, "Site"))
  expect_error(suppressWarnings(build_project_site(root, out, render = FALSE)), "didn't put there")
  expect_true(file.exists(file.path(out, "notes.txt")))
})

test_that("build_project_site renders a site that publish_site accepts", {
  q <- tryCatch(D2SBtools:::find_quarto(), error = function(e) "")
  skip_if(!nzchar(q), "Quarto not installed")
  root <- make_project()
  suppressMessages(add_to_site(share(root, "eda.html"), "report", date = "2026-10-14", root = root))
  site <- suppressMessages(build_project_site(root, file.path(withr::local_tempdir(), "b"), quarto = q))
  expect_true(file.exists(file.path(site, "index.html")))
  expect_true(file.exists(file.path(site, "reports", "eda.html")))
  expect_true(file.exists(file.path(site, "reports", "2026-10-14-eda.html")))
  expect_true(file.exists(file.path(site, "How to view these reports.pdf")))
  expect_false(file.exists(file.path(site, "search.json")))
  expect_message(publish_site(site, file.path(withr::local_tempdir(), "Site")), "new")
})

test_that("a co-edited draft is linked, not copied", {
  root <- make_project()
  url <- "https://uiowa.sharepoint.com/sites/D2SB/Manuscripts/draft.docx"
  suppressMessages(add_product("manuscript", "Trends manuscript", status = "in_progress",
                               actual_date = NA, artifact_url = url, analyst = "A", pi = "P",
                               project = "DEMO", path = file.path(root, "Admin", "products.csv")))
  out <- file.path(withr::local_tempdir(), "build")
  dir.create(file.path(root, "Site"))
  suppressWarnings(build_project_site(root, out, render = FALSE))
  index <- paste(readLines(file.path(out, "index.qmd")), collapse = "\n")
  expect_match(index, paste0("[Trends manuscript](", url, ")"), fixed = TRUE)
  expect_match(index, "shared draft")
  expect_false(dir.exists(file.path(out, "manuscripts")))
})

test_that("`as` keeps renamed files together as one document", {
  root <- make_project()
  suppressMessages({
    add_to_site(share(root, "Manuscript_v3_JD.docx"), "manuscript", as = "manuscript",
                title = "Trends manuscript", date = "2026-10-01", root = root)
    add_to_site(share(root, "RSV paper final.docx"), "manuscript", as = "manuscript",
                date = "2026-10-15", root = root)
  })
  expect_true(file.exists(file.path(root, "Site/manuscripts/2026-10-01-manuscript.docx")))
  expect_true(file.exists(file.path(root, "Site/manuscripts/2026-10-15-manuscript.docx")))
  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  expect_equal(p$title, c("Trends manuscript", "Trends manuscript"))
  expect_equal(p$related[2], "DEMO-001")
  expect_error(add_to_site(share(root, "x.docx"), "manuscript", as = "a/b", root = root), "`as`")
})

test_that("a draft link is attached to its document, not copied or listed twice", {
  root <- make_project()
  url <- "https://uiowa.sharepoint.com/sites/D2SB/Manuscripts/draft.docx"
  suppressMessages({
    add_to_site(share(root, "ms.docx"), "manuscript", as = "manuscript",
                title = "Trends manuscript", date = "2026-10-01", root = root)
    add_to_site(share(root, "ms.docx"), "manuscript", as = "manuscript", date = "2026-10-15", root = root)
    id <- add_to_site(url, "manuscript", as = "manuscript", root = root)
  })
  expect_error(add_to_site(url, "manuscript", root = root), "needs `as`")

  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  link <- p[p$id == id, ]
  expect_equal(link$artifact_url, url)
  expect_equal(link$related, "DEMO-002")
  expect_equal(link$status, "in_progress")
  expect_equal(link$title, "Trends manuscript")
  expect_equal(length(list.files(file.path(root, "Site/manuscripts"))), 2)

  out <- file.path(withr::local_tempdir(), "build")
  build_project_site(root, out, render = FALSE)
  page <- paste(readLines(file.path(out, "manuscripts", "manuscript.qmd")), collapse = "\n")
  expect_match(page, paste0("[Edit the shared draft online](", url, ")"), fixed = TRUE)
  index <- readLines(file.path(out, "index.qmd"))
  rows <- grep("^\\| \\[Trends manuscript", index, value = TRUE)   # the papers table
  expect_length(rows, 1)                       # one row per document, not per version or link
  expect_match(rows, "shared draft", fixed = TRUE)
  expect_match(rows, "2026-10-15", fixed = TRUE)

  # A planned entry for the same manuscript doesn't add a second row once it's shared.
  suppressMessages(add_product("manuscript", "Trends manuscript", status = "planned", actual_date = NA,
                               analyst = "A", pi = "P", project = "DEMO",
                               path = file.path(root, "Admin", "products.csv")))
  build_project_site(root, out, render = FALSE)
  index <- readLines(file.path(out, "index.qmd"))
  expect_length(grep("Trends manuscript", grep("^\\|", index, value = TRUE), fixed = TRUE), 1)
})

test_that("build_project_site stops when there is no Site/ folder, and warns when it's empty", {
  root <- make_project()
  out <- file.path(withr::local_tempdir(), "build")
  expect_error(build_project_site(root, out, render = FALSE), "No Site/ folder")
  dir.create(file.path(root, "Site"))
  expect_warning(build_project_site(root, out, render = FALSE), "No shared documents")
})

test_that("preview_site needs a built site", {
  expect_error(preview_site(withr::local_tempdir()), "No index.html")
})

test_that("build_project_site warns when Site/ and products.csv disagree", {
  root <- make_project()
  suppressMessages(add_to_site(share(root, "eda.html"), "report", date = "2026-10-14", root = root))
  writeLines("x", file.path(root, "Site/reports/2026-10-20-other.html"))       # not logged
  p <- D2SBtools:::read_products(file.path(root, "Admin", "products.csv"))
  p$artifact_url <- "Site/reports/2026-10-14-renamed.html"                       # logged, but no such file
  utils::write.csv(p, file.path(root, "Admin", "products.csv"), row.names = FALSE, na = "")
  out <- file.path(withr::local_tempdir(), "build")
  w <- character()
  withCallingHandlers(build_project_site(root, out, render = FALSE),
                      warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("2026-10-14-renamed.html", w, fixed = TRUE)))
  expect_true(any(grepl("Site/reports/2026-10-20-other.html", w, fixed = TRUE)))
})
