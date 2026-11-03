make_site <- function(root, files) {
  site <- file.path(root, "_site")
  for (f in names(files)) {
    dir.create(dirname(file.path(site, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[f]], file.path(site, f))
  }
  site
}

test_that("publish_site mirrors new, changed, unchanged and removed files", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "sp", "site")
  dir.create(dirname(dest))
  site <- make_site(root, list("index.html" = "home", "a.html" = "a",
                               "old/gone.html" = "gone", "keep.html" = "keep"))
  expect_message(publish_site(site, dest), "4 new")

  unlink(file.path(site, "old"), recursive = TRUE)
  writeLines("a, revised", file.path(site, "a.html"))
  writeLines("b", file.path(site, "b.html"))
  expect_message(plan <- publish_site(site, dest), "1 new, 1 updated, 1 removed, 2 unchanged")

  acts <- stats::setNames(plan$action, plan$path)
  expect_equal(acts[["a.html"]], "update")
  expect_equal(acts[["b.html"]], "new")
  expect_equal(acts[["keep.html"]], "unchanged")
  expect_equal(acts[["old/gone.html"]], "delete")
  expect_equal(readLines(file.path(dest, "a.html")), "a, revised")
  expect_false(file.exists(file.path(dest, "old/gone.html")))
  expect_false(dir.exists(file.path(dest, "old")))
  expect_true(file.exists(file.path(dest, ".d2sb-site")))
})

test_that("dry_run changes nothing", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "site-out")
  site <- make_site(root, list("index.html" = "home"))
  expect_message(plan <- publish_site(site, dest, dry_run = TRUE), "Dry run")
  expect_equal(plan$action, "new")
  expect_false(dir.exists(dest))
})

test_that("publish_site won't delete files it didn't publish", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "shared")
  dir.create(dest)
  writeLines("someone's notes", file.path(dest, "notes.docx"))
  site <- make_site(root, list("index.html" = "home"))
  expect_error(publish_site(site, dest), "didn't put there")
  expect_true(file.exists(file.path(dest, "notes.docx")))
})

test_that("an empty folder, or one with only OS clutter, is fine", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "shared")
  dir.create(dest)
  writeLines("", file.path(dest, ".DS_Store"))
  site <- make_site(root, list("index.html" = "home"))
  expect_message(publish_site(site, dest), "1 new")
})

test_that("publish_site refuses data files unless allowed", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "out")
  site <- make_site(root, list("index.html" = "home", "report_files/cohort.csv" = "id,x"))
  expect_error(publish_site(site, dest), "data file")
  expect_false(dir.exists(dest))
  expect_message(publish_site(site, dest, allow_data_files = TRUE), "2 new")
})

test_that("publish_site checks its inputs", {
  root <- withr::local_tempdir()
  expect_error(publish_site(file.path(root, "nope"), file.path(root, "out")), "No rendered site")

  no_index <- make_site(root, list("page.html" = "x"))
  expect_error(publish_site(no_index, file.path(root, "out")), "no index.html")

  site <- make_site(root, list("index.html" = "home"))
  expect_error(publish_site(site, file.path(root, "not-synced", "site")), "synced")
  expect_error(publish_site(site, file.path(site, "copy")), "overlap")
  expect_error(publish_site(site, root), "overlap")
})

test_that("publish_site stops rather than delete or overwrite outside changes", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "out")
  site <- make_site(root, list("index.html" = "home", "abstract.html" = "ours"))
  suppressMessages(publish_site(site, dest))

  # A PI saves a new file, and edits one of ours, in the synced folder.
  writeLines("PI's draft", file.path(dest, "pi-abstract.docx"))
  writeLines("PI's edit", file.path(dest, "abstract.html"))

  err <- expect_error(publish_site(site, dest), "added or changed")
  expect_match(conditionMessage(err), "pi-abstract.docx \\(added\\)")
  expect_match(conditionMessage(err), "abstract.html \\(changed\\)")
  expect_equal(readLines(file.path(dest, "pi-abstract.docx")), "PI's draft")
  expect_equal(readLines(file.path(dest, "abstract.html")), "PI's edit")

  expect_message(publish_site(site, dest, overwrite_outside_changes = TRUE), "1 updated, 1 removed")
  expect_equal(readLines(file.path(dest, "abstract.html")), "ours")
  expect_false(file.exists(file.path(dest, "pi-abstract.docx")))
  # ...and the next publish is quiet again.
  expect_message(publish_site(site, dest), "0 new, 0 updated, 0 removed, 2 unchanged")
})

test_that("a file deleted from dest is simply published again", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "out")
  site <- make_site(root, list("index.html" = "home", "a.html" = "a"))
  suppressMessages(publish_site(site, dest))
  unlink(file.path(dest, "a.html"))
  expect_message(publish_site(site, dest), "1 new")
  expect_true(file.exists(file.path(dest, "a.html")))
})

test_that("site_guide formats the guide for email, and publish_site points to it", {
  out <- capture.output(path <- site_guide(copy = FALSE, print = TRUE))
  expect_true(file.exists(path))
  expect_true(any(grepl("One-time setup", out, fixed = TRUE)))

  skip_if_not_installed("commonmark")
  html <- D2SBtools:::guide_html(readLines(path, encoding = "UTF-8"))
  expect_match(html, "<h2>One-time setup</h2>", fixed = TRUE)
  expect_match(html, "<strong>Always Keep on This Device</strong>", fixed = TRUE)
  expect_match(html, "<table border=", fixed = TRUE)

  root <- withr::local_tempdir()
  site <- make_site(root, list("index.html" = "home"))
  msgs <- character()
  withCallingHandlers(publish_site(site, file.path(root, "out")),
                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("site_guide()", msgs, fixed = TRUE)))
})
