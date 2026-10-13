test_that("create_project copies the template and fills project.yml", {
  dir <- withr::local_tempdir()
  path <- suppressMessages(create_project(
    "demo", path = dir, open = FALSE, title = "Demo", analyst = "Me"
  ))

  for (d in c("Admin", "Code", "DataRaw", "DataProcessed", "Reports",
              "Dissemination", "Background", "GrantInfo")) {
    expect_true(dir.exists(file.path(path, d)), info = d)
  }
  expect_true(file.exists(file.path(path, ".gitignore")))
  expect_false(file.exists(file.path(path, "gitignore.template")))

  yml <- yaml::read_yaml(file.path(path, "Admin", "project.yml"))
  expect_equal(yml$project_name, "demo")
  expect_equal(yml$title, "Demo")
  expect_equal(yml$analyst, "Me")
  expect_equal(yml$date_created, as.character(Sys.Date()))
})

test_that("create_project refuses a non-empty directory or a blank name", {
  dir <- withr::local_tempdir()
  fs::dir_create(file.path(dir, "demo"))
  file.create(file.path(dir, "demo", "existing.txt"))

  expect_error(create_project("demo", path = dir, open = FALSE), "not empty")
  expect_error(create_project("", path = dir, open = FALSE), "non-empty")
})

test_that("create_report writes the template and will not overwrite", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "Reports", "eda.qmd")

  suppressMessages(create_report(path))
  expect_true(file.exists(path))
  expect_gt(length(readLines(path)), 0)
  expect_error(create_report(path), "already exists")
})

test_that("sp_root reads D2SB_SP_PATH", {
  withr::local_envvar(D2SB_SP_PATH = "")
  expect_error(sp_root(), "D2SB_SP_PATH")

  withr::local_envvar(D2SB_SP_PATH = "/sp")
  expect_equal(sp_root("Projects"), file.path("/sp", "Projects"))
})

test_that("move_to_dissemination reports a missing report clearly", {
  expect_error(
    move_to_dissemination(file.path(tempdir(), "missing.html"), "drafts"),
    "No report found"
  )
})
