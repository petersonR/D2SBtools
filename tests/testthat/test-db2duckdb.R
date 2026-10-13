skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")
skip_if_not_installed("RSQLite")

# A small SQLite database with an ordinary table, a table whose name needs
# quoting, and a table whose INTEGER column holds text, which DuckDB's SQLite
# reader rejects without the VARCHAR fallback.
make_sqlite <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "cars", mtcars)
  DBI::dbWriteTable(con, "odd name", data.frame(id = 1:3, x = c("a", "b", "c")))
  DBI::dbExecute(con, "CREATE TABLE mixed (id INTEGER, val INTEGER)")
  DBI::dbExecute(con, "INSERT INTO mixed VALUES (1, 10), (2, 'not a number')")
  path
}

read_duck <- function(path, table) {
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbReadTable(con, table)
}

test_that("converts every table with identical contents", {
  dir <- withr::local_tempdir()
  src <- make_sqlite(file.path(dir, "src.db"))
  dest <- file.path(dir, "out.duckdb")

  expect_warning(
    res <- suppressMessages(
      db2duckdb(src, dest, scratch = file.path(dir, "scratch"))
    ),
    "VARCHAR"
  )

  expect_setequal(res$table, c("cars", "odd name", "mixed"))
  expect_equal(res$rows[res$table == "cars"], nrow(mtcars))
  expect_equal(res$all_varchar, res$table == "mixed")

  cars <- read_duck(dest, "cars")
  expect_equal(cars, mtcars, ignore_attr = TRUE)
  expect_equal(read_duck(dest, "odd name")$x, c("a", "b", "c"))
  expect_equal(read_duck(dest, "mixed")$val, c("10", "not a number"))

  # Scratch is cleaned up and no temporary destination is left behind.
  expect_false(file.exists(paste0(dest, ".tmp")))
  expect_length(list.files(file.path(dir, "scratch")), 0)
})

test_that("refuses to overwrite unless asked", {
  dir <- withr::local_tempdir()
  src <- make_sqlite(file.path(dir, "src.db"))
  dest <- file.path(dir, "out.duckdb")
  file.create(dest)

  expect_error(db2duckdb(src, dest, scratch = dir), "already exists")
  suppressWarnings(suppressMessages(
    db2duckdb(src, dest, scratch = file.path(dir, "scratch"), overwrite = TRUE)
  ))
  expect_equal(nrow(read_duck(dest, "cars")), nrow(mtcars))
})

test_that("a rerun skips tables already converted", {
  dir <- withr::local_tempdir()
  src <- make_sqlite(file.path(dir, "src.db"))
  dest <- file.path(dir, "out.duckdb")
  scratch <- file.path(dir, "scratch")

  first <- suppressWarnings(suppressMessages(
    db2duckdb(src, dest, scratch = scratch, keep_scratch = TRUE)
  ))
  msgs <- character()
  second <- withCallingHandlers(
    suppressWarnings(
      db2duckdb(src, dest, scratch = scratch, overwrite = TRUE)
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_false(any(grepl("done:", msgs)))
  # The VARCHAR flag survives the skip.
  expect_equal(second, first)
})

test_that("parallel workers give the same result", {
  dir <- withr::local_tempdir()
  src <- make_sqlite(file.path(dir, "src.db"))

  serial <- suppressWarnings(suppressMessages(db2duckdb(
    src, file.path(dir, "a.duckdb"), scratch = file.path(dir, "s1")
  )))
  parallel <- suppressWarnings(suppressMessages(db2duckdb(
    src, file.path(dir, "b.duckdb"), scratch = file.path(dir, "s2"),
    workers = 2
  )))
  expect_equal(parallel[order(parallel$table), ], serial[order(serial$table), ],
               ignore_attr = TRUE)
})

test_that("refuses to copy a source with an uncheckpointed write-ahead log", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "wal.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), src)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbWriteTable(con, "cars", mtcars)
  # The -wal file exists while this connection stays open.
  expect_true(file.exists(paste0(src, "-wal")))

  expect_error(
    db2duckdb(src, file.path(dir, "out.duckdb"), scratch = dir),
    "write-ahead log"
  )
  DBI::dbDisconnect(con)
})

test_that("missing source and empty database fail clearly", {
  dir <- withr::local_tempdir()
  expect_error(db2duckdb(file.path(dir, "nope.db"), file.path(dir, "o.duckdb")),
               "not found")

  empty <- file.path(dir, "empty.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), empty)
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(con, "DROP TABLE t")
  DBI::dbDisconnect(con)
  expect_error(
    suppressMessages(db2duckdb(empty, file.path(dir, "o.duckdb"), scratch = dir)),
    "No tables"
  )
})
