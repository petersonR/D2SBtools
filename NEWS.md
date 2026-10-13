# D2SBtools 0.5.0

* The products log is now `Admin/products.csv` instead of
  `Admin/products_minimal.xlsx`. New `add_product()` appends a row, filling
  analyst and PI from `project.yml`, the date, and the next `<project>-<NNN>`
  id. In a project that still has the workbook, the first `add_product()`
  call converts it to the CSV and says so.
* New `product_vocabulary()` holds the short list of product types and
  statuses that `add_product()` checks against, so counts are comparable
  across projects.
* `db2duckdb()` runs parallel workers as separate R sessions rather than
  forked processes. Forking after DuckDB has started can deadlock, leaving a
  job hung rather than failed. `workers > 1` now also works on Windows.

# D2SBtools 0.4.0

* New `add_signature()` adds the D2SB logo to a ggplot at a fixed physical
  size and position, as the group's signature on published figures.
* New `db2duckdb()` converts a SQLite database to DuckDB, in parallel, with
  row-count verification and resumable progress.
* `move_to_dissemination()` now reports a missing report instead of failing
  with "object 'path' not found".
* `sp_root()`'s setup hint now gives the correct `usethis::edit_r_environ(scope = 'user')`.
* Package dependencies are declared in DESCRIPTION, so `R CMD check` passes.
* Added a test suite.
