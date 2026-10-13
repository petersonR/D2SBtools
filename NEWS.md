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
