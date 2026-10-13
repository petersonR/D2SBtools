#' Convert a SQLite database to DuckDB
#'
#' Copies every table of a SQLite database into a new DuckDB database file,
#' verifying row counts table by table. Built for large claims databases on
#' Argon, where the source and destination sit on a network volume (LSS) and
#' the work is done on fast local scratch.
#'
#' The conversion runs in four steps, each of which can be resumed:
#'
#' 1. The source is copied to `scratch` (skipped if an identical-size copy is
#'    already there).
#' 2. Each table is converted into its own part file in `scratch`, in parallel
#'    across `workers`. The row count of every part is checked against the
#'    source with RSQLite, an engine independent of DuckDB's SQLite reader. A
#'    finished part leaves a `.done` marker, so rerunning after a crash or
#'    walltime kill skips completed tables.
#' 3. The parts are merged into one DuckDB file in `scratch`, and the row
#'    counts are checked again.
#' 4. The merged file is copied to `dest` under a temporary name, its size
#'    verified, and then renamed into place, so `dest` is never left
#'    half-written.
#'
#' Tables whose types DuckDB cannot reconcile (SQLite allows, e.g., text in an
#' INTEGER column) are retried with every column read as VARCHAR. This changes
#' the column types, so those tables are flagged in the result and in a
#' warning; convert their columns back as needed downstream.
#'
#' Only tables are converted. Views, indexes, and triggers are not.
#'
#' @param src Path to the SQLite database.
#' @param dest Path for the DuckDB database to create.
#' @param scratch Directory for the working copy, parts, and merged file. Needs
#'   free space of roughly three times the source size. On Argon, use local
#'   scratch, not your home directory or LSS.
#' @param workers Number of tables converted at once, each in its own R
#'   process.
#' @param threads,memory_limit DuckDB threads and memory limit per worker. The
#'   total is `workers` times these, which should fit the job's allocation
#'   (e.g. 12 workers x 16GB within a 256G request).
#' @param copy_source Whether to copy `src` to `scratch` first. Reading a large
#'   database across the network is slow and fragile, so this is the default.
#' @param overwrite Whether to replace an existing `dest`.
#' @param keep_scratch Whether to keep the working files in `scratch`
#'   afterwards. Keep them while checking a conversion; they allow a rerun to
#'   skip finished work.
#'
#' @return Invisibly, a data frame with one row per table: `table`, `rows`,
#'   and `all_varchar` (whether the VARCHAR fallback was used).
#'
#' @examples
#' \dontrun{
#' # Argon: qsub -cwd -q AML-HM -pe smp 48 -l mf=256G db2duckdb.sh
#' db2duckdb(
#'   src = "/Shared/Statepi_Premier/databases/premier_24.db",
#'   dest = "/Shared/Statepi_Marketscan/databases/premier/premier_24.duckdb",
#'   scratch = "/localscratch/Users/hawkid",
#'   workers = 12
#' )
#' }
#'
#' @export
db2duckdb <- function(src, dest, scratch = tempdir(), workers = 1,
                      threads = 4, memory_limit = "16GB", copy_source = TRUE,
                      overwrite = FALSE, keep_scratch = FALSE) {
  rlang::check_installed(c("DBI", "duckdb", "RSQLite"))

  if (!file.exists(src)) {
    rlang::abort(glue::glue("Source database not found: {src}"))
  }
  # Fail before hours of work, not after.
  if (file.exists(dest) && !overwrite) {
    rlang::abort(glue::glue(
      "{dest} already exists; set `overwrite = TRUE` to replace it."
    ))
  }
  # A copied .db without its write-ahead log silently loses the uncommitted
  # transactions, so refuse rather than convert a partial database.
  if (copy_source && file.exists(paste0(src, "-wal"))) {
    rlang::abort(glue::glue(
      "{src} has a write-ahead log (-wal file); checkpoint the source ",
      "(PRAGMA wal_checkpoint(TRUNCATE)) or use `copy_source = FALSE`."
    ))
  }

  fs::dir_create(scratch)
  db_name <- fs::path_ext_remove(fs::path_file(src))
  parts_dir <- fs::dir_create(file.path(scratch, paste0(db_name, "_parts")))
  tmp_root <- file.path(scratch, paste0(db_name, "_duckdb_tmp"))

  # 1. Local working copy ####
  if (copy_source) {
    src_local <- file.path(scratch, paste0(db_name, "_src.db"))
    if (!file.exists(src_local) || file.size(src_local) != file.size(src)) {
      message(format(Sys.time()), " copying ", src, " to scratch")
      ok <- file.copy(src, src_local, overwrite = TRUE)
      if (!ok || file.size(src_local) != file.size(src)) {
        rlang::abort(glue::glue("Copy of {src} to {src_local} failed."))
      }
    }
  } else {
    src_local <- src
  }

  # 2. One part file per table ####
  sq <- DBI::dbConnect(RSQLite::SQLite(), src_local,
                       flags = RSQLite::SQLITE_RO)
  tables <- DBI::dbGetQuery(
    sq,
    "SELECT name FROM sqlite_master
     WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
  )$name
  DBI::dbDisconnect(sq)
  if (length(tables) == 0) {
    rlang::abort(glue::glue("No tables found in {src}."))
  }

  # Install the extension once, here, rather than racing in every worker.
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv)
  DBI::dbExecute(con, "INSTALL sqlite")
  close_duckdb(con, drv)

  # Each table is converted in a fresh process. Errors come back as condition
  # objects rather than stopping the run, so one bad table does not discard
  # the others.
  convert <- function(t, fn, src_local, parts_dir, tmp_root, threads,
                      memory_limit) {
    tryCatch(
      fn(t, src_local, parts_dir, tmp_root, threads, memory_limit),
      error = identity
    )
  }
  # Detach the worker function from the package namespace, so the workers run
  # this code rather than whichever D2SBtools version they would load. It only
  # calls base R and `pkg::` functions, so it needs nothing else.
  fn <- db2duckdb_table
  environment(fn) <- globalenv()

  message(format(Sys.time()), " converting ", length(tables), " tables")
  args <- list(fn = fn, src_local = src_local, parts_dir = parts_dir,
               tmp_root = tmp_root, threads = threads,
               memory_limit = memory_limit)
  res <- if (workers > 1) {
    # Socket workers are fresh R sessions. Forked workers (mclapply) inherit
    # this process's state, and forking after DuckDB has started its threads
    # can deadlock: a job that hangs silently instead of failing.
    cl <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    tryCatch(
      do.call(parallel::parLapplyLB, c(list(cl, tables, convert), args)),
      error = function(e) {
        rlang::abort(c(
          "A worker process died (out of memory?).",
          i = "Finished tables are kept, so rerun to retry the rest.",
          x = conditionMessage(e)
        ))
      }
    )
  } else {
    lapply(tables, function(t) do.call(convert, c(list(t), args)))
  }

  # Merging past a failed table would produce a database silently missing it.
  failed <- vapply(res, inherits, logical(1), what = "error")
  if (any(failed)) {
    rlang::abort(c(
      "Some tables failed to convert; finished tables are kept, so rerun to retry.",
      stats::setNames(
        paste0(tables[failed], ": ",
               vapply(res[failed], conditionMessage, character(1))),
        rep("x", sum(failed))
      )
    ))
  }
  summary <- do.call(rbind, res)

  # 3. Merge ####
  final_local <- file.path(scratch, paste0(db_name, ".duckdb"))
  unlink(c(final_local, paste0(final_local, ".wal")))
  message(format(Sys.time()), " merging parts")
  drv <- duckdb::duckdb(final_local)
  con <- DBI::dbConnect(drv)
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  for (t in tables) {
    part <- file.path(parts_dir, paste0(t, ".duckdb"))
    qt <- DBI::dbQuoteIdentifier(con, t)
    DBI::dbExecute(con, paste0(
      "ATTACH ", DBI::dbQuoteString(con, part), " AS p (READ_ONLY)"
    ))
    DBI::dbExecute(con, paste0("CREATE TABLE ", qt, " AS SELECT * FROM p.", qt))
    DBI::dbExecute(con, "DETACH p")
    n <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ", qt))$n
    if (n != summary$rows[summary$table == t]) {
      close_duckdb(con, drv)
      rlang::abort(glue::glue("Row count changed while merging table {t}."))
    }
  }
  DBI::dbExecute(con, "CHECKPOINT")
  close_duckdb(con, drv)

  # 4. Copy into place ####
  message(format(Sys.time()), " copying to ", dest)
  dest_tmp <- paste0(dest, ".tmp")
  ok <- file.copy(final_local, dest_tmp, overwrite = TRUE)
  if (!ok || file.size(dest_tmp) != file.size(final_local)) {
    unlink(dest_tmp)
    rlang::abort(glue::glue("Copy to {dest_tmp} failed or is incomplete."))
  }
  if (file.exists(dest)) unlink(dest)
  if (!file.rename(dest_tmp, dest)) {
    rlang::abort(glue::glue("Could not rename {dest_tmp} to {dest}."))
  }

  if (!keep_scratch) {
    unlink(c(parts_dir, tmp_root, final_local), recursive = TRUE)
    if (copy_source) unlink(src_local)
  }

  if (any(summary$all_varchar)) {
    rlang::warn(c(
      "These tables were converted with every column as VARCHAR:",
      stats::setNames(
        summary$table[summary$all_varchar],
        rep("*", sum(summary$all_varchar))
      )
    ))
  }
  message(format(Sys.time()), " wrote ", dest, ": ", nrow(summary),
          " tables, ", sum(summary$rows), " rows")
  invisible(summary)
}

# Convert one table into its own DuckDB part file. Returns a one-row data
# frame (table, rows, all_varchar). A `.done` marker records success, and its
# contents let a rerun report a skipped table without reconverting it.
db2duckdb_table <- function(t, src_local, parts_dir, tmp_root, threads,
                            memory_limit) {
  part <- file.path(parts_dir, paste0(t, ".duckdb"))
  done <- paste0(part, ".done")
  if (file.exists(done)) {
    d <- readLines(done)
    return(data.frame(table = t, rows = as.numeric(d[1]),
                      all_varchar = as.logical(d[2])))
  }
  unlink(c(part, paste0(part, ".wal")))

  # A temp directory per table, so parallel workers never share spill files.
  tmp_dir <- fs::dir_create(file.path(tmp_root, make.names(t)))

  drv <- duckdb::duckdb(part)
  con <- DBI::dbConnect(drv)
  # A socket worker outlives the table, and DuckDB holds the file lock until
  # the database is shut down, so close it explicitly or the merge cannot
  # open this part.
  on.exit({
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(drv)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("SET threads = ", as.integer(threads)))
  DBI::dbExecute(con, paste0("SET memory_limit = ",
                             DBI::dbQuoteString(con, memory_limit)))
  DBI::dbExecute(con, paste0("SET temp_directory = ",
                             DBI::dbQuoteString(con, tmp_dir)))
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  DBI::dbExecute(con, "LOAD sqlite")
  DBI::dbExecute(con, paste0(
    "ATTACH ", DBI::dbQuoteString(con, src_local),
    " AS s (TYPE SQLITE, READ_ONLY)"
  ))

  qt <- DBI::dbQuoteIdentifier(con, t)
  q <- paste0("CREATE TABLE ", qt, " AS SELECT * FROM s.main.", qt)
  all_varchar <- FALSE
  err <- tryCatch({
    DBI::dbExecute(con, q)
    NULL
  }, error = conditionMessage)
  if (!is.null(err)) {
    if (!grepl("convert|type|mismatch|invalid", err, ignore.case = TRUE)) {
      stop(err, call. = FALSE)
    }
    message(t, ": type error, retrying with all columns as VARCHAR")
    DBI::dbExecute(con, "SET GLOBAL sqlite_all_varchar = true")
    DBI::dbExecute(con, q)
    all_varchar <- TRUE
  }

  n_duck <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ", qt))$n
  sq <- DBI::dbConnect(RSQLite::SQLite(), src_local,
                       flags = RSQLite::SQLITE_RO)
  n_src <- DBI::dbGetQuery(
    sq, paste0("SELECT count(*) AS n FROM ", DBI::dbQuoteIdentifier(sq, t))
  )$n
  DBI::dbDisconnect(sq)
  if (n_duck != n_src) {
    stop(t, ": ", n_duck, " rows in DuckDB but ", n_src, " in SQLite",
         call. = FALSE)
  }

  DBI::dbExecute(con, "CHECKPOINT")
  writeLines(c(format(n_duck, scientific = FALSE), all_varchar), done)
  message(format(Sys.time()), " done: ", t, " (", n_duck, " rows)")
  data.frame(table = t, rows = as.numeric(n_duck), all_varchar = all_varchar)
}

# Disconnect and shut the database down, releasing its file lock now rather
# than whenever the driver is garbage-collected.
close_duckdb <- function(con, drv) {
  DBI::dbDisconnect(con)
  duckdb::duckdb_shutdown(drv)
}
