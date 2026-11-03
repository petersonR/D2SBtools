#' Publish a rendered website to a SharePoint folder
#'
#' Mirrors a rendered site into a folder in the D2SB SharePoint sync, so
#' collaborators who sync that folder can open `index.html` in their browser
#' and always see the latest version. For a D2SB project, build the site with
#' [build_project_site()] and publish it to the project's `Site/` folder:
#' `publish_site(build_project_site())`.
#'
#' The destination becomes an exact copy of `site_dir`: new and changed files
#' are copied first, then files that are no longer in the site are deleted, so
#' readers never see links to pages that have been removed. Unchanged files are
#' left alone, so OneDrive only re-uploads what changed.
#'
#' Because it overwrites and deletes, `publish_site()` keeps a manifest of what
#' it published (the `.d2sb-site` file in `dest`) and checks it first:
#' * It only writes into a folder that is new, empty, or has a manifest.
#' * If anyone has added or changed files in `dest` since the last publish
#'   (for example a PI with edit access saving a revised abstract there), it
#'   stops and lists them instead of deleting or overwriting their work. Copy
#'   anything worth keeping into the project (see [add_to_site()]), then rerun
#'   with `overwrite_outside_changes = TRUE`.
#'
#' It also refuses to publish data files (`.csv`, `.rds`, `.parquet`, ...)
#' unless `allow_data_files = TRUE`, since everyone who syncs the folder gets
#' a copy of every file in it.
#'
#' @param site_dir Path to the rendered site. Must contain `index.html`.
#' @param dest Destination folder. Defaults to `Site` in the project's
#'   SharePoint folder (see [sp_path()]). Its parent folder must already
#'   exist, which catches a SharePoint library that isn't synced on this
#'   machine.
#' @param dry_run If `TRUE`, report what would change without changing
#'   anything.
#' @param allow_data_files If `TRUE`, publish data files found in the site.
#'   Only do this for aggregate results cleared for everyone with access to
#'   `dest`.
#' @param overwrite_outside_changes If `TRUE`, publish even though files in
#'   `dest` were added or changed since the last publish; those files are
#'   deleted or overwritten.
#' @return Invisibly, a data frame with one row per file: `path` (relative to
#'   the site) and `action` (`"new"`, `"update"`, `"unchanged"` or
#'   `"delete"`).
#' @examples
#' \dontrun{
#' # In a D2SB project:
#' site <- build_project_site()
#' publish_site(site, dry_run = TRUE)
#' publish_site(site)
#'
#' # Any other rendered site and destination in the SharePoint sync:
#' publish_site("_site", dest = sp_root("Website/site"))
#' }
#' @export
publish_site <- function(site_dir = "_site",
                         dest = sp_path("Site"),
                         dry_run = FALSE,
                         allow_data_files = FALSE,
                         overwrite_outside_changes = FALSE) {

  if (!dir.exists(site_dir))
    rlang::abort(glue::glue(
      "No rendered site at '{site_dir}'. Build or render it first, or set `site_dir`."))
  if (!file.exists(file.path(site_dir, "index.html")))
    rlang::abort(glue::glue(
      "'{site_dir}' has no index.html, so it doesn't look like a rendered site. Check `site_dir`."))

  site_dir <- as.character(fs::path_abs(site_dir))
  dest <- as.character(fs::path_abs(dest))

  if (fs::path_has_parent(dest, site_dir) || fs::path_has_parent(site_dir, dest))
    rlang::abort(glue::glue(
      "`dest` ('{dest}') and `site_dir` ('{site_dir}') overlap. Choose a destination outside the site."))

  if (!dir.exists(fs::path_dir(dest)))
    rlang::abort(glue::glue(
      "The folder '{fs::path_dir(dest)}' doesn't exist. Is the SharePoint library synced on this machine, and is D2SB_SP_PATH right?"))

  existing <- site_files(dest)
  if (length(existing) > 0 && !(site_marker %in% existing))
    rlang::abort(glue::glue(
      "'{dest}' already has files that publish_site() didn't put there, and publishing would delete them. ",
      "Choose a new or empty folder, or empty this one yourself first."))

  src <- setdiff(site_files(site_dir), site_marker)

  data_ext <- c("csv", "tsv", "txt", "rds", "rda", "rdata", "parquet", "feather",
                "arrow", "sas7bdat", "sav", "dta", "xpt", "sqlite", "db",
                "duckdb", "xlsx", "xls")
  data_files <- src[tolower(tools::file_ext(src)) %in% data_ext]
  # robots.txt is a harmless site file, not data.
  data_files <- setdiff(data_files, "robots.txt")
  if (length(data_files) > 0 && !allow_data_files)
    rlang::abort(c(
      glue::glue("The site contains {length(data_files)} data file(s), which everyone with access to '{dest}' would get a copy of:"),
      bullets(data_files),
      i = "Remove them from the site, or set `allow_data_files = TRUE` if they are aggregate results cleared for this audience."))

  # Compare what is in dest now with what we last published there. Office
  # files are compared by content (see content_hash()), because SharePoint
  # writes its own metadata into every Office file it stores.
  old <- setdiff(existing, site_marker)
  old_hash <- content_hash(file.path(dest, old))
  names(old_hash) <- old
  manifest <- read_site_manifest(dest)
  added <- setdiff(old, names(manifest))
  common <- intersect(old, names(manifest))
  expected <- upgrade_manifest_hashes(manifest[common], common, site_dir)
  changed <- common[old_hash[common] != expected]
  outside <- c(added, changed)
  if (length(outside) > 0 && !overwrite_outside_changes)
    rlang::abort(c(
      glue::glue("{length(outside)} file(s) in '{dest}' were added or changed since the last publish, so publishing could delete or overwrite someone's work:"),
      bullets(c(if (length(added)) paste(added, "(added)"), if (length(changed)) paste(changed, "(changed)"))),
      i = "Copy anything worth keeping into the project (e.g. with add_to_site()), then rerun with `overwrite_outside_changes = TRUE`."))

  src_hash <- content_hash(file.path(site_dir, src))
  names(src_hash) <- src
  in_dest <- src %in% old
  action <- rep("new", length(src))
  action[in_dest] <- ifelse(src_hash[src[in_dest]] == old_hash[src[in_dest]],
                            "unchanged", "update")
  stale <- setdiff(old, src)
  plan <- data.frame(path = c(src, stale),
                     action = c(action, rep("delete", length(stale))),
                     stringsAsFactors = FALSE)

  counts <- table(factor(plan$action, levels = c("new", "update", "delete", "unchanged")))
  summary <- glue::glue(
    "{counts[['new']]} new, {counts[['update']]} updated, {counts[['delete']]} removed, {counts[['unchanged']]} unchanged")

  if (dry_run) {
    message(glue::glue("Dry run, nothing changed. Publishing to {dest} would make: {summary}."))
    return(invisible(plan))
  }

  if (!dir.exists(dest)) dir.create(dest)
  # Record every file that will be ours (old and new) before touching any, so a
  # publish interrupted partway can be rerun without tripping the outside-change
  # check on our own files.
  write_site_manifest(dest, site_dir, c(src_hash, old_hash[setdiff(names(old_hash), src)]))

  to_copy <- plan$path[plan$action %in% c("new", "update")]
  for (d in unique(dirname(file.path(dest, to_copy))))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(file.path(site_dir, to_copy), file.path(dest, to_copy), overwrite = TRUE)
  if (!all(ok))
    rlang::abort(c(
      glue::glue("Failed to copy {sum(!ok)} file(s) to '{dest}'; nothing was deleted. Rerun once the problem is fixed:"),
      bullets(to_copy[!ok])))

  unlink(file.path(dest, stale))
  remove_empty_dirs(dest)
  write_site_manifest(dest, site_dir, src_hash)

  message(glue::glue("Published {site_dir} to {dest}: {summary}."))
  message(glue::glue(
    "To invite collaborators, share that folder with them (view-only) and email them the setup guide: ",
    "run D2SBtools::site_guide() to copy it, formatted, to your clipboard."))
  invisible(plan)
}

site_marker <- ".d2sb-site"

# Files under `dir`, relative to it, ignoring operating-system clutter.
site_files <- function(dir) {
  if (!dir.exists(dir)) return(character())
  f <- fs::dir_ls(dir, recurse = TRUE, all = TRUE, type = "file")
  f <- as.character(fs::path_rel(f, dir))
  f[!basename(f) %in% c(".DS_Store", "Thumbs.db", "desktop.ini")]
}

md5 <- function(paths) unname(as.character(tools::md5sum(paths)))

office_ext <- c("docx", "docm", "xlsx", "xlsm", "pptx", "pptm")

# Parts of an Office file that are bookkeeping, not content. SharePoint adds
# customXml/ and docProps/custom.xml to every Office file it stores, and
# updates [Content_Types].xml and the .rels files to list them.
office_bookkeeping <- function(parts) {
  grepl("^(customXml|docProps)/", parts) | parts == "[Content_Types].xml" | grepl("\\.rels$", parts)
}

# A file's fingerprint: its md5, or for an Office file the md5 of its content
# parts only (prefixed "office:"), so SharePoint's metadata doesn't count as a
# change while any edit to the document's text, comments or media does.
content_hash <- function(paths) {
  vapply(paths, function(p) {
    if (!tolower(tools::file_ext(p)) %in% office_ext) return(md5(p))
    tryCatch({
      parts <- utils::unzip(p, list = TRUE)$Name
      parts <- sort(parts[!office_bookkeeping(parts) & !endsWith(parts, "/")])
      d <- tempfile()
      on.exit(unlink(d, recursive = TRUE))
      utils::unzip(p, files = parts, exdir = d)
      f <- tempfile()
      on.exit(unlink(f), add = TRUE)
      writeLines(paste(parts, md5(file.path(d, parts))), f)
      paste0("office:", md5(f))
    }, error = function(e) md5(p))
  }, "", USE.NAMES = FALSE)
}

# Manifests written before content_hash() recorded raw md5s. For an Office
# file whose source hasn't changed since, the published content is the
# source's content, so compare against that.
upgrade_manifest_hashes <- function(expected, paths, site_dir) {
  legacy <- tolower(tools::file_ext(paths)) %in% office_ext & !startsWith(expected, "office:")
  for (i in which(legacy)) {
    src <- file.path(site_dir, paths[i])
    if (file.exists(src) && md5(src) == expected[i]) expected[i] <- content_hash(src)
  }
  expected
}

# Up to 10 values as rlang bullet points.
bullets <- function(x) {
  x <- x[seq_len(min(10, length(x)))]
  names(x) <- rep("*", length(x))
  x
}

# The manifest: a few comment lines, then one "md5<TAB>path" line per file.
write_site_manifest <- function(dest, site_dir, hashes) {
  hashes <- hashes[order(names(hashes))]
  writeLines(c(
    "# Published by D2SBtools::publish_site(), which overwrites and deletes files in this folder.",
    "# Don't add or edit files here by hand: the next publish will stop and list them.",
    paste("# Published:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("# From:", site_dir),
    paste(hashes, names(hashes), sep = "\t")
  ), file.path(dest, site_marker))
}

read_site_manifest <- function(dest) {
  path <- file.path(dest, site_marker)
  if (!file.exists(path)) return(stats::setNames(character(), character()))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  parts <- strsplit(lines, "\t", fixed = TRUE)
  stats::setNames(vapply(parts, `[`, "", 1), vapply(parts, `[`, "", 2))
}

# Deepest first, so a folder emptied by removing its subfolders goes too.
remove_empty_dirs <- function(dest) {
  dirs <- as.character(fs::dir_ls(dest, recurse = TRUE, all = TRUE, type = "directory"))
  dirs <- dirs[order(nchar(dirs), decreasing = TRUE)]
  for (d in dirs)
    if (length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0) fs::dir_delete(d)
}

#' The collaborator setup guide for project websites
#'
#' Copies the guide for PIs and study teams (how to sync a project's `Site/`
#' folder, open the website and get updates) to the clipboard as formatted
#' text, ready to paste into an email with its headings, lists and table
#' intact. [publish_site()] points to it after each publish. Edit the guide in
#' the package, at `inst/site/guide.md`.
#'
#' Formatted copying uses `textutil` and `pbcopy` on macOS, PowerShell's
#' `Set-Clipboard -AsHtml` on Windows, and `xclip` on Linux. If that fails,
#' the guide is printed instead.
#'
#' @param copy If `TRUE`, copy the formatted guide to the clipboard.
#' @param print If `TRUE`, print the guide's Markdown in the console.
#' @return The path to the guide (Markdown), invisibly.
#' @examples
#' \dontrun{
#' site_guide()               # copy, then paste into your email
#' site_guide(copy = FALSE, print = TRUE)
#' }
#' @export
site_guide <- function(copy = TRUE, print = FALSE) {
  path <- system.file("site", "guide.md", package = "D2SBtools")
  if (!nzchar(path)) rlang::abort("The setup guide is missing from this D2SBtools installation; reinstall it.")
  md <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (copy) {
    ok <- tryCatch(copy_html(guide_html(md)), error = function(e) FALSE)
    if (isTRUE(ok)) {
      message("Copied the setup guide to the clipboard as formatted text. Paste it into your email, under the folder link.")
    } else {
      rlang::warn("Couldn't copy the formatted guide to the clipboard here, so it's printed below instead.")
      print <- TRUE
    }
  }
  if (print) cat(md, sep = "\n")
  invisible(path)
}

# The guide as a standalone HTML document, styled for pasting into email.
guide_html <- function(md) {
  rlang::check_installed("commonmark", "to format the setup guide")
  body <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
  body <- gsub("<table>", "<table border=\"1\" cellpadding=\"6\" style=\"border-collapse:collapse\">",
               body, fixed = TRUE)
  paste0("<html><head><meta charset=\"utf-8\"></head>",
         "<body style=\"font-family:Calibri,Arial,sans-serif;font-size:11pt\">", body, "</body></html>")
}

# Put HTML on the clipboard as rich text. TRUE on success.
copy_html <- function(html) {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  writeLines(enc2utf8(html), f, useBytes = TRUE)
  sys <- Sys.info()[["sysname"]]
  status <- if (sys == "Darwin") {
    # pbcopy stores RTF input as rich text; textutil converts the HTML to RTF.
    system(paste("textutil -convert rtf -inputencoding UTF-8 -stdout", shQuote(f), "| pbcopy"))
  } else if (.Platform$OS.type == "windows") {
    system2("powershell", c("-NoProfile", "-Command",
      shQuote(paste0("Get-Content -Raw -Encoding UTF8 '", f, "' | Set-Clipboard -AsHtml"))))
  } else {
    system(paste("xclip -selection clipboard -t text/html <", shQuote(f)))
  }
  identical(as.integer(status), 0L)
}
