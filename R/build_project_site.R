#' Build a project's website from what the project already keeps
#'
#' Generates and renders a D2SB-branded Quarto website for a project, for the
#' PI and study team to browse from a synced SharePoint folder. Nothing on it
#' is written by hand; it is assembled from:
#' * `Admin/project.yml`: title, description, PI, analyst, status and dates,
#'   shown on the dashboard (`index.html`).
#' * `Site/`: the files shared with the PI, added with [add_to_site()]. Each
#'   document gets a page with its latest version and a list of earlier ones,
#'   and each section (`reports/`, `abstracts/`, ...) an index page.
#' * `Admin/products.csv`: titles and notes for those files, plus the
#'   dashboard's publications list and upcoming deliverables.
#' * `meeting_notes.md`: the shared meeting notes, newest first under
#'   `## YYYY-MM-DD` headings. The dashboard shows the latest two.
#'
#' The site works opened from disk (no server), so it has no search box.
#' Next to `index.html` goes the setup guide as a PDF, `How to view these
#' reports.pdf`, so people who open the folder link on the web see the
#' instructions first (pin it to the top of the folder in SharePoint). Publish
#' the site with `publish_site(build_project_site())`.
#'
#' @param root Project root.
#' @param out_dir Folder to build in. It is replaced on each build, so it must
#'   be new or a previous build. Defaults to a folder in the session's
#'   temporary directory.
#' @param render If `FALSE`, write the Quarto project without rendering it,
#'   and return its folder.
#' @param quarto Path to the Quarto command line tool. By default found on the
#'   `PATH`, through `QUARTO_PATH`, or in RStudio.
#'
#' @return The path to the rendered site (or, with `render = FALSE`, the
#'   Quarto project), invisibly.
#' @examples
#' \dontrun{
#' site <- build_project_site()
#' preview_site(site)  # opens it in your browser
#' publish_site(site)
#' }
#' @export
build_project_site <- function(root = here::here(),
                               out_dir = file.path(tempdir(), "d2sb-project-site"),
                               render = TRUE,
                               quarto = find_quarto()) {
  info <- read_project_yml(root)
  if (!dir.exists(file.path(root, "Site")))
    rlang::abort(c(
      glue::glue("No Site/ folder in '{root}', so there is nothing to put on the website."),
      i = "Share files with add_to_site() first. If that is the wrong folder, run this from the project, or set `root`."))
  products_path <- file.path(root, "Admin", "products.csv")
  products <- if (file.exists(products_path)) read_products(products_path) else
    stats::setNames(as.data.frame(matrix(character(), 0, length(product_columns))),
                    product_columns)
  docs <- site_documents(root, products)
  check_site_log(root, docs, products)
  if (nrow(docs) == 0)
    rlang::warn(glue::glue("No shared documents in '{file.path(root, 'Site')}': the website will have none."))
  links <- draft_links(products, docs)
  notes <- read_meeting_notes(file.path(root, "meeting_notes.md"))

  build_marker <- ".d2sb-build"
  if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE)) > 0) {
    if (!file.exists(file.path(out_dir, build_marker)))
      rlang::abort(glue::glue(
        "'{out_dir}' already has files that build_project_site() didn't put there. Choose a new folder for `out_dir`."))
    unlink(out_dir, recursive = TRUE)
  }
  fs::dir_create(out_dir)
  file.create(file.path(out_dir, build_marker))

  # The theme and logos only: guide.md in the same folder is for email, and
  # Quarto would render it as a page.
  assets <- system.file("site", c("d2sb-site.scss", "d2sb-logo-reversed.svg", "favicon.svg"),
                        package = "D2SBtools")
  fs::dir_create(file.path(out_dir, "assets"))
  file.copy(assets, file.path(out_dir, "assets"), overwrite = TRUE)

  sections <- unique(docs$section)
  sections <- names(site_section_labels)[names(site_section_labels) %in% sections]
  title <- info$title %||% info$project_name %||% "Project"

  nav <- list(list(text = "Home", href = "index.qmd"))
  for (s in sections)
    nav[[length(nav) + 1]] <- list(text = site_section_labels[[s]], href = paste0(s, "/index.qmd"))
  if (length(notes) > 0)
    nav[[length(nav) + 1]] <- list(text = "Meeting notes", href = "meeting-notes.qmd")

  updated <- format(Sys.Date(), "%B %d, %Y")
  yaml::write_yaml(list(
    project = list(type = "website"),
    website = list(
      title = title,
      favicon = "assets/favicon.svg",
      `page-navigation` = FALSE,
      search = FALSE,
      navbar = list(background = "dark", logo = "assets/d2sb-logo-reversed.svg",
                    `logo-alt` = "Data2Science BRIDGE", title = title, left = nav),
      `page-footer` = list(
        background = "dark",
        left = "Data2Science BRIDGE \u00b7 University of Iowa",
        right = paste("Updated", updated))
    ),
    format = list(html = list(
      theme = list("cosmo", "assets/d2sb-site.scss"),
      toc = FALSE, `anchor-sections` = FALSE))
  ), file.path(out_dir, "_quarto.yml"),
  # Quarto reads YAML 1.2, where only true/false are booleans, not yes/no.
  handlers = list(logical = yaml::verbatim_logical))

  write_page(file.path(out_dir, "index.qmd"),
             front_matter(title, info$description),
             dashboard_body(info, docs, products, notes, links))

  if (length(notes) > 0)
    write_page(file.path(out_dir, "meeting-notes.qmd"),
               front_matter("Meeting notes"),
               unlist(notes))

  for (s in sections) {
    sd <- docs[docs$section == s, ]
    fs::dir_create(file.path(out_dir, s))
    write_page(file.path(out_dir, s, "index.qmd"),
               front_matter(site_section_labels[[s]]),
               section_body(sd))
    for (stem in unique(sd$stem))
      write_page(file.path(out_dir, s, paste0(doc_page(stem), ".qmd")),
                 front_matter(latest(sd[sd$stem == stem, ])$title, site_section_labels[[s]], layout = "full"),
                 document_body(sd[sd$stem == stem, ],
                               links[links$section == s & links$stem == stem, ]))
  }

  if (!render) return(invisible(as.character(fs::path_abs(out_dir))))

  log <- suppressWarnings(system2(quarto, c("render", shQuote(out_dir), "--quiet"),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status") %||% 0L
  site <- file.path(out_dir, "_site")
  if (status != 0 || !file.exists(file.path(site, "index.html")))
    rlang::abort(c(
      glue::glue("Quarto couldn't render the project site in '{out_dir}'. The end of its log:"),
      stats::setNames(utils::tail(log, 15), rep(" ", min(15, length(log))))))

  # Search is off, so its index is clutter in the folder people browse.
  unlink(file.path(site, "search.json"))

  # The setup guide, for people opening the folder on the web.
  pdf <- tryCatch(render_guide_pdf(quarto, title), error = function(e) {
    rlang::warn(c("Couldn't render the setup guide as a PDF, so the site has no instructions file.",
                  conditionMessage(e)))
    NULL
  })
  if (!is.null(pdf)) file.copy(pdf, file.path(site, site_guide_pdf), overwrite = TRUE)

  # The shared files themselves, exactly as shared, next to their pages.
  for (i in seq_len(nrow(docs))) {
    to <- file.path(site, docs$section[i], docs$file[i])
    fs::dir_create(dirname(to))
    file.copy(file.path(root, docs$rel[i]), to, overwrite = TRUE)
  }

  message(glue::glue("Built the site for '{title}' with {length(unique(paste(docs$section, docs$stem)))} document(s): {site}"))
  invisible(as.character(fs::path_abs(site)))
}

# File name of the setup guide in a published site.
site_guide_pdf <- "How to view these reports.pdf"

# Render inst/site/guide.md to PDF with Quarto's Typst engine (no LaTeX), in
# its own folder so it never becomes a page of the website.
render_guide_pdf <- function(quarto, project_title) {
  md <- readLines(site_guide(print = FALSE), encoding = "UTF-8", warn = FALSE)
  h1 <- grep("^# ", md)[1]
  title <- if (is.na(h1)) "How to view these reports" else sub("^# ", "", md[h1])
  if (!is.na(h1)) md <- md[-h1]
  fm <- list(title = title, subtitle = project_title,
             format = list(typst = list(papersize = "us-letter",
                                        margin = list(x = "1in", y = "1in"))))
  d <- tempfile("guide")
  dir.create(d)
  qmd <- file.path(d, "guide.qmd")
  writeLines(enc2utf8(c("---", sub("\n$", "", yaml::as.yaml(fm)), "---", "", md)), qmd, useBytes = TRUE)
  log <- suppressWarnings(system2(quarto, c("render", shQuote(qmd), "--quiet"), stdout = TRUE, stderr = TRUE))
  pdf <- file.path(d, "guide.pdf")
  if (!file.exists(pdf)) stop(paste(utils::tail(log, 5), collapse = "\n"))
  pdf
}

# One row per file in Site/, with its version date, stem and products details.
site_documents <- function(root, products) {
  site <- file.path(root, "Site")
  cols <- c("rel", "section", "file", "date", "stem", "title", "notes", "status", "id", "type")
  empty <- stats::setNames(as.data.frame(matrix(character(), 0, length(cols))), cols)
  files <- site_files(site)
  files <- files[!tolower(basename(files)) %in% c("readme.md", ".gitkeep")]
  if (length(files) == 0) return(empty)

  section <- ifelse(grepl("/", files), sub("/.*", "", files), "")
  dated <- grepl("^\\d{4}-\\d{2}-\\d{2}-", basename(files))
  ok <- section %in% names(site_section_labels) & dirname(files) == section & dated
  if (any(!ok))
    rlang::warn(c(
      "These files in Site/ aren't on the website, because they aren't in a section folder with a dated name. Add files with add_to_site():",
      bullets(files[!ok])))
  files <- files[ok]
  section <- section[ok]
  if (length(files) == 0) return(empty)

  docs <- data.frame(
    rel = file.path("Site", files), section = section, file = basename(files),
    date = substr(basename(files), 1, 10), stem = version_stem(files),
    stringsAsFactors = FALSE)
  m <- match(docs$rel, products$artifact_url)
  docs$title <- ifelse(is.na(m), docs$stem, products$title[m])
  docs$notes <- products$notes[m]
  docs$status <- products$status[m]
  docs$id <- products$id[m]
  docs$type <- products$type[m]
  # One title per document: the latest version's.
  for (key in unique(paste(docs$section, docs$stem))) {
    rows <- which(paste(docs$section, docs$stem) == key)
    docs$title[rows] <- docs$title[rows][order(docs$date[rows], decreasing = TRUE)][1]
  }
  docs[order(docs$date, decreasing = TRUE), ]
}

# Warn when Site/ and products.csv disagree: the site would quietly show a
# file without its title and notes, or leave out a logged version.
check_site_log <- function(root, docs, products) {
  logged <- products$artifact_url[!is.na(products$artifact_url) & startsWith(products$artifact_url, "Site/")]
  missing <- logged[!file.exists(file.path(root, logged))]
  if (length(missing) > 0)
    rlang::warn(c(
      "products.csv points to these files, which aren't in Site/, so they're not on the website:",
      bullets(missing),
      i = "Copy the file there under that name, or correct `artifact_url`."))
  unlogged <- docs$rel[is.na(docs$id)]
  if (length(unlogged) > 0)
    rlang::warn(c(
      "These files in Site/ aren't in products.csv, so the website shows them without a title or notes:",
      bullets(unlogged),
      i = "Log them with add_product(artifact_url = ...), or add files with add_to_site() in future."))
  invisible(NULL)
}

# Meeting notes split into entries at their "## " headings; the text before the
# first heading (the file's title) is dropped.
read_meeting_notes <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  starts <- grep("^## ", lines)
  if (length(starts) == 0) return(list())
  ends <- c(starts[-1] - 1, length(lines))
  lapply(seq_along(starts), function(i) c(lines[starts[i]:ends[i]], ""))
}

# A document's page in its section; "index" is taken by the section's own page.
doc_page <- function(stem) ifelse(stem == "index", "index-document", stem)

# Link target, safe for file names with spaces or parentheses.
href <- function(x) paste0("<", x, ">")

latest <- function(d) d[order(d$date, decreasing = TRUE), ][1, ]

dashboard_body <- function(info, docs, products, notes, links = draft_links(products, docs)) {
  fact <- function(label, value) {
    if (is.null(value) || is.na(value) || !nzchar(value) || grepl("^<.*>$", value)) return(NULL)
    c("::: {.fact}", paste0("[", label, "]{.eyebrow}"), "", md_inline(value), ":::")
  }
  out <- c("::: {.fact-grid}",
           fact("Principal investigator", info$pi), fact("Analyst", info$analyst),
           fact("Department", info$dept), fact("Status", info$status),
           fact("Started", info$start_date), fact("Expected end", info$expected_end),
           ":::", "")

  if (nrow(docs) > 0) {
    keys <- unique(paste(docs$section, docs$stem))
    out <- c(out, "## Recently shared", "", "::: {.card-grid}")
    for (key in utils::head(keys, 6)) {
      d <- docs[paste(docs$section, docs$stem) == key, ]
      l <- latest(d)
      page <- paste0(l$section, "/", doc_page(l$stem), ".qmd")
      versions <- length(unique(d$date))
      out <- c(out, "::: {.d2sb-card}",
               paste0("[", site_section_labels[[l$section]], " \u00b7 ", l$date, "]{.eyebrow}"), "",
               paste0("### ", md_inline(l$title)), "",
               if (!is.na(l$notes)) c(md_inline(l$notes), ""),
               paste0("[Open](", href(page), ")",
                      if (versions > 1) paste0(" \u00b7 ", versions, " versions")),
               ":::")
    }
    out <- c(out, ":::", "")
  }

  if (length(notes) > 0) {
    out <- c(out, "## Latest meeting notes", "",
             sub("^## ", "### ", unlist(utils::head(notes, 2))),
             if (length(notes) > 2) c("[All meeting notes](meeting-notes.qmd)", ""))
  }

  # One row per document: shared ones by their latest version (with a link to
  # a draft edited online, if there is one), then those not shared as files.
  pub_types <- c("manuscript", "abstract", "presentation", "grant")
  pub_sections <- site_sections[pub_types]
  pd <- docs[docs$section %in% pub_sections, ]
  rows <- data.frame(cell = character(), type = character(), status = character(),
                     date = character(), stringsAsFactors = FALSE)
  for (key in unique(paste(pd$section, pd$stem))) {
    d <- pd[paste(pd$section, pd$stem) == key, ]
    l <- latest(d)
    lk <- links[links$section == l$section & links$stem == l$stem, ]
    cell <- paste0("[", md_cell(l$title), "](", href(paste0(l$section, "/", doc_page(l$stem), ".qmd")), ")",
                   if (nrow(lk) > 0) paste0(" \u00b7 [shared draft](", lk$url[nrow(lk)], "){target=\"_blank\"}"))
    rows[nrow(rows) + 1, ] <- c(cell, names(pub_sections)[match(l$section, pub_sections)],
                               gsub("_", " ", coalesce_na(l$status, "completed")), l$date)
  }
  # A planned entry for something since shared (same type and title) is the
  # same document, so it isn't listed again.
  shared_titles <- tolower(paste(names(pub_sections)[match(pd$section, pub_sections)], pd$title))
  others <- products[products$type %in% pub_types & !products$status %in% "canceled" &
                       !products$id %in% c(docs$id, links$id) &
                       !tolower(paste(products$type, products$title)) %in% shared_titles, ]
  for (i in seq_len(nrow(others))) {
    p <- others[i, ]
    rows[nrow(rows) + 1, ] <- c(product_link(p, docs), p$type, gsub("_", " ", p$status),
                               coalesce_na(p$actual_date, p$target_date, ""))
  }
  if (nrow(rows) > 0) {
    rows <- rows[order(rows$date, decreasing = TRUE), ]
    out <- c(out, "## Papers, abstracts and grants", "",
             "| Title | Type | Status | Date |", "|:--|:--|:--|:--|",
             paste0("| ", rows$cell, " | ", rows$type, " | ", rows$status, " | ", nowrap(rows$date), " |"), "")
  }

  upcoming <- products[products$status %in% c("planned", "in_progress") &
                         !products$type %in% c("manuscript", "abstract", "presentation", "grant", "dataset"), ]
  if (nrow(upcoming) > 0) {
    upcoming <- upcoming[order(coalesce_na(upcoming$target_date, "9999")), ]
    out <- c(out, "## Coming up", "", "| Deliverable | Status | Target date |", "|:--|:--|:--|")
    for (i in seq_len(nrow(upcoming))) {
      p <- upcoming[i, ]
      out <- c(out, paste0("| ", md_cell(p$title), " | ", gsub("_", " ", p$status), " | ",
                           nowrap(coalesce_na(p$target_date, "")), " |"))
    }
    out <- c(out, "")
  }
  out
}

section_body <- function(sd) {
  out <- c("| Document | Latest version | Versions | Notes |", "|:--|:--|:--|:--|")
  for (stem in unique(sd$stem)) {
    d <- sd[sd$stem == stem, ]
    l <- latest(d)
    out <- c(out, paste0("| [", md_cell(l$title), "](", href(paste0(doc_page(stem), ".qmd")), ") | [", l$date, "](", href(l$file), ") | ",
                         length(unique(d$date)), " | ", md_cell(coalesce_na(l$notes, "")), " |"))
  }
  out
}

# A document's page: the latest version shown in the page (HTML and PDF), so
# the site's navigation stays around it, then every version.
document_body <- function(d, links = NULL) {
  d <- d[order(d$date, d$file, decreasing = TRUE), ]
  l <- d[d$date == d$date[1], ]
  framed <- l$file[tolower(tools::file_ext(l$file)) %in% c("html", "htm", "pdf")][1]
  buttons <- paste0("[", if (is.na(framed)) "Open" else "Open in a new tab", " (",
                    toupper(tools::file_ext(l$file)), ")](", href(l$file),
                    "){.btn .btn-gold .me-2 target=\"_blank\"}")
  if (!is.null(links) && nrow(links) > 0)
    buttons <- c(buttons, paste0("[Edit the shared draft online](", links$url[nrow(links)],
                                 "){.btn .btn-outline-ink .me-2 target=\"_blank\"}"))
  out <- c(paste0("[Latest version \u00b7 shared ", l$date[1], "]{.eyebrow}"), "",
           if (!is.na(l$notes[1])) c(md_inline(l$notes[1]), ""),
           paste(buttons, collapse = " "), "",
           if (!is.na(framed)) c("```{=html}",
                                 paste0("<iframe class=\"doc-frame\" src=\"", html_attr(framed),
                                        "\" title=\"", html_attr(l$title[1]), "\"></iframe>"),
                                 "```", ""),
           "## All versions", "", "| Shared | File | Notes |", "|:--|:--|:--|")
  for (i in seq_len(nrow(d))) {
    out <- c(out, paste0("| ", d$date[i], if (d$date[i] == d$date[1]) " (latest)" else "",
                         " | [Open (", toupper(tools::file_ext(d$file[i])), ")](", href(d$file[i]), ") | ",
                         md_cell(coalesce_na(d$notes[i], "")), " |"))
  }
  out
}

# Links to drafts edited online, each attached (through `related`) to the
# shared document it is a draft of. Unattached links stay ordinary products.
draft_links <- function(products, docs) {
  out <- data.frame(id = character(), title = character(), url = character(),
                    section = character(), stem = character(), stringsAsFactors = FALSE)
  is_link <- !is.na(products$artifact_url) & grepl("^https?://", products$artifact_url)
  l <- products[is_link, ]
  for (i in seq_len(nrow(l))) {
    rel <- trimws(strsplit(coalesce_na(l$related[i], ""), ";", fixed = TRUE)[[1]])
    hit <- docs[!is.na(docs$id) & docs$id %in% rel, ]
    if (nrow(hit) > 0)
      out[nrow(out) + 1, ] <- c(l$id[i], l$title[i], l$artifact_url[i], hit$section[1], hit$stem[1])
  }
  out
}

# A publication's title, linked to its page if it was shared on the site, or
# to its artifact_url if that is a web link.
product_link <- function(p, docs) {
  t <- md_cell(p$title)
  hit <- docs[!is.na(docs$id) & docs$id == p$id, ]
  if (nrow(hit) > 0) return(paste0("[", t, "](", href(paste0(hit$section[1], "/", doc_page(hit$stem[1]), ".qmd")), ")"))
  if (!is.na(p$artifact_url) && grepl("^https?://", p$artifact_url)) {
    # A live, co-edited draft (e.g. a manuscript in SharePoint) opens in the browser.
    shared <- grepl("sharepoint\\.com|1drv\\.ms|onedrive", p$artifact_url, ignore.case = TRUE)
    return(paste0("[", t, "](", p$artifact_url, "){target=\"_blank\"}",
                  if (shared) " (shared draft, opens in your browser)"))
  }
  t
}

front_matter <- function(title, subtitle = NULL, layout = NULL) {
  fm <- list(title = title)
  if (!is.null(subtitle) && !is.na(subtitle) && nzchar(subtitle)) fm$subtitle <- subtitle
  if (!is.null(layout)) fm$`page-layout` <- layout
  c("---", sub("\n$", "", yaml::as.yaml(fm)), "---", "")
}

write_page <- function(path, front, body) {
  writeLines(enc2utf8(c(front, body)), path, useBytes = TRUE)
}

# Dates in tables shouldn't break at their hyphens.
nowrap <- function(x) ifelse(nzchar(x), paste0("[", x, "]{.text-nowrap}"), x)

html_attr <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(" ", "%20", x, fixed = TRUE)
}
md_inline <- function(x) gsub("[\r\n]+", " ", x)
md_cell <- function(x) gsub("|", "\\|", md_inline(x), fixed = TRUE)

# First non-missing value, elementwise.
coalesce_na <- function(...) {
  args <- list(...)
  out <- args[[1]]
  for (a in args[-1]) out[is.na(out)] <- rep_len(a, length(out))[is.na(out)]
  out
}

find_quarto <- function() {
  candidates <- c(
    Sys.getenv("QUARTO_PATH"), Sys.which("quarto"),
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "C:/Program Files/Quarto/bin/quarto.exe")
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit) == 0)
    rlang::abort("Couldn't find Quarto. Install it from https://quarto.org, or set QUARTO_PATH to the quarto program.")
  unname(hit[[1]])
}

#' Preview a built site in your web browser
#'
#' Opens the site's `index.html` from disk in your default browser, the way
#' collaborators will see it from their synced folder. Use this rather than
#' `utils::browseURL()`: inside RStudio that shows the page in the Viewer
#' through a local web server, where Quarto rewrites the menu links to folder
#' addresses the server can't open, so the menus seem broken.
#'
#' @param site A built site, as returned by [build_project_site()].
#' @return The path opened, invisibly.
#' @examples
#' \dontrun{
#' preview_site(build_project_site())
#' }
#' @export
preview_site <- function(site) {
  index <- file.path(site, "index.html")
  if (!file.exists(index))
    rlang::abort(glue::glue("No index.html in '{site}'. Pass the folder build_project_site() returned."))
  index <- normalizePath(index)
  if (.Platform$OS.type == "windows") {
    get("shell.exec", envir = baseenv())(index)
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", shQuote(index))
  } else {
    system2("xdg-open", shQuote(index))
  }
  invisible(index)
}
