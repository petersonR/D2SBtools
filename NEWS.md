# D2SBtools 0.6.0

Project websites: everything shared with a PI, browsable from a synced
SharePoint folder instead of emailed HTML files.

* New `add_to_site()` shares a file (report, abstract, manuscript draft,
  poster) in one step: it files an exact, dated copy in the project's `Site/`
  folder (`Site/reports/2026-11-02-eda.html`) and logs it in
  `Admin/products.csv`. A later file with the same name is a new version,
  logged as its own row and linked to the previous one through `related`.
  Datasets and data files are refused.
* Manuscripts are shared like any deliverable: each version (e.g. a docx with
  tracked changes) goes on the site with `add_to_site()`. `as` keeps files
  that are renamed between versions together as one document:
  `add_to_site("Manuscript_v3_JD.docx", "manuscript", as = "manuscript")`.
* A draft the team edits online is linked, not copied:
  `add_to_site("<SharePoint link>", "manuscript", as = "manuscript")` logs the
  link, related to that document, and its page gets an "Edit the shared draft
  online" button. The dashboard's papers table lists each document once,
  with its draft link.
* The report template uses the D2SB theme: `create_report()` puts `d2sb.scss`
  next to the report.
* New `build_project_site()` builds a D2SB-branded website from
  `Admin/project.yml`, `Site/`, `Admin/products.csv` and the new shared
  `meeting_notes.md`: a dashboard (overview, recently shared, latest meeting
  notes, papers and abstracts, upcoming deliverables), a page per document
  with its latest version shown inside the page (HTML and PDF) and earlier
  ones listed, and a page per section.
* New `preview_site()` opens a built site in your browser from disk, as
  collaborators see it. (RStudio's Viewer serves pages over a local web
  server, where the site's menu links don't work.)
* Every site carries the collaborator setup guide (syncing the folder,
  opening the site, getting updates, rules) as `How to view these
  reports.pdf`, for people who open the folder link on the web. `site_guide()`
  shows its source; edit it in `inst/site/guide.md`.
* New `site_email()` copies a short invitation to the clipboard as formatted
  text: the folder link and a pointer to the PDF. `publish_site()` points to
  it, and on a folder's first publish reminds you to pin the PDF to the top.
* New `publish_site()` mirrors a rendered site into a SharePoint folder,
  copying only changed files and removing pages no longer in the site. It
  keeps a manifest of what it published and stops, listing the files, if
  anyone added or changed files there since, rather than deleting their work.
  It refuses to write into a folder it didn't create, or to publish data
  files unless `allow_data_files = TRUE`.
* The project template replaces `Dissemination/` with `Site/`, adds a shared
  `meeting_notes.md` (the README no longer holds meeting notes), and adds a
  `description` to `project.yml` for the website. `move_to_dissemination()`
  still works for existing projects.

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
