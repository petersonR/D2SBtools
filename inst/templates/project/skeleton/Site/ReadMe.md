# Site

Everything shared with the PI, exactly as shared, filed by type and dated
(`reports/2026-11-02-eda.html`). Add files with `D2SBtools::add_to_site()`,
which also logs them in `Admin/products.csv`; don't copy them here by hand.

`D2SBtools::build_project_site()` turns this folder, `Admin/project.yml`,
`Admin/products.csv` and `meeting_notes.md` into the project website, and
`D2SBtools::publish_site()` puts it in the project's `Site/` folder on
SharePoint, which the PI and study team sync and browse.

Never put data here: everyone who syncs the site gets a copy of every file.
