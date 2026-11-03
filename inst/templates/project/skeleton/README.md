# <PROJECT TITLE>

Short description (1–2 sentences).

## Quick links
- **GitLab repo:** <GITLAB_URL>
- **Project website (SharePoint, view-only for PI):** <SP_SITE_URL>

## Directory layout

Your main repo should look like this: 
```
Admin/              # SOW, approvals
  products.csv        # Deliverables log; add rows with D2SBtools::add_product()
  project.yml         # Project metadata (status, dates, effort, funding)
  grants.yml          # (optional) Grants that support or result from this project
Background/         # Papers, notes from PI
Code/               # Reusable scripts & orchestration
Site/               # Exact copies as shared with PI, by type; add with D2SBtools::add_to_site()
Reports/            # Rmd/Qmd notebooks (thin; call functions in Code/). 
meeting_notes.md    # Shared meeting notes, newest first (on the project website)
```

Your SharePoint (or `data_path_from_sharepoint`) should look like this:

```
DataRaw/            # Original source files from PI (not in Git)
DataProcessed/      # Derived/cleaned data (not in Git)
Site/               # Project website, built from the repo (publish_site()); PI syncs this
```

Shared meeting notes live in `meeting_notes.md`, which appears on the project website.
