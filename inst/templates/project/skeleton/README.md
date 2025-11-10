# <PROJECT TITLE>

Short description (1–2 sentences).

## Quick links
- **GitLab repo:** <GITLAB_URL>
- **Dissemination (view-only for PI):** <SP_DISSEMINATION_URL>

## Directory layout

Your main repo should look like this: 
```
Admin/              # SOW, approvals
  products.xlsx       # Deliverables log
  project.yml         # Project metadata (status, dates, effort, funding)
  grants.yml          # (optional) Grants that support or result from this project
Background/         # Papers, notes from PI
Code/               # Reusable scripts & orchestration
Dissemination/      # Exact copies as sent to PI (PDFs, ZIPs)
Reports/            # Rmd/Qmd notebooks (thin; call functions in Code/). 
```

Your SharePoint (or `data_path_from_sharepoint`) should look like this:

```
DataRaw/            # Original source files from PI (not in Git)
DataProcessed/      # Derived/cleaned data (not in Git)
Dissemination/      # Exact copies as sent to PI (PDFs, HTMLs, DOCX; also in Git)
```

# Meeting notes

In this section, keep a reverse-chronological list of meeting note summaries (3-6 bullets per meeting)

## YYYY-MM-DD (most recent meeting)

- Went over revised SOW, plan looked acceptable & regular meetings set up.
- Discussed planned manuscript and abstracts; see new `products.xlsx` new entries with `planned` status
- PI will send additional information required by end of week

## ...

## YYYY-MM-DD (least recent meeting)

- Met to go over study details, potential partnership
- Sent PI to statGPT to revise hypotheses, develop initial SOW draft
