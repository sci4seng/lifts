# sci4seng/lifts

R + Rmd staging area for kaiaulu PRs.

Per SME's PR hygiene rule: **no `*.html`, no `data/`, no studies.**
Each kaiaulu PR cherry-picks (conf + R + Rmd) from this repo.

## Layout

```
vignettes/         one .Rmd per (model, project) lift
R/                 functions.R + per-model helpers
scripts/           Python helpers (SZZ, pattern4 parse, archpat lite)
conf/              per-project YAML (paths to git_repo, mbox, etc.)
tools.yml.example  template for tool paths — user edits, not committed
kaiaulu_notes/     parser schema audit + known-bugs notes
```

## Run a vignette

```bash
cp tools.yml.example tools.yml
# edit tools.yml to point at YOUR perceval / pattern4 / etc. paths

Rscript -e 'rmarkdown::render("vignettes/lift_brooks.Rmd")'
# produces lift_brooks.html alongside the .Rmd
# IMPORTANT: do NOT commit the .html — push it to sci4seng/core/docs/lifts/
# (or to Drive) per the no-HTML-in-PR rule
```

## Submit to kaiaulu

For each new lift:
1. Confirm the Rmd renders cleanly with current `tools.yml`
2. Copy `conf/<project>.yml`, `R/<helper>.R`, `vignettes/<topic>.Rmd`
   into a kaiaulu fork: `cp ... ../kaiaulu/{conf,R,vignettes}/`
3. Open PR with body linking the Pages-hosted HTML for review:
   `https://sci4seng.github.io/core/lifts/<name>.html`

## Sibling repos

- [sci4seng/core](https://github.com/sci4seng/core) — framework + site
- [sci4seng/data](https://github.com/sci4seng/data) — drop-zone + manifest
