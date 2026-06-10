# ISSS608 Group 14 — Visual Analytics Project (VAST Challenge 2026 MC1)

Project website built with Quarto, deployed to Vercel.

## Structure
- `_quarto.yml` — site config (navbar, theme). Edit the Shiny App and GitHub URLs here.
- `index.qmd` — home page
- `Proposal.qmd` — project proposal
- `Meeting-Minutes.qmd` — meeting records
- `methodology.qmd`, `findings.qmd`, `poster.qmd`, `user-guide.qmd`, `team.qmd` — placeholder pages to fill during the project
- `images/` — put figures here (e.g. the hand-drawn prototype sketch: images/prototype_sketch.jpg)
- `data/` — put MC1_final_00.json and data_prep.R here

## Build
Run `quarto render` in this folder. Output goes to `docs/`.

## Deploy
Connect this repo's `docs/` folder to a new Vercel project.

## TODO before first publish
- Add images/prototype_sketch.svg is included; swap for a hand-drawn photo if you prefer
- Update Shiny App + GitHub URLs in _quarto.yml
- Run `quarto render` and check all pages, the schedule chart, and the Mermaid diagrams
