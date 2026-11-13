# Analysis: Dog Poop Complaints in Manhattan

A report from BetaNYC's Research and Data Assistance Request (RADAR) service on dog poop complaints to 311 in Manhattan.

The report: https://betanyc.github.io/mbpo-dog-poop/

It is a Quarto website. `quarto render` writes it to `docs/`, which GitHub Pages publishes from the `main` branch.

## What's changed

[WHATS-CHANGED.md](WHATS-CHANGED.md) lists the changes a reader would notice, newest first, including corrections. The site shows it as a page: https://betanyc.github.io/mbpo-dog-poop/whats-changed.html. It follows BetaNYC's What's changed standard; `scripts/whats_changed.py` is a copy from the BetaNYC workspace, so never edit it here.

- `_quarto.yml` rebuilds `whats-changed.qmd` from `WHATS-CHANGED.md` before every render (`pre-render`). To rebuild only that page, without re-running the analysis: `quarto render whats-changed.qmd`. A full `quarto render` re-runs the analysis (`freeze: false`).
- The footer link to the page reaches the report itself only at its next full render. Until then, only the What's changed page carries it.
- The entry for the page is dated the day it was written. If this change merges later, the last commit before merge re-dates that entry to the merge day and rebuilds the page.
- Pull requests run `.github/workflows/whats-changed.yml`, which checks the file and that `whats-changed.qmd` is current.
