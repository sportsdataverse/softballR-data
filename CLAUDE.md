# CLAUDE.md — softballR-data

Data repo for the `softballR` R package: pre-built season `.RDS` files plus the
R scripts that refresh them. The package's `load_*` functions read these files
directly over raw GitHub URLs. Sibling package repo: `softballR` (see its
CLAUDE.md). Part of the SportsDataverse ecosystem.

## Layout

- `data/*.RDS` — the published artifacts, committed to git. Naming:
  `{div}_hitting_box_scores_{year}.RDS`, `{div}_pitching_box_scores_{year}.RDS`,
  `d{1,2,3}_fielding_box_scores_{year}.RDS`, `d{1,2,3}_ncaa_pbp_{year}.RDS`,
  `ncaa_scoreboard_{year}.RDS` / `ncaa_scoreboard_{D2,D3}_{year}.RDS`,
  `espn_scoreboard_*`, `naia_scoreboard_*`, `ncaa_rosters_2021_2023.RDS`,
  `ncaa_team_info.RDS`.
- Root `*.R` — build/refresh scripts (NOT a package; no `R/` dir, DESCRIPTION,
  or NAMESPACE):
  - `get_current_season_ncaa_scoreboard.R` — scrapes `stats.ncaa.org`
    livestream scoreboards for D1/D2/D3, writes `ncaa_scoreboard*_{year}.RDS`.
  - `get_current_season_ncaa_pbp.R` — appends new games' PBP, writes
    `d{1,2,3}_ncaa_pbp_{year}.RDS`.
  - `get_current_season_ncaa_playerbox.R` — hitting + pitching box scores,
    writes `d{1,2,3}_{hitting,pitching}_box_scores_{year}.RDS`.
  - `get_past_espn_scoreboards.R`, `get_current_season_espn_scoreboard.R` — ESPN.
  - `get_ncaa_team_info.R`, `get_team_info.R` — team metadata.

## Inputs / Outputs

- **Source:** NCAA `stats.ncaa.org` (HTML, `rvest`) + ESPN; per-division
  (D1/D2/D3) season scrapes.
- **Publish target:** the `.RDS` files are committed directly to `data/` on the
  `main` branch — there is **no release/tag mechanism**. `softballR`'s `load_*`
  functions read them via raw GitHub blob URLs
  (`github.com/{tmking2002,sportsdataverse}/softballR-data/.../data/<file>.RDS?raw=true`).
  Both org paths appear across the build scripts and the package loaders.
- **Refresh is incremental:** scripts pull the existing `.RDS` from GitHub, find
  the most-recent game already present, scrape only newer games, `rbind`, and
  re-`saveRDS`.
- Committing data to git is the intended SportsDataverse pattern — do not warn
  about repo size.

## CI

Three GitHub Actions cron workflows (`.github/workflows/`), all on
`macOS-latest` with `r-lib/actions/setup-r`, scheduled daily and also
`workflow_dispatch`:

| Workflow | Cron (UTC) | Runs | Auto-commit message |
|---|---|---|---|
| `update_ncaa_scoreboard.yml` | `0 5 * * *` | `get_current_season_ncaa_scoreboard.R` | `Update NCAA Scoreboard` |
| `update_ncaa_pbp.yml` | `0 5 * * *` | `get_current_season_ncaa_pbp.R` | `Update NCAA PBP` |
| `update_ncaa_playerbox.yml` | `0 6 * * *` | `get_current_season_ncaa_playerbox.R` | `Update NCAA Player Box` |

Each job runs the script, then `git add -A` + commit (fixed message,
`|| echo "No changes to commit."`) + `git push origin main`. The commit
message is fixed per workflow; there is no commit-message-driven CI trigger
(triggers are cron + manual dispatch only). `ACCESS_TOKEN` is passed via repo
secret.

## Conventions

- Build scripts are standalone `Rscript`-run files: they `install.packages()`
  their deps inline (`tidyverse`/`rvest`/`janitor`/`glue`/`rio`/`anytime`/...)
  then scrape. The **current season is hardcoded** in each script
  (e.g. `season = 2025`, `mutate(season = 2025)`) — bump it at season rollover.
- Output format is `.RDS` (base R `saveRDS`); read back with `readRDS` /
  `rio::import`.
- Reshape vs. raw: scoreboard scripts emit tidy game-level frames; box-score
  scripts clean column names (`janitor`) and select a fixed `*_cols` vector.
- Never add AI co-author trailers to commits or PRs.

## Reference

- `softballR/R/load_*.R` — the consumer URLs + season guards for every `.RDS`.
- `.github/workflows/*.yml` — schedule, script, and commit message per dataset.
