# Refactor log: findings, conundra, and candidate spin-off materials

Running log kept during the 2026 refactor (branch `updated/refactor`;
pre-refactor state preserved on `old/pre-refactor`). Newest entries at the
bottom of each section.

## Bugs found

1. **Operator-precedence error in `get_ll`**
   (`scripts/load_packages_and_functions.R:13`): the normal log-likelihood
   term `1 / (2 * sig_sq)` was written `(1 / 2 * sig_sq)`, which R parses as
   `0.5 * sig_sq`. Because sig^2 for annual e0 changes is ~0.01-0.04, all
   Bayes factors were attenuated toward 1 by orders of magnitude (the
   commented-out plot axis limits `c(0.999, 1.008)` in
   `markdown/bayes_factor_ons_e0.Rmd` are the visible symptom). The
   BF-maximising slowdown percentage is unaffected (monotone in the sum of
   squares), but all reported BF magnitudes are wrong. A dedicated audit
   report quantifying the effect on the 2011-2018 findings is planned
   (`audit/`).

2. **Weeks-per-year constant**: `markdown/bayes_factor_ons_e0.Rmd` converts
   annual e0 change from years to weeks by multiplying by 52.25; a year is
   365.25/7 = 52.18 weeks. Effect is cosmetic (axis values ~0.1% high) but
   worth correcting in the refactor.

## Data findings / conundra

1. **ONS no longer publishes single-year life tables beyond 2020** (the
   dataset last covers 1980-2020), so the actual e0 series is extended
   2019-2024 using the historic estimates embedded in the 2024-based NPP
   expectation-of-life files. QA over the 37 overlap years (1982-2018):
   mean absolute agreement within ~0.01 years, max ~0.05-0.08 for most
   nations, but **Wales disagrees by up to 0.17 years** (and England male
   0.08). Likely causes: population-estimate revisions (2021 census
   rebasing) feeding into re-estimated historic mortality rates. Conundrum:
   which source is "the" actual for pre-2019 years? Current choice: keep
   the original single-year life table values to 2018 (as published/used in
   the paper), NPP-embedded estimates from 2019.

2. **The 2024-based NPP files' final "historic" year (2024) is the
   projection base year** (mid-year), so the 2024 actual is part-estimated.
   Flag in any figure caption.

3. **A 2024-based NPP round exists** (beyond the 2020-based interim and
   2022-based rounds assumed when planning the update), giving three new
   projection rounds since the paper was drafted.

## Candidate spin-off materials

1. **Projections-vs-actual fan figure**
   (`bayes_paper/figures/M02_ons_lt_projns_updated.png`, script
   `scripts/update_ons_projections_figure.R`): self-contained,
   blog/preprint-ready illustration of five decades of ONS forecast error -
   under-projection to ~2010, over-projection 2010-2019, COVID shock, and
   the sharp pessimism of the 2020-2024 rounds. Could stand alone as a
   short piece ("Fifty years of forecasting UK life expectancy").

## Phase plan (larger arc)

1. ~~Data update~~ (this commit)
2. Rmd -> Quarto restructure with references.bib
3. Bug fixes + R/ function refactor (+ audit report on get_ll fix impact)
4. Re-run analyses to 2024 (explicit COVID-year treatments)
5. Manuscript update (Quarto manuscript from the chunked docx drafts)
6. Critique of key arguments; adversarial peer review pass
7. Preprint (OSF or similar) + journal targeting

## Audit findings (get_ll fix, original 2011-2018 window)

- Corrected BFs at the maximising scenario: UK males ~72, England males
  ~105 ("strong" on Kass-Raftery), UK/England/Scotland/Wales females ~2-4
  ("positive"), Northern Ireland males ~1.1 (equivocal). Legacy reported
  values were all 1.00-1.02.
- Maximising slowdown percentages reproduce manuscript Table 4 exactly
  (61% UK both sexes, etc.) - provenance of the buggy numbers confirmed.
- **Conundrum**: corrected evidence is *stronger for males than females*,
  inverting the draft's emphasis (which suggested ONS remained too
  optimistic mainly for females). The female signal is weaker because
  female annual e0 changes are noisier relative to their (smaller) mean
  improvement. Re-examine when updating the manuscript.
- Manuscript prose inconsistency: calibration window described both as
  "1990 to 2010" and "1991-2010"; code uses changes for 1991-2010.
