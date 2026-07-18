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

## Re-run to 2024: the COVID-year conundrum (corrected engine)

Corrected Bayes factors, calibration 1991-2010, observation window to 2024.
BF-maximising slowdown (% below 1991-2010 mean improvement):

| Population (UK) | Full series incl. 2020-21 | Excluding 2020-21 | Original 2011-2018 (pre-COVID) |
|---|---|---|---|
| UK female   | 65% | 30% | 61% |
| UK male     | 74% | 39% | 61% |
| Wales male  | 89% | 56% | 83% |

- The 2020 shock is huge (UK e0 change 2020: -0.88 F / -1.20 M years) and
  single-handedly inflates the full-series slowdown estimate.
- **Three-way tension**: the "true" underlying slowdown could be read as
  ~61% (pre-COVID trend, original paper), ~65-89% (full series, but
  COVID-dominated), or ~30-56% (COVID-excluded, but pulled UP by genuine
  2022-2024 recovery: +0.33/+0.38 in 2024). No single number is clean.
- 2022-2024 show real recovery, which the original 2019-vintage paper could
  not see. This materially weakens a naive "stalling has only worsened"
  narrative and is the single most important substantive update.
- Methodological point: a Normal-iid likelihood treats the 2020 shock as an
  ordinary draw, which it plainly is not. Options for the manuscript: (a)
  report all three windows transparently; (b) add a level-shift/outlier
  model for 2020-21; (c) restrict the headline to pre-2020 and treat
  2020-24 as out-of-sample validation of the earlier forecast. (c) is
  cleanest and reframes the paper around forecast validation.

## Re-run finding: the 2019 recovery year (important nuance)

The pre-pandemic headline depends sharply on where you stop:
- 2011-2018 window: UK 61%/61% slowdown (reproduces the original paper exactly).
- 2011-2019 window: UK 49%/56%. The single extra year 2019 was a strong
  recovery (UK e0 +0.29 F / +0.24 M, both above the 1991-2010 mean), which
  pulls the estimated slowdown down by ~10 points.
Implication: the original paper's headline was, unknowingly, sensitive to
ending in 2018. 2019 already hinted at recovery before COVID. This
strengthens the case for the forecast-validation framing over a single
point estimate, and should be stated plainly in the manuscript.

## Chapter 05 finding: newest rounds are NOT the pessimistic ones
Contrary to first impression from the fan figure, the 2020/2022/2024 ONS
rounds' implied mean annual gains sit close to (UK female slightly above)
the pre-pandemic BF benchmark. The dramatic downward revision happened
2012-2018; the newest rounds have largely stabilised. The fan figure's
"sharp pessimism" is about the LEVEL (post-COVID starting point) more than
the assumed rate of future gain.

## Detection framework (the project's original purpose, revived)

User's original inspiration: the "1-in-1000-year storm recurring within a
lifetime" idea - how many anomalous years to reject "the longevity DGP is
unchanged"? This is the formal complement to Hiam et al. 2023 (BMB)
"Slowing down or returning to normal?", which poses the same question
qualitatively. R/detection.R implements it. Key findings:

- **Retrospective early warning (first year LR monitor fires, p<0.05):**
  UK & England MALES fire in **2015** - using only 2011-2015 data, four
  years before the original analysis. p-trajectory: crosses 0.05 in 2015,
  0.003 by 2018, ~0 once COVID hits. This vindicates the "how few
  observations" premise - but only for the high-signal male series.
- Females NEVER fire pre-COVID (all p>0.08 through 2019); their apparent
  2020 "detection" is the pandemic shock, not the trend. Honest framing:
  the trend-based slowdown was detectable early for males, not for females.
  NI barely fires at all. Same male>female split as the LR test, by an
  independent route.
- **Years-to-detect scales as 1/((1-k)^2):** a full stall (k=0) is
  detectable in 1-3 yrs (UK), a half-slowdown in 4-12, a quarter-slowdown
  in 14-47. So the answer to "how few years" depends sharply on severity
  and signal-to-noise (mu0/sd0, much better for males).
- **Negative autocorrelation speeds detection:** rho ~ -0.5 to -0.59 for
  annual e0 change; the AR(1) correction roughly halves years-to-detect
  (UK female half-slowdown 12->3 yrs). The same negative autocorrelation
  White (2002) used to justify linear extrapolation also suppresses the
  variance of multi-year means, making regime-change detection faster.
- **Storm surprise:** a single <=0 male year is a 1-in-22 event under the
  null; observing 4 of 14 post-2010 (expected 0.6) is a ~1-in-300 surprise.
  Female 4 of 14 (expected 2.3) is ~1-in-5, unremarkable.

Decision (user): reframe the paper AROUND this detection framework (not
"monitoring index" - e_x, de_x/dt, SMRs are already monitoring indexes, so
that framing isn't distinctive). Position explicitly as the quantitative
complement to Hiam et al.

## International comparison survives the HMD refresh
On fresh common-window (2011-2019) HMD data, UK ranks 4th SLOWEST of 20
high-income countries for mean annual e0 gain (both sexes); only USA, NZ,
Canada slower. The "UK severe in international context" claim holds; the
original "more severe than all except USA" becomes "among the slowest 3-4".
