# Adversarial peer review: synthesis and revision plan

Three independent adversarial referees reviewed `manuscript/manuscript.qmd`
(the substantively revised manuscript, commit `096dfef`): a
statistics/methods referee, a demography/subject referee, and a
reproducibility referee. This document synthesises their reports, records
where they converge, and proposes a prioritised revision plan. It is an
internal working document, not part of the manuscript.

## Overall verdict

**Major revision** (two referees said so explicitly; the third — reproducibility
— found every number correct and nothing fabricated). The referees agree on a
single meta-conclusion:

> The descriptive story is sound and honestly told, but the **statistical
> apparatus is oversold**. What is presented as a bespoke "continually-updatable
> Bayes-factor method" is arithmetically a two-sample comparison of means, and
> several headline claims (out-of-sample *validation*, "strong evidence",
> relative international severity, ONS rounds "stabilising near the benchmark")
> claim more than the analysis delivers.

Crucially, none of this touches the reproducibility of the numbers (all verified
exactly) or the core descriptive finding (a real post-2010 slowdown, roughly
halving the earlier rate, with a partial post-2020 rebound). The fixes are about
**framing and rigour**, not about the arithmetic being wrong.

## Where the referees converge (highest confidence)

1. **It is not a Bayes factor.** [stats MAJOR, repro confirmed]
   `calc_bayes_factors` maximises a likelihood ratio over 101 scenarios with no
   prior and no integration. Both the statistics and reproducibility referees
   independently derived the closed form: the BF-maximising scenario is
   `perc* = mean(post-period change) / mean(calibration change)`, so
   **slowdown% = 1 − (post-period mean / calibration mean)** (verified to
   floating-point precision across all 30 cells). The whole method reduces to a
   rescaled two-sample mean comparison; the log-BF depends on the post-period
   data only through its mean. Reading `max_p L(pμ̂)/L(μ̂)` on the Kass–Raftery
   scale conflates a profile likelihood ratio with a Bayes factor and inflates
   the apparent evidence (a genuine flat-prior BF is 1.4× smaller for females,
   ~2× for males: UK male 73→37).

2. **The out-of-sample "validation" is informal, post-hoc, and does not
   validate.** [demography MAJOR, stats MAJOR]
   - The pre-2020 window was chosen *after* seeing that 2019 recovered and 2020
     crashed (NOTES.md is explicit that the reframing was "forced by" these
     observations) — so it is not prospective.
   - By the identity above, "pre-2020 implied gain" *is* the 2011–2019 sample
     mean; "validation" then just checks whether the 2022–2024 mean is close to
     the 2011–2019 mean — a persistence check, not a forecast test (no
     predictive distribution, no scoring rule, no interval).
   - Taken at face value it *fails*: the pre-2020 implied gains (≈0.098 F /
     0.121 M) are ~2.5× below observed 2022–2024 (0.247 / 0.307), and far above
     observed 2020–2024 (0.010 / −0.020). The point forecast matches the 5-year
     mean only because the shock and rebound cancel.

3. **Multiplicity is unhandled and the "family" is not independent.** [stats
   MAJOR] Recast as likelihood-ratio tests, only **2 of 10** pre-2020 cells reach
   p<0.05 (UK male p≈0.005, England male p≈0.003); all six female cells are
   non-significant (p 0.13–0.52); NI is null. The two "significant" cells are the
   **same signal** — England is 84% of the UK and cor(UK-male, England-male
   annual change) = 0.989. So "stronger evidence for males" rests on essentially
   **one** independent observation, not a cross-population pattern.

## Additional major findings (single-referee, high value)

4. **Period vs cohort / harvesting.** [demography MAJOR] Period e₀ mechanically
   over-states a mortality shock and mechanically rebounds as the depleted-frail
   cohort produces lower period rates. The paper's central new claim — the
   2022–2024 recovery weakens the "stalling has only worsened" narrative — is at
   risk of being a harvesting artefact in a synthetic index, not a return of
   underlying improvement. The competing harvesting explanation is not even
   raised. Needs a cohort/decomposition/lifespan-variation lens, or all claims
   restricted explicitly to "period e₀ as a demographic index".

5. **The international comparison uses stale, ragged data.** [demography MAJOR]
   `data/e0_hmd.csv` ends 2016 for most named comparators (UK, Germany,
   Netherlands, Spain, Canada), 2017 USA, 2013–14 for others — while the UK is
   measured to 2024 elsewhere in the paper. The "2010s decade average" ordering
   the nations is a truncated 2011–2016 window with country-specific endpoints.
   The retained Discussion sentence "the UK's experience remains severe relative
   to other high-income nations" is **not supported by the repo's own data**.
   This is a concrete data defect, not just framing. Fix: re-pull HMD to a
   current vintage on a common window; until then, cut or downgrade the section.

   **Fix-path located.** A sibling repo, `../hmd-population-discrepancies`
   (updated March 2026), has a *working, current* HMD download pipeline
   (`scripts/01_download_hmd.R` + README auth flow) using HMD's new
   session-cookie authentication, and has already pulled data to 2022–2024. Its
   committed extract covers only 5 countries (ESP, SWE, GBRTENW, FRATNP, JPN) and
   only deaths/population, so it does not directly supply the comparator
   period-e₀ series this paper needs — but the pipeline can be pointed at the
   `E0per.txt` item for the full comparator list once a session cookie is
   refreshed. **Blocker: HMD session auth** — the cookie was transient (`/tmp`)
   and is gone; a fresh pull needs the author's mortality.org credentials (the
   one step that cannot be automated here). Once re-authenticated, regenerating
   `data/e0_hmd.csv` to a current common vintage and re-rendering `analysis/01`
   closes this finding.

6. **The ONS "implied mean annual gain" is horizon-mismatched.** [demography
   MAJOR] Each ONS round is collapsed to the mean annual e₀ change over its
   *entire ~50-year horizon*, then compared to a *near-term* (2011–2019)
   benchmark. Because annual e₀ gains decline over a projection horizon
   (rectangularisation) even at constant assumed mortality-improvement rates, the
   50-year average mechanically sits below the near-term rate — so the new claim
   "newest rounds stabilised close to the benchmark" may be an artefact of the
   summary statistic. Fix: compute the ONS-implied gain over a matched near-term
   horizon (first ~8–10 years), or compare age-specific assumed rates.

7. **BF magnitude is fragile to σ.** [stats MAJOR] log BF ∝ 1/σ²; σ is estimated
   from 20 calibration points and treated as known. A ±20% error in σ moves the
   UK-male BF from ~20 to ~820. Accounting for σ uncertainty (t-predictive)
   attenuates evidence a further 1.2–1.8×. The precise magnitudes and their Kass
   labels are not robust.

8. **Data-splice does load-bearing work.** [demography + stats] 2019 (the first
   NPP-sourced point) is the single year that swings the headline 61%→49/56%; the
   2024 point is the NPP base year (part-estimated) yet enters the recovery
   average. Both deserve source-consistency and drop-one sensitivity checks.
   Wales (~0.17y source disagreement) is still tabulated at face value.

## Minor / housekeeping

- **Breakpoint circularity + seed-selection** [stats]: the ~2010 cut is chosen
  from the same series whose post-cut behaviour is then tested; the seed
  robustness check retains only the seeds that converge. Qualify "confirmed".
- iid-Normal is misspecified but **conservative** here (annual changes are
  negatively autocorrelated, lag-1 ≈ −0.49), so it does *not* inflate the
  headline evidence — worth stating, as it pre-empts the obvious objection.
- Calibration window still described as both "1990–2010" and "1991–2010".
- M03 figure y-axis is weeks/year while adjacent tables are years/year — flag in
  caption. [repro LOW]
- `references.bib`: `ons_single_year_lifetables` title says "to 2020" but URL
  slug says "to2018"; `ons_expectation_of_life` dated 2026. [repro LOW]
- International decade table remains a `[TODO]` (honestly flagged, but incomplete
  for submission).

## What the referees praised (keep)

- **The bug audit is exemplary** (all three) — the σ⁴ algebra, the invariance of
  the maximising percentage, the honest reporting that the fix *inverts* the
  paper's own prior sex emphasis.
- **Window-sensitivity is shown, not hidden** — more candid than most papers with
  this issue.
- **Reproduction discipline** — 2011–2018 recovers the original 61%/61% exactly.
- **Every quantitative claim reproduces exactly** from raw data; no fabrication;
  committed original figures provably unaltered; all citations resolve; manuscript
  and full book render.
- The **corrected sex contrast is real** (males carry more signal), even if it
  rests on one independent cell.

## Proposed revision plan (prioritised)

**Tier 1 — do before any submission (address the converged majors):**
1. **Recast the estimator honestly.** Either (a) implement a genuine Bayes
   factor (prior over slowdown, integrate), or (b) drop "Bayes factor" and
   present it as what it is: a profile likelihood-ratio / two-sample mean-shift
   comparison, with the `slowdown% = 1 − mean-ratio` identity stated up front.
   Recommendation: (b) is more honest and still useful; frame the contribution as
   a *transparent interim monitoring index*, not a novel estimator.
2. **Downgrade "validation" to "comparison"**, report the 2.5× overshoot and the
   shock/rebound cancellation explicitly, and either pre-register a prospective
   test or drop OOS-validation language.
3. **Handle multiplicity**: report the family as ~4 independent nations (UK
   redundant), state that the male result is one signal replicated by
   construction, and give the LR-test p-values alongside the BF labels.
4. **Fix the international comparison** (re-pull HMD to a common current vintage)
   or remove the relative-severity claim.
5. **Re-do the ONS comparison on a matched near-term horizon** before repeating
   "stabilised near the benchmark".
6. **Add the period-vs-cohort / harvesting caveat with teeth** — raise harvesting
   as the competing explanation for the 2022–24 rebound; restrict claims to
   period e₀ or add a decomposition.

**Tier 2 — strengthens but not blocking:**
7. σ-uncertainty (t-predictive) and a robustness note that iid is conservative.
8. Source-splice sensitivity (drop-2024, NPP-throughout vs lifetables-throughout,
   Wales handled separately).
9. Breakpoint qualification; standardise the 1990/1991 wording.

**Tier 3 — cosmetic:**
10. M03 units caption; bib fields; regenerate the international decade table.

## Reframing option worth considering

The referees jointly point at a cleaner, defensible paper: drop the "Bayes
factor" and "validation" framing entirely and present a **transparent, rapidly
updatable interim index of the UK period-e₀ improvement rate**, honestly a
two-sample mean comparison, whose value is (i) timeliness between biennial NPP
rounds and (ii) the audit-grade reproducibility demonstrated here — with the
five-decade projections-vs-actual figure (the spin-off) as the motivating
exhibit. That paper makes claims it can fully support. The current paper's
weaknesses all stem from claiming a stronger inferential apparatus than a mean
ratio can carry.
