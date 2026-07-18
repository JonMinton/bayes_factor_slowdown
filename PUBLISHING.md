# Publication strategy

Assessment of preprint and journal options for the refactored project, given
its current framing (a **detection framework** for regime change in longevity
improvement, positioned as the quantitative complement to Hiam, Minton, Dorling
& McKee 2023, *British Medical Bulletin*). Recommendations, not decisions.

## Two outputs

The material naturally splits into two publishable pieces:

1. **Main paper** — the detection framework (`manuscript/manuscript.qmd`):
   "how many years of stalled improvement does it take to conclude the
   longevity DGP has changed?", with the storm/power/early-warning analysis,
   the ONS-projection evaluation, and the corrected-magnitude estimate as a
   secondary result.
2. **Short companion** — the five-decade projections-vs-actual fan figure
   (`spinoff/forecasting_note.qmd`), a legible standalone exhibit on how
   official forecasters repeatedly misjudged which regime they were in.

## Preprint

**Recommendation: medRxiv**, with an OSF project (SocArXiv) as the umbrella for
data + code.

- **medRxiv** fits the population-health framing and the Hiam continuity (that
  paper is in a medical outlet; McKee/Dorling/Hiam's stalling work lives in the
  health literature). It gives the paper a citable DOI and reaches the public
  health audience quickly. (Requires a health-sciences framing, which the paper
  has.)
- **OSF / SocArXiv** as the home for the reproducible artefact: the Quarto
  project, `R/` engine, data, and the bug-fix audit. The audit and full
  reproducibility are a selling point — lead with them in the OSF project
  description. SocArXiv also suits the demographic-methods angle if medRxiv's
  health framing feels too narrow.
- Post the preprint only after the Tier-1 revision checklist below is clear.

## Main-paper journal targets (ranked)

1. **Demographic Research** (open access, Max Planck Institute for Demographic
   Research). *Best fit.* Fast, rigorous, open, and its readership is exactly
   right for a detection-of-regime-change method plus an evaluation of official
   population projections. No APC. The window-sensitivity honesty and
   reproducibility will be appreciated rather than penalised.
2. **Journal of the Royal Statistical Society, Series A (Statistics in
   Society)**. Strong fit for the *method + official-statistics* angle: a
   transparent early-warning test for ONS-style monitoring, with the
   projection-accuracy evaluation. Higher bar; would reward sharpening the
   detection-power contribution and the pre-registration proposal.
3. **Journal of Epidemiology & Community Health (JECH)**. The home of the UK
   stalling literature and the natural audience continuous with Hiam et al.
   Best if the framing leans health-policy (early warning for population-health
   monitoring) rather than method. Would want the period-vs-cohort/harvesting
   caveats front and centre.
4. **Population Studies** / **European Journal of Population** — solid
   demography alternatives if 1 is unavailable.

Actuarial outlets (e.g. *British Actuarial Journal*) are a plausible secondary
audience given the ONS-projection angle, but the framing would need to shift
toward longevity-risk practice.

## Companion-piece target

**Significance** (RSS/ASA magazine) is an excellent home for the fan figure as a
short, visual piece — it publishes exactly this kind of legible,
one-figure-tells-the-story item for a broad statistical audience. A blog/Substack
version is a zero-friction alternative.

## Readiness checklist (before submission)

Done in the refactor:
- [x] Honest inferential framing (LR test, not "Bayes factor"); Kass labels removed.
- [x] Multiplicity / one-signal (England≈UK) stated.
- [x] Out-of-sample downgraded to a persistence comparison.
- [x] Period-vs-cohort / harvesting caveat with teeth.
- [x] International comparison refreshed on current HMD (UK 4th slowest of 20).
- [x] Detection framework as the contribution; Hiam positioning.
- [x] Bug-fix audit appendix; full reproducibility.

Remaining before submission:
- [ ] **Matched-horizon ONS recompute** (referee finding #6): recompute the
      ONS-implied gains over a near-term horizon before leaning on "newest rounds
      stabilised near the benchmark". (Open [TODO] in the manuscript.)
- [ ] **Tier-2 robustness** (peer_review_synthesis.md): σ-uncertainty
      (t-predictive), source-splice sensitivity (drop-2024; NPP-throughout vs
      lifetables-throughout; Wales separately), breakpoint qualification.
- [ ] **Pre-registration** of the prospective early-warning test — this is what
      turns the retrospective early-warning result into a genuine forward claim,
      and is the paper's strongest "so what". Worth doing as a companion OSF
      registration.
- [ ] Regenerate `analysis/01` (book chapter) on the refreshed HMD data for
      consistency with the manuscript's international section.
- [ ] Cosmetic (Tier-3): M03 figure units caption; `ons_single_year_lifetables`
      bib title/URL year; standardise any residual 1990/1991 wording.
- [ ] Author decisions: author list/order, funding/COI, data-availability
      statement (all inputs are public; note HMD account requirement).
