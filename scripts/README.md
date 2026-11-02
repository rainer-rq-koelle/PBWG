# Script inventory

## Production refresh

- `refresh-eur-data.R CONFIG.R [--check]`: supported NM/APDF refresh entry point.
- `config/meeting-reference.R`: operator configuration example, references only by default.
- `config/meeting-tma.R`: existing full-year 2023–2025 meeting baseline for all 13 airports.
- `lib/refresh-workflow.R`: source resolution, coverage, reference/output orchestration.
- `cache-aviation-data.R CACHE DATASET YEAR [AIRPORTS]`: read-only, manifest-pinned R2 download.
- `lib/aviation-r2.R`: validates upstream manifests and cached file sizes/MD5.
- `config/r2-2026-apdf.R`: 2026 APDF through September, retaining the local 2024 reference inputs.
- `config/r2-2026-network.R`: 2026 NM through 28 October; requires `PBWG_STATFOR_RULES`.
- `tests/test-refresh-workflow.R`: bounded workflow regression tests.
- `tests/test-aviation-r2.R`: cache integrity, snapshot reuse and unsafe-manifest checks.

See `../inst/process-guide/data-preparation-process.md` for instructions. For a
real-data smoke test, select `airports = c("EGLL", "LPPT")`, `products = c("tma")`,
and a new temporary destination. A subset run is not a full study-airport output.

## R2 archive consumer

The `aviation-data` bucket contains upstream-prepared NM/APDF extracts. This
project does not clean, rewrite or upload them. Configure `AVIATION_R2_ACCESS_KEY_ID`,
`AVIATION_R2_SECRET_ACCESS_KEY`, `AVIATION_R2_ENDPOINT`, and `AVIATION_R2_BUCKET`
locally in `.Renviron`; `paws.storage` is required only for downloading.

```sh
Rscript scripts/cache-aviation-data.R .tmp/aviation-data/2026-11-02 apdf 2026 EDDF,EDDM,EGKK,EGLL,EHAM,LEBL,LEMD,LFPG,LGAV,LIRF,LPPT,LSZH,LTFM
Rscript scripts/cache-aviation-data.R .tmp/aviation-data/2026-11-02 nm-flt 2026
Rscript scripts/refresh-eur-data.R scripts/config/r2-2026-apdf.R --check
Rscript scripts/refresh-eur-data.R scripts/config/r2-2026-apdf.R
# Set PBWG_STATFOR_RULES to the local SID 2025 rules workbook before this command.
Rscript scripts/refresh-eur-data.R scripts/config/r2-2026-network.R
```

Use a new cache directory for each extraction snapshot. Existing snapshots retain
their manifest and verify local files before reuse. Set `PBWG_R2_CACHE` to select
another snapshot and `PBWG_REFRESH_OUTPUT` to a new output folder. The dated configs
have explicit 2026 cut-offs: update those deliberately for a later refresh.
`PBWG_AIRPORTS=LSZH` selects a one-airport APDF trial (set a separate output folder).
R2 download errors or checksum mismatches stop processing; a refreshed object must
not be silently paired with an older manifest.

The downstream run retains the upstream manifests, object keys, schema versions,
expected row counts and checksums in `sources.csv`/`source-manifests/`. Upstream
`written_utc` is a manifest publication time, not an extraction date. `complete`
describes the upstream partition and is not automatically promoted to confirmed
monthly completeness. MD5 checks transfer consistency, not authenticity.

## Approved-sector evidence

### Fixed-reference coverage review

`build-tma-coverage-review.R CACHE REFERENCE_RUN OUTPUT` produces coverage counts
for the original 2024 reference inputs and the pinned 2024–2026 APDF archive.
It requires the 13 study airports cached for all three years and a new output
directory. The current study window is explicitly fixed to December 2024/2025
and September 2026. Extend that window deliberately when later data arrive.

```sh
Rscript scripts/build-tma-coverage-review.R .tmp/aviation-data/2026-11-02 outputs/meeting-reference-2024 outputs/tma-coverage-review-2024-2026-20261102
Rscript scripts/validate-tma-coverage-review.R outputs/tma-coverage-review-2024-2026-20261102 outputs/pbwg-refresh-2026-20261102/apdf
Rscript scripts/inspect-tma-coverage-exceptions.R .tmp/aviation-data/2026-11-02 outputs/meeting-reference-2024 outputs/tma-coverage-review-2024-2026-20261102
```

The outputs contain reference-release counts, full available-year and common
January–September comparisons, monthly phase/range details, unmatched keys,
daily reference misses, input inventory and source provenance. Both reference
variants must have identical group keys and sample counts. Neither references
nor production eligibility are changed. Unknown runway labels remain diagnostic
categories, and missing crossing times are not automatically labelled data errors.
Screening bands (<2%, 2–<5%, 5–<10%, ≥10%) require interpretation; they do not
clear or reject results automatically.

The companion `templates/tma-coverage-report.qmd` renders the review tables and a
curated `exception-register.csv` placed in the output directory. Exceptions
record evidence, interpretation and follow-up; they are not an automatic repair
queue. `tests/test-tma-coverage.R` checks denominator and classification edge cases.

- `export-approved-tma-sectors.R`: exports both phases from reviewed sources.
- `build-dsma-sector-review.R`: rebuilds the technical note's diagnostic evidence.
  This is a review tool, not a production refresh. PBWG_APDF_* environment
  variables override its machine-specific input paths.

## Study completion workflow

The completion workflow uses the pinned 2024–2026 archive and creates a new run
directory. It leaves existing reference values, prior outputs and R2 objects
unchanged. The 2026 airport cut-off is explicitly September.

```sh
Rscript scripts/complete-study-airports.R .tmp/aviation-data/2026-11-02 NEW_OUTPUT
Rscript scripts/build-study-capacity-loads.R NEW_OUTPUT [CAPACITY_OVERRIDES_CSV]
Rscript scripts/backfill-study-products.R .tmp/aviation-data/2026-11-02 outputs/meeting-reference-2024 NEW_OUTPUT
Rscript scripts/audit-study-bundle.R NEW_OUTPUT
python3 scripts/validate-study-completion.py NEW_OUTPUT
```

- `complete-study-airports.R` prepares both 2024 taxi reference variants, daily
  applications for 2024–2026, coverage/miss/support tables and observed hourly
  throughput. Optional third argument selects comma-separated airports for a pilot.
  Taxi defaults remain explicit: `min_n=5`, retain sparse reference rows, apply
  eligible references only, and exclude actual missing/empty reference keys.
- `build-study-capacity-loads.R` assembles historical capacity evidence and optional
  explicitly sourced overrides, then saves BLI/PLI and hourly/daily load inputs.
  Overrides require `ICAO,YEAR,MAX_CAP,SOURCE,STATUS`. It never silently fills capacity
  gaps. Preserve a previous `capacity-loads` folder before another capacity scenario.
  Public Airport Corner values are cross-check evidence, not automatic overrides.
  Agreement with an older value does not establish historical applicability;
  discrepancies trigger review. Cross-check-only overrides are rejected.
  Indices use observed hours with movements; missing capacity yields missing indices.
- `backfill-study-products.R` fills the historical ASMA/DSMA variant/airport gaps
  and two missing traffic slices discovered in this study. This is a study-specific
  adapter, with checks against the saved 2024–2026 coverage review.
- `audit-study-bundle.R` inventories current and existing historical products,
  distinguishing availability, date coverage, legacy validation status and missing
  capacities. It also recognises the existing network-total corrections without
  claiming their missing breakdowns are complete.
- `validate-study-completion.py` requires pandas/numpy and independently checks
  the new taxi/load counts, LSZH historical comparison and 2026 hourly reconciliation.

The completed snapshot and its qualification register are in
`outputs/study-completion-2024-2026-20261102/`. Source/update dates are not effective
dates, and calculation completion is not a release decision.

### Reference maintenance experiment

`analyse-reference-maintenance.R STUDY_OUTPUT NEW_OUTPUT` tests a targeted taxi
reference supplement for the six priority airports in the current study. It
pools 2024–2025 observations only for identified combinations without an eligible
2024 reference, retaining existing eligible references unchanged. Both percentile
variants are prepared, with supplement support scenarios of N ≥ 5 and N ≥ 20.
Coverage in 2024/2025 is retrospective and in-sample; 2026 is held out from
estimation. Saved monthly counts must reproduce the original coverage run.

The output includes candidate values, monthly group exposure, recovered and
remaining misses, source hashes and reconciliation checks. It is an application
experiment, not an automatic recalibration or package eligibility change.
Operational identity, sample representativeness, reference-value stability and
additional-time effects require review before adopting a supplement. Meeting
papers, slides and their authoring scripts remain local application artefacts.

## Historical project adapter

- `build-bra-eur-bli-pli-report-inputs.R`: BRA-EUR-2025 BLI/PLI and ordered-throughput
  report inputs. It relies on that project's throughput/capacity history and is
  not a general 2026 refresh tool.

The dated 2026 refresh scripts, automatic-sector TMA builder, early DSMA tests,
separate smoke workflow, ARR-only exporter, and Windows diagnostic script have
been retired. Use Git history for historical reconstruction.
