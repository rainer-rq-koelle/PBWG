# Data preparation operator guide

## Inputs

Warehouse extraction happens outside PBWG. Keep original ZIP files unchanged.
The `aviation-data` R2 archive now also supplies individual upstream-prepared
Parquet files. Download selected files through `scripts/cache-aviation-data.R`;
the refresh accepts both local ZIPs and local Parquet files. Downloads are a local
cache, and PBWG products remain separate from the upstream archive.
NM supplies network traffic; APDF supplies airport traffic, punctuality,
15-minute runway throughput, and ASMA/DSMA. Taxi additional time and BLI/PLI are
implemented in the package but are not products of the combined refresh script.

The airport set is EDDF, EDDM, EGKK, EGLL, EHAM, LEBL, LEMD, LFPG, LGAV, LIRF,
LPPT, LSZH, and LTFM. Each airport/year must resolve to one APDF source. LGAV and
LPPT use supplementary 2024 archives in the existing local inventory.

## Source currency and reporting periods

Keep three different dates:

1. `EXTRACTED_ON`: actual extraction date, supplied by the operator.
2. Observed coverage: first/last timestamps and monthly counts found in the data.
   A last observation is not evidence of a complete month.
3. `COMPLETE_THROUGH`: optional source-confirmed coverage, per airport. Leave
   missing when unknown. Unknown extraction dates must also remain missing.

NM usually reaches D-1. APDF generally has about a six-week backlog, varying by
airport. A late-October 2026 extraction may cover September, or only August at
some airports. Do not infer coverage from `pJune`, `latest`, modification times,
or a single late record.

`reporting_end` supplies an explicit ceiling for each analysis year. An airport's
effective ceiling is the earlier of that date and its known COMPLETE_THROUGH.
With unknown completeness, the requested ceiling is used and coverage remains
labelled `unconfirmed`. This permits inspecting new extracts without certifying
partial months.

Boundaries are inclusive UTC dates. Throughput and TMA use runway movement time.
Traffic uses block time with movement-time fallback; punctuality uses block time.
These bases can differ around midnight. Spillovers outside the window are counted
in diagnostics and excluded from outputs.

For equal-period comparisons, set a common ceiling and inspect per-airport
coverage before deciding whether to reduce it. For latest-available NM reporting,
use a separate network-only run with a later ceiling. Do not present unequal
periods as like-for-like comparisons.

## Configure and preflight

Copy `scripts/config/meeting-reference.R` to an operator-owned location. It defines
`config` using `apdf_source()` and `nm_source()`. Use forward slashes on Windows
and macOS. The example builds references from the existing full 2024 archives.

| Setting | Meaning |
| --- | --- |
| `output_dir` | New/empty run directory; never an existing canonical root |
| `products` | `references`, `tma`, `traffic`, `punctuality`, `throughput`, `network` |
| `airports` | All 13 by default; explicit subsets support smoke tests |
| `reference_year` | 2024, the approved sector-review year |
| `analysis_years` | Years to process; required configuration even for references-only |
| `reporting_end` | Named vector, e.g. `c("2026" = "2026-09-30")` |
| `apdf_sources` | YEAR, ICAO, ARCHIVE, MEMBER, EXTRACTED_ON, COMPLETE_THROUGH |
| `nm_sources` | One source per network year, with ICAO = EUR |
| `statfor_rules` | SID 2025 STATFOR workbook, required for network traffic |
| `save_augmented` | FALSE by default; TRUE retains large movement-level TMA files |

For APDF, a missing MEMBER resolves by anchored airport identifier, year, and
parquet extension inside the specified ZIP. Zero/multiple matches stop the run.
Set MEMBER explicitly for other naming schemes. For NM, missing MEMBER reads
all archive members: use non-overlapping parquet partitions, without duplicate
quarterly/annual copies.

For direct Parquet, `ARCHIVE` is the local Parquet path and `MEMBER` is missing.
APDF still requires exactly one file per airport/year. NM permits multiple
Parquet partitions per year and rejects overlapping daily coverage. It does not
mix a yearly ZIP with quarterly Parquet inputs. R2 inputs retain their expected
MD5 and row count, checked before use, plus a copy of each pinned manifest.

APDF schema `fltid-2026-10-30` provides an upstream-standardised `FLTID` alongside
`FLTID_AP`, `FLTID_SOURCE`, and `NM_MATCH_OK`. PBWG preserves these fields in the
prepared input and keeps `AP_C_FLTID` as the original source value. Older extracts
without `FLTID` continue using the legacy rename. No flight-ID matching or
cleaning is repeated downstream. Aggregate output schemas remain unchanged.

```sh
Rscript /path/to/PBWG/scripts/refresh-eur-data.R /path/to/config.R --check
```

Preflight checks configuration, archives/members, approved sector coverage, and
the empty destination. It writes nothing and does not establish record-level
validity or completeness.

## Meeting ASMA/DSMA outputs

Use `products = c("references")` for updated reference tables, or `c("tma")` for
references plus analysis-year outputs. Add the selected analysis-year sources
while retaining all 2024 sources. The example contains commented 2026 settings.

For 2023–2025, configure all three years with December 31 ceilings and explicit
archive rows, including LGAV/LPPT supplements where necessary. For new 2026 data,
use its actual extraction date, chosen ceiling, and any confirmed airport-level
coverage. Do not reuse old May/June paths or 12-airport references.

```sh
Rscript /path/to/PBWG/scripts/refresh-eur-data.R /path/to/config.R
```

The workflow processes one airport at a time, applies approved ARR/DEP boundaries
at 40/100 NM, and builds both `icao_ganp_p20` and `pbwg_avg_p05_p15` references.
N < 5 cells are retained with their counts. Variants are written and applied
separately, avoiding double counting and preserving unmatched movements.

Valid 2024 reference samples must all match a reference. Later years can contain
new runway/class/sector combinations; these remain visible in daily outputs and
`tma-match-coverage.csv`. Their zero reference/additional-time sums do not mean
measured zero additional time. Compute mean additional time over matched
movements and report the unmatched share.

## Outputs and review

- `RUN-STATUS.txt`: RUNNING, FAILED, or COMPLETE. Never consume a failed run.
- `manifest.csv`: relative product paths, row counts, airport/year, variant, MD5.
- `sources.csv`: resolved inputs, extraction/coverage dates, archive MD5.
- `source-quality.csv`: raw row counts and excluded rows whose phase/route cannot
  identify an airport. A conflicting known airport stops the run; it is never
  silently reassigned to the archive's airport.
- `approved-sectors.csv`, `code-and-method.csv`, `run-config.rds`,
  `session-info.txt`: method snapshot, code checksums, configuration, R environment.
- `coverage.csv`: observed/output boundaries, requested/effective ceilings,
  excluded rows, missing dates, days without observations. The latter are
  diagnostic counts, not automatic missing-data classifications.
- `monthly-observations.csv`: observed row counts, including excluded months.
  TMA rows are range-expanded observations, throughput rows are runway bins,
  and basic daily-product rows are aggregates, not individual flights.
- `reference-coverage.csv` and `tma-match-coverage.csv` where applicable.
- Product folders for network/airport traffic, punctuality, throughput, and TMA
  references/daily/optional augmented output.

MD5 provides reproducibility checks, not security. Review all airports, phases,
gaps, late records, both variants, and unmatched shares. A `p` filename suffix
means a partial requested/effective year, not a fixed date: use coverage.csv.
COMPLETE indicates successful processing, not complete source months.

Daily TMA retains sector, class, runway, variant, reference sample size and match
status. Older reports may expect only airport/phase/range/date. Aggregate within
one variant and preserve match counts when adapting those reports; the schema
has changed since the legacy extracts.

When reading references or detailed daily CSVs, explicitly keep runway identifiers
as text: `readr::read_csv(path, col_types = readr::cols(RWY = readr::col_character()))`.
Automatic guessing can turn numeric-looking runways into numbers and make
cross-airport binding fail or discard leading zeros. Use the manifest's exact
files and preserve REF_VARIANT when combining them.

## Replace and retire obsolete products

Legacy `PBWG-data/airport-tma` uses 12 airports, automatic sector proposals and
single-variant `-n5` references. It is superseded for new calculations. Its 2026
TMA data ends in May; updated code alone does not replace those data.

After replacement outputs pass review:

1. Point report/application inputs to exact manifest paths in the completed run.
2. Verify equivalent or deliberately changed airport/year/date coverage and
   explicitly select the intended variant.
3. Remove replaced legacy reference, sector, plot, daily, augmented and aggregate
   files, plus obsolete report copies. Update their manifests at the same time.
   Retain unreplaced years/products.
4. Retire older run folders only once no consumer refers to them.

Do not delete source extracts, approved review evidence, or the sole historical
product copy to make a directory look current. The refresh script does not delete
external data or rewrite unrelated reports automatically.

October 2026 package cleanup removed the dated refresh scripts, early DSMA tests
and preview, Windows incident report, stale status summaries, and duplicate root
ARR parquet. Their history remains in Git. The technical note, approved decisions,
and working-group briefing remain as evidence.
