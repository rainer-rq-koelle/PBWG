# PBWG

R tools for preparing international ANS performance benchmarking data from NM
flight-table and APDF warehouse extracts. The package implements ingestion and
decoding, network/airport traffic, punctuality, throughput/load indices, taxi
additional time, and ASMA/DSMA reference and daily-output workflows. Warehouse
extraction itself happens outside PBWG.

## Current method

The approved 2024 ARR and DEP sectors cover 13 European study airports at 40 and
100 NM, including LPPT. Production refreshes load these approved boundaries using
`read_approved_tma_sector_definitions()`; they do not propose sectors or impose a
fixed sector count.

Both TMA reference variants are produced separately:

- `icao_ganp_p20`: 20th percentile.
- `pbwg_avg_p05_p15`: average of the 5th and 15th percentiles.

Sparse reference cells, including N < 5, remain available with their sample
counts. Unmatched analysis movements remain visible in diagnostics and daily
outputs. See the [decision index](inst/TMA-SECTOR-DECISIONS.md) and
[technical note](inst/TN-XSMA.qmd) for the review evidence.

## Refreshing data

One entry point replaces the previous dated refresh scripts:

```sh
Rscript scripts/refresh-eur-data.R scripts/config/meeting-reference.R --check
Rscript scripts/refresh-eur-data.R scripts/config/meeting-reference.R
```

The supplied configuration builds 2024 references only, for all 13 airports and
both variants. It defaults to sibling `__DATA/APDF` archives and a new
`PBWG-data/runs/meeting-reference-2024` output folder. Override `PBWG_SOURCE_ROOT`
and `PBWG_REFRESH_OUTPUT`, or copy the configuration outside the repository and
edit its explicit paths. The entry point works independently of the working directory.

`scripts/config/meeting-tma.R` is a second ready-to-run configuration for the
existing full-year 2023–2025 ASMA/DSMA baseline. Add 2026 only after the new sources
and reporting cut-offs have been checked.

For the verified 2026 archive snapshot, `scripts/config/r2-2026-apdf.R` and
`scripts/config/r2-2026-network.R` define the separate airport and network
cut-offs. The [script inventory](scripts/README.md) describes manifest-pinned R2
downloads, local credentials and the taxi/capacity study-completion workflows.

`products` selects `references`, `tma`, `traffic`, `punctuality`, `throughput`, and/or
`network`. Selecting `tma` also rebuilds the approved 2024 references. Each run
requires a new/empty destination and writes a manifest, source/checksum inventory,
coverage tables, settings, and completion marker. Use only runs marked COMPLETE;
this means processing finished, not that every source month is complete.

NM normally reaches D-1 of extraction. APDF typically lags about six weeks and
can differ by airport. An extraction in late October 2026 may cover September,
or only August for some airports. Record extraction dates, observed coverage,
and source-confirmed cut-offs separately rather than assuming completeness.

The [operator guide](inst/process-guide/data-preparation-process.md) explains
configuration, cut-offs, meeting outputs, validation, and retirement of old data.
The [script inventory](scripts/README.md) identifies the supported tools.

## Validation

```sh
Rscript -e 'devtools::test()'
Rscript -e 'devtools::load_all(); testthat::test_file("scripts/tests/test-refresh-workflow.R", stop_on_failure=TRUE)'
```

Workflow regression tests exercise ZIP/parquet ingestion, UTC boundaries,
ambiguous sources, separate variants, sparse cells, and unmatched movements.
Real-data refreshes provide an additional operational check.

## Data boundaries

Raw extracts belong in `__DATA` or another configured source location; analytical
products belong in `PBWG-data/runs/<run>`. The BRA–EUR and international PBWG
projects also contain historical/project-specific derived products. Read exact
files from a selected completed run's manifest; do not glob across old/new
directories or mix reference variants.

The old 12-airport TMA archive and `*-n5.csv` references predate the approved
method. They must not be used for new refreshes. Existing report copies are not
updated automatically. Retire historical products only after replacement
coverage and consuming report paths have been checked.
